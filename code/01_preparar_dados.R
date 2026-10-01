# =============================================================================
# 01_preparar_dados.R
# Lê, limpa e combina os ficheiros de input para um dado mês.
#
# Ficheiros de entrada (em data/<ano>_<mm>_<mes>/input/):
#   - <ano><mm>_PasseJovem_ValidaçõesLimpas.csv  (ou .xlsx a partir de julho 2026)
#   - TML_<mes><ano>.xlsx                          (dados Cascais)
#   - <ano><mm>_Complemento_Azambuja.csv           (complemento Azambuja, opcional)
#
# Ficheiros de saída (em data/<ano>_<mm>_<mes>/output/):
#   - <ano>_<mm>_limpos_preagregacao.csv
#   - <ano>_<mm>_complemento_azambuja.csv  (se existir)
#
# Também aplica a repescagem e correções manuais de:
#   - data/auxiliar/nif_repescagem2024.csv      (repescagem de NIFs em falta)
#   - data/auxiliar/nif_correct.csv             (NIFs errados)
#   - data/auxiliar/data_nascimento_correct.csv  (datas de nascimento erradas)
#
# Uso:
#   source("code/01_preparar_dados.R")
#   preparar_dados(ano = 2026, mes = 7)
# =============================================================================

source(file.path(dirname(sys.frame(1)$ofile), "00_config.R"),
       local = TRUE, chdir = FALSE)


# =============================================================================
# Função principal
# =============================================================================

#' Lê e combina os ficheiros de input para o mês indicado, aplica limpezas
#' e correções manuais, e grava os ficheiros intermédios de output.
#'
#' @param ano  Ano de referência (ex: 2026)
#' @param mes  Mês de referência (1–12)
#' @param raiz Raiz do projecto (detectada automaticamente)
#' @return Lista invisível com os data.tables: passe_jovem, azambuja
preparar_dados <- function(ano, mes, raiz = here_root()) {

  paths <- month_paths(ano, mes, raiz)
  stopifnot(dir.exists(paths$input))
  dir.create(paths$output, recursive = TRUE, showWarnings = FALSE)

  message(sprintf("[01] A preparar dados para %d/%02d ...", ano, mes))
  t0 <- proc.time()


  # ── 1. Ficheiro principal PasseJovem ─────────────────────────────────────
  # Pode ser CSV (antes de julho 2026) ou XLSX (julho 2026 em diante)
  f_pj <- list.files(paths$input,
                     pattern    = "PasseJovem",
                     full.names = TRUE,
                     ignore.case = TRUE)

  if (length(f_pj) == 0)
    stop("Ficheiro PasseJovem nao encontrado em: ", paths$input)
  if (length(f_pj) > 1) {
    warning("Mais do que um ficheiro PasseJovem encontrado; a usar o primeiro.")
    f_pj <- f_pj[1]
  }

  message("  Lendo: ", basename(f_pj))
  dt_pj <- read_passe_jovem(f_pj)


  # ── 2. Ficheiro Cascais (TML_*.xlsx) ─────────────────────────────────────
  f_casc <- list.files(paths$input,
                       pattern    = "TML_.*\\.(xlsx|xls)$",
                       full.names = TRUE,
                       ignore.case = TRUE)

  if (length(f_casc) == 0) {
    warning("Ficheiro Cascais (TML_*.xlsx) nao encontrado; a continuar sem dados de Cascais.")
    dt_casc <- NULL
  } else {
    if (length(f_casc) > 1) {
      warning("Mais do que um ficheiro Cascais encontrado; a usar o primeiro.")
      f_casc <- f_casc[1]
    }
    message("  Lendo: ", basename(f_casc))
    dt_casc <- read_cascais(f_casc, ano, mes)
  }


  # ── 3. Complemento Azambuja ───────────────────────────────────────────────
  f_az <- list.files(paths$input,
                     pattern    = "Complemento_Azambuja",
                     full.names = TRUE,
                     ignore.case = TRUE)

  if (length(f_az) == 0) {
    # Fallback: segunda folha do XLSX principal (formato julho 2026+)
    ext_pj <- tolower(tools::file_ext(f_pj))
    if (ext_pj %in% c("xlsx", "xls")) {
      sheets_pj <- tryCatch(readxl::excel_sheets(f_pj), error = function(e) character(0))
      az_sheet  <- grep("Azambuja", sheets_pj, value = TRUE, ignore.case = TRUE)
      if (length(az_sheet) > 0) {
        message("  Complemento Azambuja encontrado na folha '", az_sheet[1],
                "' de ", basename(f_pj))
        dt_az <- read_complemento_azambuja_xlsx(f_pj, sheet = az_sheet[1])
      } else {
        warning("Ficheiro Complemento Azambuja nao encontrado; a continuar sem complemento.")
        dt_az <- NULL
      }
    } else {
      warning("Ficheiro Complemento Azambuja nao encontrado; a continuar sem complemento.")
      dt_az <- NULL
    }
  } else {
    if (length(f_az) > 1) {
      warning("Mais do que um ficheiro Azambuja encontrado; a usar o primeiro.")
      f_az <- f_az[1]
    }
    message("  Lendo: ", basename(f_az))
    dt_az <- read_complemento_azambuja(f_az)
  }


  # ── 4. Combinar PasseJovem + Cascais ─────────────────────────────────────
  # bind_rows() alinha automaticamente por nome de coluna e preenche com NA
  # as colunas que existam num tibble mas não no outro (ex: doc_number, doc_type)
  if (!is.null(dt_casc)) {
    dt_main <- bind_rows(dt_pj, dt_casc) |> as.data.table()
  } else {
    dt_main <- dt_pj
  }

  # Garantir que card_serial_number é character (necessário para os joins abaixo)
  dt_main <- dt_main |>
    mutate(card_serial_number = as.character(card_serial_number)) |>
    as.data.table()


  # ── 4b. Preencher NIFs em falta via repescagem ───────────────────────────
  # data/auxiliar/nif_repescagem2024.csv contém NIFs identificados para cartões
  # cujo fiscal_number não consta nos dados primários.
  # Faz um left_join pelo número de cartão e preenche apenas onde o NIF está a NA.
  f_rep <- file.path(paths$auxiliar, "nif_repescagem2024.csv")
  if (file.exists(f_rep)) {

    # Ler tabela de repescagem e limpar com um pipe dplyr
    dt_rep <- fread(f_rep, colClasses = "character") |>
      rename_with(tolower) |>
      mutate(
        fiscal_number      = normalizar_nif(fiscal_number),
        card_serial_number = as.character(card_serial_number)
      ) |>
      filter(!is.na(fiscal_number), nzchar(trimws(card_serial_number))) |>
      distinct(card_serial_number, .keep_all = TRUE)

    # Contar quantos cartões sem NIF vão receber um NIF da repescagem
    cartoes_na <- dt_main |> filter(is.na(fiscal_number)) |> pull(card_serial_number)
    n_cartoes  <- length(unique(intersect(cartoes_na, dt_rep$card_serial_number)))

    # Preencher fiscal_number apenas onde está a NA (coalesce: mantém o original se existir)
    n_vazios_antes <- sum(is.na(dt_main$fiscal_number))
    dt_main <- dt_main |>
      left_join(
        dt_rep |> select(card_serial_number, fiscal_number_rep = fiscal_number),
        by = "card_serial_number"
      ) |>
      mutate(
        fiscal_number = coalesce(fiscal_number, fiscal_number_rep)
      ) |>
      select(-fiscal_number_rep) |>
      as.data.table()
    n_recuperados <- n_vazios_antes - sum(is.na(dt_main$fiscal_number))

    message(sprintf("  Repescagem de NIF aplicada: %d registos preenchidos (%d cartões únicos)",
                    n_recuperados, n_cartoes))

    # Aplicar a mesma repescagem ao Azambuja, se existir
    if (!is.null(dt_az)) {
      dt_az <- dt_az |>
        mutate(card_serial_number = as.character(card_serial_number)) |>
        left_join(
          dt_rep |> select(card_serial_number, fiscal_number_rep = fiscal_number),
          by = "card_serial_number"
        ) |>
        mutate(fiscal_number = coalesce(fiscal_number, fiscal_number_rep)) |>
        select(-fiscal_number_rep) |>
        as.data.table()
    }
  }


  # ── 5. Limpeza dos dados ──────────────────────────────────────────────────

  # 5a. Remover bilhetes LX-BT (não elegíveis para compensação)
  n_antes <- nrow(dt_main)
  dt_main <- remove_lxbt(dt_main)
  message(sprintf("  Registos LX-BT removidos: %d", n_antes - nrow(dt_main)))

  # 5b. Corrigir o "perfil de criança" — linha sem operador/bilhete
  #     O sistema ITS gera uma linha extra por cartão para crianças (<12 anos)
  #     com ticket_operator_code e ticket_code a NA.
  #     Esta linha deve ser tratada como Navegante 12 (código 1020).
  mask_crianca <- is.na(dt_main$ticket_operator_code) & is.na(dt_main$ticket_code)
  if (any(mask_crianca)) {
    dt_main <- dt_main |>
      mutate(
        ticket_operator_code = if_else(mask_crianca, 31L,          ticket_operator_code),
        ticket_code          = if_else(mask_crianca, 1020L,         ticket_code),
        ticket_name          = if_else(mask_crianca, "Navegante 12", ticket_name),
        price                = if_else(mask_crianca, 0,              price),
        price_ticket_base    = if_else(mask_crianca, 40,             price_ticket_base)
      ) |>
      as.data.table()
    message(sprintf("  Perfil crianca corrigido: %d registos", sum(mask_crianca)))
  }

  # 5c. Remover registos sem year_dist (dados inválidos)
  dt_main <- dt_main |>
    filter(!is.na(year_dist))

  # 5d. Verificar se existem registos de outros meses (aviso apenas)
  n_outros <- dt_main |>
    filter(year_dist != ano | month_dist != mes) |>
    nrow()
  if (n_outros > 0)
    warning(sprintf("%d registos com ano/mes diferente de %d/%02d", n_outros, ano, mes))


  # ── 6. Aplicar correções manuais ─────────────────────────────────────────

  # 6a. Correções de data de nascimento (data/auxiliar/data_nascimento_correct.csv)
  #     Corrige datas de nascimento erradas exportadas pelo sistema ITS.
  f_dn_corr <- file.path(paths$auxiliar, "data_nascimento_correct.csv")
  if (file.exists(f_dn_corr)) {
    dt_dn <- fread(f_dn_corr, sep = ";", colClasses = "character") |>
      mutate(dn_corrigida = parse_date_safe(dn_corrigida)) |>
      filter(!is.na(dn_corrigida)) |>
      distinct(card_serial_number, .keep_all = TRUE)    # garante chave única para o join

    dt_main <- dt_main |>
      left_join(dt_dn |> select(card_serial_number, dn_corrigida), by = "card_serial_number") |>
      mutate(birth_date = coalesce(dn_corrigida, birth_date)) |>
      select(-dn_corrigida) |>
      as.data.table()
    message(sprintf("  Correcoes de data de nascimento aplicadas: %d registos", nrow(dt_dn)))
  }

  # 6b. Correções de NIF (data/auxiliar/nif_correct.csv)
  #     Formato: card_serial_number; birth_date; fiscal_number
  #     Aplica tanto o NIF corrigido como a data de nascimento (quando presente),
  #     pois NIFs errados estão muitas vezes associados a datas de nascimento erradas.
  f_nif_corr <- file.path(paths$auxiliar, "nif_correct.csv")
  if (file.exists(f_nif_corr)) {
    dt_nif <- fread(f_nif_corr, sep = ";", colClasses = "character") |>
      mutate(
        fiscal_number   = normalizar_nif(fiscal_number),
        birth_date_corr = parse_date_safe(birth_date)
      )

    # Aplicar NIF e data de nascimento corrigidos num único left_join
    # (coalesce: o valor corrigido substitui o original apenas quando existe)
    dt_nif_join <- dt_nif |>
      filter(!is.na(fiscal_number) | !is.na(birth_date_corr)) |>
      distinct(card_serial_number, .keep_all = TRUE) |>   # garante chave única para o join
      select(card_serial_number, fiscal_number_corr = fiscal_number, birth_date_corr)

    n_nif_corr <- dt_nif |> filter(!is.na(fiscal_number)) |> nrow()

    dt_main <- dt_main |>
      left_join(dt_nif_join, by = "card_serial_number") |>
      mutate(
        fiscal_number = coalesce(fiscal_number_corr, fiscal_number),
        birth_date    = coalesce(birth_date_corr,    birth_date)
      ) |>
      select(-fiscal_number_corr, -birth_date_corr) |>
      as.data.table()

    message(sprintf("  Correcoes de NIF aplicadas: %d registos", n_nif_corr))
  }

  # 6c. Recalcular age a partir da birth_date corrigida
  #     Após aplicar nif_correct e data_nascimento_correct, a birth_date pode ter
  #     mudado mas a coluna age continua com o valor original do ITS.
  #     Recalculamos age para todos os registos onde birth_date está preenchida,
  #     usando o critério do ITS: diferença em anos completos até ao 1º do mês de referência.
  ref_date <- as.Date(sprintf("%d-%02d-01", ano, mes))
  dt_main <- dt_main |>
    mutate(
      age = if_else(
        !is.na(birth_date),
        as.integer(floor(as.numeric(difftime(ref_date, birth_date, units = "days")) / 365.25)),
        age
      )
    ) |>
    as.data.table()
  message("  Idade recalculada a partir da birth_date corrigida.")

  # 6d. Correcao pontual da idade para casos limite (mes de nascimento = mes de referencia)
  #
  #     O sistema ITS calcula a idade ignorando o dia (usa ano e mes).
  #     Quando o mes de nascimento coincide com o mes de referencia, pode
  #     devolver a idade errada por 1 unidade. Corrigimos dois casos:
  #
  #     Regra 1 — limite superior: se age == 25 E nasceu no mes de referencia,
  #       a pessoa ainda nao fez 25 anos completos -> corrigir para age = 24.
  #
  #     Regra 2 — limite inferior: se age == 3 E nasceu no mes de referencia,
  #       a pessoa ja fez 4 anos neste mes -> corrigir para age = 4.
  dt_main <- dt_main |>
    mutate(
      age = if_else(age == 25L & lubridate::month(birth_date) == mes, 24L, age),
      age = if_else(age == 3L  & lubridate::month(birth_date) == mes, 4L,  age)
    ) |>
    as.data.table()

  message(sprintf("  Total de registos limpos: %d", nrow(dt_main)))


  # ── 7. Gravar ficheiros intermédios ──────────────────────────────────────
  out_limpos <- file.path(paths$output, output_filename(ano, mes, "limpos_preagregacao"))
  write_csv_pj(dt_main, out_limpos)
  message(sprintf("  Gravado: %s", basename(out_limpos)))

  if (!is.null(dt_az)) {
    out_az <- file.path(paths$output, output_filename(ano, mes, "complemento_azambuja"))
    write_csv_pj(dt_az, out_az)
    message(sprintf("  Gravado: %s", basename(out_az)))
  }

  elapsed <- round((proc.time() - t0)[["elapsed"]], 1)
  message(sprintf("[01] Concluido em %.1f segundos.\n", elapsed))
  invisible(list(passe_jovem = dt_main, azambuja = dt_az))
}


# -----------------------------------------------------------------------------
# Execução directa: Rscript code/01_preparar_dados.R <ano> <mes>
# -----------------------------------------------------------------------------
if (!interactive() && identical(sys.frames(), list())) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2) {
    preparar_dados(ano = as.integer(args[1]), mes = as.integer(args[2]))
  } else {
    cat("Uso: Rscript code/01_preparar_dados.R <ano> <mes>\n")
    cat("Ex:  Rscript code/01_preparar_dados.R 2026 7\n")
  }
}
