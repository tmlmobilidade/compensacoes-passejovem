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
#   - <ano>_<mm>_dados_limpos.csv
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


  # ── 1. Ficheiro principal PasseJovem ─────────────────────────────────────
  # Pode ser CSV (antes de julho 2026) ou XLSX (julho 2026 em diante)
  f_pj <- list.files(paths$input,
                     pattern = "PasseJovem",
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
                       pattern = "TML_.*\\.(xlsx|xls)$",
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
                     pattern = "Complemento_Azambuja",
                     full.names = TRUE,
                     ignore.case = TRUE)
  if (length(f_az) == 0) {
    warning("Ficheiro Complemento Azambuja nao encontrado; a continuar sem complemento.")
    dt_az <- NULL
  } else {
    if (length(f_az) > 1) {
      warning("Mais do que um ficheiro Azambuja encontrado; a usar o primeiro.")
      f_az <- f_az[1]
    }
    message("  Lendo: ", basename(f_az))
    dt_az <- read_complemento_azambuja(f_az)
  }


  # ── 4. Combinar PasseJovem + Cascais ─────────────────────────────────────
  if (!is.null(dt_casc)) {
    # Garantir que ambas as tabelas têm as mesmas colunas antes de fazer bind
    cols_pj   <- names(dt_pj)
    cols_casc <- names(dt_casc)

    # Adicionar colunas que faltam no Cascais (ex: doc_number, doc_type)
    for (col in setdiff(cols_pj, cols_casc)) {
      dt_casc[, (col) := NA]
    }
    # Remover colunas extra do Cascais que não existam no main
    cols_extra <- setdiff(cols_casc, cols_pj)
    if (length(cols_extra) > 0) dt_casc[, (cols_extra) := NULL]

    dt_main <- rbindlist(list(dt_pj, dt_casc), use.names = TRUE)
  } else {
    dt_main <- dt_pj
  }


  # ── 4b. Preencher NIFs em falta via repescagem ───────────────────────────
  # data/auxiliar/nif_repescagem2024.csv contém NIFs identificados para cartões
  # cujo fiscal_number não consta nos dados primários.
  # Preenche o fiscal_number apenas quando este se encontra vazio (NA),
  # antes de realizar a manipulação dos dados.
  f_rep <- file.path(paths$auxiliar, "nif_repescagem2024.csv")
  if (file.exists(f_rep)) {
    dt_rep <- fread(f_rep, colClasses = "character")
    setnames(dt_rep, tolower(names(dt_rep)))
    dt_rep[, fiscal_number := normalizar_nif(fiscal_number)]
    dt_rep <- dt_rep[!is.na(fiscal_number) & nzchar(trimws(card_serial_number))]

    # Garantir tipo character para matching
    dt_rep[, card_serial_number := as.character(card_serial_number)]
    dt_main[, card_serial_number := as.character(card_serial_number)]

    # Identificar cartões que vão ser preenchidos
    cartoes_preenchidos <- intersect(dt_main[is.na(fiscal_number), card_serial_number], dt_rep$card_serial_number)
    n_cartoes <- length(unique(cartoes_preenchidos))

    # Preencher apenas onde fiscal_number está em falta
    n_vazios_antes <- sum(is.na(dt_main$fiscal_number))
    dt_main[dt_rep, on = .(card_serial_number),
            fiscal_number := fifelse(is.na(fiscal_number), i.fiscal_number, fiscal_number)]
    n_recuperados <- n_vazios_antes - sum(is.na(dt_main$fiscal_number))
    message(sprintf("  Repescagem de NIF aplicada: %d registos preenchidos (%d cartões únicos)",
                    n_recuperados, n_cartoes))

    if (!is.null(dt_az)) {
      dt_az[, card_serial_number := as.character(card_serial_number)]
      dt_az[dt_rep, on = .(card_serial_number),
            fiscal_number := fifelse(is.na(fiscal_number), i.fiscal_number, fiscal_number)]
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
    dt_main[mask_crianca, `:=`(
      ticket_operator_code = 31L,
      ticket_code          = 1020L,
      ticket_name          = "Navegante 12",
      price                = 0,
      price_ticket_base    = 40
    )]
    message(sprintf("  Perfil crianca corrigido: %d registos", sum(mask_crianca)))
  }

  # 5c. Remover registos sem year_dist (dados inválidos)
  dt_main <- dt_main[!is.na(year_dist)]

  # 5d. Verificar se existem registos de outros meses (aviso apenas)
  n_outros <- nrow(dt_main[year_dist != ano | month_dist != mes])
  if (n_outros > 0)
    warning(sprintf("%d registos com ano/mes diferente de %d/%02d", n_outros, ano, mes))


  # ── 6. Aplicar correções manuais ─────────────────────────────────────────

  # 6a. Correções de data de nascimento (data/auxiliar/data_nascimento_correct.csv)
  #     Corrige datas de nascimento erradas exportadas pelo sistema ITS.
  f_dn_corr <- file.path(paths$auxiliar, "data_nascimento_correct.csv")
  if (file.exists(f_dn_corr)) {
    dt_dn <- fread(f_dn_corr, sep = ";", colClasses = "character")
    dt_dn[, dn_corrigida := parse_date_safe(dn_corrigida)]
    dt_dn <- dt_dn[!is.na(dn_corrigida)]  # ignorar linhas sem correção
    dt_main[dt_dn, on = .(card_serial_number), birth_date := i.dn_corrigida]
    message(sprintf("  Correcoes de data de nascimento aplicadas: %d registos", nrow(dt_dn)))
  }

  # 6b. Correções de NIF (data/auxiliar/nif_correct.csv)
  #     Formato: card_serial_number; birth_date; fiscal_number
  #     Aplica tanto o NIF corrigido como a data de nascimento (quando presente),
  #     pois NIFs errados estão muitas vezes associados a datas de nascimento erradas.
  f_nif_corr <- file.path(paths$auxiliar, "nif_correct.csv")
  if (file.exists(f_nif_corr)) {
    dt_nif <- fread(f_nif_corr, sep = ";", colClasses = "character")
    dt_nif[, fiscal_number   := normalizar_nif(fiscal_number)]
    dt_nif[, birth_date_corr := parse_date_safe(birth_date)]

    # Aplicar NIF corrigido
    dt_nif_nif <- dt_nif[!is.na(fiscal_number)]
    if (nrow(dt_nif_nif) > 0)
      dt_main[dt_nif_nif, on = .(card_serial_number), fiscal_number := i.fiscal_number]

    # Aplicar data de nascimento corrigida (quando presente no ficheiro nif_correct)
    dt_nif_dn <- dt_nif[!is.na(birth_date_corr)]
    if (nrow(dt_nif_dn) > 0)
      dt_main[dt_nif_dn, on = .(card_serial_number), birth_date := i.birth_date_corr]

    message(sprintf("  Correcoes de NIF aplicadas: %d registos", nrow(dt_nif_nif)))
  }

  # 6c. Recalcular age a partir da birth_date corrigida
  #     Após aplicar nif_correct e data_nascimento_correct, a birth_date pode ter
  #     mudado (ex: 2024-10-01 -> 2004-12-26) mas a coluna age continua com o
  #     valor original exportado pelo ITS. Recalculamos age para todos os registos
  #     onde birth_date está preenchida, usando o critério do sistema ITS:
  #     diferença em anos completos entre birth_date e o 1º dia do mês de referência.
  ref_date <- as.Date(sprintf("%d-%02d-01", ano, mes))
  dt_main[!is.na(birth_date),
          age := as.integer(
            floor(as.numeric(difftime(ref_date, birth_date, units = "days")) / 365.25)
          )]
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

  dt_main[age == 25L & lubridate::month(birth_date) == mes, age := 24L]
  dt_main[age == 3L  & lubridate::month(birth_date) == mes, age := 4L]

  message(sprintf("  Total de registos limpos: %d", nrow(dt_main)))


  # ── 7. Gravar ficheiros intermédios ──────────────────────────────────────
  out_limpos <- file.path(paths$output, output_filename(ano, mes, "dados_limpos"))
  write_csv_pj(dt_main, out_limpos)
  message(sprintf("  Gravado: %s", basename(out_limpos)))

  if (!is.null(dt_az)) {
    out_az <- file.path(paths$output, output_filename(ano, mes, "complemento_azambuja"))
    write_csv_pj(dt_az, out_az)
    message(sprintf("  Gravado: %s", basename(out_az)))
  }

  message("[01] Concluido.\n")
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
