# =============================================================================
# 01_preparar_dados.R
# Lê, combina e limpa os ficheiros de input para um dado mês.
# Produz: data/<ano>_<mm>_<mes>/output/<ano>_<mm>_dados_limpos.csv
#
# Uso:
#   source("R/01_preparar_dados.R")
#   preparar_dados(ano = 2026, mes = 5)
# =============================================================================

source(file.path(dirname(sys.frame(1)$ofile), "00_utils.R"),
       local = TRUE, chdir = FALSE)


# -----------------------------------------------------------------------------
# Função principal
# -----------------------------------------------------------------------------

#' Lê e combina os três ficheiros de input (PasseJovem ITS + Cascais + Azambuja)
#' para o mês indicado, aplica limpezas iniciais e grava o resultado.
#'
#' @param ano  integer — ano de referência (ex: 2026)
#' @param mes  integer — mês de referência (1-12)
#' @param raiz character — raiz do projecto (detectada automaticamente)
#' @return data frame com os dados combinados e limpos
preparar_dados <- function(ano, mes, raiz = here_root()) {

  paths <- month_paths(ano, mes, raiz)
  stopifnot(dir.exists(paths$input))
  dir.create(paths$output, recursive = TRUE, showWarnings = FALSE)

  message(sprintf("[01] A preparar dados para %d/%02d ...", ano, mes))

  # ── 1. Ficheiro principal ITS ──────────────────────────────────────────────
  f_pj <- list.files(paths$input, pattern = "PasseJovem", full.names = TRUE)
  if (length(f_pj) == 0)
    stop("Ficheiro PasseJovem não encontrado em: ", paths$input)
  if (length(f_pj) > 1) {
    warning("Mais do que um ficheiro PasseJovem encontrado; a usar o primeiro.")
    f_pj <- f_pj[1]
  }
  message("  Lendo: ", basename(f_pj))
  df_pj <- read_passe_jovem(f_pj)

  # ── 2. Ficheiro Cascais ────────────────────────────────────────────────────
  f_casc <- list.files(paths$input, pattern = "\\.(xlsx|xls)$",
                       full.names = TRUE, ignore.case = TRUE)
  # Excluir eventuais ficheiros de output que estejam na pasta input
  f_casc <- f_casc[!grepl("comNIF|semNIF|reporte|auxiliar", f_casc,
                           ignore.case = TRUE)]
  if (length(f_casc) == 0) {
    warning("Ficheiro Cascais (xlsx) não encontrado em: ", paths$input,
            " — a continuar sem dados de Cascais.")
    df_casc <- NULL
  } else {
    if (length(f_casc) > 1) {
      warning("Mais do que um ficheiro xlsx encontrado; a usar o primeiro.")
      f_casc <- f_casc[1]
    }
    message("  Lendo: ", basename(f_casc))
    df_casc <- read_cascais(f_casc, ano, mes)
  }

  # ── 3. Complemento Azambuja ────────────────────────────────────────────────
  f_az <- list.files(paths$input, pattern = "Complemento_Azambuja",
                     full.names = TRUE)
  if (length(f_az) == 0) {
    warning("Ficheiro Complemento Azambuja não encontrado em: ", paths$input,
            " — a continuar sem complemento.")
    df_az <- NULL
  } else {
    if (length(f_az) > 1) {
      warning("Mais do que um ficheiro Azambuja encontrado; a usar o primeiro.")
      f_az <- f_az[1]
    }
    message("  Lendo: ", basename(f_az))
    df_az <- read_complemento_azambuja(f_az)
  }

  # ── 4. Combinar PasseJovem + Cascais ──────────────────────────────────────
  # Garantir que ambos têm as mesmas colunas antes de fazer bind_rows
  cols_pj <- names(df_pj)

  if (!is.null(df_casc)) {
    # Adicionar colunas que possam faltar no Cascais
    for (col in setdiff(cols_pj, names(df_casc))) {
      df_casc[[col]] <- NA
    }
    df_casc <- df_casc[, cols_pj]
    df_main <- bind_rows(df_pj, df_casc)
  } else {
    df_main <- df_pj
  }

  # ── 5. Limpezas ───────────────────────────────────────────────────────────

  # 5a. Remover LX-BT
  n_antes <- nrow(df_main)
  df_main <- remove_lxbt(df_main)
  message(sprintf("  LX-BT removidos: %d registos", n_antes - nrow(df_main)))

  # 5b. Corrigir perfil "criança" sem ticket_operator_code
  #     (equivalente ao UPDATE no SQL original)
  mask_crianca <- is.na(df_main$ticket_operator_code) &
                  is.na(df_main$ticket_code)
  if (any(mask_crianca)) {
    df_main[mask_crianca, "ticket_operator_code"] <- 31L
    df_main[mask_crianca, "ticket_code"]          <- 1020L
    df_main[mask_crianca, "ticket_name"]          <- "Navegante 12"
    df_main[mask_crianca, "price"]                <- 0
    df_main[mask_crianca, "ticket_code_base"]     <- 900L
    df_main[mask_crianca, "price_ticket_base"]    <- 40
    message(sprintf("  Perfil criança corrigido: %d registos",
                    sum(mask_crianca)))
  }

  # 5c. Normalizar colunas numéricas
  if (is.character(df_main$price_ticket_base)) {
    df_main$price_ticket_base <- as.double(
      str_replace(df_main$price_ticket_base, ",", "."))
  }
  if (is.character(df_main$price)) {
    df_main$price <- as.double(
      str_replace(df_main$price, ",", "."))
  }
  df_main$price <- as.double(df_main$price)
  df_main$price_ticket_base <- as.double(df_main$price_ticket_base)
  df_main$birth_date <- as.Date(df_main$birth_date)

  # 5d. Remover registos sem year_dist (dados inválidos)
  df_main <- df_main |> filter(!is.na(year_dist))

  # 5e. Garantir que year_dist e month_dist coincidem com o mês solicitado
  n_errado <- sum(df_main$year_dist != ano | df_main$month_dist != mes,
                  na.rm = TRUE)
  if (n_errado > 0)
    warning(sprintf("%d registos com year_dist/month_dist diferente de %d/%02d",
                    n_errado, ano, mes))

  # ── 6. Gravar dados limpos ────────────────────────────────────────────────
  out_pj   <- file.path(paths$output,
                         output_filename(ano, mes, "dados_limpos"))
  out_az   <- file.path(paths$output,
                         output_filename(ano, mes, "complemento_azambuja"))

  write_csv_pj(df_main, out_pj)
  message(sprintf("  Gravado: %s  (%d registos)", basename(out_pj), nrow(df_main)))

  if (!is.null(df_az)) {
    write_csv_pj(df_az, out_az)
    message(sprintf("  Gravado: %s  (%d registos)", basename(out_az), nrow(df_az)))
  }

  message("[01] Concluído.\n")
  invisible(list(passe_jovem = df_main, azambuja = df_az))
}


# -----------------------------------------------------------------------------
# Execução directa (quando o script é corrido via Rscript)
# -----------------------------------------------------------------------------
if (!interactive() && identical(sys.frames(), list())) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2) {
    preparar_dados(ano = as.integer(args[1]), mes = as.integer(args[2]))
  } else {
    cat("Uso: Rscript R/01_preparar_dados.R <ano> <mes>\n")
    cat("Ex:  Rscript R/01_preparar_dados.R 2026 5\n")
  }
}
