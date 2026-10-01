# =============================================================================
# 03_exportar_reporte.R
# Preenche o template oficial Excel com os resultados de comNIF e semNIF.
#
# O template é:
#   Exemplos/Template_PASSE GRATUITO JOVEM - REPORTE - CIM_AT para IMT (Mensal) - 2026 - V2.xlsx
#
# O ficheiro de saída é copiado do template e preenchido com os dados,
# preservando toda a formatação e estilos do original.
#
# Produz:
#   data/<ano>_<mm>_<mes>/output/<ano>_<mm>_reporte_AML_IMT.xlsx
#
# Uso:
#   source("code/03_exportar_reporte.R")
#   exportar_reporte(ano = 2026, mes = 7)
# =============================================================================

source(file.path(dirname(sys.frame(1)$ofile), "00_config.R"),
       local = TRUE, chdir = FALSE)


# =============================================================================
# Constantes do template
# =============================================================================

# Nome do ficheiro template (relativo à raiz do projecto)
TEMPLATE_FILE <- file.path(
  "Exemplos",
  "Template_PASSE GRATUITO JOVEM - REPORTE - CIM_AT para IMT (Mensal) - 2026 - V2.xlsx"
)

# Linha a partir da qual se escrevem os dados nas abas do template
TEMPLATE_DATA_START_ROW <- 13L

# Mapeamento entre nomes das colunas internas e colunas do template
# (a ordem das colunas no template Excel é fixa)
COLS_COMNIF <- c(
  "am",                       # col 1:  Identificação AM/CIM
  "emissor",                  # col 2:  Entidade emissora
  "year_dist",                # col 3:  Ano
  "month_dist",               # col 4:  Mês
  "fiscal_number",            # col 5:  N.º NIF
  "birth_date_new",           # col 6:  Data de nascimento
  "nome_imt",                 # col 7:  Designação (título referência)
  "price_ticket_base_new",    # col 8:  PVP
  "numval",                   # col 9:  N.º Validações
  "compensacao",              # col 11: Compensação (euros)
  "nome_dist"                 # col 12: Categoria (nome distribuição)
)

COLS_SEMNIF <- c(
  "am",                       # col 1
  "emissor",                  # col 2
  "year_dist",                # col 3
  "month_dist",               # col 4
  "card_serial_number",       # col 5:  N.º de cartão (em vez de NIF)
  "birth_date_new",           # col 6
  "nome_imt",                 # col 7
  "price_ticket_base_new",    # col 8
  "numval",                   # col 9
  "compensacao",              # col 11
  "nome_dist"                 # col 12
)

# Colunas oficiais do template v2 (julho 2026 em diante, com colunas de documento)
COLS_COMNIF_V2 <- c(
  "am",                       # col 1:  Identificação AM/CIM
  "emissor",                  # col 2:  Entidade emissora
  "year_dist",                # col 3:  Ano
  "month_dist",               # col 4:  Mês
  "fiscal_number",            # col 5:  N.º NIF
  "birth_date_new",           # col 6:  Data de nascimento
  "nome_imt",                 # col 7:  Designação (título referência)
  "price_ticket_base_new",    # col 8:  PVP
  "numval",                   # col 9:  N.º Validações
  "estimativa",               # col 10: Estimativa (vazio / NA)
  "compensacao",              # col 11: Compensação (euros)
  "categoria",                # col 12: Categoria ("A" ou "B")
  "cardserialnumber_new"      # col 13: Número do cartão de suporte ao título de transporte
)

COLS_SEMNIF_V2 <- c(
  "am",                       # col 1:  Identificação AM/CIM
  "emissor",                  # col 2:  Entidade emissora
  "year_dist",                # col 3:  Ano
  "month_dist",               # col 4:  Mês
  "doc_type",                 # col 5:  Tipo de Documento
  "doc_number",               # col 6:  N.º DO DOCUMENTO
  "birth_date_new",           # col 7:  Data de nascimento
  "nome_imt",                 # col 8:  Designação (título referência)
  "price_ticket_base_new",    # col 9:  PVP
  "numval",                   # col 10: N.º Validações
  "estimativa",               # col 11: Estimativa (vazio / NA)
  "compensacao",              # col 12: Compensação (euros)
  "categoria",                # col 13: Categoria ("A" ou "B")
  "cardserialnumber_new"      # col 14: Número do cartão de suporte ao título de transporte
)


# =============================================================================
# Funções auxiliares
# =============================================================================

#' Prepara o data frame para escrita no template: selecciona e ordena colunas,
#' assegura tipos numéricos (NIF, PVP, compensação com 2 decimais) e Date para data de nascimento,
#' e adiciona as variáveis estimativa, categoria e cartão de suporte quando aplicável.
#'
#' @param dt   data.table com os dados de comNIF ou semNIF
#' @param cols vector de nomes de colunas a seleccionar (na ordem do template)
prep_para_template <- function(dt, cols) {
  # Garantir coluna de cartão se não existir
  if (!"cardserialnumber_new" %in% names(dt)) {
    if ("card_serial_number" %in% names(dt)) {
      dt$cardserialnumber_new <- as.character(dt$card_serial_number)
    } else {
      dt$cardserialnumber_new <- NA_character_
    }
  }

  res <- dt |>
    as_tibble() |>
    mutate(
      birth_date_new        = as.Date(birth_date_new),
      price_ticket_base_new = as.numeric(price_ticket_base_new),
      compensacao           = round(as.numeric(compensacao), 2),
      estimativa            = NA_real_,
      categoria             = if ("ticket_name_new" %in% names(dt)) {
        if_else(grepl("Complemento", ticket_name_new), "B", "A")
      } else "A",
      cardserialnumber_new  = as.character(cardserialnumber_new)
    )

  if ("fiscal_number" %in% names(res)) {
    res <- res |> mutate(fiscal_number = as.numeric(fiscal_number))
  }

  res |>
    select(all_of(cols)) |>
    as.data.frame()
}

#' Preenche as células de cabeçalho do template com Ano, Mês e nome da AT.
#'
#' @param wb       workbook openxlsx
#' @param sheet    nome da aba
#' @param ano      inteiro
#' @param mes      inteiro
fill_cabecalho <- function(wb, sheet, ano, mes) {
  writeData(wb, sheet, x = "ÁREA METROPOLITANA DE LISBOA", startRow = 5, startCol = 2,
            colNames = FALSE)
  writeData(wb, sheet, x = as.character(ano), startRow = 6, startCol = 2,
            colNames = FALSE)
  writeData(wb, sheet, x = MESES_PT_UPPER[mes], startRow = 7, startCol = 2,
            colNames = FALSE)
}


# =============================================================================
# Função principal
# =============================================================================

#' Exporta os resultados para o ficheiro Excel oficial (cópia do template).
#'
#' @param ano  Ano de referência
#' @param mes  Mês de referência
#' @param raiz Raiz do projecto
exportar_reporte <- function(ano, mes, raiz = here_root()) {

  paths <- month_paths(ano, mes, raiz)
  stopifnot(dir.exists(paths$output))

  message(sprintf("[03] A exportar reporte para %d/%02d ...", ano, mes))


  # ── Verificar template ────────────────────────────────────────────────────
  template_path <- file.path(raiz, TEMPLATE_FILE)
  if (!file.exists(template_path))
    stop("Template nao encontrado: ", template_path)


  # ── Ler comNIF e semNIF ───────────────────────────────────────────────────
  f_com <- file.path(paths$output, output_filename(ano, mes, "comNIF"))
  f_sem <- file.path(paths$output, output_filename(ano, mes, "semNIF"))

  if (!file.exists(f_com))
    stop("comNIF nao encontrado: ", f_com,
         "\n  Execute primeiro: calcular_compensacoes(", ano, ", ", mes, ")")

  dt_comNIF <- fread(f_com, sep = ";", dec = ".",
    colClasses = list(
      character = c("am", "emissor", "fiscal_number", "nome_imt",
                    "price_ticket_base_new", "ticket_operator_code_new",
                    "ticket_code_new", "ticket_name_new", "nome_dist",
                    "cardserialnumber_new"),
      integer   = c("year_dist", "month_dist", "numval", "age_new"),
      numeric   = c("compensacao"),
      IDate     = c("birth_date_new")
    ))

  dt_semNIF <- fread(f_sem, sep = ";", dec = ".",
    colClasses = list(
      character = c("am", "emissor", "card_serial_number", "nome_imt",
                    "price_ticket_base_new", "ticket_operator_code_new",
                    "ticket_code_new", "ticket_name_new", "nome_dist",
                    "doc_type", "doc_number", "cardserialnumber_new"),
      integer   = c("year_dist", "month_dist", "numval", "age_new"),
      numeric   = c("compensacao"),
      IDate     = c("birth_date_new")
    ))

  # ── Ordenar dados ─────────────────────────────────────────────────────────
  # comNIF: ordenar por NIF (numérico)
  dt_comNIF <- dt_comNIF |>
    arrange(as.numeric(fiscal_number))

  # semNIF: ordenar por tipo de documento e depois por número de documento
  if ("doc_type" %in% names(dt_semNIF) && "doc_number" %in% names(dt_semNIF)) {
    dt_semNIF <- dt_semNIF |>
      arrange(doc_type, doc_number)
  }


  # ── Preparar dados para escrita ───────────────────────────────────────────
  if ("doc_number" %in% names(dt_semNIF)) {
    # Formato v2 (julho 2026 em diante, com colunas de documento)
    df_com <- prep_para_template(dt_comNIF, COLS_COMNIF_V2)
    df_sem <- prep_para_template(dt_semNIF, COLS_SEMNIF_V2)
  } else {
    df_com <- prep_para_template(dt_comNIF, COLS_COMNIF)
    df_sem <- prep_para_template(dt_semNIF, COLS_SEMNIF)
  }


  # ── Copiar template e abrir com openxlsx ─────────────────────────────────
  # Usamos loadWorkbook (que preserva todos os estilos do template)
  # em vez de createWorkbook (que criaria um ficheiro vazio).
  out_xlsx <- file.path(paths$output,
                        output_filename(ano, mes, "reporte_AML_IMT", ext = "xlsx"))
  file.copy(template_path, out_xlsx, overwrite = TRUE)
  wb <- loadWorkbook(out_xlsx)

  sheets <- names(wb)
  # O template tem 4 abas; as duas primeiras são comNIF e semNIF
  sheet_com    <- sheets[1]   # "CIM_AT - ANO_MÊS - Com NIF"
  sheet_sem    <- sheets[2]   # "CIM_AT - ANO_MÊS - Sem NIF"
  sheet_resumo <- sheets[3]   # "RESUMO ANO_MÊS - CIM"


  # ── Preencher cabeçalhos (Ano, Mês, AT) ──────────────────────────────────
  for (s in c(sheet_com, sheet_sem, sheet_resumo)) {
    fill_cabecalho(wb, s, ano, mes)
  }


  # ── Escrever dados a partir da linha TEMPLATE_DATA_START_ROW ─────────────
  options("openxlsx.dateFormat" = "dd/mm/yyyy")
  options("openxlsx.datetimeFormat" = "dd/mm/yyyy")

  # col 10 (Estimativa sem bilhética) é deixada a 0 (vazio no template)
  writeData(wb, sheet_com, df_com,
            startRow = TEMPLATE_DATA_START_ROW, startCol = 1,
            colNames = FALSE)

  writeData(wb, sheet_sem, df_sem,
            startRow = TEMPLATE_DATA_START_ROW, startCol = 1,
            colNames = FALSE)


  # ── Estilos e colunas adicionais para comNIF e semNIF ───────────────────
  header_style <- createStyle(
    fontName       = "Verdana",
    fontSize       = 9,
    textDecoration = "bold",
    halign         = "center",
    valign         = "center",
    wrapText       = TRUE,
    fgFill         = "#D9D9D9",
    border         = "TopBottomLeftRight",
    borderColour   = "#000000"
  )

  style_date <- createStyle(numFmt = "dd/mm/yyyy")
  style_num2 <- createStyle(numFmt = "#,##0.00")
  style_nif  <- createStyle(numFmt = "0")

  if ("doc_number" %in% names(dt_semNIF)) {
    # ── Formato v2 (julho 2026 em diante) ──
    # Sheet 1: Coluna M (13) - Número do cartão de suporte
    writeData(wb, sheet_com, x = "Número do cartão de suporte ao título de transporte",
              startRow = 11, startCol = 13)
    mergeCells(wb, sheet_com, cols = 13, rows = 10:12)
    addStyle(wb, sheet_com, header_style, rows = 10:12, cols = 13, gridExpand = TRUE)
    setColWidths(wb, sheet_com, cols = 13, widths = 25)

    # Formatar colunas no comNIF
    end_row_com <- TEMPLATE_DATA_START_ROW + nrow(df_com) - 1L
    addStyle(wb, sheet_com, style_nif,  rows = TEMPLATE_DATA_START_ROW:end_row_com, cols = 5,  gridExpand = TRUE)
    addStyle(wb, sheet_com, style_date, rows = TEMPLATE_DATA_START_ROW:end_row_com, cols = 6,  gridExpand = TRUE)
    addStyle(wb, sheet_com, style_num2, rows = TEMPLATE_DATA_START_ROW:end_row_com, cols = 8,  gridExpand = TRUE)
    addStyle(wb, sheet_com, style_num2, rows = TEMPLATE_DATA_START_ROW:end_row_com, cols = 11, gridExpand = TRUE)

    # Sheet 2: Coluna N (14) - Número do cartão de suporte (col M mantém Categoria)
    writeData(wb, sheet_sem, x = "Número do cartão de suporte ao título de transporte",
              startRow = 11, startCol = 14)
    mergeCells(wb, sheet_sem, cols = 14, rows = 10:12)
    addStyle(wb, sheet_sem, header_style, rows = 10:12, cols = 14, gridExpand = TRUE)
    setColWidths(wb, sheet_sem, cols = 14, widths = 25)

    # Formatar colunas no semNIF
    end_row_sem <- TEMPLATE_DATA_START_ROW + nrow(df_sem) - 1L
    addStyle(wb, sheet_sem, style_date, rows = TEMPLATE_DATA_START_ROW:end_row_sem, cols = 7,  gridExpand = TRUE)
    addStyle(wb, sheet_sem, style_num2, rows = TEMPLATE_DATA_START_ROW:end_row_sem, cols = 9,  gridExpand = TRUE)
    addStyle(wb, sheet_sem, style_num2, rows = TEMPLATE_DATA_START_ROW:end_row_sem, cols = 12, gridExpand = TRUE)
  }


  # ── Preencher resumo ──────────────────────────────────────────────────────
  # Total de compensação por emissor (comNIF + semNIF combinados)
  # Mapeamento oficial dos emissores no resumo: AT AML -> AT TML, AT Cascais -> AT CASCAIS
  resumo_dados <- bind_rows(
    dt_comNIF |> select(emissor, compensacao),
    dt_semNIF |> select(emissor, compensacao)
  ) |>
    mutate(emissor_norm = case_when(
      grepl("AML", emissor, ignore.case = TRUE) ~ "AT TML",
      grepl("Cascais", emissor, ignore.case = TRUE) ~ "AT CASCAIS",
      TRUE ~ toupper(emissor)
    )) |>
    group_by(emissor_norm) |>
    summarise(compensacao = sum(compensacao, na.rm = TRUE), .groups = "drop")

  comp_tml <- resumo_dados |>
    filter(emissor_norm == "AT TML") |>
    pull(compensacao)
  if (length(comp_tml) == 0) comp_tml <- 0

  comp_cascais <- resumo_dados |>
    filter(emissor_norm == "AT CASCAIS") |>
    pull(compensacao)
  if (length(comp_cascais) == 0) comp_cascais <- 0

  comp_total <- comp_tml + comp_cascais

  # Estrutura exacta das colunas do template RESUMO:
  # Col 1: IDENTIFICAÇÃO AM/CIM ("AML")
  # Col 2: ENTIDADE EMISSORA DE TITULOS DE TRANSPORTE ("AT TML", "AT CASCAIS", "TOTAL")
  # Col 3: ANO (2026)
  # Col 4: MÊS (7)
  # Col 5: COMPENSAÇÃO (euros)
  df_resumo <- data.frame(
    col1 = c("AML", "AML", NA_character_),
    col2 = c("AT TML", "AT CASCAIS", "TOTAL"),
    col3 = c(ano, ano, NA_integer_),
    col4 = c(mes, mes, NA_integer_),
    col5 = c(round(comp_tml, 2), round(comp_cascais, 2), round(comp_total, 2)),
    stringsAsFactors = FALSE
  )

  writeData(wb, sheet_resumo, df_resumo,
            startRow = TEMPLATE_DATA_START_ROW, startCol = 1,
            colNames = FALSE)

  # Limpar eventuais zeros/fórmulas residuais das linhas 16 a 25 da coluna 5 do template
  for (r in 16:25) {
    writeData(wb, sheet_resumo, x = "", startRow = r, startCol = 5)
  }

  # Formatação da folha RESUMO exactamente como na imagem
  style_resumo_num    <- createStyle(fontName = "Calibri", fontSize = 11, numFmt = "#,##0.00", halign = "right")
  style_resumo_center <- createStyle(fontName = "Calibri", fontSize = 11, halign = "center")
  style_total_line    <- createStyle(border = "top", borderColour = "#000000")
  style_total_label   <- createStyle(fontName = "Calibri", fontSize = 11, textDecoration = "bold", halign = "left", border = "top", borderColour = "#000000")
  style_total_val     <- createStyle(fontName = "Calibri", fontSize = 11, numFmt = "#,##0.00", textDecoration = "bold", halign = "right", border = "top", borderColour = "#000000")

  addStyle(wb, sheet_resumo, style_resumo_center, rows = 13:14, cols = 3:4, gridExpand = TRUE)
  addStyle(wb, sheet_resumo, style_resumo_num,    rows = 13:14, cols = 5,   gridExpand = TRUE)
  addStyle(wb, sheet_resumo, style_total_line,    rows = 15,    cols = 2:5, gridExpand = TRUE)
  addStyle(wb, sheet_resumo, style_total_label,   rows = 15,    cols = 2)
  addStyle(wb, sheet_resumo, style_total_val,     rows = 15,    cols = 5)


  # ── Gravar ────────────────────────────────────────────────────────────────
  saveWorkbook(wb, out_xlsx, overwrite = TRUE)

  message(sprintf("  Gravado: %s", basename(out_xlsx)))
  message("[03] Concluido.\n")

  # Mostrar resumo no ecrã
  message("  === RESUMO ===")
  message(sprintf("  comNIF: %d passes | %.2f EUR",
                  nrow(dt_comNIF), sum(dt_comNIF$compensacao, na.rm = TRUE)))
  message(sprintf("  semNIF: %d passes | %.2f EUR",
                  nrow(dt_semNIF), sum(dt_semNIF$compensacao, na.rm = TRUE)))
  message(sprintf("  TOTAL:               %.2f EUR",
                  sum(dt_comNIF$compensacao, na.rm = TRUE) +
                  sum(dt_semNIF$compensacao, na.rm = TRUE)))

  invisible(out_xlsx)
}


# -----------------------------------------------------------------------------
# Execução directa: Rscript code/03_exportar_reporte.R <ano> <mes>
# -----------------------------------------------------------------------------
if (!interactive() && identical(sys.frames(), list())) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2) {
    exportar_reporte(ano = as.integer(args[1]), mes = as.integer(args[2]))
  } else {
    cat("Uso: Rscript code/03_exportar_reporte.R <ano> <mes>\n")
    cat("Ex:  Rscript code/03_exportar_reporte.R 2026 7\n")
  }
}
