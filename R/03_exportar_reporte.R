# =============================================================================
# 03_exportar_reporte.R
# Consolida comNIF e semNIF numa folha de reporte Excel (XLSX) formal,
# incluindo uma aba de Resumo com as compensações totais por emissor.
#
# Produz:
#   data/<ano>_<mm>_<mes>/output/<ano>_<mm>_reporte_AML_IMT.xlsx
#
# Uso:
#   source("R/03_exportar_reporte.R")
#   exportar_reporte(ano = 2026, mes = 5)
# =============================================================================

source(file.path(dirname(sys.frame(1)$ofile), "00_utils.R"),
       local = TRUE, chdir = FALSE)

# Pacote adicional para formatação Excel
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
library(openxlsx)


# -----------------------------------------------------------------------------
# Mapeamento mês número -> nome PT
# -----------------------------------------------------------------------------
MESES_PT <- c("JANEIRO", "FEVEREIRO", "MARÇO", "ABRIL", "MAIO", "JUNHO",
               "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO",
               "DEZEMBRO")

mes_nome_pt <- function(mes) MESES_PT[mes]


# -----------------------------------------------------------------------------
# Construção das abas de dados
# -----------------------------------------------------------------------------

#' Selecciona e renomeia colunas para o formato do reporte oficial
prep_aba_comNIF <- function(df, ano, mes) {
  df |>
    mutate(
      birth_date_new = format(as.Date(birth_date_new), "%d/%m/%Y")
    ) |>
    select(
      `Identificação AM/CIM`                      = am,
      `Entidade emissora de títulos de transporte` = emissor,
      `Ano`                                        = year_dist,
      `Mês`                                        = month_dist,
      `NIF do jovem`                               = fiscal_number,
      `N.º de cartão`                              = cardserialnumber_new,
      `Nome IMT`                                   = nome_imt,
      `Título base (preço)`                        = price_ticket_base_new,
      `N.º validações`                             = numval,
      `Compensação (euros)`                        = compensacao,
      `Código operador`                            = ticket_operator_code_new,
      `Código título`                              = ticket_code_new,
      `Nome título`                                = ticket_name_new,
      `Complemento Azambuja`                       = compaz,
      `Nome distribuição`                          = nome_dist,
      `Idade`                                      = age_new,
      `Data nascimento`                            = birth_date_new
    )
}

#' Selecciona e renomeia colunas para semNIF
prep_aba_semNIF <- function(df, ano, mes) {
  df |>
    mutate(
      birth_date_new = format(as.Date(birth_date_new), "%d/%m/%Y")
    ) |>
    select(
      `Identificação AM/CIM`                      = am,
      `Entidade emissora de títulos de transporte` = emissor,
      `Ano`                                        = year_dist,
      `Mês`                                        = month_dist,
      `N.º de cartão`                              = card_serial_number,
      `Nome IMT`                                   = nome_imt,
      `Título base (preço)`                        = price_ticket_base_new,
      `N.º validações`                             = numval,
      `Compensação (euros)`                        = compensacao,
      `Código operador`                            = ticket_operator_code_new,
      `Código título`                              = ticket_code_new,
      `Nome título`                                = ticket_name_new,
      `Complemento Azambuja`                       = compaz,
      `Nome distribuição`                          = nome_dist,
      `Idade`                                      = age_new,
      `Data nascimento`                            = birth_date_new
    )
}


# -----------------------------------------------------------------------------
# Construção da aba Resumo
# -----------------------------------------------------------------------------

#' Cria data frame de resumo com totais por emissor (comNIF + semNIF)
build_resumo <- function(df_com, df_sem, ano, mes) {
  # ComNIF por emissor
  res_com <- df_com |>
    group_by(emissor) |>
    summarise(
      am          = first(am),
      ano         = ano,
      mes         = mes,
      compensacao = sum(compensacao, na.rm = TRUE),
      n_passes    = n(),
      origem      = "comNIF",
      .groups     = "drop"
    )

  # SemNIF por emissor
  res_sem <- df_sem |>
    group_by(emissor) |>
    summarise(
      am          = first(am),
      ano         = ano,
      mes         = mes,
      compensacao = sum(compensacao, na.rm = TRUE),
      n_passes    = n(),
      origem      = "semNIF",
      .groups     = "drop"
    )

  bind_rows(res_com, res_sem) |>
    select(
      `Identificação AM/CIM`                       = am,
      `Entidade emissora de títulos de transporte`  = emissor,
      `Ano`                                         = ano,
      `Mês`                                         = mes,
      `Compensação total (euros)`                   = compensacao,
      `N.º de passes`                               = n_passes,
      `Tipo`                                        = origem
    )
}


# -----------------------------------------------------------------------------
# Escrita Excel com openxlsx
# -----------------------------------------------------------------------------

#' Escreve o ficheiro Excel de reporte com 3 abas:
#'   AML_<ano>_<mm>_ComNIF, AML_<ano>_<mm>_SemNIF, RESUMO_<ano>_<mm>_AML
exportar_reporte <- function(ano, mes, raiz = here_root()) {

  paths <- month_paths(ano, mes, raiz)
  stopifnot(dir.exists(paths$output))

  message(sprintf("[03] A exportar reporte Excel para %d/%02d ...", ano, mes))

  # ── Ler ficheiros CSV de output ───────────────────────────────────────────
  f_com <- file.path(paths$output, output_filename(ano, mes, "comNIF"))
  f_sem <- file.path(paths$output, output_filename(ano, mes, "semNIF"))

  if (!file.exists(f_com))
    stop("Ficheiro comNIF não encontrado: ", f_com,
         "\n  Execute primeiro: calcular_compensacoes(", ano, ", ", mes, ")")

  df_com <- as.data.frame(fread(f_com, sep = ";", dec = ".", colClasses = list(
    character = c("am", "emissor", "fiscal_number", "nome_imt", "price_ticket_base_new",
                  "ticket_operator_code_new", "ticket_code_new", "ticket_name_new",
                  "nome_dist", "cardserialnumber_new"),
    integer   = c("year_dist", "month_dist", "numval", "compaz", "age_new"),
    numeric   = c("compensacao"),
    IDate     = c("birth_date_new")
  )))

  df_sem <- as.data.frame(fread(f_sem, sep = ";", dec = ".", colClasses = list(
    character = c("am", "emissor", "card_serial_number", "nome_imt", "price_ticket_base_new",
                  "ticket_operator_code_new", "ticket_code_new", "ticket_name_new",
                  "nome_dist"),
    integer   = c("year_dist", "month_dist", "numval", "compaz", "age_new"),
    numeric   = c("compensacao"),
    IDate     = c("birth_date_new")
  )))

  # ── Preparar abas ─────────────────────────────────────────────────────────
  aba_com    <- prep_aba_comNIF(df_com, ano, mes)
  aba_sem    <- prep_aba_semNIF(df_sem, ano, mes)
  aba_resumo <- build_resumo(df_com, df_sem, ano, mes)

  # ── Criar workbook ────────────────────────────────────────────────────────
  wb <- createWorkbook()

  sheet_com    <- sprintf("AML_%d_%02d_ComNIF",  ano, mes)
  sheet_sem    <- sprintf("AML_%d_%02d_SemNIF",  ano, mes)
  sheet_resumo <- sprintf("RESUMO_%d_%02d_AML",  ano, mes)

  addWorksheet(wb, sheet_com)
  addWorksheet(wb, sheet_sem)
  addWorksheet(wb, sheet_resumo)

  # Estilos
  header_style <- createStyle(
    fontColour = "#FFFFFF", fgFill = "#2E75B6",
    halign = "LEFT", textDecoration = "Bold",
    border = "Bottom", borderColour = "#FFFFFF"
  )
  money_style <- createStyle(numFmt = "#,##0.00")
  date_style  <- createStyle(numFmt = "DD/MM/YYYY")

  # Escrever e formatar aba ComNIF
  writeDataTable(wb, sheet_com, aba_com, tableStyle = "TableStyleMedium9",
                 startRow = 1, startCol = 1, withFilter = TRUE)
  addStyle(wb, sheet_com, header_style, rows = 1, cols = 1:ncol(aba_com),
           gridExpand = TRUE)
  comp_col_com <- which(names(aba_com) == "Compensação (euros)")
  addStyle(wb, sheet_com, money_style, rows = 2:(nrow(aba_com) + 1),
           cols = comp_col_com, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet_com, cols = 1:ncol(aba_com), widths = "auto")

  # Escrever e formatar aba SemNIF
  writeDataTable(wb, sheet_sem, aba_sem, tableStyle = "TableStyleMedium9",
                 startRow = 1, startCol = 1, withFilter = TRUE)
  addStyle(wb, sheet_sem, header_style, rows = 1, cols = 1:ncol(aba_sem),
           gridExpand = TRUE)
  comp_col_sem <- which(names(aba_sem) == "Compensação (euros)")
  addStyle(wb, sheet_sem, money_style, rows = 2:(nrow(aba_sem) + 1),
           cols = comp_col_sem, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet_sem, cols = 1:ncol(aba_sem), widths = "auto")

  # Escrever aba Resumo com cabeçalho formal
  mes_pt <- mes_nome_pt(mes)
  cabecalho <- data.frame(
    A = c("PASSE GRATUITO PARA JOVENS",
          "Portaria n.\u00ba 7-A/2024, de 5 de janeiro, na sua reda\u00e7\u00e3o atual",
          "(Art.\u00ba 9.\u00ba - n.\u00ba 2)",
          "",
          sprintf("AUTORIDADE DE TRANSPORTE (AM/CIM): \u00c1REA METROPOLITANA DE LISBOA"),
          sprintf("ANO: %d", ano),
          sprintf("M\u00caS: %s", mes_pt),
          "")
  )
  writeData(wb, sheet_resumo, cabecalho, startRow = 1, startCol = 1,
            colNames = FALSE)

  writeDataTable(wb, sheet_resumo, aba_resumo,
                 tableStyle = "TableStyleMedium9",
                 startRow = nrow(cabecalho) + 2, startCol = 1,
                 withFilter = FALSE)
  comp_col_res <- which(names(aba_resumo) == "Compensação total (euros)")
  addStyle(wb, sheet_resumo, money_style,
           rows = (nrow(cabecalho) + 3):(nrow(cabecalho) + 2 + nrow(aba_resumo)),
           cols = comp_col_res, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet_resumo, cols = 1:ncol(aba_resumo), widths = "auto")

  # ── Gravar ficheiro ───────────────────────────────────────────────────────
  out_xlsx <- file.path(paths$output,
    output_filename(ano, mes, "reporte_AML_IMT", ext = "xlsx"))

  saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  message(sprintf("  Gravado: %s", basename(out_xlsx)))
  message("[03] Concluído.\n")

  invisible(out_xlsx)
}


# -----------------------------------------------------------------------------
# Execução directa
# -----------------------------------------------------------------------------
if (!interactive() && identical(sys.frames(), list())) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2) {
    exportar_reporte(ano = as.integer(args[1]), mes = as.integer(args[2]))
  } else {
    cat("Uso: Rscript R/03_exportar_reporte.R <ano> <mes>\n")
  }
}
