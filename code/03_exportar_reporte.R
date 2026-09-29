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


# =============================================================================
# Funções auxiliares
# =============================================================================

#' Prepara o data frame para escrita no template: selecciona e ordena colunas,
#' formata datas como texto DD/MM/YYYY (o template usa texto, não datas Excel).
#'
#' @param dt   data.table com os dados de comNIF ou semNIF
#' @param cols vector de nomes de colunas a seleccionar (na ordem do template)
prep_para_template <- function(dt, cols) {
  df <- as.data.frame(dt)[, cols]
  # Formatar data de nascimento como texto (ex: "25/12/2003")
  df$birth_date_new <- format(as.Date(df$birth_date_new), "%d/%m/%Y")
  df
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
                    "ticket_code_new", "ticket_name_new", "nome_dist"),
      integer   = c("year_dist", "month_dist", "numval", "age_new"),
      numeric   = c("compensacao"),
      IDate     = c("birth_date_new")
    ))


  # ── Preparar dados para escrita ───────────────────────────────────────────
  df_com <- prep_para_template(dt_comNIF, COLS_COMNIF)
  df_sem <- prep_para_template(dt_semNIF, COLS_SEMNIF)


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
  # col 10 (Estimativa sem bilhética) é deixada a 0 (vazio no template)
  writeData(wb, sheet_com, df_com,
            startRow = TEMPLATE_DATA_START_ROW, startCol = 1,
            colNames = FALSE)

  writeData(wb, sheet_sem, df_sem,
            startRow = TEMPLATE_DATA_START_ROW, startCol = 1,
            colNames = FALSE)


  # ── Preencher resumo ──────────────────────────────────────────────────────
  # Total por emissor (comNIF + semNIF combinados)
  resumo_com <- dt_comNIF[, .(
    am           = first(am),
    emissor,
    ano          = ano,
    mes          = mes,
    compensacao  = sum(compensacao, na.rm = TRUE),
    tipo         = "comNIF"
  ), by = .(emissor)][, emissor := NULL]

  resumo_sem <- dt_semNIF[, .(
    am           = first(am),
    emissor,
    ano          = ano,
    mes          = mes,
    compensacao  = sum(compensacao, na.rm = TRUE),
    tipo         = "semNIF"
  ), by = .(emissor)][, emissor := NULL]

  df_resumo <- as.data.frame(rbindlist(list(resumo_com, resumo_sem)))

  writeData(wb, sheet_resumo, df_resumo,
            startRow = TEMPLATE_DATA_START_ROW, startCol = 1,
            colNames = FALSE)


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
