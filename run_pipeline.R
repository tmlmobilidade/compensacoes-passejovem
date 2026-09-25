# =============================================================================
# run_pipeline.R
# Script orquestrador: corre os 3 passos para um ou mais meses.
#
# Uso interactivo (RStudio / R console):
#   source("run_pipeline.R")
#   run_pipeline(ano = 2026, mes = 5)
#   run_pipeline(ano = 2026, mes = 6)
#
# Uso em linha de comandos:
#   Rscript run_pipeline.R 2026 5
#   Rscript run_pipeline.R 2026 5 6    # vários meses
# =============================================================================

# Determinar raiz do projecto
RAIZ <- if (nzchar(Sys.getenv("PASSE_JOVEM_ROOT"))) {
  Sys.getenv("PASSE_JOVEM_ROOT")
} else {
  # Quando corrido via Rscript, usar dirname do script
  args0 <- commandArgs(FALSE)
  m <- regmatches(args0, regexpr("(?<=--file=).+", args0, perl = TRUE))
  if (length(m) > 0) dirname(m) else getwd()
}

R_DIR <- file.path(RAIZ, "R")

source(file.path(R_DIR, "01_preparar_dados.R"),     local = TRUE)
source(file.path(R_DIR, "02_calcular_compensacoes.R"), local = TRUE)
source(file.path(R_DIR, "03_exportar_reporte.R"),    local = TRUE)


#' Corre o pipeline completo para um mês.
#'
#' @param ano  integer — ano de referência
#' @param mes  integer — mês de referência (1–12)
#' @param raiz character — raiz do projecto
run_pipeline <- function(ano, mes, raiz = RAIZ) {
  t0 <- proc.time()
  message(rep("=", 60))
  message(sprintf("  PIPELINE PasseJovem  —  %d / %02d", ano, mes))
  message(rep("=", 60))

  tryCatch({
    preparar_dados(ano, mes, raiz)
    calcular_compensacoes(ano, mes, raiz)
    exportar_reporte(ano, mes, raiz)
  }, error = function(e) {
    message("\n[ERRO] ", conditionMessage(e))
    stop(e)
  })

  elapsed <- round((proc.time() - t0)[["elapsed"]], 1)
  message(sprintf("  Pipeline concluído em %.1f segundos.", elapsed))
  message(rep("=", 60), "\n")
  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# Execução directa via Rscript
# -----------------------------------------------------------------------------
args_user <- commandArgs(trailingOnly = TRUE)
if (length(args_user) >= 2) {
  ano_arg <- as.integer(args_user[1])
  for (mes_arg in as.integer(args_user[-1])) {
    run_pipeline(ano = ano_arg, mes = mes_arg)
  }
}
