# =============================================================================
# 99_run_pipeline.R
# Orquestrador principal do pipeline PasseJovem.
#
# Executa os 6 passos do pipeline para um ou mais meses:
#   1. Preparar dados                  (01_preparar_dados.R)
#   2. Calcular compensações           (02_calcular_compensacoes.R)
#   3. Exportar reporte Excel          (03_exportar_reporte.R)
#   4. Estimar compensação operadores  (04_estimar_compensacao_operadores.R)
#   5. Exportar Anexo II Repartição    (05_exportar_anexo_reparticao.R)
#   6. Renderizar documentos           (10_documentos.R)
#
# Uso (linha de comandos):
#   Rscript 99_run_pipeline.R              # processa todos os meses em data/
#   Rscript 99_run_pipeline.R 2026 7       # processa julho 2026
#   Rscript 99_run_pipeline.R 2026 5 6 7   # processa maio, junho e julho 2026
#
# Uso (dentro do R / RStudio):
#   source("99_run_pipeline.R")
#   run_pipeline(ano = 2026, meses = 7)
#   run_pipeline(ano = 2026, meses = c(5, 6, 7))
# =============================================================================


# Determinar a pasta code/ — o próprio 99_run_pipeline.R está dentro de code/
# Por isso o code_dir é simplesmente a pasta onde este ficheiro está.
code_dir <- tryCatch({
  # Quando executado via Rscript: extrair pasta do --file=
  args0 <- commandArgs(FALSE)
  m <- regmatches(args0, regexpr("(?<=--file=).+", args0, perl = TRUE))
  if (length(m) > 0) dirname(normalizePath(m))
  else {
    # Quando sourced: usar sys.frames
    ofile <- NULL
    for (i in seq_len(sys.nframe())) {
      ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
      if (!is.null(ofile)) break
    }
    if (!is.null(ofile)) dirname(normalizePath(ofile)) else file.path(getwd(), "code")
  }
}, error = function(e) file.path(getwd(), "code"))

# Carregar os scripts do pipeline (cada um faz source() de 00_config.R internamente)
source(file.path(code_dir, "01_preparar_dados.R"),                 local = TRUE)
source(file.path(code_dir, "02_calcular_compensacoes.R"),          local = TRUE)
source(file.path(code_dir, "03_exportar_reporte.R"),               local = TRUE)
source(file.path(code_dir, "04_estimar_compensacao_operadores.R"), local = TRUE)
source(file.path(code_dir, "05_exportar_anexo_reparticao.R"),      local = TRUE)
source(file.path(code_dir, "10_documentos.R"),                     local = TRUE)


# =============================================================================
# Função principal
# =============================================================================

#' Corre o pipeline completo para o ano e meses indicados.
#'
#' @param ano         Ano de referência (ex: 2026)
#' @param meses       Vector de meses a processar (ex: c(5, 6, 7))
#'                    Se NULL, detecta automaticamente os meses disponíveis em data/
#' @param render_docs Lógico; se TRUE renderiza os documentos oficiais (PDF/Word).
run_pipeline <- function(ano, meses = NULL, render_docs = TRUE) {
  t0_pipe <- proc.time()

  raiz <- here_root()

  # Se não foram especificados meses, detectar automaticamente as pastas em data/
  if (is.null(meses)) {
    pasta_data <- file.path(raiz, "data")

    # Listar subpastas de data/ com o formato YYYY_MM_mes (ex: "2026_05_maio")
    meses <- list.dirs(pasta_data, recursive = FALSE, full.names = FALSE) |>
      # Manter apenas pastas do ano pedido
      (\(dirs) dirs[grepl(sprintf("^%d_", ano), dirs)])() |>
      # Extrair o número do mês do nome da pasta (posição 6-7)
      (\(dirs) as.integer(sub(sprintf("^%d_(\\d{2})_.*", ano), "\\1", dirs)))() |>
      (\(m) sort(m[!is.na(m)]))()

    if (length(meses) == 0)
      stop("Nenhum mes encontrado em data/ para o ano ", ano)
    message("Meses detectados automaticamente: ", paste(meses, collapse = ", "))
  }

  # Processar cada mês sequencialmente
  for (mes in meses) {
    message("============================================================")
    message(sprintf(" Ano: %d | Mes: %02d (%s)", ano, mes, MESES_PT_UPPER[mes]))
    message("============================================================")

    # Correr os passos do pipeline; continuar para o próximo mês se houver erro
    tryCatch({
      preparar_dados(ano, mes, raiz)
      calcular_compensacoes(ano, mes, raiz)
      exportar_reporte(ano, mes, raiz)
      estimar_compensacao_operadores(ano, mes, raiz)
      exportar_anexo_reparticao(ano, mes, raiz)
      if (isTRUE(render_docs)) {
        render_documentos(ano = ano, mes = mes, raiz = raiz)
      }
    }, error = function(e) {
      message(sprintf("ERRO ao processar %d/%02d: %s", ano, mes, conditionMessage(e)))
    })
  }

  elapsed_pipe <- round((proc.time() - t0_pipe)[["elapsed"]], 1)
  message(sprintf("\nPipeline concluido em %.1f segundos.", elapsed_pipe))
}


# =============================================================================
# Execução directa via Rscript
# =============================================================================
# Nota: não usamos identical(sys.frames(), list()) porque este script
# usa source() internamente, o que altera sys.frames().
# Usamos apenas !interactive() para detectar execução via Rscript.
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    # Sem argumentos: detectar meses automaticamente para o ano actual
    run_pipeline(ano = as.integer(format(Sys.Date(), "%Y")))

  } else if (length(args) == 1) {
    # Rscript 99_run_pipeline.R 2026  -> todos os meses de 2026
    run_pipeline(ano = as.integer(args[1]))

  } else {
    # Rscript 99_run_pipeline.R 2026 5 6 7  -> meses específicos
    ano   <- as.integer(args[1])
    meses <- as.integer(args[-1])
    run_pipeline(ano = ano, meses = meses)
  }
}
