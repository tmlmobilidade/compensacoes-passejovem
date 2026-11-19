# =============================================================================
# 10_documentos.R — Renderiza relatórios em Typst (PDF) e Word (DOCX)
# Passe Gratuito para Jovens — TML
#
# Uso (linha de comandos):
#   Rscript code/10_documentos.R              # usa parâmetros de doc_params.R
#   Rscript code/10_documentos.R 2026 7       # renderiza julho 2026
#
# Uso (dentro do R / RStudio):
#   source("code/10_documentos.R")
#   render_documentos(ano = 2026, mes = 7)
# =============================================================================

source(here::here("code", "00_config.R"))
source(here::here("code", "doc_params.R"))

#' Renderiza um documento Quarto (.qmd) para Typst (PDF) e/ou Word (DOCX)
#'
#' @param qmd_name    Nome do ficheiro .qmd em code/
#' @param ano         Ano de referência
#' @param mes         Mês de referência
#' @param raiz        Raiz do projeto
#' @param render_pdf  Gerar PDF (Typst)? Por omissão respeita RENDER_PDF de doc_params.R
#' @param render_docx Gerar DOCX (Word)? Por omissão respeita RENDER_DOCX de doc_params.R
#' @param out_name    Nome base do ficheiro de saída (sem extensão e sem prefixo ano_mes_). Se NULL, usa o nome do .qmd
render_qmd_doc <- function(qmd_name, ano = ANO, mes = MES, raiz = here_root(),
                           render_pdf = isTRUE(get0("RENDER_PDF", ifnotfound = TRUE)),
                           render_docx = isTRUE(get0("RENDER_DOCX", ifnotfound = TRUE)),
                           out_name = NULL) {
  qmd_file <- file.path(raiz, "code", qmd_name)
  if (!file.exists(qmd_file)) {
    warning(sprintf("Ficheiro %s não encontrado em code/.", qmd_name))
    return(FALSE)
  }

  paths <- month_paths(ano, mes, raiz)
  docs_dir <- if (!is.null(paths$docs)) paths$docs else file.path(paths$output, "docs")
  dir.create(docs_dir, showWarnings = FALSE, recursive = TRUE)

  base_name <- sub("\\.qmd$", "", qmd_name)
  dest_name <- if (!is.null(out_name) && nzchar(out_name)) out_name else base_name
  mes_nm    <- tolower(MESES_PT[mes])

  message("=================================================================")
  message(sprintf("A renderizar %s para: %s / %d", qmd_name, mes_nm, ano))
  message(sprintf("Pasta de saída: %s", docs_dir))
  message("=================================================================")

  # 1. Garantir reference-doc atualizado (apenas se for gerar Word)
  if (isTRUE(render_docx)) {
    refdoc_script <- file.path(raiz, "code", "templates", "build_reference_doc.R")
    if (file.exists(refdoc_script)) {
      tryCatch({
        source(refdoc_script, local = TRUE)
        build_reference_doc()
      }, error = function(e) {
        warning("Aviso ao preparar reference-doc: ", conditionMessage(e))
      })
    }
  }

  # 2. Renderizar Typst (PDF) se configurado
  if (isTRUE(render_pdf)) {
    message("-> A gerar PDF (Typst)...")
    status_pdf <- system2("quarto", c("render", shQuote(qmd_file), "--to", "typst"))
    pdf_src <- file.path(raiz, "code", paste0(base_name, ".pdf"))
    pdf_dest <- file.path(docs_dir, sprintf("%d_%02d_%s.pdf", ano, mes, dest_name))
    if (status_pdf == 0 && file.exists(pdf_src)) {
      file.copy(pdf_src, pdf_dest, overwrite = TRUE)
      message(sprintf("✓ PDF guardado em:  %s", pdf_dest))
    } else {
      warning(sprintf("Falha ao gerar %s.pdf.", base_name))
    }
  }

  # 3. Gerar Word (DOCX) diretamente via Quarto
  if (isTRUE(render_docx)) {
    message("-> A gerar Word (DOCX)...")
    status_docx <- system2("quarto", c("render", shQuote(qmd_file), "--to", "docx"))
    docx_src <- file.path(raiz, "code", paste0(base_name, ".docx"))
    docx_dest <- file.path(docs_dir, sprintf("%d_%02d_%s.docx", ano, mes, dest_name))
    if (status_docx == 0 && file.exists(docx_src)) {
      file.copy(docx_src, docx_dest, overwrite = TRUE)
      message(sprintf("✓ DOCX guardado em: %s", docx_dest))
    } else {
      warning(sprintf("Falha ao gerar %s.docx.", base_name))
    }
  }

  message("=================================================================")
  message(sprintf("Concluída a renderização de %s", qmd_name))
  message("=================================================================")
  TRUE
}

render_documentos <- function(ano = ANO, mes = MES, raiz = here_root(),
                              run_fase1 = isTRUE(get0("RUN_FASE1", ifnotfound = TRUE)),
                              run_fase2 = isTRUE(get0("RUN_FASE2", ifnotfound = FALSE))) {
  # Fase 1 — Apuramento mensal (IMT) (quando RUN_FASE1 = TRUE)
  if (isTRUE(run_fase1)) {
    render_qmd_doc("informacao1.qmd",  ano = ano, mes = mes, raiz = raiz)
    render_qmd_doc("oficio1.qmd",      ano = ano, mes = mes, raiz = raiz)
    render_qmd_doc("metodologia1.qmd", ano = ano, mes = mes, raiz = raiz, render_docx = FALSE)
  }

  # Fase 2 — Repartição pelos operadores e congelamento (quando RUN_FASE2 = TRUE)
  if (isTRUE(run_fase2)) {
    render_qmd_doc("metodologia2.qmd", ano = ano, mes = mes, raiz = raiz, render_docx = FALSE, out_name = "AnexoI_metodologia")
    render_qmd_doc("informacao2.qmd", ano = ano, mes = mes, raiz = raiz) 
    render_qmd_doc("oficio2.qmd",     ano = ano, mes = mes, raiz = raiz)
  }
}

if (!interactive() && identical(sys.frames(), list())) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2) {
    render_documentos(ano = as.integer(args[1]), mes = as.integer(args[2]))
  } else {
    render_documentos(ano = ANO, mes = MES)
  }
}
