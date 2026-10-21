# =============================================================================
# doc_params.R — Parâmetros de emissão dos documentos oficiais (mês a mês)
# Passe Gratuito para Jovens — TML
#
# Editar este ficheiro a cada nova emissão mensal.
# É automaticamente carregado por informacao1.qmd e 10_documentos.R via source().
# =============================================================================

ANO      <- 2026
MES      <- 7
mes_nome <- "julho"

# ---- Pagamentos ETF (Tabela 5) -----------------------------------------------
# Meses do ano corrente que já foram pagos pela ETF à data da emissão
meses_pagos_ano_atual <- 1:5   # jan a mai pagos; jun e jul por pagar

# ---- Pagamentos por conta (Portaria n.º 287/2026/1) --------------------------
# Total já recebido da ETF em pagamentos por conta (texto da secção Pagamento)
pagamentos_conta_n     <- 2   # n.º de pagamentos por conta já recebidos
pagamentos_conta_valor <- pagamentos_conta_n * 11216593.11
pagamentos_ETF_valor      <- 12985596 # 
pagamentos_ETF_data       <- "28 de julho de 2026"   # data do último pagamento da ETF

# ---- Configuração de renderização --------------------------------------------
RENDER_PDF  <- TRUE
RENDER_DOCX <- TRUE

# ---- Referências oficiais ----------------------------------------------------
docs_nr_path <- here::here("..", "Compensacoes", "lists", "documents_nr.csv")
docs_nr_row <- if (file.exists(docs_nr_path)) {
  df_docs <- utils::read.csv2(docs_nr_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  df_docs[df_docs$tipo == "passejovem" & df_docs$ano == ANO & df_docs$mes == MES, ]
} else {
  data.frame()
}

get_doc_nr <- function(col, default) {
  if (nrow(docs_nr_row) > 0 && col %in% names(docs_nr_row)) {
    val <- trimws(as.character(docs_nr_row[[col]][1]))
    if (!is.na(val) && nzchar(val)) return(val)
  }
  default
}

# fallback defaults for document numbers if not found in documents_nr.csv
numero_informacao1 <- get_doc_nr("numero_informacao1", "TML-XXXX-XXXX-INF-XXXX-DEP")  # Nº da Informação IMT
numero_oficio1     <- get_doc_nr("numero_oficio1",     "TML-XXXX-XXXX-OF-XXXX-DEP")   # Nº do Ofício IMT
numero_informacao2 <- get_doc_nr("numero_informacao2", "TML-YYYY-YYYY-INF-YYYY-DEP")  # Nº da Informação Operadores
numero_oficio2     <- get_doc_nr("numero_oficio2",     "TML-YYYY-YYYY-OF-YYYY-DEP")   # Nº do Ofício Operadores
data_documento     <- format(Sys.Date(), "%d/%m/%Y")  # ou fixar manualmente: "24/09/2026"

# ---- Autores / Intervenientes ------------------------------------------------
de_pessoa1_nome  <- "Ana Isabel Capote Fernandes Rosado"
de_pessoa1_cargo <- "Técnica Superior"

de_pessoa2_nome  <- "Luísa Nogueira"
de_pessoa2_cargo <- "Técnica Superior"

de_pessoa3_nome  <- "Rosa Félix"
de_pessoa3_cargo <- "Técnica Superior"

de_divisao       <- "Estudos e Planeamento - Sistemas de Transportes"

# ---- Destinatário (interno) --------------------------------------------------
para_pessoa_nome <- "António Sérgio Vaz Rei Manso Pinheiro"
para_cargo       <- "Diretor"
para_divisao     <- "Estudos e Planeamento"


