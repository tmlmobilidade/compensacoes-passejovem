# =============================================================================
# doc_params.R — Parâmetros de emissão dos documentos oficiais (mês a mês)
# Passe Gratuito para Jovens — TML
#
# Editar este ficheiro a cada nova emissão mensal.
# É automaticamente carregado por informacao1.qmd e 10_documentos.R via source().
# =============================================================================

ANO      <- 2026
MES      <- 8
mes_nome <- "agosto"

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
df_docs <- if (file.exists(docs_nr_path)) {
  utils::read.csv2(docs_nr_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
} else {
  data.frame()
}

get_doc_nr <- function(doc_type, default, grp = "passejovem") {
  if (nrow(df_docs) > 0 && all(c("group", "ano", "mes", "type", "numero") %in% names(df_docs))) {
    doc_type <- sub("^numero_", "", doc_type)
    row <- df_docs[df_docs$group == grp & df_docs$ano == ANO & df_docs$mes == MES & df_docs$type == doc_type, ]
    if (nrow(row) > 0) {
      val <- trimws(as.character(row$numero[1]))
      if (!is.na(val) && nzchar(val)) return(val)
    }
  }
  default
}

get_doc_date <- function(doc_type, default = NA_character_, grp = "passejovem", num_fallback = "") {
  doc_type <- sub("^numero_", "", doc_type)
  if (nrow(df_docs) > 0 && all(c("group", "ano", "mes", "type") %in% names(df_docs))) {
    row <- df_docs[df_docs$group == grp & df_docs$ano == ANO & df_docs$mes == MES & df_docs$type == doc_type, ]
    if (nrow(row) > 0) {
      if ("date" %in% names(row)) {
        val <- trimws(as.character(row$date[1]))
        if (!is.na(val) && nzchar(val)) return(val)
      }
      if ("numero" %in% names(row) && !nzchar(num_fallback)) {
        num_fallback <- trimws(as.character(row$numero[1]))
      }
    }
  }
  m <- regmatches(num_fallback, regexpr("\\b20\\d{6}\\b", num_fallback))
  if (length(m) == 1 && nzchar(m)) {
    return(sprintf("%s-%s-%s", substr(m, 1, 4), substr(m, 5, 6), substr(m, 7, 8)))
  }
  default
}

format_date_pt <- function(date_str, default = "XX de XXXX de 2026") {
  if (is.na(date_str) || !nzchar(date_str)) return(default)
  d_obj <- tryCatch(as.Date(date_str), error = function(e) NA)
  if (is.na(d_obj)) return(default)
  meses_nm <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho",
                "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
  sprintf("%d de %s de %d",
          as.integer(format(d_obj, "%d")),
          meses_nm[as.integer(format(d_obj, "%m"))],
          as.integer(format(d_obj, "%Y")))
}

# fallback defaults for document numbers if not found in documents_nr.csv
numero_informacao1           <- get_doc_nr("informacao1", "TML-XXXX-XXXX-INF-XXXX-DEP", "passejovem")  # Nº da Informação IMT
numero_oficio1               <- get_doc_nr("oficio1",     "TML-XXXX-XXXX-OF-XXXX-DEP",  "passejovem")  # Nº do Ofício IMT
numero_informacao2           <- get_doc_nr("informacao2", "TML-YYYY-YYYY-INF-YYYY-DEP", "passejovem")  # Nº da Informação Operadores
numero_oficio2               <- get_doc_nr("oficio2",     "TML-YYYY-YYYY-OF-YYYY-DEP",  "passejovem")  # Nº do Ofício Operadores
numero_informacao1_navegante <- get_doc_nr("informacao1", "TML-XXXX-XXXX-INF-XXXX-DEP", "navegante")   # Nº da Informação 1 quotas Navegante
numero_oficio1_navegante     <- get_doc_nr("oficio1",     "TML-XXXX-XXXX-OF-XXXX-DEP",  "navegante")   # Nº do Ofício 1 quotas Navegante
data_documento               <- format(Sys.Date(), "%d/%m/%Y")  # ou fixar manualmente: "24/09/2026"
data_oficio1                 <- format_date_pt(get_doc_date("oficio1", grp = "passejovem", num_fallback = numero_oficio1))
data_oficio1_navegante       <- format_date_pt(get_doc_date("oficio1", grp = "navegante",  num_fallback = numero_oficio1_navegante))

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


