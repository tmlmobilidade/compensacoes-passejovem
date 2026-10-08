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

# ---- Referências oficiais ----------------------------------------------------
numero_informacao <- "TML-I02308-20260930-INF-00326-DEP"   # Nº da Informação Técnica
data_documento    <- "30/09/2026"                          # Data do documento

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

# ---- Pagamentos ETF (Tabela 5) -----------------------------------------------
# Meses do ano corrente que já foram pagos pela ETF à data da emissão
meses_pagos_ano_atual <- 1:5   # jan a mai pagos; jun e jul por pagar

# ---- Configuração de renderização --------------------------------------------
RENDER_PDF  <- TRUE
RENDER_DOCX <- TRUE

