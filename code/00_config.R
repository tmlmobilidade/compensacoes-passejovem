# =============================================================================
# 00_config.R
# Configurações globais, constantes de negócio e funções utilitárias
# partilhadas por todos os scripts do projeto PasseJovem.
#
# Este ficheiro é carregado via source() pelos outros scripts.
# =============================================================================


# -----------------------------------------------------------------------------
# Pacotes
# -----------------------------------------------------------------------------
pkgs <- c("data.table", "readxl", "openxlsx", "lubridate", "stringr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages({
  library(data.table)   # leitura rápida e manipulação de tabelas grandes
  library(readxl)       # leitura de ficheiros Excel (.xlsx)
  library(openxlsx)     # escrita de ficheiros Excel com formatação
  library(lubridate)    # manipulação de datas
  library(stringr)      # manipulação de strings
})


# -----------------------------------------------------------------------------
# Constantes de negócio
# (baseadas na Portaria n.º 7-A/2024, de 5 de janeiro)
# -----------------------------------------------------------------------------

# Regra de compensação AML:
#   - < 20 validações no mês -> 5% * preço_base * n_validações
#   - >= 20 validações        -> preço_base (título completo)
THRESHOLD_VAL <- 20L    # mínimo de validações para compensação total
PCT_VAL       <- 0.05   # percentagem por validação abaixo do threshold

# Códigos de bilhete para o "Complemento Metropolitano"
CODES_COMP <- c(6170L, 7670L)

# Preço fixo do Cascais quando o ficheiro xlsx não tem preço
PRECO_CASCAIS <- 20

# Mapeamento preço_base -> nome IMT (para o relatório)
NOME_IMT <- c(
  "60"    = "Navegante Metropolitano + Complemento",
  "50"    = "Navegante Municipal + Complemento",
  "40"    = "Navegante Metropolitano",
  "30"    = "Navegante Lisboa",
  "20"    = "Viver Cascais",
  "16.75" = "MTS Passe"
)

# Meses em português (para nomes de pastas e relatórios)
MESES_PT <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho",
               "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")

MESES_PT_UPPER <- toupper(MESES_PT)


# -----------------------------------------------------------------------------
# Funções de caminho
# -----------------------------------------------------------------------------

#' Devolve a raiz do projecto.
#' Tenta várias estratégias por ordem:
#'   1. Variável de ambiente PASSE_JOVEM_ROOT
#'   2. Localização do script em execução (sys.frames)
#'   3. Argumento --file= do Rscript
#'   4. Directório de trabalho actual
here_root <- function() {

  # 1. Variável de ambiente (permite override manual)
  env <- Sys.getenv("PASSE_JOVEM_ROOT", unset = "")
  if (nzchar(env) && dir.exists(env)) return(normalizePath(env))

  # 2. Localização do script via sys.frames (quando sourced)
  for (i in seq_len(sys.nframe())) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(ofile)) {
      # O script está em code/, por isso sobe um nível
      candidate <- dirname(dirname(normalizePath(ofile, mustWork = FALSE)))
      if (dir.exists(file.path(candidate, "code"))) return(normalizePath(candidate))
    }
  }

  # 3. Argumento --file= do Rscript (quando executado via Rscript)
  args0 <- commandArgs(FALSE)
  m <- regmatches(args0, regexpr("(?<=--file=).+", args0, perl = TRUE))
  if (length(m) > 0) {
    candidate <- dirname(normalizePath(m, mustWork = FALSE))
    if (dir.exists(file.path(candidate, "code"))) return(normalizePath(candidate))
  }

  # 4. Directório de trabalho como último recurso
  wd <- normalizePath(getwd())
  if (dir.exists(file.path(wd, "code"))) return(wd)

  stop(
    "Nao foi possivel determinar a raiz do projecto PasseJovem.\n",
    "Defina a variavel de ambiente PASSE_JOVEM_ROOT ",
    "ou execute a partir da pasta do projecto (c:/GIS/PasseJovem)."
  )
}

#' Devolve o nome da pasta do mês (ex: "2026_05_maio")
mes_folder <- function(ano, mes) {
  sprintf("%d_%02d_%s", ano, mes, MESES_PT[mes])
}

#' Devolve os caminhos input/output para um dado mês
month_paths <- function(ano, mes, raiz = here_root()) {
  base <- file.path(raiz, "data", mes_folder(ano, mes))
  list(
    base   = base,
    input  = file.path(base, "input"),
    output = file.path(base, "output"),
    auxiliar = file.path(raiz, "data", "auxiliar")
  )
}

#' Constrói o nome de um ficheiro de output sem caracteres especiais
#' Exemplo: output_filename(2026, 5, "comNIF") -> "2026_05_comNIF.csv"
output_filename <- function(ano, mes, sufixo, ext = "csv") {
  sprintf("%d_%02d_%s.%s", ano, mes, sufixo, ext)
}


# -----------------------------------------------------------------------------
# Funções de leitura de ficheiros
# -----------------------------------------------------------------------------

#' Lê o ficheiro principal PasseJovem.
#' Suporta CSV (separador ";" e decimais ",") ou XLSX (a partir de julho 2026).
#' Devolve sempre as mesmas colunas base + eventuais colunas extra (doc_number, doc_type).
#'
#' @param path Caminho para o ficheiro CSV ou XLSX
read_passe_jovem <- function(path) {

  ext <- tolower(tools::file_ext(path))

  if (ext == "csv") {
    # Leitura rápida com data.table::fread
    dt <- fread(path,
      sep = ";", dec = ",",
      encoding = "UTF-8",
      colClasses = list(
        character = c("CARD_SERIAL_NUMBER", "FISCAL_NUMBER", "TICKET_NAME", "BIRTH_DATE"),
        integer   = c("YEAR_DIST", "MONTH_DIST", "AGE", "TICKET_OPERATOR_CODE",
                      "TICKET_CODE", "COUNT_VAL", "V_DELIVERED"),
        numeric   = c("PRICE", "PRICE_TICKET_BASE")
      ),
      select = c("YEAR_DIST", "MONTH_DIST", "CARD_SERIAL_NUMBER", "AGE",
                 "FISCAL_NUMBER", "TICKET_OPERATOR_CODE", "TICKET_CODE",
                 "TICKET_NAME", "PRICE", "TICKET_CODE_BASE", "PRICE_TICKET_BASE",
                 "COUNT_VAL", "BIRTH_DATE")
    )
  } else if (ext %in% c("xlsx", "xls")) {
    # Leitura do XLSX (formato julho 2026+)
    df <- read_excel(path, sheet = 1)
    dt <- as.data.table(df)
    # Manter colunas extra de documento (doc_number, doc_type) se existirem
  } else {
    stop("Formato de ficheiro nao suportado: ", path)
  }

  # Normalizar nomes das colunas para lowercase
  setnames(dt, tolower(names(dt)))

  # Remover coluna ticket_code_base se existir (não usada)
  if ("ticket_code_base" %in% names(dt)) dt[, ticket_code_base := NULL]
  if ("v_delivered"      %in% names(dt)) dt[, v_delivered      := NULL]

  # Converter tipos
  # suppressWarnings: XLSX pode ter células não-numéricas (erros Excel, brancos)
  # que geram avisos de coerção inofensivos — os NAs resultantes são filtrados adiante.
  dt[, birth_date        := parse_date_safe(as.character(birth_date))]
  dt[, fiscal_number     := normalizar_nif(fiscal_number)]
  dt[, price             := suppressWarnings(as.numeric(price))]
  dt[, price_ticket_base := suppressWarnings(as.numeric(price_ticket_base))]

  # Renomear colunas de documento se existirem (formato julho 2026+)
  if ("id_doc_number"      %in% names(dt)) setnames(dt, "id_doc_number",      "doc_number")
  if ("id_doc_number_desc" %in% names(dt)) setnames(dt, "id_doc_number_desc", "doc_type")

  dt
}

#' Lê o ficheiro de Complemento Azambuja (CSV com mesmo formato base)
read_complemento_azambuja <- function(path) {
  dt <- fread(path,
    sep = ";", dec = ",",
    encoding = "UTF-8",
    colClasses = list(
      character = c("CARD_SERIAL_NUMBER", "FISCAL_NUMBER"),
      integer   = c("YEAR_DIST", "MONTH_DIST", "TICKET_CODE", "COUNT_VAL")
    ),
    select = c("YEAR_DIST", "MONTH_DIST", "CARD_SERIAL_NUMBER",
               "FISCAL_NUMBER", "TICKET_CODE", "COUNT_VAL")
  )
  setnames(dt, tolower(names(dt)))
  dt[, fiscal_number := normalizar_nif(fiscal_number)]
  dt
}

#' Lê o ficheiro Excel de Cascais (TML_<mes><ano>.xlsx)
#' O ficheiro não tem preço base preenchido — usamos o valor fixo PRECO_CASCAIS.
#'
#' @param path Caminho para o ficheiro XLSX
#' @param ano  Ano de referência
#' @param mes  Mês de referência
read_cascais <- function(path, ano, mes) {
  df <- read_excel(path)
  dt <- as.data.table(df)
  setnames(dt, tolower(names(dt)))

  # Normalizar campos essenciais
  dt[, year_dist            := as.integer(ano)]
  dt[, month_dist           := as.integer(mes)]
  dt[, ticket_operator_code := 21L]              # código do operador Cascais
  dt[, price                := 0]                # sempre gratuito
  dt[, price_ticket_base    := PRECO_CASCAIS]    # preço fixo (ficheiro vem a 0)
  dt[, birth_date           := as.Date(birth_date)]
  dt[, fiscal_number        := normalizar_nif(as.character(fiscal_number))]

  # Manter só as colunas que existem no main
  cols_base <- c("year_dist", "month_dist", "card_serial_number", "age",
                 "fiscal_number", "ticket_operator_code", "ticket_code",
                 "ticket_name", "price", "price_ticket_base", "count_val",
                 "birth_date")
  cols_existentes <- intersect(cols_base, names(dt))
  dt <- dt[, ..cols_existentes]

  dt
}


# -----------------------------------------------------------------------------
# Funções de limpeza
# -----------------------------------------------------------------------------

#' Normaliza NIF: "" e "0" passam a NA
normalizar_nif <- function(x) {
  x <- as.character(x)
  x[x %in% c("", "0", "NA")] <- NA_character_
  x
}

#' Parse de datas em vários formatos (DD/MM/YYYY ou YYYY-MM-DD)
parse_date_safe <- function(x) {
  out <- as.Date(x, format = "%d/%m/%Y")
  # Segunda tentativa: formato ISO
  bad <- is.na(out) & !is.na(x) & nchar(trimws(x)) > 0
  if (any(bad)) out[bad] <- as.Date(x[bad], format = "%Y-%m-%d")
  out
}

#' Remove registos LX-BT (bilhetes de transporte de Lisboa exclusivos)
#' Estes bilhetes não são elegíveis para compensação.
remove_lxbt <- function(dt) {
  # NA em ticket_name são mantidos (perfil de criança sem bilhete explícito)
  dt[is.na(ticket_name) | !str_starts(ticket_name, "LX-BT")]
}


# -----------------------------------------------------------------------------
# Funções auxiliares de cálculo
# -----------------------------------------------------------------------------

#' Prioridade do operador dentro de um grupo (para ordenação)
#' Op 31 (AT AML) tem prioridade máxima
op_prio <- function(op) {
  fifelse(op == 31L, 1L, fifelse(op == 0L, 2L, 3L))
}

#' Prioridade do código de bilhete dentro de um grupo (para ordenação)
code_prio <- function(c) {
  fifelse(c == 6181L, 1L,
  fifelse(c == 6180L, 2L,
  fifelse(c == 6150L, 3L,
  fifelse(c == 7650L, 4L,
  fifelse(c == 1020L, 5L,
  fifelse(c >= 6151L & c <= 6171L, 6L,
  fifelse(c >= 7650L & c <= 7671L, 7L,
  fifelse(c == 0L, 8L, 9L))))))))
}

#' Calcula nome_dist a partir do ticket_name_new
#' - Sem "+": devolve o nome tal qual
#' - Com complemento 418/sub23: mantém "Parte + Navegante Complemento 418/sub23"
#' - Outros: devolve apenas a parte antes do primeiro "+"
calc_nome_dist <- function(tn) {
  has_plus <- grepl("+", tn, fixed = TRUE)
  has_comp_a <- grepl("Navegante Complemento 418/sub23 \\(A\\)", tn)
  has_comp   <- grepl("Navegante Complemento 418/sub23", tn) & !has_comp_a

  ifelse(!has_plus, tn,
  ifelse(has_comp_a,
         paste0(sub("\\+.*", "+", tn), "Navegante Complemento 418/sub23 (A)"),
  ifelse(has_comp,
         paste0(sub("\\+.*", "+", tn), "Navegante Complemento 418/sub23"),
         sub("\\+.*", "", tn)
  )))
}

#' Calcula emissor a partir do código do operador (como string)
calc_emissor <- function(ops_str) {
  if (startsWith(ops_str, "31")) "AT AML"
  else if (startsWith(ops_str, "21")) "AT Cascais"
  else if (ops_str == "16") "MTS"
  else ""
}

#' Calcula nome_imt a partir do preço base (como string)
calc_nome_imt <- function(ptb_str) {
  NOME_IMT[ptb_str]  # devolve NA se não encontrado
}


# -----------------------------------------------------------------------------
# Escrita de CSV
# -----------------------------------------------------------------------------

#' Grava um data.table como CSV separado por ";" com encoding UTF-8
write_csv_pj <- function(dt, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  fwrite(dt, path, sep = ";", dec = ".", na = "", dateTimeAs = "write.csv")
}
