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
pkgs <- c("data.table", "dplyr", "readxl", "openxlsx", "lubridate", "stringr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages({
  library(data.table)   # leitura rápida (fread/fwrite) e tabelas grandes
  library(dplyr)        # manipulação de dados com verbos legíveis
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
    dt <- as.data.table(read_excel(path, sheet = 1))
    # Manter colunas extra de documento (doc_number, doc_type) se existirem
  } else {
    stop("Formato de ficheiro nao suportado: ", path)
  }

  dt |>
    # Normalizar nomes das colunas para lowercase
    rename_with(tolower) |>
    # Remover colunas não usadas (silencioso se não existirem)
    select(-any_of(c("ticket_code_base", "v_delivered"))) |>
    mutate(
      # Converter tipos — suppressWarnings porque XLSX pode ter células não-numéricas
      # (erros Excel, brancos) que geram avisos de coerção; os NAs são filtrados adiante
      year_dist            = suppressWarnings(as.integer(year_dist)),
      month_dist           = suppressWarnings(as.integer(month_dist)),
      age                  = suppressWarnings(as.integer(age)),
      ticket_operator_code = suppressWarnings(as.integer(ticket_operator_code)),
      ticket_code          = suppressWarnings(as.integer(ticket_code)),
      count_val            = suppressWarnings(as.integer(count_val)),
      card_serial_number   = as.character(card_serial_number),
      ticket_name          = as.character(ticket_name),
      birth_date           = parse_date_safe(as.character(birth_date)),
      fiscal_number        = normalizar_nif(fiscal_number),
      price                = suppressWarnings(as.numeric(price)),
      price_ticket_base    = suppressWarnings(as.numeric(price_ticket_base))
    ) |>
    # Renomear colunas de documento se existirem (formato julho 2026+)
    rename(any_of(c(doc_number = "id_doc_number", doc_type = "id_doc_number_desc"))) |>
    mutate(across(any_of(c("doc_number", "doc_type")), ~ {
      val <- trimws(as.character(.x))
      if_else(val %in% c("", "NA"), NA_character_, val)
    })) |>
    as.data.table()
}

#' Lê o ficheiro de Complemento Azambuja (CSV com mesmo formato base)
read_complemento_azambuja <- function(path) {
  fread(path,
    sep = ";", dec = ",",
    encoding = "UTF-8",
    colClasses = list(
      character = c("CARD_SERIAL_NUMBER", "FISCAL_NUMBER"),
      integer   = c("YEAR_DIST", "MONTH_DIST", "TICKET_CODE", "COUNT_VAL")
    ),
    select = c("YEAR_DIST", "MONTH_DIST", "CARD_SERIAL_NUMBER",
               "FISCAL_NUMBER", "TICKET_CODE", "COUNT_VAL")
  ) |>
    rename_with(tolower) |>
    mutate(fiscal_number = normalizar_nif(fiscal_number)) |>
    as.data.table()
}

#' Lê o Complemento Azambuja a partir de uma folha de um ficheiro XLSX.
#' Usado quando os dados Azambuja estão na segunda folha do XLSX principal
#' (formato a partir de julho 2026).
#'
#' @param path  Caminho para o ficheiro XLSX
#' @param sheet Nome ou índice da folha (default: 2)
read_complemento_azambuja_xlsx <- function(path, sheet = 2) {

  # Colunas necessárias (any_of ignora silenciosamente as que não existam)
  cols_needed <- c("year_dist", "month_dist", "card_serial_number",
                   "fiscal_number", "ticket_code", "count_val")

  read_excel(path, sheet = sheet) |>
    rename_with(tolower) |>
    select(any_of(cols_needed)) |>
    mutate(
      year_dist          = suppressWarnings(as.integer(year_dist)),
      month_dist         = suppressWarnings(as.integer(month_dist)),
      card_serial_number = as.character(card_serial_number),
      fiscal_number      = normalizar_nif(as.character(fiscal_number)),
      ticket_code        = suppressWarnings(as.integer(ticket_code)),
      count_val          = suppressWarnings(as.integer(count_val))
    ) |>
    as.data.table()
}


#' Lê o ficheiro Excel de Cascais (TML_<mes><ano>.xlsx)
#' O ficheiro não tem preço base preenchido — usamos o valor fixo PRECO_CASCAIS.
#'
#' @param path Caminho para o ficheiro XLSX
#' @param ano  Ano de referência
#' @param mes  Mês de referência
read_cascais <- function(path, ano, mes) {

  # Colunas a manter (na ordem do ficheiro principal)
  cols_base <- c("year_dist", "month_dist", "card_serial_number", "age",
                 "fiscal_number", "ticket_operator_code", "ticket_code",
                 "ticket_name", "price", "price_ticket_base", "count_val",
                 "birth_date")

  read_excel(path) |>
    # Normalizar nomes das colunas para lowercase
    rename_with(tolower) |>
    mutate(
      year_dist            = as.integer(ano),
      month_dist           = as.integer(mes),
      card_serial_number   = as.character(card_serial_number),
      age                  = suppressWarnings(as.integer(age)),
      ticket_operator_code = 21L,                              # código do operador Cascais
      ticket_code          = suppressWarnings(as.integer(ticket_code)),
      ticket_name          = as.character(ticket_name),
      price                = 0,                                # sempre gratuito
      price_ticket_base    = PRECO_CASCAIS,                    # preço fixo (ficheiro vem a 0)
      count_val            = suppressWarnings(as.integer(count_val)),
      birth_date           = as.Date(birth_date),
      fiscal_number        = normalizar_nif(as.character(fiscal_number))
    ) |>
    # Manter só as colunas que existem no ficheiro principal (ignora extras silenciosamente)
    select(any_of(cols_base)) |>
    as.data.table()
}


# -----------------------------------------------------------------------------
# Funções de limpeza
# -----------------------------------------------------------------------------

#' Normaliza NIF: "", "0" e "NA" passam a NA
normalizar_nif <- function(x) {
  x <- as.character(x)
  # Substituir valores inválidos por NA (if_else preserva o tipo character)
  if_else(x %in% c("", "0", "NA"), NA_character_, x)
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
  dt |> filter(is.na(ticket_name) | !str_starts(ticket_name, "LX-BT"))
}

#' Verifica se um documento está na lista de exceções (id_exception.csv)
#' Permite correspondência por (doc_type, doc_number) ou apenas por doc_number
#' quando doc_type não está especificado no ficheiro de exceções.
is_in_id_exception <- function(type_vec, num_vec, dt_exc) {
  if (is.null(dt_exc) || nrow(dt_exc) == 0) return(rep(FALSE, length(num_vec)))
  num_clean  <- trimws(as.character(num_vec))
  type_clean <- toupper(trimws(as.character(type_vec)))

  exc_with_type <- dt_exc[!is.na(doc_type) & nzchar(doc_type)]
  match_with_type <- if (nrow(exc_with_type) > 0) {
    paste0(type_clean, "::", num_clean) %in% paste0(exc_with_type$doc_type, "::", exc_with_type$doc_number)
  } else rep(FALSE, length(num_clean))

  exc_no_type <- dt_exc[is.na(doc_type) | !nzchar(doc_type)]
  match_no_type <- if (nrow(exc_no_type) > 0) {
    num_clean %in% exc_no_type$doc_number
  } else rep(FALSE, length(num_clean))

  match_with_type | match_no_type
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
  has_plus  <- grepl("+", tn, fixed = TRUE)
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
  tryCatch(
    fwrite(dt, path, sep = ";", dec = ".", na = "", dateTimeAs = "write.csv", row.names = FALSE),
    error = function(e) {
      if (grepl("Permission denied", conditionMessage(e))) {
        stop(sprintf("Nao foi possivel gravar '%s'.\nO ficheiro esta aberto noutro programa (ex: Excel)? Feche o ficheiro e tente novamente.", basename(path)), call. = FALSE)
      } else {
        stop(e)
      }
    }
  )
}

