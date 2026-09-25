# =============================================================================
# 00_utils.R
# Funções utilitárias partilhadas por todos os scripts do projeto PasseJovem
# =============================================================================

# -----------------------------------------------------------------------------
# Pacotes necessários (instalados se ausentes)
# -----------------------------------------------------------------------------
pkgs <- c("dplyr", "readr", "readxl", "writexl", "lubridate", "stringr",
          "tidyr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(writexl)
  library(lubridate)
  library(stringr)
  library(tidyr)
})


# -----------------------------------------------------------------------------
# Constantes de negócio
# -----------------------------------------------------------------------------

# Threshold de validações para compensação total (AM/AML)
THRESHOLD_AM  <- 20L   # >= 20 validações -> paga título por inteiro
PCT_AM        <- 0.05  # 5 % por validação abaixo do threshold

# Mapeamento price_ticket_base_new -> nome_imt
NOME_IMT_MAP <- c(
  "60"    = "Navegante Metropolitano + Complemento",
  "50"    = "Navegante Municipal + Complemento",
  "40"    = "Navegante Metropolitano",
  "30"    = "Navegante Lisboa",
  "20"    = "Viver Cascais",
  "16.75" = "MTS Passe"
)

# Mapeamento ticket_operator_code_new -> emissor
EMISSOR_MAP <- list(
  `31` = "AT AML",
  `21` = "AT Cascais",
  `16` = "MTS"
)


# -----------------------------------------------------------------------------
# Caminhos de dados
# -----------------------------------------------------------------------------

#' Devolve o caminho base de dados para um dado ano/mês
#' @param ano  integer (ex: 2026)
#' @param mes  integer (ex: 5)
#' @param raiz raiz do projeto (por omissão detecta automaticamente)
data_dir <- function(ano, mes, raiz = here_root()) {
  mes_nome <- tolower(format(as.Date(sprintf("%d-%02d-01", ano, mes)), "%B",
                             locale = "Portuguese_Portugal.1252"))
  folder   <- sprintf("%d_%02d_%s", ano, mes, mes_nome)
  file.path(raiz, "data", folder)
}

#' Devolve a raiz do projeto (directório que contém a pasta R/)
here_root <- function() {
  # 1. Variavel de ambiente (permite override externo)
  env_root <- Sys.getenv("PASSE_JOVEM_ROOT", unset = "")
  if (nzchar(env_root) && dir.exists(env_root)) return(normalizePath(env_root))

  # 2. Tentar a partir do ficheiro do script em execucao (sys.frames)
  for (i in seq_len(sys.nframe())) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(ofile)) {
      candidate <- dirname(dirname(normalizePath(ofile, mustWork = FALSE)))
      if (dir.exists(file.path(candidate, "R"))) return(normalizePath(candidate))
    }
  }

  # 3. Tentar a partir do --file= de Rscript
  args0 <- commandArgs(FALSE)
  m <- regmatches(args0, regexpr("(?<=--file=).+", args0, perl = TRUE))
  if (length(m) > 0) {
    candidate <- dirname(dirname(normalizePath(m, mustWork = FALSE)))
    if (dir.exists(file.path(candidate, "R"))) return(normalizePath(candidate))
    candidate2 <- dirname(normalizePath(m, mustWork = FALSE))
    if (dir.exists(file.path(candidate2, "R"))) return(normalizePath(candidate2))
  }

  # 4. Directorio de trabalho como ultimo recurso
  wd <- normalizePath(getwd())
  if (dir.exists(file.path(wd, "R"))) return(wd)

  stop("Nao foi possivel determinar a raiz do projecto PasseJovem. ",
       "Defina a variavel de ambiente PASSE_JOVEM_ROOT ou execute a partir ",
       "da pasta do projecto (c:/GIS/PasseJovem).")
}

#' Caminhos standard das pastas input/output para um mês
month_paths <- function(ano, mes, raiz = here_root()) {
  base <- data_dir(ano, mes, raiz)
  list(
    base   = base,
    input  = file.path(base, "input"),
    output = file.path(base, "output")
  )
}


# -----------------------------------------------------------------------------
# Leitura de ficheiros de entrada
# -----------------------------------------------------------------------------

#' Lê o ficheiro principal PasseJovem (CSV com separador ";", encoding UTF-8)
#' Normaliza os nomes das colunas para lowercase e converte tipos.
read_passe_jovem <- function(path) {
  df <- read_delim(path,
    delim     = ";",
    locale    = locale(encoding = "UTF-8", decimal_mark = ",",
                       grouping_mark = "."),
    col_types = cols(
      YEAR_DIST           = col_integer(),
      MONTH_DIST          = col_integer(),
      CARD_SERIAL_NUMBER  = col_character(),
      AGE                 = col_integer(),
      FISCAL_NUMBER       = col_character(),
      TICKET_OPERATOR_CODE = col_integer(),
      TICKET_CODE         = col_integer(),
      TICKET_NAME         = col_character(),
      PRICE               = col_double(),
      TICKET_CODE_BASE    = col_integer(),
      PRICE_TICKET_BASE   = col_double(),
      COUNT_VAL           = col_integer(),
      V_DELIVERED         = col_integer(),
      BIRTH_DATE          = col_character()
    ),
    show_col_types = FALSE
  )
  df <- rename_cols_lower(df)
  df <- df |>
    mutate(
      birth_date     = parse_date_safe(birth_date),
      fiscal_number  = na_if(fiscal_number, ""),
      fiscal_number  = na_if(fiscal_number, "0")
    )
  df
}

#' Lê o ficheiro de Complemento Azambuja (mesmo formato)
read_complemento_azambuja <- function(path) {
  df <- read_delim(path,
    delim     = ";",
    locale    = locale(encoding = "UTF-8", decimal_mark = ",",
                       grouping_mark = "."),
    col_types = cols(
      YEAR_DIST           = col_integer(),
      MONTH_DIST          = col_integer(),
      CARD_SERIAL_NUMBER  = col_character(),
      AGE                 = col_integer(),
      FISCAL_NUMBER       = col_character(),
      TICKET_OPERATOR_CODE = col_integer(),
      TICKET_CODE         = col_integer(),
      TICKET_NAME         = col_character(),
      PRICE               = col_double(),
      TICKET_CODE_BASE    = col_integer(),
      PRICE_TICKET_BASE   = col_double(),
      COUNT_VAL           = col_integer(),
      V_DELIVERED         = col_integer(),
      BIRTH_DATE          = col_character()
    ),
    show_col_types = FALSE
  )
  df <- rename_cols_lower(df)
  df <- df |>
    mutate(
      birth_date    = parse_date_safe(birth_date),
      fiscal_number = na_if(fiscal_number, ""),
      fiscal_number = na_if(fiscal_number, "0")
    )
  df
}

#' Lê o ficheiro Excel de Cascais (TML_<mes><ano>.xlsx)
#' Acrescenta colunas em falta (price, ticket_code_base, price_ticket_base)
read_cascais <- function(path, ano, mes) {
  df <- read_excel(path,
    col_types = c(
      "numeric",   # year_dist
      "numeric",   # month_dist
      "text",      # card_serial_number
      "numeric",   # age
      "numeric",   # fiscal_number
      "numeric",   # ticket_operator_code
      "numeric",   # ticket_code
      "text",      # ticket_name
      "numeric",   # ticket_code_base
      "numeric",   # price_ticket_base
      "numeric",   # count_val
      "date"       # birth_date
    )
  )
  df <- rename_cols_lower(df)
  df <- df |>
    mutate(
      year_dist            = as.integer(ano),
      month_dist           = as.integer(mes),
      ticket_operator_code = 21L,          # Cascais operator code
      price                = 0,            # sempre gratuito
      price_ticket_base    = coalesce(as.double(price_ticket_base), 20),
      ticket_code_base     = coalesce(as.integer(ticket_code_base), 900L),
      birth_date           = as.Date(birth_date),
      fiscal_number        = as.character(as.integer(fiscal_number)),
      fiscal_number        = na_if(fiscal_number, "NA"),
      fiscal_number        = na_if(fiscal_number, "0")
    )
  df
}


# -----------------------------------------------------------------------------
# Transformações / limpeza
# -----------------------------------------------------------------------------

#' Converte nomes de colunas para lowercase
rename_cols_lower <- function(df) {
  names(df) <- tolower(names(df))
  df
}

#' Tenta fazer parse de datas em vários formatos portugueses
parse_date_safe <- function(x) {
  out <- as.Date(x, format = "%d/%m/%Y")
  bad <- is.na(out) & !is.na(x) & nchar(trimws(x)) > 0
  if (any(bad)) {
    out[bad] <- as.Date(x[bad], format = "%Y-%m-%d")
  }
  out
}

#' Remove registos LX-BT (conforme SQL original)
remove_lxbt <- function(df) {
  df |> filter(!str_starts(ticket_name, "LX-BT"))
}

#' Filtra apenas passes gratuitos com pelo menos 1 validação
filter_gratuitos <- function(df) {
  df |> filter(price == 0)
}


# -----------------------------------------------------------------------------
# Lógica de compensação (AML - Área Metropolitana)
# -----------------------------------------------------------------------------

#' Calcula compensação para AML:
#'   < 20 validações -> 5% * price_ticket_base * numval
#'   >= 20 validações -> price_ticket_base (valor inteiro)
#' @param price_ticket_base_new  numeric
#' @param numval                 integer
calc_compensacao_am <- function(price_ticket_base_new, numval) {
  ifelse(
    numval < THRESHOLD_AM,
    PCT_AM * price_ticket_base_new * numval,
    price_ticket_base_new
  )
}


#' Determina emissor a partir de ticket_operator_code_new (string)
get_emissor <- function(cod) {
  case_when(
    str_starts(as.character(cod), "31") ~ "AT AML",
    str_starts(as.character(cod), "21") ~ "AT Cascais",
    as.character(cod) == "16"           ~ "MTS",
    TRUE                                ~ ""
  )
}

#' Determina nome_imt a partir de price_ticket_base_new (string/numeric)
get_nome_imt <- function(ptb) {
  ptb_chr <- as.character(ptb)
  dplyr::recode(ptb_chr, !!!NOME_IMT_MAP, .default = NA_character_)
}

#' Calcula price_ticket_base_new para um grupo de registos do mesmo cartão/NIF.
#' Complemento (código 6170/7670) acrescenta 20 ao max existente.
calc_price_ticket_base_new <- function(ticket_code, price_ticket_base) {
  codes_str <- paste(sort(ticket_code), collapse = "_")
  has_comp  <- grepl("6170|7670", codes_str)
  max_ptb   <- max(price_ticket_base, na.rm = TRUE)
  if (has_comp && grepl("_", codes_str)) {
    as.character(max_ptb + 20)
  } else if (has_comp) {
    "60"
  } else {
    as.character(max_ptb)
  }
}


# -----------------------------------------------------------------------------
# Funções de ordenação de tickets (replicam a lógica ORDER BY do SQL)
# -----------------------------------------------------------------------------

#' Prioridade do operador para ordenação dentro do grupo
op_priority <- function(ticket_operator_code) {
  case_when(
    ticket_operator_code == 31 ~ 1L,
    ticket_operator_code == 0  ~ 2L,
    TRUE                       ~ 3L
  )
}

#' Prioridade do código de título para ordenação dentro do grupo
code_priority <- function(ticket_code) {
  case_when(
    ticket_code == 6181                         ~ 1L,
    ticket_code == 6180                         ~ 2L,
    ticket_code == 6150                         ~ 3L,
    ticket_code == 7650                         ~ 4L,
    ticket_code == 1020                         ~ 5L,
    ticket_code >= 6151 & ticket_code <= 6171   ~ 6L,
    ticket_code >= 7650 & ticket_code <= 7671   ~ 7L,
    ticket_code == 0                            ~ 8L,
    TRUE                                        ~ 9L
  )
}


#' Constrói nome_dist a partir de ticket_name_new:
#'   - sem '+' -> devolve o próprio
#'   - com complemento 418/sub23 -> toma a parte antes do '+' + label complemento
#'   - outros -> toma a parte antes do primeiro '+'
calc_nome_dist <- function(ticket_name_new) {
  case_when(
    !str_detect(ticket_name_new, fixed("+")) ~ ticket_name_new,
    str_detect(ticket_name_new,
               "Navegante Complemento 418/sub23 \\(A\\)") ~
      paste0(str_sub(ticket_name_new, 1,
                     str_locate(ticket_name_new, fixed("+"))[, "start"]),
             "Navegante Complemento 418/sub23 (A)"),
    str_detect(ticket_name_new,
               "Navegante Complemento 418/sub23") ~
      paste0(str_sub(ticket_name_new, 1,
                     str_locate(ticket_name_new, fixed("+"))[, "start"]),
             "Navegante Complemento 418/sub23"),
    TRUE ~
      str_sub(ticket_name_new, 1,
              str_locate(ticket_name_new, fixed("+"))[, "start"] - 1)
  )
}


# -----------------------------------------------------------------------------
# Escrita de outputs
# -----------------------------------------------------------------------------

#' Guarda um data frame como CSV (separador ";", na = "")
write_csv_pj <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_delim(df, path, delim = ";", na = "")
}

#' Nome de ficheiro de output sem caracteres especiais
#' Ex: output_filename(2026, 5, "comNIF") -> "2026_05_comNIF.csv"
output_filename <- function(ano, mes, sufixo, ext = "csv") {
  sprintf("%d_%02d_%s.%s", ano, mes, sufixo, ext)
}
