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
# Controlo de fases do pipeline
# -----------------------------------------------------------------------------

# Executar a Fase 2 (repartição por operador e congelamento: scripts 04 e 05 +
# documentos metodologia2, informacao2 e oficio2)?
#   FALSE -> corre apenas a Fase 1 (apuramento para o IMT: scripts 01 a 03 + docs 1)
#   TRUE  -> corre também a Fase 2 (quando as quotas mensais já estão disponíveis)
RUN_FASE2 <- FALSE

# Renderizar os documentos oficiais em PDF/Word (10_documentos.R)?
#   FALSE -> salta a renderização Quarto (informacao, oficio, metodologia)
#   TRUE  -> executa render_documentos() no final do pipeline
RENDER_DOCS <- TRUE


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
CODES_COMP       <- c(6170L, 7670L) # para detectar os complementos nos ticket_code_new
CODE_COMP_RESUMO <- 1070L   # código do Complemento CM na tabela de resumo/repartição

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

  # 4. Directório de trabalho (ou pasta-mãe se estiver em code/)
  wd <- normalizePath(getwd())
  if (dir.exists(file.path(wd, "code"))) return(wd)
  if (dir.exists(file.path(dirname(wd), "code"))) return(normalizePath(dirname(wd)))
  if (requireNamespace("here", quietly = TRUE)) {
    h <- tryCatch(normalizePath(here::here()), error = function(e) NULL)
    if (!is.null(h) && dir.exists(file.path(h, "code"))) return(h)
  }

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
  base   <- file.path(raiz, "data", mes_folder(ano, mes))
  output <- file.path(base, "output")
  list(
    base     = base,
    input    = file.path(base, "input"),
    output   = output,
    docs     = file.path(output, "docs"),
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


# -----------------------------------------------------------------------------
# Atualização de compensações anteriores (data/auxiliar/compensacao_pj_anteriores.csv)
# -----------------------------------------------------------------------------

#' Atualiza a tabela de compensações anteriores com os dados da run do mês.
#' Lê os outputs gerados pelo pipeline (<ano>_<mm>_comNIF.csv e <ano>_<mm>_semNIF.csv)
#' e adiciona ou atualiza a linha correspondente em data/auxiliar/compensacao_pj_anteriores.csv.
#'
#' @param ano  Ano de referência (ex: 2026)
#' @param mes  Mês de referência (1–12)
#' @param raiz Raiz do projecto (detectada automaticamente)
#' @return data.frame com a tabela atualizada (invisível)
atualizar_compensacao_pj_anteriores <- function(ano, mes, raiz = here_root()) {
  paths <- month_paths(ano, mes, raiz)
  f_ant <- file.path(paths$auxiliar, "compensacao_pj_anteriores.csv")

  f_com <- file.path(paths$output, output_filename(ano, mes, "comNIF"))
  f_sem <- file.path(paths$output, output_filename(ano, mes, "semNIF"))

  if (!file.exists(f_com) || !file.exists(f_sem)) {
    warning("Ficheiros comNIF/semNIF nao encontrados para atualizar compensacao_pj_anteriores.csv.")
    return(invisible(NULL))
  }

  dt_com <- fread(f_com, sep = ";", dec = ".")
  dt_sem <- fread(f_sem, sep = ";", dec = ".")

  # Registos brutos do mês (se gravado pelo 01_preparar_dados.R em contagens_01.rds)
  f_cont <- file.path(paths$output, output_filename(ano, mes, "contagens_01", ext = "rds"))
  n_registos <- if (file.exists(f_cont)) {
    cont_01 <- readRDS(f_cont)
    if (!is.null(cont_01$n_brutos)) cont_01$n_brutos else NA_integer_
  } else {
    NA_integer_
  }
  if (is.na(n_registos)) {
    f_limpos <- file.path(paths$output, output_filename(ano, mes, "limpos_preagregacao"))
    if (file.exists(f_limpos)) n_registos <- nrow(fread(f_limpos, sep = ";", dec = "."))
  }

  n_users      <- nrow(dt_com) + nrow(dt_sem)
  n_semnif     <- nrow(dt_sem)
  n_vals       <- sum(dt_com$numval, na.rm = TRUE) + sum(dt_sem$numval, na.rm = TRUE)
  v_user       <- if (n_users > 0) n_vals / n_users else 0
  comp_tot     <- sum(dt_com$compensacao, na.rm = TRUE) + sum(dt_sem$compensacao, na.rm = TRUE)
  comp_user    <- if (n_users > 0) comp_tot / n_users else 0

  if (file.exists(f_ant)) {
    df_ant <- utils::read.csv2(f_ant, stringsAsFactors = FALSE, dec = ",")
  } else {
    df_ant <- data.frame(
      ano                    = integer(),
      mes                    = integer(),
      registos_pj            = integer(),
      utilizadores_pj        = integer(),
      utilizadores_pj_semNIF = integer(),
      validacoes_pj          = integer(),
      validacoes_utilizador  = numeric(),
      compensacao_valor      = numeric(),
      compensasao_utilizador = numeric(),
      stringsAsFactors       = FALSE
    )
  }

  idx <- which(df_ant$ano == ano & df_ant$mes == mes)
  if (length(idx) > 0) {
    reg_val <- if (!is.na(n_registos)) n_registos else df_ant$registos_pj[idx]
    df_ant$registos_pj[idx]              <- reg_val
    df_ant$utilizadores_pj[idx]         <- n_users
    df_ant$utilizadores_pj_semNIF[idx]  <- n_semnif
    df_ant$validacoes_pj[idx]           <- n_vals
    df_ant$validacoes_utilizador[idx]   <- round(v_user, 8)
    df_ant$compensacao_valor[idx]       <- round(comp_tot, 2)
    df_ant$compensasao_utilizador[idx]  <- round(comp_user, 8)
    message(sprintf("[00] Mes %d/%02d atualizado em compensacao_pj_anteriores.csv", ano, mes))
  } else {
    nova_linha <- data.frame(
      ano                    = as.integer(ano),
      mes                    = as.integer(mes),
      registos_pj            = as.integer(n_registos),
      utilizadores_pj        = as.integer(n_users),
      utilizadores_pj_semNIF = as.integer(n_semnif),
      validacoes_pj          = as.integer(n_vals),
      validacoes_utilizador  = round(v_user, 8),
      compensacao_valor      = round(comp_tot, 2),
      compensasao_utilizador = round(comp_user, 8),
      stringsAsFactors       = FALSE
    )
    df_ant <- rbind(df_ant, nova_linha)
    df_ant <- df_ant[order(df_ant$ano, df_ant$mes), ]
    message(sprintf("[00] Novo mes %d/%02d adicionado a compensacao_pj_anteriores.csv", ano, mes))
  }

  dir.create(dirname(f_ant), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(df_ant, f_ant, sep = ";", dec = ",", row.names = FALSE, na = "", quote = FALSE)
  invisible(df_ant)
}


# -----------------------------------------------------------------------------
# Integração com o repositório Compensacoes (partilhado por 04 e 05)
# -----------------------------------------------------------------------------

# Raiz do repositório Compensacoes (pode ser sobreposta por variável de ambiente)
COMPENSACOES_ROOT <- Sys.getenv("COMPENSACOES_ROOT", unset = "C:/Repos/Compensacoes")

# Fonte alternativa (repositório público) caso os ficheiros locais não existam
COMPENSACOES_GITHUB <-
  "https://raw.githubusercontent.com/tmlmobilidade/compensacoes-navegante/main/lists"

# Nomes das entidades que recebem 100% das parcelas "não Navegante"
OPERADOR_CM      <- "AT TML - CM"
OPERADOR_CASCAIS <- "AT Cascais"

MESES_ABREV <- c("jan", "fev", "mar", "abr", "mai", "jun",
                 "jul", "ago", "set", "out", "nov", "dez")

# Colunas de operadores do resumo (ordem do Excel de referência) e respectivos
# códigos no ficheiro de quotas do Compensacoes.
OPERADORES_RESUMO <- c(
  "CP"          = "CP",
  "FT"          = "FT",
  "TT"          = "TT",
  "SL"          = "SL",
  "ML"          = "ML",
  "MTS"         = "MS",
  "Carris"      = "CA",
  "TCB"         = "TB",
  "AT Cascais"  = "CAP",
  "AT TML - CM" = "CM",
  "BT Oeste"    = "BQ"
)

#' Lê uma lista do Compensacoes (local; se não existir, do GitHub).
ler_lista_compensacoes <- function(nome) {
  local <- file.path(COMPENSACOES_ROOT, "lists", nome)
  src   <- if (file.exists(local)) local else file.path(COMPENSACOES_GITHUB, nome)
  # fill = TRUE: algumas linhas de titulos_nomes.csv têm menos colunas
  fread(src, sep = ";", fill = TRUE, encoding = "UTF-8", na.strings = c("", "NA")) |>
    as_tibble()
}

#' Caminho do ficheiro de quotas do Compensacoes para um mês.
caminho_quotas <- function(ano, mes) {
  pasta <- sprintf("%02d_%s_%d", mes, MESES_ABREV[mes], ano)
  file.path(COMPENSACOES_ROOT, "data", pasta, "output",
            sprintf("1_quotas_%s%02d.csv", MESES_ABREV[mes], ano %% 100))
}



