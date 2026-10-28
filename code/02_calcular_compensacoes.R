# =============================================================================
# 02_calcular_compensacoes.R
# Calcula as compensações comNIF e semNIF para um dado mês.
#
# Lê os ficheiros intermédios gerados por 01_preparar_dados.R e produz:
#   - <ano>_<mm>_comNIF.csv   (agregado por NIF)
#   - <ano>_<mm>_semNIF.csv   (agregado por número de cartão, para quem não tem NIF)
#   - <ano>_<mm>_a_verificar.csv  (lista de pessoas com dados suspeitos)
#
# Também suporta exceções manuais de identificação de:
#   - data/auxiliar/id_exception.csv (documentos conhecidos por serem usados por crianças diferentes)
#
# Regra de compensação (AML):
#   - < 20 validações -> 5% * preço_base * n_validações
#   - >= 20 validações -> preço_base (título completo)
#
# Uso:
#   source("code/02_calcular_compensacoes.R")
#   calcular_compensacoes(ano = 2026, mes = 7)
# =============================================================================

source(file.path(dirname(sys.frame(1)$ofile), "00_config.R"),
       local = TRUE, chdir = FALSE)


# =============================================================================
# Funções internas de agregação
# =============================================================================

# Estas funções constroem os campos "_new" para um grupo de registos
# correspondentes ao mesmo NIF (comNIF) ou cartão (semNIF).
# Um grupo pode ter 1 bilhete (caso simples) ou vários (ex: Metropolitano + Complemento).


#' Agrega todos os registos de um grupo (mesmo NIF ou mesmo cartão) num único resultado.
#'
#' @param ticket_operator_code  vector de inteiros
#' @param ticket_code           vector de inteiros
#' @param ticket_name           vector de caracteres
#' @param price_ticket_base     vector numérico
#' @param count_val             vector de inteiros
#' @param age                   vector de inteiros
#' @param birth_date            vector de datas
#' @param card_serial_number    vector de caracteres (NULL para semNIF)
#' @param para_comnif           lógico: TRUE = comNIF, FALSE = semNIF
#' @return lista com os campos calculados para o grupo
agregar_grupo <- function(ticket_operator_code, ticket_code, ticket_name,
                          price_ticket_base, count_val, age, birth_date,
                          card_serial_number = NULL, para_comnif = TRUE) {

  # Ordenar bilhetes por prioridade (operador primeiro, depois código de bilhete)
  ord      <- order(op_prio(ticket_operator_code), code_prio(ticket_code))
  ord_op   <- order(op_prio(ticket_operator_code))

  codes_ord  <- ticket_code[ord]
  codes_str  <- paste(codes_ord, collapse = "_")
  ops_str    <- paste(ticket_operator_code[ord_op], collapse = "_")
  names_str  <- paste(ticket_name[ord], collapse = "+")

  age_min    <- min(age, na.rm = TRUE)
  has_comp   <- any(ticket_code %in% CODES_COMP)
  max_ptb    <- max(price_ticket_base, na.rm = TRUE)

  # Preço base do grupo:
  #   - só complemento -> 60
  #   - complemento + outro -> max_ptb + 20
  #   - sem complemento -> max_ptb
  ptb_new <- if (length(ticket_code) == 1L && has_comp) "60"
              else if (has_comp) as.character(max_ptb + 20)
              else as.character(max_ptb)

  # Para comNIF: o complemento do operador 31 gera códigos/nomes especiais
  if (para_comnif && ops_str == "31" && codes_str %in% c("6170", "7670")) {
    prefixo_cod  <- if (age_min < 19L) "6180_" else "6181_"
    prefixo_nome <- if (age_min < 19L) "Navegante Metropolitano 418 Gratuito+"
                    else "Navegante Metropolitano sub23 Gratuito+"
    ticket_code_new <- paste0(prefixo_cod,  codes_str)
    ticket_name_new <- paste0(prefixo_nome, names_str)
  } else {
    ticket_code_new <- codes_str
    ticket_name_new <- names_str
  }

  nome_dist <- calc_nome_dist(ticket_name_new)

  # Número total de validações (soma do grupo + eventual Azambuja — juntado depois)
  nv      <- sum(count_val)
  ptb_num <- as.numeric(ptb_new)

  # Aplicar regra de compensação AML
  comp <- if (nv < THRESHOLD_VAL) PCT_VAL * ptb_num * nv else ptb_num

  list(
    am                       = "AML",
    emissor                  = calc_emissor(ops_str),
    nome_imt                 = unname(calc_nome_imt(ptb_new)),
    price_ticket_base_new    = ptb_new,
    numval                   = nv,
    compensacao              = comp,
    ticket_operator_code_new = ops_str,
    ticket_code_new          = ticket_code_new,
    ticket_name_new          = ticket_name_new,
    compaz                   = NA_integer_,
    nome_dist                = nome_dist,
    age_new                  = age_min,
    birth_date_new           = max(birth_date, na.rm = TRUE),
    cardserialnumber_new     = if (!is.null(card_serial_number))
                                 paste(sort(unique(card_serial_number)), collapse = "_")
                               else NA_character_
  )
}


#' Agrega um data.table de registos do mesmo mês por NIF ou por cartão.
#' Usa a estratégia "singles rápido, multis por grupo" para máximo desempenho.
#'
#' @param dt      data.table com os registos gratuitos
#' @param por     "fiscal" (comNIF) ou "card" (semNIF)
#' @return data.table com uma linha por NIF (ou cartão)
agregar_por_grupo <- function(dt, por = c("fiscal", "card")) {
  por <- match.arg(por)
  key <- if (por == "fiscal") "fiscal_number" else "card_serial_number"

  # Contar quantos bilhetes distintos tem cada NIF/cartão
  contagens   <- dt[, .N, by = c(key)]
  keys_single <- contagens[N == 1L][[key]]   # apenas 1 bilhete (~95% dos casos)
  keys_multi  <- contagens[N  > 1L][[key]]   # vários bilhetes (~5% dos casos)

  # ── Casos simples: apenas 1 bilhete por pessoa (≈95% dos dados) ───────────
  # Processado de forma vectorizada, sem loops
  dt_single <- dt[get(key) %in% keys_single]

  has_comp_single <- dt_single$ticket_code %in% CODES_COMP
  # Se for complemento sozinho: preço base é sempre 60; senão usa o valor da coluna
  ptb_single      <- fifelse(has_comp_single, 60, dt_single$price_ticket_base)
  ptb_str_single  <- fifelse(has_comp_single, "60", as.character(dt_single$price_ticket_base))
  nv_single       <- dt_single$count_val
  # Aplicar regra de compensação AML a cada linha individual
  comp_single     <- fifelse(nv_single < THRESHOLD_VAL,
                             PCT_VAL * ptb_single * nv_single,
                             ptb_single)

  ops_single   <- as.character(dt_single$ticket_operator_code)
  codes_single <- dt_single$ticket_code
  names_single <- dt_single$ticket_name

  if (por == "fiscal") {
    # comNIF: bilhete complemento do operador 31 tem código/nome especial consoante a idade
    e_comp_31 <- dt_single$ticket_operator_code == 31L & has_comp_single

    # Primeiro assume código 6180 (< 19 anos / 418); depois corrige para 6181 se sub23
    tc_new <- fifelse(e_comp_31,
                      paste0("6180_", codes_single),
                      as.character(codes_single))
    tc_new <- fifelse(e_comp_31 & dt_single$age >= 19L,
                      paste0("6181_", codes_single), tc_new)

    tn_new <- fifelse(e_comp_31,
                      fifelse(dt_single$age < 19L,
                              paste0("Navegante Metropolitano 418 Gratuito+", names_single),
                              paste0("Navegante Metropolitano sub23 Gratuito+", names_single)),
                      names_single)
  } else {
    # semNIF: sem lógica especial de complemento
    tc_new <- as.character(codes_single)
    tn_new <- names_single
  }

  # Construir data.table de resultado para os casos simples
  res_single <- data.table(
    am                       = "AML",
    # Emissor deduzido do código de operador
    emissor                  = fifelse(dt_single$ticket_operator_code == 31L, "AT TML",
                               fifelse(dt_single$ticket_operator_code == 21L, "AT Cascais",
                               fifelse(dt_single$ticket_operator_code == 16L, "MTS", ""))),
    year_dist                = dt_single$year_dist,
    month_dist               = dt_single$month_dist,
    nome_imt                 = unname(NOME_IMT[ptb_str_single]),
    price_ticket_base_new    = ptb_str_single,
    numval                   = nv_single,
    compensacao              = comp_single,
    ticket_operator_code_new = ops_single,
    ticket_code_new          = tc_new,
    ticket_name_new          = tn_new,
    compaz                   = NA_integer_,
    nome_dist                = calc_nome_dist(tn_new),
    age_new                  = dt_single$age,
    birth_date_new           = dt_single$birth_date
  )

  # Adicionar coluna identificadora (NIF para comNIF, cartão para semNIF)
  if (por == "fiscal") {
    res_single[, fiscal_number        := dt_single$fiscal_number]
    res_single[, cardserialnumber_new := dt_single$card_serial_number]
  } else {
    res_single[, card_serial_number := dt_single$card_serial_number]
  }


  # ── Casos com múltiplos bilhetes (≈5% dos dados) ──────────────────────────
  # Processado por grupo — mais lento mas necessário para lógica correcta
  dt_multi <- dt[get(key) %in% keys_multi]

  if (nrow(dt_multi) > 0) {
    if (por == "fiscal") {
      res_multi <- dt_multi[, {
        g <- agregar_grupo(ticket_operator_code, ticket_code, ticket_name,
                           price_ticket_base, count_val, age, birth_date,
                           card_serial_number, para_comnif = TRUE)
        .(am = g$am, emissor = g$emissor,
          year_dist = year_dist[1], month_dist = month_dist[1],
          nome_imt = g$nome_imt, price_ticket_base_new = g$price_ticket_base_new,
          numval = g$numval, compensacao = g$compensacao,
          ticket_operator_code_new = g$ticket_operator_code_new,
          ticket_code_new = g$ticket_code_new, ticket_name_new = g$ticket_name_new,
          compaz = g$compaz, nome_dist = g$nome_dist,
          age_new = g$age_new, birth_date_new = g$birth_date_new,
          cardserialnumber_new = g$cardserialnumber_new)
      }, by = .(fiscal_number)]
    } else {
      res_multi <- dt_multi[, {
        g <- agregar_grupo(ticket_operator_code, ticket_code, ticket_name,
                           price_ticket_base, count_val, age, birth_date,
                           para_comnif = FALSE)
        .(am = g$am, emissor = g$emissor,
          year_dist = year_dist[1], month_dist = month_dist[1],
          nome_imt = g$nome_imt, price_ticket_base_new = g$price_ticket_base_new,
          numval = g$numval, compensacao = g$compensacao,
          ticket_operator_code_new = g$ticket_operator_code_new,
          ticket_code_new = g$ticket_code_new, ticket_name_new = g$ticket_name_new,
          compaz = g$compaz, nome_dist = g$nome_dist,
          age_new = g$age_new, birth_date_new = g$birth_date_new,
          cardserialnumber_new = NA_character_)
      }, by = .(card_serial_number)]
    }
  } else {
    res_multi <- NULL
  }

  rbindlist(list(res_single, res_multi), use.names = TRUE, fill = TRUE)
}


# =============================================================================
# Função principal
# =============================================================================

#' Calcula as compensações comNIF e semNIF para o mês indicado.
#'
#' @param ano  Ano de referência (ex: 2026)
#' @param mes  Mês de referência (1–12)
#' @param raiz Raiz do projecto (detectada automaticamente)
#' @return Lista invisível com os data.tables: comNIF, semNIF
calcular_compensacoes <- function(ano, mes, raiz = here_root()) {

  paths <- month_paths(ano, mes, raiz)
  stopifnot(dir.exists(paths$output))

  message(sprintf("[02] A calcular compensacoes para %d/%02d ...", ano, mes))
  t0 <- proc.time()


  # ── Ler ficheiros intermédios ─────────────────────────────────────────────
  f_limpos <- file.path(paths$output, output_filename(ano, mes, "limpos_preagregacao"))
  f_az <- file.path(paths$output, output_filename(ano, mes, "complemento_azambuja"))

  if (!file.exists(f_limpos))
    stop("Ficheiro limpos_preagregacao nao encontrado: ", f_limpos,
         "\n  Execute primeiro: preparar_dados(", ano, ", ", mes, ")")

  dt_main <- fread(f_limpos, sep = ";", dec = ".",
    colClasses = list(
      character = c("card_serial_number", "fiscal_number", "ticket_name",
                    "doc_number", "doc_type"),
      integer   = c("year_dist", "month_dist", "age",
                    "ticket_operator_code", "ticket_code", "count_val"),
      numeric   = c("price", "price_ticket_base"),
      IDate     = c("birth_date")
    ))

  # Ler Azambuja se existir
  dt_az <- if (file.exists(f_az)) {
    fread(f_az, sep = ";", dec = ".",
      colClasses = list(
        character = c("card_serial_number", "fiscal_number"),
        integer   = c("year_dist", "month_dist", "ticket_code", "count_val")
      ))
  } else NULL

  # Ler lista de exceções de documentos de identificação (data/auxiliar/id_exception.csv)
  # Documentos conhecidos por serem usados por crianças diferentes (fraude / partilhado)
  f_id_exc <- file.path(paths$auxiliar, "id_exception.csv")
  dt_id_exc <- if (file.exists(f_id_exc)) {
    dt_tmp <- fread(f_id_exc, colClasses = "character")
    names(dt_tmp) <- tolower(names(dt_tmp))
    if (!"doc_type" %in% names(dt_tmp)) dt_tmp[, doc_type := NA_character_]
    dt_tmp[, doc_type := toupper(trimws(as.character(doc_type)))]
    dt_tmp[, doc_number := trimws(as.character(doc_number))]
    dt_tmp[!is.na(doc_number) & nzchar(doc_number)]
  } else NULL

  # Ler ficheiro de exceções sub23+1 (perfis 127/130: medicina, arquitetura, etc.)
  # Procura em paths$input pelo ficheiro mais recente com padrão "idades23*.csv"
  f_sub23_files <- list.files(paths$input, pattern = "^idades23.*\\.csv$",
                              full.names = TRUE, ignore.case = TRUE)
  dt_sub23_exc <- if (length(f_sub23_files) > 0) {
    f_sub23 <- sort(f_sub23_files, decreasing = TRUE)[1]
    message(sprintf("  Ficheiro de perfis sub23+1 encontrado: %s", basename(f_sub23)))
    dt_s23 <- fread(f_sub23, colClasses = "character")
    names(dt_s23) <- tolower(names(dt_s23))
    dt_s23
  } else NULL



  # ── Filtrar: apenas passes gratuitos do mês ───────────────────────────────
  # Excluir: price != 0, price_ticket_base ausente, ou sem validações
  dt_mes <- dt_main |>
    filter(
      year_dist  == ano,
      month_dist == mes,
      price      == 0,
      !is.na(price_ticket_base),
      count_val  > 0L
    )

  # Garantir que NIF vazio está como NA (normalização extra por precaução)
  dt_mes <- dt_mes |>
    mutate(fiscal_number = if_else(fiscal_number %in% c("", "0", "NA"),
                                   NA_character_, fiscal_number)) |>
    as.data.table()
  message(sprintf("  Passes gratis com validacoes: %d", nrow(dt_mes)))


  # ── Agregar Azambuja por NIF e por cartão ─────────────────────────────────
  # O Azambuja dá validações adicionais a quem tem o complemento.
  # Separamos por NIF (para juntar ao comNIF) e por cartão (para semNIF).
  if (!is.null(dt_az)) {
    dt_az <- dt_az |>
      mutate(fiscal_number = if_else(fiscal_number %in% c("", "0", "NA"),
                                     NA_character_, fiscal_number)) |>
      as.data.table()

    # Azambuja com NIF identificado -> vai juntar-se ao comNIF
    az_fiscal <- dt_az |>
      filter(!is.na(fiscal_number)) |>
      group_by(fiscal_number) |>
      summarise(
        count_val_az  = sum(count_val),
        ticket_code_az = first(ticket_code),
        .groups = "drop"
      ) |>
      as.data.table()

    # Azambuja sem NIF -> vai juntar-se ao semNIF por cartão
    az_card <- dt_az |>
      filter(is.na(fiscal_number)) |>
      group_by(card_serial_number) |>
      summarise(
        count_val_az   = sum(count_val),
        ticket_code_az = first(ticket_code),
        .groups = "drop"
      ) |>
      as.data.table()
  } else {
    az_fiscal <- NULL
    az_card   <- NULL
  }


  # ── ComNIF: agregar por NIF ───────────────────────────────────────────────
  message("  A processar comNIF ...")

  # Filtrar apenas registos com NIF válido
  dt_com_raw <- dt_mes |> filter(!is.na(fiscal_number))
  dt_comNIF  <- agregar_por_grupo(dt_com_raw, "fiscal")

  # Separar registos NOK (fora da faixa etária elegível) antes de os remover
  # age_new == 3  -> demasiado novo (< 4 anos)
  # age_new >= 25 -> demasiado velho (fora do passe jovem)
  nok_com   <- dt_comNIF |> filter(age_new == 3L | age_new >= 25L)
  dt_comNIF <- dt_comNIF |> filter(age_new != 3L, age_new < 25L)

  # Juntar validações do Azambuja e recalcular compensação com o total
  if (!is.null(az_fiscal) && nrow(az_fiscal) > 0) {
    dt_comNIF[az_fiscal, on = .(fiscal_number), `:=`(
      numval      = numval + i.count_val_az,
      compaz      = i.ticket_code_az,
      compensacao = {
        nv_total <- numval + i.count_val_az
        ptb      <- as.numeric(price_ticket_base_new)
        fifelse(nv_total < THRESHOLD_VAL, PCT_VAL * ptb * nv_total, ptb)
      }
    )]
  }

  # Seleccionar e ordenar as colunas finais do comNIF
  cols_comNIF <- c("am", "emissor", "year_dist", "month_dist", "fiscal_number",
                   "nome_imt", "price_ticket_base_new", "numval", "compensacao",
                   "ticket_operator_code_new", "ticket_code_new", "ticket_name_new",
                   "compaz", "nome_dist", "age_new", "birth_date_new",
                   "cardserialnumber_new")
  dt_comNIF <- dt_comNIF |> select(all_of(cols_comNIF)) |> as.data.table()
  message(sprintf("  comNIF: %d registos, compensacao total: %.2f EUR",
                  nrow(dt_comNIF), sum(dt_comNIF$compensacao, na.rm = TRUE)))


  # ── SemNIF: processar ─────────────────────────────────────────────────────
  message("  A processar semNIF ...")

  # Filtrar apenas registos sem NIF
  dt_sem_raw <- dt_mes |> filter(is.na(fiscal_number))

  # Verificar se existem dados de documento (julho 2026 em diante)
  tem_docs <- "doc_number" %in% names(dt_sem_raw) && any(!is.na(dt_sem_raw$doc_number))

  if (tem_docs) {
    # ── Validação e tratamento de documentos (julho 2026+) ───────────────────
    # 1. Identificar documentos com múltiplas datas de nascimento distintas
    docs_multi_bd <- dt_sem_raw |>
      filter(!is.na(doc_number) & nzchar(trimws(doc_number))) |>
      group_by(doc_number) |>
      filter(n_distinct(birth_date) > 1) |>
      pull(doc_number) |>
      unique()

    # 2. Identificar documentos na lista de exceções (id_exception.csv)
    #    Estes documentos são exceções para JUNTAR: mesmo que tenham anomalias
    #    (como múltiplas datas de nascimento distintas), sabemos que é a mesma pessoa.
    dt_sem_raw <- dt_sem_raw |>
      mutate(
        doc_clean      = trimws(as.character(doc_number)),
        doc_type_clean = toupper(trimws(as.character(doc_type))),
        is_id_exc      = is_in_id_exception(doc_type, doc_number, dt_id_exc)
      )

    # 3. Classificar anomalias nos números de documento com dplyr
    # Documentos na lista de exceções (id_exception.csv) são identificados com motivo dedicado
    dt_sem_raw <- dt_sem_raw |>
      mutate(
        is_cc      = doc_type_clean == "CITIZEN_CARD",
        motivo_doc = case_when(
          is_id_exc ~ paste0(
            if_else(doc_type_clean == "PASSPORT", "Passaporte", "Documento"),
            " partilhado por jovens diferentes (fraude/excecao)"
          ),
          doc_number %in% docs_multi_bd & grepl("^0+$", doc_clean) ~ "Cartao de Cidadao com apenas zeros e datas de nascimento distintas",
          doc_number %in% docs_multi_bd ~ "Documento com datas de nascimento distintas",
          is_cc & grepl("^0+$", doc_clean) ~ "Cartao de Cidadao com apenas zeros",
          is_cc & grepl("^9+$", doc_clean) ~ "Cartao de Cidadao com apenas noves",
          is_cc & grepl("^(.)\\1+$", doc_clean) ~ "Cartao de Cidadao com digitos repetidos",
          is_cc & nchar(gsub("\\s+", "", doc_clean)) < 6 ~ "Cartao de Cidadao com numero invulgar (< 6 digitos)",
          !is_cc & grepl("^0+$", doc_clean) ~ "Documento com apenas zeros",
          !is_cc & grepl("^9+$", doc_clean) ~ "Documento com apenas noves",
          tolower(doc_clean) %in% c("test", "teste", "null", "none", "na", "xxx", "12345", "123456") ~ "Documento com valor invalido",
          TRUE ~ NA_character_
        )
      )

    # 4. Lista exportada de documentos estranhos / suspeitos (semNIF)
    #    Remove linhas duplicadas mantendo apenas um registo por cartão
    dt_docs_estranhos <- dt_sem_raw |>
      filter(!is.na(motivo_doc)) |>
      transmute(
        motivo = motivo_doc,
        doc_type,
        doc_number,
        card_serial_number,
        birth_date,
        age,
        ticket_name,
        count_val,
        price_ticket_base,
        year_dist,
        month_dist
      ) |>
      distinct(card_serial_number, .keep_all = TRUE) |>
      arrange(doc_type, doc_number, card_serial_number) |>
      as.data.table()

    message(sprintf("  Documentos estranhos/suspeitos (semNIF): %d registos detetados", nrow(dt_docs_estranhos)))

    # 5. Chave de agrupamento:
    #    - Se tem documento válido e sem anomalia (OU é exceção para juntar) -> agrupa por documento
    #    - Se tem documento anómalo ou ausente -> agrupa por cartão (evita fundir titulares distintos com dummy docs)
    dt_sem_raw <- dt_sem_raw |>
      mutate(
        group_id = if_else(
          (!is.na(doc_clean) & nzchar(doc_clean) & is.na(motivo_doc)) | is_id_exc,
          paste0("DOC_", doc_type_clean, "_", doc_clean),
          paste0("CARD_", card_serial_number)
        )
      )

    # Separar singles e multis para processamento eficiente
    group_counts <- dt_sem_raw |> count(group_id)
    singles <- group_counts |> filter(n == 1L) |> pull(group_id)
    multis  <- group_counts |> filter(n > 1L)  |> pull(group_id)

    # Singles: processamento vectorizado via dplyr
    dt_sem_single <- dt_sem_raw |>
      filter(group_id %in% singles) |>
      mutate(
        has_comp = ticket_code %in% CODES_COMP,
        ptb_new = if_else(has_comp, "60", as.character(price_ticket_base)),
        ptb_num = as.numeric(ptb_new),
        comp = if_else(count_val < THRESHOLD_VAL, PCT_VAL * ptb_num * count_val, ptb_num),
        emissor = case_when(
          ticket_operator_code == 31L ~ "AT TML",
          ticket_operator_code == 21L ~ "AT Cascais",
          ticket_operator_code == 16L ~ "MTS",
          TRUE ~ ""
        ),
        ticket_operator_code_new = as.character(ticket_operator_code),
        ticket_code_new = as.character(ticket_code),
        ticket_name_new = ticket_name,
        nome_dist = calc_nome_dist(ticket_name),
        nome_imt = unname(NOME_IMT[ptb_new]),
        card_serial_number = as.character(card_serial_number),
        cardserialnumber_new = as.character(card_serial_number),
        numval = count_val,
        compensacao = comp,
        compaz = NA_integer_,
        age_new = age,
        birth_date_new = birth_date,
        am = "AML"
      ) |>
      select(
        am, emissor, year_dist, month_dist,
        doc_type, doc_number, card_serial_number,
        nome_imt, price_ticket_base_new = ptb_new,
        numval, compensacao,
        ticket_operator_code_new, ticket_code_new, ticket_name_new,
        compaz, nome_dist, age_new, birth_date_new,
        cardserialnumber_new
      )

    # Multis: agregação por grupo (mesmo documento com múltiplos cartões/bilhetes)
    dt_sem_multi <- dt_sem_raw |>
      filter(group_id %in% multis) |>
      group_by(group_id) |>
      summarise(
        am = "AML",
        emissor = {
          ord_op <- order(op_prio(ticket_operator_code))
          calc_emissor(paste(ticket_operator_code[ord_op], collapse = "_"))
        },
        year_dist = first(year_dist),
        month_dist = first(month_dist),
        doc_type = first(na.omit(doc_type)),
        doc_number = first(na.omit(doc_number)),
        card_serial_number = paste(sort(unique(card_serial_number)), collapse = "_"),
        cardserialnumber_new = paste(sort(unique(card_serial_number)), collapse = "_"),
        nome_imt = {
          has_comp <- any(ticket_code %in% CODES_COMP)
          max_ptb  <- max(price_ticket_base, na.rm = TRUE)
          ptb_new  <- if (n() == 1L && has_comp) "60"
                      else if (has_comp) as.character(max_ptb + 20)
                      else as.character(max_ptb)
          unname(calc_nome_imt(ptb_new))
        },
        price_ticket_base_new = {
          has_comp <- any(ticket_code %in% CODES_COMP)
          max_ptb  <- max(price_ticket_base, na.rm = TRUE)
          if (n() == 1L && has_comp) "60"
          else if (has_comp) as.character(max_ptb + 20)
          else as.character(max_ptb)
        },
        numval = sum(count_val),
        compensacao = {
          has_comp <- any(ticket_code %in% CODES_COMP)
          max_ptb  <- max(price_ticket_base, na.rm = TRUE)
          ptb_new  <- if (n() == 1L && has_comp) "60"
                      else if (has_comp) as.character(max_ptb + 20)
                      else as.character(max_ptb)
          ptb_num  <- as.numeric(ptb_new)
          nv       <- sum(count_val)
          if (nv < THRESHOLD_VAL) PCT_VAL * ptb_num * nv else ptb_num
        },
        ticket_operator_code_new = {
          ord_op <- order(op_prio(ticket_operator_code))
          paste(ticket_operator_code[ord_op], collapse = "_")
        },
        ticket_code_new = {
          ord <- order(op_prio(ticket_operator_code), code_prio(ticket_code))
          paste(ticket_code[ord], collapse = "_")
        },
        ticket_name_new = {
          ord <- order(op_prio(ticket_operator_code), code_prio(ticket_code))
          paste(ticket_name[ord], collapse = "+")
        },
        compaz = NA_integer_,
        nome_dist = {
          ord <- order(op_prio(ticket_operator_code), code_prio(ticket_code))
          calc_nome_dist(paste(ticket_name[ord], collapse = "+"))
        },
        age_new = min(age, na.rm = TRUE),
        birth_date_new = max(birth_date, na.rm = TRUE),
        .groups = "drop"
      ) |>
      select(-group_id)

    dt_semNIF <- bind_rows(dt_sem_single, dt_sem_multi)

    # Separar registos NOK (fora da faixa etária elegível)
    nok_sem   <- dt_semNIF |> filter(age_new == 3L | age_new >= 25L)
    dt_semNIF <- dt_semNIF |> filter(age_new != 3L, age_new < 25L)

    # Juntar validações do Azambuja via dplyr e recalcular compensação
    if (!is.null(az_card) && nrow(az_card) > 0) {
      dt_semNIF <- dt_semNIF |>
        left_join(az_card, by = "card_serial_number") |>
        mutate(
          compaz = ticket_code_az,
          numval = numval + coalesce(count_val_az, 0L),
          compensacao = if_else(
            numval < THRESHOLD_VAL,
            PCT_VAL * as.numeric(price_ticket_base_new) * numval,
            as.numeric(price_ticket_base_new)
          )
        ) |>
        select(-count_val_az, -ticket_code_az)
    }

    # Seleccionar e ordenar as colunas finais do semNIF
    cols_semNIF <- c("am", "emissor", "year_dist", "month_dist",
                     "doc_type", "doc_number", "card_serial_number",
                     "nome_imt", "price_ticket_base_new", "numval", "compensacao",
                     "ticket_operator_code_new", "ticket_code_new", "ticket_name_new",
                     "compaz", "nome_dist", "age_new", "birth_date_new",
                     "cardserialnumber_new")
    dt_semNIF <- dt_semNIF |> select(all_of(cols_semNIF)) |> as.data.table()

  } else {
    # Meses anteriores (sem campos de documento) -> mantém agregação por cartão
    dt_docs_estranhos <- NULL
    docs_multi_bd     <- character(0)
    dt_semNIF  <- agregar_por_grupo(dt_sem_raw, "card")

    # Separar registos NOK (fora da faixa etária elegível)
    nok_sem   <- dt_semNIF |> filter(age_new == 3L | age_new >= 25L)
    dt_semNIF <- dt_semNIF |> filter(age_new != 3L, age_new < 25L)

    # Juntar validações do Azambuja e recalcular compensação
    if (!is.null(az_card) && nrow(az_card) > 0) {
      dt_semNIF[az_card, on = .(card_serial_number), `:=`(
        numval      = numval + i.count_val_az,
        compaz      = i.ticket_code_az,
        compensacao = {
          nv_total <- numval + i.count_val_az
          ptb      <- as.numeric(price_ticket_base_new)
          fifelse(nv_total < THRESHOLD_VAL, PCT_VAL * ptb * nv_total, ptb)
        }
      )]
    }

    # Seleccionar e ordenar as colunas finais do semNIF
    cols_semNIF <- c("am", "emissor", "year_dist", "month_dist", "card_serial_number",
                     "nome_imt", "price_ticket_base_new", "numval", "compensacao",
                     "ticket_operator_code_new", "ticket_code_new", "ticket_name_new",
                     "compaz", "nome_dist", "age_new", "birth_date_new")
    dt_semNIF <- dt_semNIF |> select(all_of(cols_semNIF)) |> as.data.table()
  }
  message(sprintf("  semNIF: %d registos, compensacao total: %.2f EUR",
                  nrow(dt_semNIF), sum(dt_semNIF$compensacao, na.rm = TRUE)))


  # ── Lista de registos a verificar ────────────────────────────────────────
  # Exporta pessoas com dados suspeitos para confirmar manualmente:
  #   - "Exceção ID (documento partilhado)": documento na lista de exceções (fraude / partilhado)
  #   - "Documento com datas de nascimento distintas": mesmo doc_number com múltiplas birth_date
  #   - "Sem NIF": não foi possível identificar o titular
  #   - "Idade > 23": titular perto do limite de idade (24 ainda é elegível mas merece atenção)

  tem_docs_mes <- "doc_number" %in% names(dt_mes) && any(!is.na(dt_mes$doc_number))
  is_exc_mes <- if (tem_docs_mes) {
    is_in_id_exception(dt_mes$doc_type, dt_mes$doc_number, dt_id_exc)
  } else {
    rep(FALSE, nrow(dt_mes))
  }

  is_multi_bd_mes <- if (tem_docs_mes && exists("docs_multi_bd") && length(docs_multi_bd) > 0) {
    !is_exc_mes & is.na(dt_mes$fiscal_number) & (dt_mes$doc_number %in% docs_multi_bd)
  } else {
    rep(FALSE, nrow(dt_mes))
  }

  # 1. Casos com exceção de documento ID (juntos por exceção apesar de datas distintas)
  dt_ver_exc <- if (any(is_exc_mes)) {
    dt_mes |>
      filter(is_exc_mes) |>
      transmute(
        motivo             = "Excecao ID (datas de nascimento distintas)",
        card_serial_number = as.character(card_serial_number),
        fiscal_number      = as.character(fiscal_number),
        birth_date,
        age,
        ticket_name
      )
  } else NULL

  # 2. Casos com documento associado a datas de nascimento distintas
  dt_ver_multi_bd <- if (any(is_multi_bd_mes)) {
    dt_mes |>
      filter(is_multi_bd_mes) |>
      transmute(
        motivo             = "Documento com datas de nascimento distintas",
        card_serial_number = as.character(card_serial_number),
        fiscal_number      = as.character(fiscal_number),
        birth_date,
        age,
        ticket_name
      )
  } else NULL

  # 3. Casos sem NIF (excluindo os já capturados na exceção de ID ou com datas distintas)
  dt_ver_sem <- dt_mes |>
    filter(is.na(fiscal_number) & !is_exc_mes & !is_multi_bd_mes) |>
    transmute(
      motivo             = "Sem NIF",
      card_serial_number = as.character(card_serial_number),
      fiscal_number      = NA_character_,
      birth_date,
      age,
      ticket_name
    )  |>
    arrange(ticket_name, age, fiscal_number)

  # 4. Casos com NIF e idade > 23 (excluindo exceções de ID e com datas distintas)
  #    Se existir ficheiro de perfis 127/130 (idades23*.csv), exclui os estudantes confirmados (sub23+1)
  dt_ver_idade_raw <- dt_mes |>
    filter(!is.na(fiscal_number), age > 23L, !is_exc_mes, !is_multi_bd_mes)

  n_idade24_total <- sum(dt_ver_idade_raw$age == 24L, na.rm = TRUE)

  if (!is.null(dt_sub23_exc)) {
    # Suporta colunas card_serial_number / serial_number e fiscal_number
    s23_cards <- character(0)
    if ("card_serial_number" %in% names(dt_sub23_exc)) {
      s23_cards <- c(s23_cards, dt_sub23_exc$card_serial_number)
    }
    if ("serial_number" %in% names(dt_sub23_exc)) {
      s23_cards <- c(s23_cards, dt_sub23_exc$serial_number)
    }
    s23_cards <- unique(s23_cards[!is.na(s23_cards) & nzchar(s23_cards)])

    s23_nifs <- character(0)
    if ("fiscal_number" %in% names(dt_sub23_exc)) {
      s23_nifs <- dt_sub23_exc$fiscal_number
      s23_nifs <- unique(s23_nifs[!is.na(s23_nifs) & nzchar(s23_nifs)])
    }

    # Estudante confirmado se o cartão ou o NIF constar na lista de exceção sub23+1
    is_sub23_conf <- (as.character(dt_ver_idade_raw$card_serial_number) %in% s23_cards) |
                     (as.character(dt_ver_idade_raw$fiscal_number) %in% s23_nifs)

    n_sub23_confirmados <- sum(is_sub23_conf)
    dt_ver_idade_filtrado <- dt_ver_idade_raw |> filter(!is_sub23_conf)
  } else {
    n_sub23_confirmados <- 0L
    dt_ver_idade_filtrado <- dt_ver_idade_raw
  }

  dt_ver_idade <- dt_ver_idade_filtrado |>
    transmute(
      motivo = case_when(
        age == 25L ~ "Idade = 25",
        TRUE       ~ "Idade > 23"
      ),
      card_serial_number = as.character(card_serial_number),
      fiscal_number      = as.character(fiscal_number),
      birth_date,
      age,
      ticket_name
    ) |>
    arrange(age, ticket_name, fiscal_number)

  dt_verificar <- bind_rows(dt_ver_exc, dt_ver_multi_bd, dt_ver_idade, dt_ver_sem) |>
    as.data.table()

  n_ver_24 <- sum(dt_ver_idade$motivo == "Idade > 23")
  n_ver_25 <- sum(dt_ver_idade$motivo == "Idade = 25")

  msg_sub23 <- if (n_sub23_confirmados > 0L) {
    sprintf(" [%d confirmados sub23+1 via BD excluídos de %d]", n_sub23_confirmados, n_idade24_total)
  } else ""

  message(sprintf("  A verificar: %d registos suspeitos (%d excecao ID, %d datas distintas, %d com idade > 23%s, %d com idade = 25, %d sem NIF)",
                  nrow(dt_verificar),
                  if (is.null(dt_ver_exc)) 0 else nrow(dt_ver_exc),
                  if (is.null(dt_ver_multi_bd)) 0 else nrow(dt_ver_multi_bd),
                  n_ver_24,
                  msg_sub23,
                  n_ver_25,
                  nrow(dt_ver_sem)))


  # ── Registos NOK (idade incompatível após correcção) ─────────────────────
  # Após a correcção pontual em 01_preparar_dados.R (regras dos meses limite),
  # ainda podem restar registos com age_new == 3 (demasiado novo) ou age_new >= 25
  # (demasiado velho). Estes são excluídos da compensação e guardados em age_nok.csv.
  dt_age_nok <- bind_rows(
    nok_com |> mutate(tipo = "comNIF"),
    nok_sem |> mutate(tipo = "semNIF")
  ) |>
    as.data.table()

  message(sprintf("  NOK (age 3 ou >=25): %d comNIF + %d semNIF = %d registos",
                  nrow(nok_com), nrow(nok_sem), nrow(dt_age_nok)))


  # ── Gravar ficheiros de output ────────────────────────────────────────────
  out_com  <- file.path(paths$output, output_filename(ano, mes, "comNIF"))
  out_sem  <- file.path(paths$output, output_filename(ano, mes, "semNIF"))
  out_ver  <- file.path(paths$output, output_filename(ano, mes, "a_verificar"))
  out_nok  <- file.path(paths$output, output_filename(ano, mes, "age_nok"))
  out_estranhos <- file.path(paths$output, output_filename(ano, mes, "documentos_estranhos_semNIF"))

  write_csv_pj(dt_comNIF,    out_com)
  write_csv_pj(dt_semNIF,    out_sem)
  write_csv_pj(dt_verificar, out_ver)
  write_csv_pj(dt_age_nok,   out_nok)

  if (!is.null(dt_docs_estranhos) && nrow(dt_docs_estranhos) > 0) {
    write_csv_pj(dt_docs_estranhos, out_estranhos)
    message(sprintf("  Gravado: %s (%d registos)", basename(out_estranhos), nrow(dt_docs_estranhos)))
  }

  elapsed <- round((proc.time() - t0)[["elapsed"]], 1)
  message(sprintf("  Gravado: %s", basename(out_com)))
  message(sprintf("  Gravado: %s", basename(out_sem)))
  message(sprintf("  Gravado: %s", basename(out_ver)))
  message(sprintf("  Gravado: %s", basename(out_nok)))
  message(sprintf("[02] Concluido em %.1f segundos.\n", elapsed))

  invisible(list(
    comNIF                      = dt_comNIF,
    semNIF                      = dt_semNIF,
    age_nok                     = dt_age_nok,
    documentos_estranhos_semNIF = dt_docs_estranhos
  ))
}


# -----------------------------------------------------------------------------
# Execução directa: Rscript code/02_calcular_compensacoes.R <ano> <mes>
# -----------------------------------------------------------------------------
if (!interactive() && identical(sys.frames(), list())) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2) {
    calcular_compensacoes(ano = as.integer(args[1]), mes = as.integer(args[2]))
  } else {
    cat("Uso: Rscript code/02_calcular_compensacoes.R <ano> <mes>\n")
    cat("Ex:  Rscript code/02_calcular_compensacoes.R 2026 7\n")
  }
}
