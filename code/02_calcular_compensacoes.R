# =============================================================================
# 02_calcular_compensacoes.R
# Calcula as compensações comNIF e semNIF para um dado mês.
#
# Lê os ficheiros intermédios gerados por 01_preparar_dados.R e produz:
#   - <ano>_<mm>_comNIF.csv   (agregado por NIF)
#   - <ano>_<mm>_semNIF.csv   (agregado por número de cartão, para quem não tem NIF)
#   - <ano>_<mm>_a_verificar.csv  (lista de pessoas com dados suspeitos)
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
  nv  <- sum(count_val)
  ptb_num <- as.numeric(ptb_new)

  # Compensação AML
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

  # Separar registos únicos (um bilhete) de múltiplos (vários bilhetes)
  contagens   <- dt[, .N, by = c(key)]
  keys_single <- contagens[N == 1L][[key]]
  keys_multi  <- contagens[N  > 1L][[key]]

  # ── Casos simples: apenas 1 bilhete por pessoa (≈95% dos dados) ───────────
  # Processado de forma vectorizada, sem loops
  dt_single <- dt[get(key) %in% keys_single]

  has_comp_single <- dt_single$ticket_code %in% CODES_COMP
  ptb_single      <- fifelse(has_comp_single, 60, dt_single$price_ticket_base)
  ptb_str_single  <- fifelse(has_comp_single, "60", as.character(dt_single$price_ticket_base))
  nv_single       <- dt_single$count_val
  comp_single     <- fifelse(nv_single < THRESHOLD_VAL,
                             PCT_VAL * ptb_single * nv_single,
                             ptb_single)

  # Para comNIF: bilhete complemento do operador 31 tem código/nome especial
  ops_single   <- as.character(dt_single$ticket_operator_code)
  codes_single <- dt_single$ticket_code
  names_single <- dt_single$ticket_name

  if (por == "fiscal") {
    e_comp_31 <- dt_single$ticket_operator_code == 31L & has_comp_single
    tc_new <- fifelse(e_comp_31,
                      fifelse(dt_single$age < 19L,
                              paste0("6180_", codes_single),
                              paste0("6180_", codes_single)),
                      as.character(codes_single))
    # Corrigir: sub23 usa 6181
    tc_new <- fifelse(e_comp_31 & dt_single$age >= 19L,
                      paste0("6181_", codes_single), tc_new)

    tn_new <- fifelse(e_comp_31,
                      fifelse(dt_single$age < 19L,
                              paste0("Navegante Metropolitano 418 Gratuito+", names_single),
                              paste0("Navegante Metropolitano sub23 Gratuito+", names_single)),
                      names_single)
  } else {
    tc_new <- as.character(codes_single)
    tn_new <- names_single
  }

  res_single <- data.table(
    am                       = "AML",
    emissor                  = fifelse(dt_single$ticket_operator_code == 31L, "AT AML",
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

  if (por == "fiscal") {
    res_single[, fiscal_number        := dt_single$fiscal_number]
    res_single[, cardserialnumber_new := dt_single$card_serial_number]
  } else {
    res_single[, card_serial_number := dt_single$card_serial_number]
  }


  # ── Casos com múltiplos bilhetes (≈5% dos dados) ──────────────────────────
  # Processado em grupo — mais lento mas necessário para lógica correcta
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
  f_limpos <- file.path(paths$output, output_filename(ano, mes, "dados_limpos"))
  f_az     <- file.path(paths$output, output_filename(ano, mes, "complemento_azambuja"))

  if (!file.exists(f_limpos))
    stop("Ficheiro dados_limpos nao encontrado: ", f_limpos,
         "\n  Execute primeiro: preparar_dados(", ano, ", ", mes, ")")

  dt_main <- fread(f_limpos, sep = ";", dec = ".",
    colClasses = list(
      character = c("card_serial_number", "fiscal_number", "ticket_name"),
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


  # ── Filtrar: apenas passes gratuitos do mês ───────────────────────────────
  # Excluir: price != 0, price_ticket_base ausente, ou sem validações
  dt_mes <- dt_main[year_dist == ano & month_dist == mes &
                    price == 0 &
                    !is.na(price_ticket_base) &
                    count_val > 0L]
  # Garantir que NIF vazio está como NA
  dt_mes[fiscal_number %in% c("", "0", "NA"), fiscal_number := NA_character_]
  message(sprintf("  Passes gratuis com validacoes: %d", nrow(dt_mes)))


  # ── Agregar Azambuja por NIF e por cartão ─────────────────────────────────
  # O Azambuja dá validações adicionais a quem tem o complemento
  if (!is.null(dt_az)) {
    dt_az[fiscal_number %in% c("", "0", "NA"), fiscal_number := NA_character_]

    az_fiscal <- dt_az[!is.na(fiscal_number),
                        .(count_val_az = sum(count_val), ticket_code_az = first(ticket_code)),
                        by = .(fiscal_number)]

    az_card   <- dt_az[is.na(fiscal_number),
                        .(count_val_az = sum(count_val), ticket_code_az = first(ticket_code)),
                        by = .(card_serial_number)]
  } else {
    az_fiscal <- NULL
    az_card   <- NULL
  }


  # ── ComNIF: agregar por NIF ───────────────────────────────────────────────
  message("  A processar comNIF ...")
  dt_com_raw <- dt_mes[!is.na(fiscal_number)]
  dt_comNIF  <- agregar_por_grupo(dt_com_raw, "fiscal")

  # Juntar validações do Azambuja e recalcular compensação
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

  # Seleccionar colunas finais na ordem correcta
  cols_comNIF <- c("am", "emissor", "year_dist", "month_dist", "fiscal_number",
                   "nome_imt", "price_ticket_base_new", "numval", "compensacao",
                   "ticket_operator_code_new", "ticket_code_new", "ticket_name_new",
                   "compaz", "nome_dist", "age_new", "birth_date_new",
                   "cardserialnumber_new")
  dt_comNIF <- dt_comNIF[, ..cols_comNIF]
  message(sprintf("  comNIF: %d registos, compensacao total: %.2f EUR",
                  nrow(dt_comNIF), sum(dt_comNIF$compensacao, na.rm = TRUE)))


  # ── SemNIF: agregar por cartão ────────────────────────────────────────────
  message("  A processar semNIF ...")
  dt_sem_raw <- dt_mes[is.na(fiscal_number)]
  dt_semNIF  <- agregar_por_grupo(dt_sem_raw, "card")

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

  cols_semNIF <- c("am", "emissor", "year_dist", "month_dist", "card_serial_number",
                   "nome_imt", "price_ticket_base_new", "numval", "compensacao",
                   "ticket_operator_code_new", "ticket_code_new", "ticket_name_new",
                   "compaz", "nome_dist", "age_new", "birth_date_new")
  dt_semNIF <- dt_semNIF[, ..cols_semNIF]
  message(sprintf("  semNIF: %d registos, compensacao total: %.2f EUR",
                  nrow(dt_semNIF), sum(dt_semNIF$compensacao, na.rm = TRUE)))


  # ── Lista de registos a verificar ────────────────────────────────────────
  # Exporta pessoas com dados suspeitos para confirmar manualmente
  dt_verificar <- rbindlist(list(
    # Pessoas sem NIF e sem número de documento (semNIF genuíno vs. erro de dados)
    dt_mes[is.na(fiscal_number), .(
      motivo             = "Sem NIF",
      card_serial_number,
      fiscal_number      = NA_character_,
      birth_date,
      age,
      ticket_name
    )],
    # Pessoas com NIF mas idade > 23 (potencialmente inelegíveis)
    dt_mes[!is.na(fiscal_number) & age > 23L, .(
      motivo             = "Idade > 23",
      card_serial_number,
      fiscal_number,
      birth_date,
      age,
      ticket_name
    )]
  ), use.names = TRUE)

  message(sprintf("  A verificar: %d registos suspeitos", nrow(dt_verificar)))


  # ── Gravar ficheiros de output ────────────────────────────────────────────
  out_com  <- file.path(paths$output, output_filename(ano, mes, "comNIF"))
  out_sem  <- file.path(paths$output, output_filename(ano, mes, "semNIF"))
  out_ver  <- file.path(paths$output, output_filename(ano, mes, "a_verificar"))

  write_csv_pj(dt_comNIF,    out_com)
  write_csv_pj(dt_semNIF,    out_sem)
  write_csv_pj(dt_verificar, out_ver)

  elapsed <- round((proc.time() - t0)[["elapsed"]], 1)
  message(sprintf("  Gravado: %s", basename(out_com)))
  message(sprintf("  Gravado: %s", basename(out_sem)))
  message(sprintf("  Gravado: %s", basename(out_ver)))
  message(sprintf("[02] Concluido em %.1f segundos.\n", elapsed))

  invisible(list(comNIF = dt_comNIF, semNIF = dt_semNIF))
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
