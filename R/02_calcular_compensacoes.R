# =============================================================================
# 02_calcular_compensacoes.R
# Calcula as compensações comNIF e semNIF para um dado mês (ultra-otimizado via data.table).
# Requer que 01_preparar_dados.R já tenha sido executado.
#
# Produz:
#   data/<ano>_<mm>_<mes>/output/<ano>_<mm>_comNIF.csv
#   data/<ano>_<mm>_<mes>/output/<ano>_<mm>_semNIF.csv
#
# Uso:
#   source("R/02_calcular_compensacoes.R")
#   calcular_compensacoes(ano = 2026, mes = 5)
# =============================================================================

source(file.path(dirname(sys.frame(1)$ofile), "00_utils.R"),
       local = TRUE, chdir = FALSE)


# =============================================================================
# Funções de prioridade para ordenação dentro de cada grupo
# =============================================================================
op_prio <- function(op) fifelse(op == 31L, 1L, fifelse(op == 0L, 2L, 3L))

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


# =============================================================================
# Agregação rápida de grupos (split single vs multi para máximo desempenho)
# =============================================================================

#' Agrega registos do mesmo NIF (ou cartão) dentro do mês de forma ultra-rápida.
#' @param dt     data.table com os registos gratuitos do mês
#' @param por    "fiscal" ou "card"
agregar_grupo_dt <- function(dt, por = c("fiscal", "card")) {
  por <- match.arg(por)
  key_col <- if (por == "fiscal") "fiscal_number" else "card_serial_number"

  # Identificar registos únicos vs múltiplos
  n_counts <- dt[, .N, by = c(key_col)]
  single_keys <- n_counts[N == 1L][[key_col]]
  multi_keys  <- n_counts[N > 1L][[key_col]]

  # ── 1. Registos singulares (vetorizado instantâneo, ~95% dos dados) ────────
  dt_singles <- dt[get(key_col) %in% single_keys]

  if (por == "fiscal") {
    res_singles <- dt_singles[, .(
      am = "AML",
      emissor = fifelse(ticket_operator_code == 31L, "AT AML",
                fifelse(ticket_operator_code == 21L, "AT Cascais",
                fifelse(ticket_operator_code == 16L, "MTS", ""))),
      year_dist,
      month_dist,
      fiscal_number,
      nome_imt = fifelse(ticket_code %in% c(6170L, 7670L), "Navegante Metropolitano + Complemento",
                 fifelse(price_ticket_base == 40, "Navegante Metropolitano",
                 fifelse(price_ticket_base == 20, "Viver Cascais",
                 fifelse(price_ticket_base == 50, "Navegante Municipal + Complemento",
                 fifelse(price_ticket_base == 30, "Navegante Lisboa",
                 fifelse(price_ticket_base == 16.75, "MTS Passe", as.character(NA))))))),
      price_ticket_base_new = fifelse(ticket_code %in% c(6170L, 7670L), "60", as.character(price_ticket_base)),
      numval = count_val,
      compensacao = fifelse(count_val < 20L,
                            0.05 * fifelse(ticket_code %in% c(6170L, 7670L), 60, price_ticket_base) * count_val,
                            fifelse(ticket_code %in% c(6170L, 7670L), 60, price_ticket_base)),
      ticket_operator_code_new = as.character(ticket_operator_code),
      ticket_code_new = fifelse(ticket_operator_code == 31L & ticket_code %in% c(6170L, 7670L),
                                paste0(fifelse(age < 19L, "6180_", "6181_"), ticket_code),
                                as.character(ticket_code)),
      ticket_name_new = fifelse(ticket_operator_code == 31L & ticket_code %in% c(6170L, 7670L),
                                paste0(fifelse(age < 19L, "Navegante Metropolitano 418 Gratuito+", "Navegante Metropolitano sub23 Gratuito+"), ticket_name),
                                ticket_name),
      compaz = as.integer(NA),
      nome_dist = ticket_name,
      age_new = age,
      birth_date_new = birth_date,
      cardserialnumber_new = card_serial_number
    )]
  } else {
    res_singles <- dt_singles[, .(
      am = "AML",
      emissor = fifelse(ticket_operator_code == 31L, "AT AML",
                fifelse(ticket_operator_code == 21L, "AT Cascais",
                fifelse(ticket_operator_code == 16L, "MTS", ""))),
      year_dist,
      month_dist,
      card_serial_number,
      nome_imt = fifelse(ticket_code %in% c(6170L, 7670L), "Navegante Metropolitano + Complemento",
                 fifelse(price_ticket_base == 40, "Navegante Metropolitano",
                 fifelse(price_ticket_base == 20, "Viver Cascais",
                 fifelse(price_ticket_base == 50, "Navegante Municipal + Complemento",
                 fifelse(price_ticket_base == 30, "Navegante Lisboa",
                 fifelse(price_ticket_base == 16.75, "MTS Passe", as.character(NA))))))),
      price_ticket_base_new = fifelse(ticket_code %in% c(6170L, 7670L), "60", as.character(price_ticket_base)),
      numval = count_val,
      compensacao = fifelse(count_val < 20L,
                            0.05 * fifelse(ticket_code %in% c(6170L, 7670L), 60, price_ticket_base) * count_val,
                            fifelse(ticket_code %in% c(6170L, 7670L), 60, price_ticket_base)),
      ticket_operator_code_new = as.character(ticket_operator_code),
      ticket_code_new = as.character(ticket_code),
      ticket_name_new = ticket_name,
      compaz = as.integer(NA),
      nome_dist = ticket_name,
      age_new = age,
      birth_date_new = birth_date
    )]
  }

  # ── 2. Registos múltiplos (apenas ~5% dos casos, agregados em segundos) ─────
  dt_multis <- dt[get(key_col) %in% multi_keys]

  res_multis <- if (nrow(dt_multis) > 0) {
    if (por == "fiscal") {
      dt_multis[, {
        ord       <- order(op_prio(ticket_operator_code), code_prio(ticket_code), ticket_code)
        ord_op    <- order(op_prio(ticket_operator_code), ticket_operator_code)
        codes_ord <- ticket_code[ord]
        codes_str <- paste(codes_ord, collapse = "_")
        ops_str   <- paste(ticket_operator_code[ord_op], collapse = "_")
        names_str <- paste(ticket_name[ord], collapse = "+")
        age_min   <- min(age)
        has_comp  <- any(ticket_code %in% c(6170L, 7670L))
        max_ptb   <- max(price_ticket_base)
        ptb_new   <- if (length(ticket_code) == 1L && has_comp) "60" else if (has_comp) as.character(max_ptb + 20) else as.character(max_ptb)

        tc_new <- if (ops_str == "31" && codes_str %in% c("6170", "7670")) {
          paste0(if (age_min < 19L) "6180_" else "6181_", codes_str)
        } else codes_str

        tn_new <- if (ops_str == "31" && codes_str %in% c("6170", "7670")) {
          paste0(if (age_min < 19L) "Navegante Metropolitano 418 Gratuito+" else "Navegante Metropolitano sub23 Gratuito+", names_str)
        } else names_str

        n_dist <- if (!grepl("\\+", tn_new)) tn_new
        else if (grepl("Navegante Complemento 418/sub23 \\(A\\)", tn_new)) paste0(sub("^([^\\+]*\\+).*", "\\1", tn_new), "Navegante Complemento 418/sub23 (A)")
        else if (grepl("Navegante Complemento 418/sub23", tn_new)) paste0(sub("^([^\\+]*\\+).*", "\\1", tn_new), "Navegante Complemento 418/sub23")
        else sub("\\+.*", "", tn_new)

        nv <- sum(count_val)
        ptb_num <- as.numeric(ptb_new)
        comp <- if (nv < 20L) 0.05 * ptb_num * nv else ptb_num

        em <- if (startsWith(ops_str, "31")) "AT AML" else if (startsWith(ops_str, "21")) "AT Cascais" else if (ops_str == "16") "MTS" else ""
        n_imt <- switch(ptb_new, "60" = "Navegante Metropolitano + Complemento", "50" = "Navegante Municipal + Complemento", "40" = "Navegante Metropolitano", "30" = "Navegante Lisboa", "20" = "Viver Cascais", "16.75" = "MTS Passe", as.character(NA))

        .(
          am = "AML",
          emissor = em,
          year_dist = year_dist[1],
          month_dist = month_dist[1],
          nome_imt = n_imt,
          price_ticket_base_new = ptb_new,
          numval = nv,
          compensacao = comp,
          ticket_operator_code_new = ops_str,
          ticket_code_new = tc_new,
          ticket_name_new = tn_new,
          compaz = as.integer(NA),
          nome_dist = n_dist,
          age_new = age_min,
          birth_date_new = max(birth_date),
          cardserialnumber_new = paste(sort(unique(card_serial_number)), collapse = "_")
        )
      }, by = .(fiscal_number)]
    } else {
      dt_multis[, {
        ord       <- order(op_prio(ticket_operator_code), code_prio(ticket_code), ticket_code)
        ord_op    <- order(op_prio(ticket_operator_code), ticket_operator_code)
        codes_ord <- ticket_code[ord]
        codes_str <- paste(codes_ord, collapse = "_")
        ops_str   <- paste(ticket_operator_code[ord_op], collapse = "_")
        names_str <- paste(ticket_name[ord], collapse = "+")
        age_min   <- min(age)
        has_comp  <- any(ticket_code %in% c(6170L, 7670L))
        max_ptb   <- max(price_ticket_base)
        ptb_new   <- if (length(ticket_code) == 1L && has_comp) "60" else if (has_comp) as.character(max_ptb + 20) else as.character(max_ptb)

        n_dist <- if (!grepl("\\+", names_str)) names_str
        else if (grepl("Navegante Complemento 418/sub23 \\(A\\)", names_str)) paste0(sub("^([^\\+]*\\+).*", "\\1", names_str), "Navegante Complemento 418/sub23 (A)")
        else if (grepl("Navegante Complemento 418/sub23", names_str)) paste0(sub("^([^\\+]*\\+).*", "\\1", names_str), "Navegante Complemento 418/sub23")
        else sub("\\+.*", "", names_str)

        nv <- sum(count_val)
        ptb_num <- as.numeric(ptb_new)
        comp <- if (nv < 20L) 0.05 * ptb_num * nv else ptb_num

        em <- if (startsWith(ops_str, "31")) "AT AML" else if (startsWith(ops_str, "21")) "AT Cascais" else if (ops_str == "16") "MTS" else ""
        n_imt <- switch(ptb_new, "60" = "Navegante Metropolitano + Complemento", "50" = "Navegante Municipal + Complemento", "40" = "Navegante Metropolitano", "30" = "Navegante Lisboa", "20" = "Viver Cascais", "16.75" = "MTS Passe", as.character(NA))

        .(
          am = "AML",
          emissor = em,
          year_dist = year_dist[1],
          month_dist = month_dist[1],
          nome_imt = n_imt,
          price_ticket_base_new = ptb_new,
          numval = nv,
          compensacao = comp,
          ticket_operator_code_new = ops_str,
          ticket_code_new = codes_str,
          ticket_name_new = names_str,
          compaz = as.integer(NA),
          nome_dist = n_dist,
          age_new = age_min,
          birth_date_new = max(birth_date)
        )
      }, by = .(card_serial_number)]
    }
  } else {
    NULL
  }

  rbindlist(list(res_singles, res_multis), use.names = TRUE)
}


# =============================================================================
# Função principal exportada
# =============================================================================

#' Calcula compensações comNIF e semNIF para o mês indicado.
#' @param ano  integer
#' @param mes  integer
#' @param raiz character
#' @return list com data frames comNIF e semNIF (invisível)
calcular_compensacoes <- function(ano, mes, raiz = here_root()) {

  paths <- month_paths(ano, mes, raiz)
  stopifnot(dir.exists(paths$output))

  message(sprintf("[02] A calcular compensações para %d/%02d (modo rápido)...", ano, mes))
  t0 <- proc.time()

  # ── Ler dados limpos via fread ───────────────────────────────────────────
  f_limpos <- file.path(paths$output, output_filename(ano, mes, "dados_limpos"))
  f_az     <- file.path(paths$output, output_filename(ano, mes, "complemento_azambuja"))

  if (!file.exists(f_limpos))
    stop("Ficheiro de dados limpos não encontrado: ", f_limpos,
         "\n  Execute primeiro: preparar_dados(", ano, ", ", mes, ")")

  dt_main <- fread(f_limpos, sep = ";", dec = ".", colClasses = list(
    character = c("card_serial_number", "fiscal_number", "ticket_name"),
    integer   = c("year_dist", "month_dist", "age", "ticket_operator_code", "ticket_code", "count_val"),
    numeric   = c("price", "price_ticket_base"),
    IDate     = c("birth_date")
  ))

  dt_az <- if (file.exists(f_az)) {
    az_dt <- fread(f_az, sep = ";", dec = ".", colClasses = list(
      character = c("card_serial_number", "fiscal_number"),
      integer   = c("year_dist", "month_dist", "ticket_code", "count_val")
    ))
    az_dt[fiscal_number %in% c("", "0", "NA"), fiscal_number := NA_character_]
    az_dt
  } else NULL

  # ── Filtrar: apenas gratuitos do mês ────────────────────────────────────
  dt_mes <- dt_main[year_dist == ano & month_dist == mes & price == 0]
  dt_mes[fiscal_number %in% c("", "0", "NA"), fiscal_number := NA_character_]
  message(sprintf("  Passes gratuitos com validações: %d", nrow(dt_mes)))

  # ── Agregar Azambuja ─────────────────────────────────────────────────────
  az_fiscal <- if (!is.null(dt_az)) {
    dt_az[!is.na(fiscal_number),
          .(count_val_az = sum(count_val), ticket_code_az = first(ticket_code)),
          by = .(fiscal_number)]
  } else NULL

  az_card <- if (!is.null(dt_az)) {
    dt_az[is.na(fiscal_number),
          .(count_val_az = sum(count_val), ticket_code_az = first(ticket_code)),
          by = .(card_serial_number)]
  } else NULL

  # ── ComNIF ────────────────────────────────────────────────────────────────
  message("  A processar comNIF ...")
  dt_com_raw <- dt_mes[!is.na(fiscal_number)]
  dt_comNIF   <- agregar_grupo_dt(dt_com_raw, "fiscal")

  # Join Azambuja e recalcular compensação
  if (!is.null(az_fiscal) && nrow(az_fiscal) > 0) {
    dt_comNIF[az_fiscal, on = .(fiscal_number), `:=`(
      numval = numval + i.count_val_az,
      compaz = i.ticket_code_az,
      compensacao = fifelse((numval + i.count_val_az) < 20L,
                            0.05 * as.numeric(price_ticket_base_new) * (numval + i.count_val_az),
                            as.numeric(price_ticket_base_new))
    )]
  }

  cols_com <- c("am", "emissor", "year_dist", "month_dist", "fiscal_number",
                "nome_imt", "price_ticket_base_new", "numval", "compensacao",
                "ticket_operator_code_new", "ticket_code_new", "ticket_name_new",
                "compaz", "nome_dist", "age_new", "birth_date_new", "cardserialnumber_new")
  dt_comNIF <- dt_comNIF[, ..cols_com]
  message(sprintf("  comNIF: %d registos", nrow(dt_comNIF)))

  # ── SemNIF ────────────────────────────────────────────────────────────────
  message("  A processar semNIF ...")
  dt_sem_raw <- dt_mes[is.na(fiscal_number)]
  dt_semNIF   <- agregar_grupo_dt(dt_sem_raw, "card")

  # Join Azambuja e recalcular compensação
  if (!is.null(az_card) && nrow(az_card) > 0) {
    dt_semNIF[az_card, on = .(card_serial_number), `:=`(
      numval = numval + i.count_val_az,
      compaz = i.ticket_code_az,
      compensacao = fifelse((numval + i.count_val_az) < 20L,
                            0.05 * as.numeric(price_ticket_base_new) * (numval + i.count_val_az),
                            as.numeric(price_ticket_base_new))
    )]
  }

  cols_sem <- c("am", "emissor", "year_dist", "month_dist", "card_serial_number",
                "nome_imt", "price_ticket_base_new", "numval", "compensacao",
                "ticket_operator_code_new", "ticket_code_new", "ticket_name_new",
                "compaz", "nome_dist", "age_new", "birth_date_new")
  dt_semNIF <- dt_semNIF[, ..cols_sem]
  message(sprintf("  semNIF: %d registos", nrow(dt_semNIF)))

  # ── Gravar ────────────────────────────────────────────────────────────────
  out_com <- file.path(paths$output, output_filename(ano, mes, "comNIF"))
  out_sem <- file.path(paths$output, output_filename(ano, mes, "semNIF"))

  write_csv_pj(dt_comNIF, out_com)
  write_csv_pj(dt_semNIF, out_sem)

  elapsed <- round((proc.time() - t0)[["elapsed"]], 1)
  message(sprintf("  Gravado: %s", basename(out_com)))
  message(sprintf("  Gravado: %s", basename(out_sem)))
  message(sprintf("[02] Concluído em %.1f segundos.\n", elapsed))

  invisible(list(comNIF = as.data.frame(dt_comNIF), semNIF = as.data.frame(dt_semNIF)))
}


# -----------------------------------------------------------------------------
# Execução directa
# -----------------------------------------------------------------------------
if (!interactive() && identical(sys.frames(), list())) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2) {
    calcular_compensacoes(ano = as.integer(args[1]), mes = as.integer(args[2]))
  } else {
    cat("Uso: Rscript R/02_calcular_compensacoes.R <ano> <mes>\n")
  }
}
