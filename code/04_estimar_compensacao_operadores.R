# =============================================================================
# 04_estimar_compensacao_operadores.R
# 2.ª fase: estimativa da compensação por operador e do congelamento.
#
# Passos:
#   1. Junta comNIF + semNIF (apenas registos OK) e agrega por nome_dist e
#      código de título base (extraído de ticket_code_new):
#        numval, compensacao (somas) e passes (n()).
#   2. Cruza directamente por ticket_code com lists/titulos.csv (preco_base,
#      modalidade, título base) do repositório Compensacoes, calculando
#      base_weight = preco_base / price_ticket_base_new (sem depender de
#      lists/titulos_nomes.csv).
#   3. Separa a compensação de cada título em:
#        - Navegante   = compensacao * base_weight
#        - Complemento = compensacao * (1 - base_weight)   -> 100% "AT TML - CM"
#        - Viver Cascais (nome_dist "Viver Cascais ...")   -> 100% "AT Cascais"
#      Ex.: Metropolitano 418/sub23 + Complemento: 40 EUR + 20 EUR (base 2/3);
#           Montijo + Complemento: 30 EUR + 20 EUR (base 3/5).
#   4. Distribui a parte Navegante por operador com as quotas estimadas do
#      mesmo mês/ano (Compensacoes/data/<mm_mes_aaaa>/output/1_quotas_<mes><aa>.csv).
#   5. Estima o congelamento (factor TAT cumulativo, como em
#      Compensacoes/code/05_reparticao_B.R).
#
# Produz em data/<ano>_<mm>_<mes>/output/:
#   <ano>_<mm>_compensacao_por_titulo.csv   (agregado por nome_dist + valores)
#   <ano>_<mm>_passejovem_resumo.csv        (compensação por título e operador, s/ TOTAL*)
#   <ano>_<mm>_passejovem_congelamento.csv  (congelamento por operador)
#
# Uso:
#   source("code/04_estimar_compensacao_operadores.R")
#   estimar_compensacao_operadores(ano = 2026, mes = 7)
# =============================================================================

source(file.path(dirname(sys.frame(1)$ofile), "00_config.R"),
       local = TRUE, chdir = FALSE)


# =============================================================================
# Funções auxiliares
# =============================================================================

#' 1. Lê comNIF + semNIF, mantém só registos OK e agrega por nome_dist e
#' código de bilhete principal (primeiro código de ticket_code_new, que já vem
#' ordenado por prioridade em 02_calcular_compensacoes.R).
#' "OK" = registos com compensação calculada e idade elegível
#' (os fora da faixa etária já foram removidos em 02_calcular_compensacoes.R).
agregar_por_titulo <- function(paths, ano, mes) {
  cols  <- c("numval", "compensacao", "nome_dist", "age_new",
             "ticket_code_new", "ticket_operator_code_new", "price_ticket_base_new")
  f_com <- file.path(paths$output, output_filename(ano, mes, "comNIF"))
  f_sem <- file.path(paths$output, output_filename(ano, mes, "semNIF"))

  dt <- bind_rows(
    fread(f_com, sep = ";", select = cols, dec = ","),
    fread(f_sem, sep = ";", select = cols, dec = ",")
  )
  n0 <- nrow(dt)

  dt_ok <- dt |>
    filter(
      !is.na(compensacao),
      !is.na(nome_dist),
      !(age_new %in% 3L),
      !(age_new >= 25L)
    ) |>
    mutate(
      # O primeiro código em ticket_code_new / ticket_operator_code_new é o título principal
      ticket_code          = suppressWarnings(as.integer(sub("_.*", "", as.character(ticket_code_new)))),
      ticket_operator_code = suppressWarnings(as.integer(sub("_.*", "", as.character(ticket_operator_code_new)))),
      ptb_new              = suppressWarnings(as.numeric(price_ticket_base_new)),
      # Deteta Complemento CM pelos códigos CODES_COMP (6170/7670) ou pelo nome_dist
      tem_comp_code        = str_detect(
        as.character(ticket_code_new),
        paste0("(^|_)(", paste(CODES_COMP, collapse = "|"), ")($|_)")
      )
    )

  message(sprintf("  Registos OK: %s de %s",
                  format(nrow(dt_ok), big.mark = " "),
                  format(n0, big.mark = " ")))

  dt_ok |>
    group_by(nome_dist, ticket_code, ticket_operator_code, ptb_new) |>
    summarise(
      tem_comp_code = any(tem_comp_code, na.rm = TRUE),
      numval        = sum(numval, na.rm = TRUE),
      compensacao   = sum(compensacao, na.rm = TRUE),
      passes        = n(),
      .groups       = "drop"
    )
}

#' 2. Cruza directamente por ticket_code com lists/titulos.csv e calcula
#' complemento_CM e base_weight a partir dos preços (sem titulos_nomes.csv).
enriquecer_titulos <- function(agg) {
  info_tit <- ler_lista_compensacoes("titulos.csv") |>
    mutate(
      ticket_code = suppressWarnings(as.integer(ticket_code)),
      pvp         = suppressWarnings(as.numeric(str_replace(as.character(pvp), ",", "."))),
      preco_base  = suppressWarnings(as.numeric(str_replace(as.character(preco_base), ",", "."))),
      op_tit      = suppressWarnings(as.integer(ticket_operator_code))
    ) |>
    select(ticket_code, titulo_nome, modalidade, pvp, titulo_nome_base, preco_base, op_tit) |>
    distinct(ticket_code, .keep_all = TRUE)

  agg |>
    left_join(info_tit, by = "ticket_code") |>
    mutate(
      viver_cascais        = coalesce(ticket_operator_code == 21L, FALSE) | str_detect(nome_dist, "^Viver Cascais"),
      complemento_CM       = !viver_cascais & (tem_comp_code | str_detect(nome_dist, "Complemento")),
      ticket_operator_code = coalesce(op_tit, ticket_operator_code),
      # Se um título novo ainda não constar em titulos.csv, usa o preço base do registo
      preco_base           = if_else(
        viver_cascais,
        NA_real_,
        coalesce(preco_base, if_else(complemento_CM, ptb_new - 20, ptb_new))
      ),
      # Peso da componente Navegante: ex. 40/60 = 2/3 (Metropolitano+Comp) ou 30/50 = 3/5 (Municipal+Comp)
      base_weight          = if_else(
        !viver_cascais & complemento_CM & !is.na(ptb_new) & ptb_new > 0,
        preco_base / ptb_new,
        1
      ),
      ticket_name_reduzido = titulo_nome
    ) |>
    select(-ptb_new, -tem_comp_code, -titulo_nome, -op_tit)
}

#' 3. Factor TAT cumulativo desde 2024 até ao ano (igual a Compensacoes/05_reparticao_B.R).
fator_tat <- function(ano_ref) {
  ler_lista_compensacoes("tat.csv") |>
    mutate(tat = as.numeric(str_replace(tat, ",", "."))) |>
    filter(ano <= ano_ref) |>
    summarise(ft = prod(1 + tat) - 1) |>
    pull(ft)
}

#' 4. Factores de congelamento:
#'   Navegante:     (1 + tat2024)(1 + tat2025)(1 + tat2026) - 1
#'   Outros passes: tat2024 * (1 + tat2025)(1 + tat2026)
fatores_congelamento <- function(ano_ref) {
  tat_df <- ler_lista_compensacoes("tat.csv") |>
    mutate(tat = as.numeric(str_replace(tat, ",", "."))) |>
    filter(ano <= ano_ref)

  ft_nav <- tat_df |>
    summarise(ft = prod(1 + tat) - 1) |>
    pull(ft)

  tat_2024 <- tat_df |>
    filter(ano == 2024) |>
    pull(tat)

  ft_apos_2024 <- tat_df |>
    filter(ano > 2024) |>
    summarise(ft = prod(1 + tat)) |>
    pull(ft)

  list(
    navegante = ft_nav,
    outros    = tat_2024 * ft_apos_2024
  )
}


# =============================================================================
# Função principal
# =============================================================================

estimar_compensacao_operadores <- function(ano, mes, raiz = here_root()) {
  paths <- month_paths(ano, mes, raiz)
  message(sprintf("[04] Estimativa por operador %d/%02d", ano, mes))

  # 1-2. Agregado por título, com reduzido / complemento / base_weight ------
  agg <- agregar_por_titulo(paths, ano, mes) |>
    enriquecer_titulos()

  sem_codigo <- agg |>
    filter(is.na(ticket_code), !viver_cascais) |>
    pull(nome_dist)
  if (length(sem_codigo) > 0) {
    warning("Títulos sem ticket_code: ", paste(sem_codigo, collapse = "; "))
  }

  # 3. Parte Navegante / Complemento CM / Viver Cascais ---------------------
  #    Navegante     = compensacao * base_weight   (ex.: 40 de 60 = 2/3)
  #    Complemento   = compensacao * (1 - base_weight) -> 100% AT TML - CM
  #    Viver Cascais = compensacao                     -> 100% AT Cascais
  agg <- agg |>
    mutate(
      comp_navegante   = if_else(viver_cascais, 0, compensacao * base_weight),
      comp_complemento = if_else(complemento_CM, compensacao * (1 - base_weight), 0),
      comp_cascais     = if_else(viver_cascais, compensacao, 0),
      teto_passe       = preco_base / base_weight   # ex.: 40/(2/3)=60; Montijo 30/0.6=50
    )

  acima_teto <- agg |>
    filter(!viver_cascais, compensacao > teto_passe * passes + 1e-6)
  if (nrow(acima_teto) > 0) {
    warning("Compensação acima do tecto (preco_base/base_weight x passes) em algum título.")
  }

  # Linhas "Navegante": agrega por ticket de quota (passes e validações
  # contam por inteiro; a compensação é só a parte Navegante).
  tit <- agg |>
    filter(!viver_cascais) |>
    group_by(ticket_operator_code, ticket_code, modalidade,
             pvp, titulo_nome_base, preco_base) |>
    summarise(
      linha             = coalesce(first(na.omit(ticket_name_reduzido)), nome_dist[which.max(passes)]),
      passes            = sum(passes, na.rm = TRUE),
      validacoes        = sum(numval, na.rm = TRUE),
      compensacao_total = sum(comp_navegante, na.rm = TRUE),
      .groups           = "drop"
    ) |>
    arrange(ticket_code)

  # 4. Quotas do mês -----------------------------------------------------------
  f_quotas <- caminho_quotas(ano, mes)
  if (!file.exists(f_quotas)) stop("Ficheiro de quotas não encontrado: ", f_quotas)

  ops <- names(OPERADORES_RESUMO)
  mapa_ops <- tibble(
    operador      = unname(OPERADORES_RESUMO),
    nome_operador = names(OPERADORES_RESUMO)
  )

  quotas_wide <- fread(f_quotas, sep = ";", dec = ",",
                       select = c("operador", "ticket_code", "quota")) |>
    as_tibble() |>
    inner_join(mapa_ops, by = "operador") |>
    select(ticket_code, nome_operador, quota) |>
    as.data.table() |>
    dcast(ticket_code ~ nome_operador, value.var = "quota", fill = 0) |>
    as_tibble() |>
    select(ticket_code, all_of(ops))

  tit_dist <- tit |>
    left_join(quotas_wide, by = "ticket_code")

  falta <- tit_dist |>
    filter(is.na(.data[[ops[1]]])) |>
    pull(ticket_code)
  if (length(falta) > 0) {
    warning("Sem quotas para ticket_code: ", paste(falta, collapse = ", "))
  }

  # Multiplicar cada quota pela compensação total do título (sem arredondar)
  tit_dist <- tit_dist |>
    mutate(across(all_of(ops), ~ compensacao_total * coalesce(.x, 0)))

  # 5. Outros passes (Complemento CM e Viver Cascais) -------------------------
  zeros_ops <- as_tibble(setNames(as.list(rep(0, length(ops))), ops))

  r_cm <- agg |>
    filter(complemento_CM) |>
    summarise(
      ticket_operator_code = first(na.omit(ticket_operator_code)),
      ticket_code          = as.character(CODE_COMP_RESUMO),
      modalidade           = paste(sort(unique(na.omit(modalidade))), collapse = "_"),
      linha                = paste("Complemento CM", modalidade),
      pvp                  = 0,
      titulo_nome_base     = "Complemento CM",
      preco_base           = first(na.omit(preco_base * (1 - base_weight) / base_weight)),
      passes               = sum(passes, na.rm = TRUE),
      validacoes           = sum(numval, na.rm = TRUE),
      compensacao_total    = sum(comp_complemento, na.rm = TRUE)
    ) |>
    bind_cols(zeros_ops) |>
    mutate(`AT TML - CM` = compensacao_total)

  r_ca <- agg |>
    filter(viver_cascais) |>
    summarise(
      ticket_operator_code = first(na.omit(ticket_operator_code)),
      ticket_code          = paste(sort(unique(na.omit(ticket_code))), collapse = "_"),
      linha                = "Viver Cascais Residente ou Estudante",
      modalidade           = "Residente_Estudante",
      pvp                  = 0,
      titulo_nome_base     = "Viver Cascais",
      preco_base           = PRECO_CASCAIS,
      passes               = sum(passes, na.rm = TRUE),
      validacoes           = sum(numval, na.rm = TRUE),
      compensacao_total    = sum(comp_cascais, na.rm = TRUE)
    ) |>
    bind_cols(zeros_ops) |>
    mutate(`AT Cascais` = compensacao_total)

  # Totais por grupo para cálculo do congelamento
  tot_nav <- tit_dist |>
    summarise(across(all_of(ops), ~ sum(.x, na.rm = TRUE)))

  tot_out <- bind_rows(r_cm, r_ca) |>
    summarise(across(all_of(ops), ~ sum(.x, na.rm = TRUE)))

  # Totais gerais (para conferência) arredondados a 2 casas decimais
  total_geral <- bind_rows(tot_nav, tot_out) |>
    summarise(across(all_of(ops), ~ sum(.x, na.rm = TRUE))) |>
    mutate(compensacao_total = round(rowSums(across(all_of(ops))), 2))

  # 6. Congelamento -----------------------------------------------------------
  fc <- fatores_congelamento(ano)

  cong_nav <- tot_nav |>
    mutate(across(all_of(ops), ~ .x * fc$navegante)) |>
    mutate(
      linha             = "Congelamento Navegante",
      compensacao_total = rowSums(across(all_of(ops)))
    ) |>
    select(linha, compensacao_total, all_of(ops))

  cong_out <- tot_out |>
    mutate(across(all_of(ops), ~ .x * fc$outros)) |>
    mutate(
      linha             = "Congelamento Outros Passes",
      compensacao_total = rowSums(across(all_of(ops)))
    ) |>
    select(linha, compensacao_total, all_of(ops))

  cong_tot <- bind_rows(cong_nav, cong_out) |>
    summarise(across(all_of(ops), ~ round(sum(.x, na.rm = TRUE), 2))) |> # arredondado apenas na linha de total
    mutate(
      linha             = "Congelamento TOTAL",
      compensacao_total = rowSums(across(all_of(ops)))
    ) |>
    select(linha, compensacao_total, all_of(ops))

  message(sprintf("  Factores de congelamento: Navegante %.6f | Outros %.6f",
                  fc$navegante, fc$outros))

  # Saídas -------------------------------------------------------------------
  cols_resumo <- c("ticket_operator_code", "ticket_code", "linha",
                   "modalidade", "passes", "validacoes",
                   "compensacao_total", ops)

  # _resumo.csv: apenas títulos individuais + Complemento CM + Viver Cascais (sem TOTAL*)
  resumo <- bind_rows(
    tit_dist |> mutate(ticket_code = as.character(ticket_code)) |> select(all_of(cols_resumo)),
    r_cm     |> select(all_of(cols_resumo)),
    r_ca     |> select(all_of(cols_resumo))
  ) |>
    mutate(ano = ano, mes = mes, .before = 1)

  # _congelamento.csv: linhas de congelamento num CSV separado
  congelamento <- bind_rows(cong_nav, cong_out, cong_tot) |>
    mutate(ano = ano, mes = mes, .before = 1)

  agg_out <- agg |>
    arrange(desc(compensacao)) |>
    select(nome_dist, ticket_name_reduzido, complemento_CM, base_weight,
           ticket_operator_code, ticket_code, modalidade,
           preco_base, passes, numval, compensacao,
           comp_navegante, comp_complemento, comp_cascais)

  f_tit  <- file.path(paths$output, output_filename(ano, mes, "compensacao_por_titulo"))
  f_res  <- file.path(paths$output, output_filename(ano, mes, "passejovem_resumo"))
  f_cong <- file.path(paths$output, output_filename(ano, mes, "passejovem_congelamento"))

  gravar_csv <- function(dt, path) {
    tryCatch(
      # bom = TRUE garante que o Excel no Windows lê correctamente caracteres UTF-8 (ex.: CRIANÇA)
      fwrite(dt, path, sep = ";", dec = ",", bom = TRUE),
      error = function(e) {
        warning(sprintf("Não foi possível gravar '%s' (estará aberto no Excel?): %s",
                        basename(path), conditionMessage(e)), call. = FALSE)
      }
    )
  }

  gravar_csv(agg_out,      f_tit)
  gravar_csv(resumo,       f_res)
  gravar_csv(congelamento, f_cong)

  message(sprintf("  Gravados: %s, %s, %s",
                  basename(f_tit), basename(f_res), basename(f_cong)))

  print(resumo |> select(ticket_operator_code, ticket_code, linha,
                         modalidade, passes, validacoes, compensacao_total))
  print(congelamento |> select(linha, compensacao_total))
  invisible(list(resumo = resumo, congelamento = congelamento,
                 titulos = agg_out, fatores = fc))
}
