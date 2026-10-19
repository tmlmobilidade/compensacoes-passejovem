# =============================================================================
# 04_estimar_compensacao_operadores.R
# 2.ª fase: estimativa da compensação por operador e do congelamento.
#
# Passos:
#   1. Junta comNIF + semNIF (apenas registos OK) e agrega por nome_dist:
#        numval, compensacao (somas) e passes (n()).
#   2. Cruza com lists/titulos_nomes.csv (ticket_name_reduzido,
#      complemento_CM, base_weight) e lists/titulos.csv (preco_base) do
#      repositório Compensacoes.
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
#   <ano>_<mm>_comp_por_titulo.csv          (agregado por nome_dist + valores)
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
# Constantes
# =============================================================================

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


# =============================================================================
# Funções auxiliares
# =============================================================================

#' Lê uma lista do Compensacoes (local; se não existir, do GitHub).
ler_lista_compensacoes <- function(nome) {
  local <- file.path(COMPENSACOES_ROOT, "lists", nome)
  src   <- if (file.exists(local)) local else file.path(COMPENSACOES_GITHUB, nome)
  # fill = TRUE: algumas linhas de titulos_nomes.csv têm menos colunas
  fread(src, sep = ";", fill = TRUE, encoding = "UTF-8", na.strings = c("", "NA")) |>
    as_tibble()
}

#' Converte "2/3", "3/5" ou "1" em número.
parse_peso <- function(x) {
  partes <- str_split_fixed(as.character(x), "/", 2)
  num    <- suppressWarnings(as.numeric(partes[, 1]))
  den    <- suppressWarnings(as.numeric(partes[, 2]))
  peso   <- if_else(!is.na(den) & den != 0, num / den, num)
  coalesce(peso, 1)
}

#' Caminho do ficheiro de quotas do Compensacoes para um mês.
caminho_quotas <- function(ano, mes) {
  pasta <- sprintf("%02d_%s_%d", mes, MESES_ABREV[mes], ano)
  file.path(COMPENSACOES_ROOT, "data", pasta, "output",
            sprintf("1_quotas_%s%02d.csv", MESES_ABREV[mes], ano %% 100))
}

#' 1. Lê comNIF + semNIF, mantém só registos OK e agrega por nome_dist.
#' "OK" = registos com compensação calculada e idade elegível
#' (os fora da faixa etária já foram removidos em 02_calcular_compensacoes.R).
agregar_por_titulo <- function(paths, ano, mes) {
  cols  <- c("numval", "compensacao", "nome_dist", "age_new")
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
    )

  message(sprintf("  Registos OK: %s de %s",
                  format(nrow(dt_ok), big.mark = " "),
                  format(n0, big.mark = " ")))

  dt_ok |>
    group_by(nome_dist) |>
    summarise(
      numval      = sum(numval, na.rm = TRUE),
      compensacao = sum(compensacao, na.rm = TRUE),
      passes      = n(),
      .groups     = "drop"
    )
}

#' 2. Associa a cada nome_dist: ticket_name_reduzido, complemento_CM, base_weight,
#' ticket_code de referência (para as quotas) e preco_base.
enriquecer_titulos <- function(agg) {
  tn <- ler_lista_compensacoes("titulos_nomes.csv") |>
    mutate(
      ticket_code     = suppressWarnings(as.integer(ticket_code)),
      base_weight_num = parse_peso(base_weight),
      complemento_CM  = as.logical(complemento_CM)
    )

  tit <- ler_lista_compensacoes("titulos.csv") |>
    mutate(ticket_code = suppressWarnings(as.integer(ticket_code)))

  # Linhas "mestre" de complemento (complemento_CM == TRUE), indexadas por
  # ticket_name_completo (que coincide com ticket_name_short das variantes
  # "X+Navegante Complemento 418/sub23").
  mestre_comp <- tn |>
    filter(complemento_CM) |>
    select(
      chave_comp    = ticket_name_completo,
      reduzido_comp = ticket_name_reduzido,
      peso_comp     = base_weight_num
    ) |>
    distinct(chave_comp, .keep_all = TRUE)

  # Códigos de bilhete a partir de titulos_nomes.csv e de titulos.csv (418_sub23 / Criança)
  codigos_tn <- tn |>
    filter(!is.na(ticket_code)) |>
    select(nome_ref = ticket_name_completo, code_tn = ticket_code) |>
    distinct(nome_ref, .keep_all = TRUE)

  codigos_tit <- tit |>
    filter(modalidade %in% c("418_sub23", "Criança", "crianca")) |>
    select(nome_ref = titulo_nome, code_tit = ticket_code) |>
    distinct(nome_ref, .keep_all = TRUE)

  info_tit <- tit |>
    select(ticket_code, preco_base, ticket_operator_code, ticket_modalidade_operador) |>
    distinct(ticket_code, .keep_all = TRUE)

  ops_domain <- ler_lista_compensacoes("operadores.csv") |>
    filter(!is.na(OPERATOR_NAME_NAVEGANTE)) |>
    select(ticket_domain = OPERATOR_NAME_NAVEGANTE, op_code_domain = ENTITY_CODE) |>
    mutate(op_code_domain = as.integer(op_code_domain)) |>
    distinct(ticket_domain, .keep_all = TRUE)

  agg |>
    left_join(
      tn |>
        select(ticket_name_completo, ticket_name_short, ticket_domain,
               code_proprio = ticket_code, ticket_name_reduzido,
               complemento_CM, base_weight_num) |>
        distinct(ticket_name_completo, .keep_all = TRUE),
      by = c("nome_dist" = "ticket_name_completo")
    ) |>
    left_join(ops_domain, by = "ticket_domain") |>
    mutate(chave_comp = coalesce(ticket_name_short, nome_dist)) |>
    left_join(mestre_comp, by = "chave_comp") |>
    mutate(
      viver_cascais        = str_detect(nome_dist, "^Viver Cascais"),
      complemento_CM       = if_else(viver_cascais, FALSE,
                                     coalesce(!is.na(reduzido_comp), complemento_CM, FALSE)),
      ticket_name_reduzido = coalesce(reduzido_comp, ticket_name_reduzido),
      base_weight          = if_else(viver_cascais, 1,
                                     coalesce(peso_comp, base_weight_num, 1))
    ) |>
    left_join(codigos_tn,  by = c("ticket_name_reduzido" = "nome_ref")) |>
    left_join(codigos_tit, by = c("ticket_name_reduzido" = "nome_ref")) |>
    mutate(
      # Código Navegante de referência (para quotas e preco_base)
      ticket_code = case_when(
        viver_cascais  ~ code_proprio,
        complemento_CM ~ coalesce(code_tn, code_tit, code_proprio),
        TRUE           ~ coalesce(code_proprio, code_tn, code_tit)
      )
    ) |>
    left_join(info_tit, by = "ticket_code") |>
    mutate(
      ticket_operator_code = coalesce(as.integer(ticket_operator_code), op_code_domain)
    ) |>
    select(-ticket_name_short, -ticket_domain, -op_code_domain, -base_weight_num,
           -chave_comp, -reduzido_comp, -peso_comp, -code_proprio, -code_tn, -code_tit)
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
    group_by(ticket_operator_code, ticket_code, ticket_modalidade_operador) |>
    summarise(
      linha             = nome_dist[which.max(passes)],
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

  # Multiplicar cada quota pela compensação total do título
  tit_dist <- tit_dist |>
    mutate(across(all_of(ops), ~ compensacao_total * coalesce(.x, 0)))

  # 5. Outros passes (Complemento CM e Viver Cascais) -------------------------
  zeros_ops <- as_tibble(setNames(as.list(rep(0, length(ops))), ops))

  codigos_outros <- ler_lista_compensacoes("titulos_nomes.csv") |>
    mutate(ticket_code = suppressWarnings(as.integer(ticket_code))) |>
    filter(!is.na(ticket_name_reduzido), !is.na(ticket_code)) |>
    select(linha = ticket_name_reduzido, ticket_code) |>
    distinct(linha, .keep_all = TRUE)

  r_cm <- agg |>
    filter(complemento_CM) |>
    summarise(
      ticket_operator_code       = first(na.omit(ticket_operator_code)),
      linha                      = "Complemento CM",
      ticket_modalidade_operador = NA_character_,
      passes                     = sum(passes, na.rm = TRUE),
      validacoes                 = sum(numval, na.rm = TRUE),
      compensacao_total          = sum(comp_complemento, na.rm = TRUE)
    ) |>
    left_join(codigos_outros, by = "linha") |>
    bind_cols(zeros_ops) |>
    mutate(`AT TML - CM` = compensacao_total)

  r_ca <- agg |>
    filter(viver_cascais) |>
    summarise(
      ticket_operator_code       = first(na.omit(ticket_operator_code)),
      linha                      = "Viver Cascais",
      ticket_modalidade_operador = NA_character_,
      passes                     = sum(passes, na.rm = TRUE),
      validacoes                 = sum(numval, na.rm = TRUE),
      compensacao_total          = sum(comp_cascais, na.rm = TRUE)
    ) |>
    left_join(codigos_outros, by = "linha") |>
    bind_cols(zeros_ops) |>
    mutate(`AT Cascais` = compensacao_total)

  # Totais por grupo para cálculo do congelamento
  tot_nav <- tit_dist |>
    summarise(across(all_of(ops), ~ sum(.x, na.rm = TRUE)))

  tot_out <- bind_rows(r_cm, r_ca) |>
    summarise(across(all_of(ops), ~ sum(.x, na.rm = TRUE)))

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
    summarise(across(all_of(ops), ~ round(sum(.x, na.rm = TRUE), 2))) |>
    mutate(
      linha             = "Congelamento TOTAL",
      compensacao_total = rowSums(across(all_of(ops)))
    ) |>
    select(linha, compensacao_total, all_of(ops))

  message(sprintf("  Factores de congelamento: Navegante %.6f | Outros %.6f",
                  fc$navegante, fc$outros))

  # Saídas -------------------------------------------------------------------
  cols_resumo <- c("ticket_operator_code", "ticket_code", "linha",
                   "ticket_modalidade_operador", "passes", "validacoes",
                   "compensacao_total", ops)

  # _resumo.csv: apenas títulos individuais + Complemento CM + Viver Cascais (sem TOTAL*)
  resumo <- bind_rows(
    tit_dist |> select(all_of(cols_resumo)),
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
           ticket_operator_code, ticket_code, ticket_modalidade_operador,
           preco_base, passes, numval, compensacao,
           comp_navegante, comp_complemento, comp_cascais)

  f_tit  <- file.path(paths$output, output_filename(ano, mes, "comp_por_titulo"))
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
                         ticket_modalidade_operador, passes, validacoes, compensacao_total))
  print(congelamento |> select(linha, compensacao_total))
  invisible(list(resumo = resumo, congelamento = congelamento,
                 titulos = agg_out, fatores = fc))
}
