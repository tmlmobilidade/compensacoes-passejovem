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
#   <ano>_<mm>_compensacao_por_titulo.csv   (agregado por nome_dist + valores)
#   <ano>_<mm>_passejovem_resumo.csv        (compensação por título e operador, s/ TOTAL*)
#   <ano>_<mm>_passejovem_congelamento.csv  (congelamento por operador)
#   docs/<ano>_<mm>_AnexoII_reparticao.xlsx (anexo Excel formatado com fórmulas)
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
    select(ticket_code, pvp, titulo_nome_base, preco_base,
           ticket_operator_code, ticket_modalidade_operador) |>
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
    outros    = tat_2024 * ft_apos_2024,
    tat_df    = tat_df
  )
}


# =============================================================================
# Exportação Excel (estrutura AnexoII_reparticao + estilo Anexo_Calculos)
# =============================================================================

#' Exporta o ficheiro <ano>_<mm>_AnexoII_reparticao.xlsx em output/docs/ com 2 folhas:
#'   1. <mes><aa>                      (repartição por título e operador + quotas)
#'   2. <mes><aa>_resumo_congelamento  (resumo por operador, congelamento e parâmetros TAT)
exportar_anexo_reparticao_xlsx <- function(ano, mes, path_xlsx,
                                           tit_dist, quotas_wide,
                                           r_cm, r_ca,
                                           tot_nav, tot_out,
                                           cong_nav, cong_out, cong_tot,
                                           fc) {
  ops      <- names(OPERADORES_RESUMO)
  n_ops    <- length(ops)
  sufixo   <- sprintf("%s%02d", MESES_ABREV[mes], ano %% 100)
  mes_nome <- stringr::str_to_title(MESES_PT[mes])

  # Paleta e estilos (idênticos a Compensacoes/code/08_tables.R + tons de cinzento p/ Congelamento)
  cor_gold_dark   <- "#FFC000"
  cor_gold_sub    <- "#FFE699"
  cor_gold_light  <- "#FFF2CC"
  cor_op_col      <- "#FAF6EC"
  cor_alt_row     <- "#FCFBF7"
  cor_white       <- "#FFFFFF"
  cor_darkgray    <- "#2c2c2c"
  cor_border      <- "#e5b32b"
  cor_border_soft <- "#D9D9D9"
  cor_gray_text   <- "#A6A6A6"

  # Paleta de cinzentos para diferenciar a secção de Congelamento Tarifário (TAT) na Folha 2
  cor_gray_dark   <- "#BFBFBF"
  cor_gray_sub    <- "#D9D9D9"
  cor_gray_light  <- "#EDEDED"
  cor_gray_op     <- "#F2F2F2"
  cor_gray_border <- "#808080"

  st_title <- createStyle(
    fontSize = 13, textDecoration = "bold",
    halign = "left", valign = "center",
    fgFill = cor_gold_dark, fontColour = cor_darkgray
  )
  st_sub_header <- createStyle(
    fgFill = cor_gold_sub, fontColour = cor_darkgray,
    halign = "center", valign = "center",
    textDecoration = "bold", border = "TopBottomLeftRight",
    borderColour = cor_border, fontSize = 9
  )
  st_sub_header_gray <- createStyle(
    fgFill = cor_gray_sub, fontColour = cor_darkgray,
    halign = "center", valign = "center",
    textDecoration = "bold", border = "TopBottomLeftRight",
    borderColour = cor_gray_border, fontSize = 9
  )
  st_sub_header_gray_left <- createStyle(
    fgFill = cor_gray_sub, fontColour = cor_darkgray,
    halign = "left", valign = "center",
    textDecoration = "bold", border = "TopBottomLeftRight",
    borderColour = cor_gray_border, fontSize = 9
  )
  st_header_gold <- createStyle(
    fgFill = cor_gold_dark, fontColour = cor_darkgray,
    halign = "center", valign = "center",
    textDecoration = "bold", border = "TopBottomLeftRight",
    borderColour = cor_border, wrapText = TRUE, fontSize = 9
  )
  st_header_gray <- createStyle(
    fgFill = cor_gray_dark, fontColour = cor_darkgray,
    halign = "center", valign = "center",
    textDecoration = "bold", border = "TopBottomLeftRight",
    borderColour = cor_gray_border, wrapText = TRUE, fontSize = 9
  )
  # Linhas de Total: apenas bordas horizontais (TopBottom), sem bordas verticais
  st_total_lbl <- createStyle(
    fgFill = cor_gold_light, fontColour = cor_darkgray, textDecoration = "bold",
    halign = "left", valign = "center",
    border = "TopBottom", borderColour = cor_border, fontSize = 9
  )
  st_total_cod <- createStyle(
    fgFill = cor_gold_light, fontColour = cor_darkgray, textDecoration = "bold",
    halign = "center", valign = "center",
    border = "TopBottom", borderColour = cor_border, fontSize = 9
  )
  st_total_int <- createStyle(
    fgFill = cor_gold_light, fontColour = cor_darkgray, textDecoration = "bold",
    numFmt = "#,##0", halign = "right", valign = "center",
    border = "TopBottom", borderColour = cor_border, fontSize = 9
  )
  st_total_num <- createStyle(
    fgFill = cor_gold_light, fontColour = cor_darkgray, textDecoration = "bold",
    numFmt = "#,##0.00", halign = "right", valign = "center",
    border = "TopBottom", borderColour = cor_border, fontSize = 9
  )
  st_total_num_gray <- createStyle(
    fgFill = cor_gray_light, fontColour = cor_darkgray, textDecoration = "bold",
    numFmt = "#,##0.00", halign = "right", valign = "center",
    border = "TopBottom", borderColour = cor_gray_border, fontSize = 9
  )
  st_cod <- createStyle(
    fgFill = cor_white, fontColour = cor_darkgray, halign = "center",
    border = "LeftRight", borderColour = cor_border_soft, fontSize = 9
  )
  st_desig <- createStyle(
    fgFill = cor_white, fontColour = cor_darkgray, halign = "left",
    border = "LeftRight", borderColour = cor_border_soft, fontSize = 9
  )
  st_int <- createStyle(
    fgFill = cor_white, fontColour = cor_darkgray, numFmt = "#,##0", halign = "right",
    border = "LeftRight", borderColour = cor_border_soft, fontSize = 9
  )
  st_curr <- createStyle(
    fgFill = cor_white, fontColour = cor_darkgray, numFmt = "#,##0.00", halign = "right",
    border = "LeftRight", borderColour = cor_border_soft, fontSize = 9
  )
  st_pct <- createStyle(
    fgFill = cor_white, fontColour = cor_darkgray, numFmt = "0.0000%", halign = "right",
    border = "LeftRight", borderColour = cor_border_soft, fontSize = 9
  )
  st_pct_tat <- createStyle(
    fgFill = cor_white, fontColour = cor_darkgray, numFmt = "0.00%", halign = "right",
    border = "TopBottomLeftRight", borderColour = cor_border_soft, fontSize = 9
  )
  st_meta_lbl <- createStyle(
    fgFill = cor_gray_op, fontColour = cor_darkgray, textDecoration = "bold",
    halign = "left", valign = "center",
    border = "TopBottomLeftRight", borderColour = cor_border_soft, fontSize = 9
  )
  st_meta_cod <- createStyle(
    fgFill = cor_gray_op, fontColour = cor_darkgray, textDecoration = "bold",
    halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = cor_border_soft, fontSize = 9
  )
  st_meta_txt <- createStyle(
    fgFill = cor_white, fontColour = cor_darkgray,
    halign = "left", valign = "center",
    border = "TopBottomLeftRight", borderColour = cor_border_soft, fontSize = 9
  )
  st_meta_pct <- createStyle(
    fgFill = cor_gray_light, fontColour = cor_darkgray, textDecoration = "bold",
    numFmt = "0.0000%", halign = "right", valign = "center",
    border = "TopBottomLeftRight", borderColour = cor_gray_border, fontSize = 9
  )
  st_alt  <- createStyle(fgFill = cor_alt_row)
  st_zero <- createStyle(fontColour = cor_gray_text, fontSize = 9)

  wb <- createWorkbook()

  # ===========================================================================
  # FOLHA 1: <mes><aa> (Distribuição por título e operador + Quotas)
  # ===========================================================================
  ws1 <- sufixo
  addWorksheet(wb, ws1, tabColour = cor_gold_dark)

  # Quotas não arredondadas para cada linha de título
  q_nav <- tit_dist |>
    select(ticket_code) |>
    left_join(quotas_wide, by = "ticket_code") |>
    select(all_of(ops))

  zeros_q <- as_tibble(setNames(as.list(rep(0, n_ops)), ops))
  q_cm    <- zeros_q |> mutate(`AT TML - CM` = 1)
  q_ca    <- zeros_q |> mutate(`AT Cascais`  = 1)
  q_tot   <- as_tibble(setNames(as.list(rep(NA_real_, n_ops)), ops))

  # Linhas de dados (sem arredondamento intermédio)
  base_cols <- c("ticket_operator_code", "ticket_code", "linha",
                 "ticket_modalidade_operador", "pvp", "titulo_nome_base",
                 "preco_base", "passes", "validacoes", "compensacao_total", ops)

  df_rows <- bind_rows(
    tit_dist |> select(all_of(base_cols)),
    r_cm     |> select(all_of(base_cols)),
    r_ca     |> select(all_of(base_cols))
  ) |>
    mutate(ano = as.integer(ano), mes = as.integer(mes), .before = 1)

  df_quotas <- bind_rows(q_nav, q_cm, q_ca)

  # Estrutura base das 3 linhas de Total (os valores numéricos são escritos via fórmulas Excel)
  linha_tot_base <- function(rotulo) {
    tibble(
      ano                        = as.integer(ano),
      mes                        = as.integer(mes),
      ticket_operator_code       = NA_integer_,
      ticket_code                = NA_integer_,
      linha                      = rotulo,
      ticket_modalidade_operador = NA_character_,
      pvp                        = NA_real_,
      titulo_nome_base           = NA_character_,
      preco_base                 = NA_real_,
      passes                     = NA_integer_,
      validacoes                 = NA_integer_,
      compensacao_total          = NA_real_
    ) |>
      bind_cols(q_tot)
  }

  r_tot_nav <- linha_tot_base("Total Passes Navegante")
  r_tot_out <- linha_tot_base("Total Outros Passes")
  r_tot_all <- linha_tot_base("Total")

  df_s1_left  <- bind_rows(df_rows, r_tot_nav, r_tot_out, r_tot_all)
  df_s1_right <- bind_rows(df_quotas, q_tot, q_tot, q_tot)

  # Título e sub-cabeçalhos da Folha 1
  mergeCells(wb, ws1, rows = 1, cols = 1:35)
  writeData(wb, ws1,
            x = sprintf("     Distribuição das compensações Passe Jovem pelos Operadores — %s %d", mes_nome, ano),
            startRow = 1, startCol = 1)
  addStyle(wb, ws1, st_title, rows = 1, cols = 1:35, gridExpand = TRUE)
  setRowHeights(wb, ws1, rows = 1, heights = 26)
  setRowHeights(wb, ws1, rows = 2, heights = 8)

  mergeCells(wb, ws1, rows = 3, cols = 13:23)
  writeData(wb, ws1, x = "Distribuição da compensação Passe Jovem por título e operador (€)",
            startRow = 3, startCol = 13)
  addStyle(wb, ws1, st_sub_header, rows = 3, cols = 13:23, gridExpand = TRUE)

  mergeCells(wb, ws1, rows = 3, cols = 25:35)
  writeData(wb, ws1, x = "Quota (%) por título e operador",
            startRow = 3, startCol = 25)
  addStyle(wb, ws1, st_sub_header, rows = 3, cols = 25:35, gridExpand = TRUE)
  setRowHeights(wb, ws1, rows = 3, heights = 22)

  headers_left <- c(
    "ano", "mês", "ticket_operator_code", "ticket_code", "titulo",
    "Modalidade título", "pvp", "Título Base", "Preço do título base",
    "Nº passes com validações", "Validações", "Compensação total", ops
  )
  writeData(wb, ws1, x = as.data.frame(t(headers_left)), startRow = 4, startCol = 1, colNames = FALSE)
  addStyle(wb, ws1, st_header_gold, rows = 4, cols = 1:23, gridExpand = TRUE)

  writeData(wb, ws1, x = as.data.frame(t(ops)), startRow = 4, startCol = 25, colNames = FALSE)
  addStyle(wb, ws1, st_header_gold, rows = 4, cols = 25:35, gridExpand = TRUE)
  setRowHeights(wb, ws1, rows = 4, heights = 32)

  # Escrever dados da Folha 1 (a partir da linha 5)
  writeData(wb, ws1, x = df_s1_left,  startRow = 5, startCol = 1,  colNames = FALSE)
  writeData(wb, ws1, x = df_s1_right, startRow = 5, startCol = 25, colNames = FALSE)

  n_nav_s1   <- nrow(tit_dist)
  n_data_s1  <- nrow(df_rows)
  r_data_s1  <- 5:(4 + n_data_s1)
  r_nav_s1   <- 5:(4 + n_nav_s1)
  r_out_s1   <- (5 + n_nav_s1):(4 + n_data_s1)
  even_s1    <- r_data_s1[seq_len(n_data_s1) %% 2 == 0]
  rw_tot_nav <- 5 + n_data_s1
  rw_tot_out <- 6 + n_data_s1
  rw_tot_all <- 7 + n_data_s1
  r_tots_s1  <- c(rw_tot_nav, rw_tot_out, rw_tot_all)

  # Fórmulas Excel na Folha 1:
  # 1) Nas linhas de título (cols 13:23): Compensação total ($L<row>) * Quota (<col_quota><row>)
  for (j in seq_len(n_ops)) {
    col_comp  <- 12 + j
    col_quota <- int2col(24 + j)
    f_mult    <- sprintf("$L%d*%s%d", r_data_s1, col_quota, r_data_s1)
    writeFormula(wb, ws1, x = f_mult, startCol = col_comp, startRow = 5)
  }

  # 2) Nas linhas de Total:
  #    - Total Passes Navegante e Total Outros Passes: NÃO arredondados (SUM simples)
  #    - Total (soma dos 2 subtotais): arredondado a 2 casas decimais nos valores em €
  for (col_idx in 10:23) {
    col_let <- int2col(col_idx)
    is_int  <- col_idx %in% 10:11
    f_nav <- sprintf("SUM(%s%d:%s%d)", col_let, min(r_nav_s1), col_let, max(r_nav_s1))
    f_out <- sprintf("SUM(%s%d:%s%d)", col_let, min(r_out_s1), col_let, max(r_out_s1))
    f_all <- if (is_int) {
      sprintf("SUM(%s%d:%s%d)", col_let, rw_tot_nav, col_let, rw_tot_out)
    } else {
      sprintf("ROUND(SUM(%s%d:%s%d), 2)", col_let, rw_tot_nav, col_let, rw_tot_out)
    }
    writeFormula(wb, ws1, x = f_nav, startCol = col_idx, startRow = rw_tot_nav)
    writeFormula(wb, ws1, x = f_out, startCol = col_idx, startRow = rw_tot_out)
    writeFormula(wb, ws1, x = f_all, startCol = col_idx, startRow = rw_tot_all)
  }

  # Estilos das linhas de dados (Folha 1)
  addStyle(wb, ws1, st_cod,   rows = r_data_s1, cols = c(1:4, 6),     gridExpand = TRUE)
  addStyle(wb, ws1, st_desig, rows = r_data_s1, cols = c(5, 8),       gridExpand = TRUE)
  addStyle(wb, ws1, st_curr,  rows = r_data_s1, cols = c(7, 9, 12:23), gridExpand = TRUE)
  addStyle(wb, ws1, st_int,   rows = r_data_s1, cols = 10:11,         gridExpand = TRUE)
  addStyle(wb, ws1, st_pct,   rows = r_data_s1, cols = 25:35,         gridExpand = TRUE)
  addStyle(wb, ws1, st_alt,   rows = even_s1,   cols = c(1:23, 25:35), gridExpand = TRUE, stack = TRUE)

  conditionalFormatting(wb, ws1, cols = c(7, 12:23, 25:35), rows = r_data_s1, rule = "==0", style = st_zero)

  # Estilos das linhas de Total (Folha 1)
  addStyle(wb, ws1, st_total_cod, rows = r_tots_s1, cols = 1:4,   gridExpand = TRUE)
  addStyle(wb, ws1, st_total_lbl, rows = r_tots_s1, cols = 5:9,   gridExpand = TRUE)
  addStyle(wb, ws1, st_total_int, rows = r_tots_s1, cols = 10:11, gridExpand = TRUE)
  addStyle(wb, ws1, st_total_num, rows = r_tots_s1, cols = 12:23, gridExpand = TRUE)
  conditionalFormatting(wb, ws1, cols = 12:23, rows = r_tots_s1, rule = "==0", style = st_zero)

  setColWidths(wb, ws1, cols = 1:2,   widths = 6)
  setColWidths(wb, ws1, cols = 3,     widths = 11)
  setColWidths(wb, ws1, cols = 4,     widths = 9)
  setColWidths(wb, ws1, cols = 5,     widths = 31)
  setColWidths(wb, ws1, cols = 6,     widths = 11)
  setColWidths(wb, ws1, cols = 7,     widths = 5)
  setColWidths(wb, ws1, cols = 8,     widths = 19)
  setColWidths(wb, ws1, cols = 9,     widths = 8)
  setColWidths(wb, ws1, cols = 10:11, widths = 14)
  setColWidths(wb, ws1, cols = 12,    widths = 15)
  setColWidths(wb, ws1, cols = 13:23, widths = 11)
  setColWidths(wb, ws1, cols = 24,    widths = 4)
  setColWidths(wb, ws1, cols = 25:35, widths = 11)
  freezePane(wb, ws1, firstActiveRow = 5, firstActiveCol = 6)

  # ===========================================================================
  # FOLHA 2: <mes><aa>_resumo_congelamento (Resumo por Operador e Congelamento)
  # ===========================================================================
  ws2 <- paste0(sufixo, "_resumo_congelamento")
  addWorksheet(wb, ws2, tabColour = cor_gold_sub)

  df_s2_base <- tibble(
    ano      = c(rep(as.integer(ano), n_ops), as.integer(ano)),
    mes      = c(rep(as.integer(mes), n_ops), as.integer(mes)),
    operador = c(ops, "TOTAL")
  )

  # Títulos e sub-cabeçalhos da Folha 2 (com linha 2 de separação igual à Folha 1)
  mergeCells(wb, ws2, rows = 1, cols = 1:9)
  writeData(wb, ws2,
            x = sprintf("     Distribuição das compensações Passe Jovem pelos Operadores — %s %d", mes_nome, ano),
            startRow = 1, startCol = 1)
  addStyle(wb, ws2, st_title, rows = 1, cols = 1:9, gridExpand = TRUE)
  setRowHeights(wb, ws2, rows = 1, heights = 26)
  setRowHeights(wb, ws2, rows = 2, heights = 8)

  mergeCells(wb, ws2, rows = 3, cols = 1:9)
  writeData(wb, ws2, x = "     Resumo por Operador e Congelamento (€)",
            startRow = 3, startCol = 1)
  addStyle(wb, ws2, st_sub_header, rows = 3, cols = 1:9, gridExpand = TRUE)
  setRowHeights(wb, ws2, rows = 3, heights = 20)

  mergeCells(wb, ws2, rows = 4, cols = 4:6)
  writeData(wb, ws2, x = "Compensação Passe Jovem (€)", startRow = 4, startCol = 4)
  addStyle(wb, ws2, st_sub_header, rows = 4, cols = 4:6, gridExpand = TRUE)

  mergeCells(wb, ws2, rows = 4, cols = 7:9)
  writeData(wb, ws2, x = "Compensação por Congelamento Tarifário — TAT (€)", startRow = 4, startCol = 7)
  addStyle(wb, ws2, st_sub_header_gray, rows = 4, cols = 7:9, gridExpand = TRUE)
  setRowHeights(wb, ws2, rows = 4, heights = 20)

  headers_s2 <- c("ano", "mês", "operador", "Passes Navegante", "Outros Passes",
                  "TOTAL", "Congelamento Passes Navegante", "Congelamento Outros Passes",
                  "Congelamento TOTAL")
  writeData(wb, ws2, x = as.data.frame(t(headers_s2)), startRow = 5, startCol = 1, colNames = FALSE)
  addStyle(wb, ws2, st_header_gold, rows = 5, cols = 1:6, gridExpand = TRUE)
  addStyle(wb, ws2, st_header_gray, rows = 5, cols = 7:9, gridExpand = TRUE)
  setRowHeights(wb, ws2, rows = 5, heights = 32)

  writeData(wb, ws2, x = df_s2_base, startRow = 6, startCol = 1, colNames = FALSE)

  r_data_s2 <- 6:(5 + n_ops)
  even_s2   <- r_data_s2[seq_len(n_ops) %% 2 == 0]
  rw_tot_s2 <- 6 + n_ops

  # Posições das células de Fator Cumulativo no bloco de Notas (abaixo da tabela)
  rw_meta   <- rw_tot_s2 + 2
  rw_fc_nav <- rw_meta + 2
  rw_fc_out <- rw_meta + 3

  # Fórmulas nas linhas de operadores da Folha 2 (sem arredondamento intermédio):
  #   Col D (4): =SUM(<ws1>!<col_op>5:<col_op>8)   (Passes Navegante do operador na Folha 1)
  #   Col E (5): =SUM(<ws1>!<col_op>9:<col_op>10)  (Outros Passes do operador na Folha 1)
  #   Col F (6): =D<row>+E<row>                    (TOTAL Passe Jovem do operador)
  #   Col G (7): =D<row>*$I$<rw_fc_nav>            (Congelamento Passes Navegante)
  #   Col H (8): =E<row>*$I$<rw_fc_out>            (Congelamento Outros Passes)
  #   Col I (9): =G<row>+H<row>                    (Congelamento TOTAL do operador)
  cols_op_s1 <- vapply(12 + seq_len(n_ops), int2col, character(1))
  f_s2_nav   <- sprintf("SUM(%s!%s%d:%s%d)", ws1, cols_op_s1, min(r_nav_s1), cols_op_s1, max(r_nav_s1))
  f_s2_out   <- sprintf("SUM(%s!%s%d:%s%d)", ws1, cols_op_s1, min(r_out_s1), cols_op_s1, max(r_out_s1))
  f_s2_tot   <- sprintf("D%d+E%d", r_data_s2, r_data_s2)
  f_s2_cnav  <- sprintf("D%d*$I$%d", r_data_s2, rw_fc_nav)
  f_s2_cout  <- sprintf("E%d*$I$%d", r_data_s2, rw_fc_out)
  f_s2_ctot  <- sprintf("G%d+H%d", r_data_s2, r_data_s2)

  writeFormula(wb, ws2, x = f_s2_nav,  startCol = 4, startRow = 6)
  writeFormula(wb, ws2, x = f_s2_out,  startCol = 5, startRow = 6)
  writeFormula(wb, ws2, x = f_s2_tot,  startCol = 6, startRow = 6)
  writeFormula(wb, ws2, x = f_s2_cnav, startCol = 7, startRow = 6)
  writeFormula(wb, ws2, x = f_s2_cout, startCol = 8, startRow = 6)
  writeFormula(wb, ws2, x = f_s2_ctot, startCol = 9, startRow = 6)

  # Fórmulas na linha TOTAL da Folha 2 (arredondamento apenas na linha de total)
  for (col_idx in 4:9) {
    col_let <- int2col(col_idx)
    f_tot_col <- sprintf("ROUND(SUM(%s%d:%s%d), 2)", col_let, min(r_data_s2), col_let, max(r_data_s2))
    writeFormula(wb, ws2, x = f_tot_col, startCol = col_idx, startRow = rw_tot_s2)
  }

  st_op_bold <- createStyle(
    fgFill = cor_white, fontColour = cor_darkgray, textDecoration = "bold",
    halign = "left", border = "LeftRight", borderColour = cor_border_soft, fontSize = 9
  )

  addStyle(wb, ws2, st_cod,     rows = r_data_s2, cols = 1:2, gridExpand = TRUE)
  addStyle(wb, ws2, st_op_bold, rows = r_data_s2, cols = 3,   gridExpand = TRUE)
  addStyle(wb, ws2, st_curr,    rows = r_data_s2, cols = 4:9, gridExpand = TRUE)
  addStyle(wb, ws2, st_alt,     rows = even_s2,   cols = 1:9, gridExpand = TRUE, stack = TRUE)

  addStyle(wb, ws2, st_total_cod,      rows = rw_tot_s2, cols = 1:2, gridExpand = TRUE)
  addStyle(wb, ws2, st_total_lbl,      rows = rw_tot_s2, cols = 3,   gridExpand = TRUE)
  addStyle(wb, ws2, st_total_num,      rows = rw_tot_s2, cols = 4:6, gridExpand = TRUE)
  addStyle(wb, ws2, st_total_num_gray, rows = rw_tot_s2, cols = 7:9, gridExpand = TRUE)

  conditionalFormatting(wb, ws2, cols = 4:9, rows = c(r_data_s2, rw_tot_s2), rule = "==0", style = st_zero)

  # ---------------------------------------------------------------------------
  # Bloco de Notas / Parâmetros de Cálculo do Congelamento Tarifário (TAT)
  # ---------------------------------------------------------------------------
  setRowHeights(wb, ws2, rows = rw_tot_s2 + 1, heights = 10)

  mergeCells(wb, ws2, rows = rw_meta, cols = 1:9)
  writeData(wb, ws2,
            x = "     Notas — Taxas de Atualização Tarifária (TAT) e Fatores de Congelamento Aplicados",
            startRow = rw_meta, startCol = 1)
  addStyle(wb, ws2, st_sub_header_gray_left, rows = rw_meta, cols = 1:9, gridExpand = TRUE)
  setRowHeights(wb, ws2, rows = rw_meta, heights = 20)

  # Cabeçalhos das mini-tabelas de parâmetros (linha rw_meta + 1)
  rw_mh <- rw_meta + 1
  mergeCells(wb, ws2, rows = rw_mh, cols = 1:2)
  writeData(wb, ws2, x = "Ano", startRow = rw_mh, startCol = 1)
  writeData(wb, ws2, x = "Taxa TAT (%)", startRow = rw_mh, startCol = 3)
  addStyle(wb, ws2, st_header_gray, rows = rw_mh, cols = 1:3, gridExpand = TRUE)

  mergeCells(wb, ws2, rows = rw_mh, cols = 4:5)
  writeData(wb, ws2, x = "Componente de Congelamento", startRow = rw_mh, startCol = 4)
  mergeCells(wb, ws2, rows = rw_mh, cols = 6:8)
  writeData(wb, ws2, x = "Fórmula de Cálculo", startRow = rw_mh, startCol = 6)
  writeData(wb, ws2, x = "Fator Cumulativo", startRow = rw_mh, startCol = 9)
  addStyle(wb, ws2, st_header_gray, rows = rw_mh, cols = 4:9, gridExpand = TRUE)
  setRowHeights(wb, ws2, rows = rw_mh, heights = 22)

  # Tabela esquerda: taxas anuais de TAT (2024..ano)
  tat_df <- fc$tat_df |> arrange(ano)
  n_tat  <- nrow(tat_df)
  for (k in seq_len(n_tat)) {
    rw_k <- rw_mh + k
    mergeCells(wb, ws2, rows = rw_k, cols = 1:2)
    writeData(wb, ws2, x = as.integer(tat_df$ano[k]), startRow = rw_k, startCol = 1)
    writeData(wb, ws2, x = tat_df$tat[k], startRow = rw_k, startCol = 3)
    addStyle(wb, ws2, st_meta_cod, rows = rw_k, cols = 1:2, gridExpand = TRUE)
    addStyle(wb, ws2, st_pct_tat,  rows = rw_k, cols = 3,   gridExpand = TRUE)
  }

  # Tabela direita: fórmulas e fatores cumulativos (em rw_fc_nav e rw_fc_out)
  r_tat_cells <- sprintf("C%d", rw_mh + seq_len(n_tat))
  expr_prod_all  <- paste(sprintf("(1+%s)", r_tat_cells), collapse = "*")
  expr_prod_apos <- if (n_tat > 1) paste(sprintf("(1+%s)", r_tat_cells[-1]), collapse = "*") else "1"

  txt_f_nav <- paste0(paste(sprintf("(1 + TAT %d)", tat_df$ano), collapse = " × "), " - 1")
  txt_f_out <- paste0(
    sprintf("TAT %d", tat_df$ano[1]),
    if (n_tat > 1) paste0(" × ", paste(sprintf("(1 + TAT %d)", tat_df$ano[-1]), collapse = " × ")) else ""
  )

  # Linha 1 de fator: Passes Navegante
  mergeCells(wb, ws2, rows = rw_fc_nav, cols = 4:5)
  writeData(wb, ws2, x = "Congelamento Passes Navegante", startRow = rw_fc_nav, startCol = 4)
  mergeCells(wb, ws2, rows = rw_fc_nav, cols = 6:8)
  writeData(wb, ws2, x = txt_f_nav, startRow = rw_fc_nav, startCol = 6)
  writeFormula(wb, ws2, x = paste0(expr_prod_all, "-1"), startCol = 9, startRow = rw_fc_nav)
  addStyle(wb, ws2, st_meta_lbl, rows = rw_fc_nav, cols = 4:5, gridExpand = TRUE)
  addStyle(wb, ws2, st_meta_txt, rows = rw_fc_nav, cols = 6:8, gridExpand = TRUE)
  addStyle(wb, ws2, st_meta_pct, rows = rw_fc_nav, cols = 9,   gridExpand = TRUE)

  # Linha 2 de fator: Outros Passes
  mergeCells(wb, ws2, rows = rw_fc_out, cols = 4:5)
  writeData(wb, ws2, x = "Congelamento Outros Passes", startRow = rw_fc_out, startCol = 4)
  mergeCells(wb, ws2, rows = rw_fc_out, cols = 6:8)
  writeData(wb, ws2, x = txt_f_out, startRow = rw_fc_out, startCol = 6)
  writeFormula(wb, ws2, x = paste0(r_tat_cells[1], "*", expr_prod_apos), startCol = 9, startRow = rw_fc_out)
  addStyle(wb, ws2, st_meta_lbl, rows = rw_fc_out, cols = 4:5, gridExpand = TRUE)
  addStyle(wb, ws2, st_meta_txt, rows = rw_fc_out, cols = 6:8, gridExpand = TRUE)
  addStyle(wb, ws2, st_meta_pct, rows = rw_fc_out, cols = 9,   gridExpand = TRUE)

  setColWidths(wb, ws2, cols = 1:2, widths = 6)
  setColWidths(wb, ws2, cols = 3,   widths = 15)
  setColWidths(wb, ws2, cols = 4:9, widths = 18)
  freezePane(wb, ws2, firstActiveRow = 6, firstActiveCol = 4)

  dir.create(dirname(path_xlsx), recursive = TRUE, showWarnings = FALSE)
  tryCatch(
    saveWorkbook(wb, path_xlsx, overwrite = TRUE),
    error = function(e) {
      warning(sprintf("Não foi possível gravar '%s' (estará aberto no Excel?): %s",
                      basename(path_xlsx), conditionMessage(e)), call. = FALSE)
    }
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
    group_by(ticket_operator_code, ticket_code, ticket_modalidade_operador,
             pvp, titulo_nome_base, preco_base) |>
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

  # Multiplicar cada quota pela compensação total do título (sem arredondar)
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
      pvp                        = 0,
      titulo_nome_base           = NA_character_,
      preco_base                 = first(na.omit(preco_base * (1 - base_weight) / base_weight)),
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
      pvp                        = 0,
      titulo_nome_base           = NA_character_,
      preco_base                 = PRECO_CASCAIS,
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

  f_tit  <- file.path(paths$output, output_filename(ano, mes, "compensacao_por_titulo"))
  f_res  <- file.path(paths$output, output_filename(ano, mes, "passejovem_resumo"))
  f_cong <- file.path(paths$output, output_filename(ano, mes, "passejovem_congelamento"))
  dir.create(paths$docs, recursive = TRUE, showWarnings = FALSE)
  f_xlsx <- file.path(paths$docs, output_filename(ano, mes, "AnexoII_reparticao", "xlsx"))

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

  exportar_anexo_reparticao_xlsx(
    ano = ano, mes = mes, path_xlsx = f_xlsx,
    tit_dist = tit_dist, quotas_wide = quotas_wide,
    r_cm = r_cm, r_ca = r_ca,
    tot_nav = tot_nav, tot_out = tot_out,
    cong_nav = cong_nav, cong_out = cong_out, cong_tot = cong_tot,
    fc = fc
  )

  message(sprintf("  Gravados: %s, %s, %s, docs/%s",
                  basename(f_tit), basename(f_res), basename(f_cong), basename(f_xlsx)))

  print(resumo |> select(ticket_operator_code, ticket_code, linha,
                         ticket_modalidade_operador, passes, validacoes, compensacao_total))
  print(congelamento |> select(linha, compensacao_total))
  invisible(list(resumo = resumo, congelamento = congelamento,
                 titulos = agg_out, fatores = fc, xlsx = f_xlsx))
}
