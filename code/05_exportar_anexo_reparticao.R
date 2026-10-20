# =============================================================================
# 05_exportar_anexo_reparticao.R
# Exporta o ficheiro Excel de repartição de compensações por operador e
# congelamento tarifário (TAT), a partir dos CSVs gerados por
# 04_estimar_compensacao_operadores.R.
#
# Produz em data/<ano>_<mm>_<mes>/output/docs/:
#   <ano>_<mm>_AnexoII_reparticao.xlsx
#     - Folha 1: <mes><aa>                      (repartição por título/operador + quotas)
#     - Folha 2: <mes><aa>_resumo_congelamento  (resumo por operador, congelamento e TAT)
#
# Uso:
#   source("code/05_exportar_anexo_reparticao.R")
#   exportar_anexo_reparticao(ano = 2026, mes = 7)
# =============================================================================

source(file.path(dirname(sys.frame(1)$ofile), "00_config.R"),
       local = TRUE, chdir = FALSE)


#' Exporta o workbook <ano>_<mm>_AnexoII_reparticao.xlsx em output/docs/.
#'
#' @param ano  Ano de referência (ex.: 2026)
#' @param mes  Mês de referência (1–12)
#' @param raiz Raiz do projecto PasseJovem
exportar_anexo_reparticao <- function(ano, mes, raiz = here_root()) {
  paths    <- month_paths(ano, mes, raiz)
  ops      <- names(OPERADORES_RESUMO)
  n_ops    <- length(ops)
  sufixo   <- sprintf("%s%02d", MESES_ABREV[mes], ano %% 100)
  mes_nome <- stringr::str_to_title(MESES_PT[mes])

  message(sprintf("[05] Exportar Anexo II Repartição %d/%02d", ano, mes))

  # ---- 1. Ler inputs (resumo CSV, títulos, quotas e TAT) --------------------
  f_res <- file.path(paths$output, output_filename(ano, mes, "passejovem_resumo"))
  if (!file.exists(f_res)) {
    stop("Ficheiro de resumo não encontrado (corra primeiro 04_estimar_compensacao_operadores.R): ", f_res)
  }

  resumo <- fread(f_res, sep = ";", dec = ",", encoding = "UTF-8", na.strings = c("", "NA")) |>
    as_tibble()

  info_tit <- ler_lista_compensacoes("titulos.csv") |>
    mutate(
      ticket_code = as.integer(ticket_code),
      pvp         = suppressWarnings(as.numeric(str_replace(pvp, ",", "."))),
      preco_base  = suppressWarnings(as.numeric(str_replace(preco_base, ",", ".")))
    ) |>
    select(ticket_code, pvp, titulo_nome_base, preco_base) |>
    distinct(ticket_code, .keep_all = TRUE)

  f_quotas <- caminho_quotas(ano, mes)
  if (!file.exists(f_quotas)) stop("Ficheiro de quotas não encontrado: ", f_quotas)

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

  tat_df <- ler_lista_compensacoes("tat.csv") |>
    mutate(tat = as.numeric(str_replace(tat, ",", "."))) |>
    filter(ano <= !!ano) |>
    arrange(ano)

  # ---- 2. Preparar tabelas da Folha 1 (dados + quotas não arredondadas) -----
  df_rows <- resumo |>
    left_join(info_tit, by = "ticket_code") |>
    mutate(
      eh_outro = linha %in% c("Complemento CM", "Viver Cascais"),
      pvp      = coalesce(pvp, 0),
      preco_base = case_when(
        !is.na(preco_base)          ~ preco_base,
        linha == "Viver Cascais"    ~ PRECO_CASCAIS,
        linha == "Complemento CM"   ~ 20,
        TRUE                        ~ NA_real_
      )
    ) |>
    select(
      ano, mes, ticket_operator_code, ticket_code, linha,
      ticket_modalidade_operador, pvp, titulo_nome_base,
      preco_base, passes, validacoes, compensacao_total, all_of(ops), eh_outro
    )

  n_nav_s1  <- sum(!df_rows$eh_outro)
  n_data_s1 <- nrow(df_rows)

  df_rows_nav <- df_rows |> filter(!eh_outro) |> select(-eh_outro)
  df_rows_out <- df_rows |> filter(eh_outro)  |> select(-eh_outro)
  df_rows     <- bind_rows(df_rows_nav, df_rows_out)

  q_nav <- df_rows_nav |>
    select(ticket_code) |>
    left_join(quotas_wide, by = "ticket_code") |>
    select(all_of(ops))

  zeros_q <- as_tibble(setNames(as.list(rep(0, n_ops)), ops))
  q_out <- df_rows_out |>
    select(linha) |>
    rowwise() |>
    reframe(
      if (linha == "Complemento CM") {
        zeros_q |> mutate(`AT TML - CM` = 1)
      } else if (linha == "Viver Cascais") {
        zeros_q |> mutate(`AT Cascais` = 1)
      } else {
        zeros_q
      }
    )

  q_tot     <- as_tibble(setNames(as.list(rep(NA_real_, n_ops)), ops))
  df_quotas <- bind_rows(q_nav, q_out)

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

  # ---- 3. Paleta de cores e estilos -----------------------------------------
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

  # Paleta de cinzentos para a secção de Congelamento Tarifário (TAT) na Folha 2
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
  writeData(wb, ws2, x = "Compensação por Congelamento Tarifário - TAT (€)", startRow = 4, startCol = 7)
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

  # Fórmulas nas linhas de operadores da Folha 2 (sem arredondamento intermédio)
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
            x = "     Notas - Taxas de Atualização Tarifária (TAT) e Fatores de Congelamento Aplicados",
            startRow = rw_meta, startCol = 1)
  addStyle(wb, ws2, st_sub_header_gray_left, rows = rw_meta, cols = 1:9, gridExpand = TRUE)
  setRowHeights(wb, ws2, rows = rw_meta, heights = 20)

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
  n_tat <- nrow(tat_df)
  for (k in seq_len(n_tat)) {
    rw_k <- rw_mh + k
    mergeCells(wb, ws2, rows = rw_k, cols = 1:2)
    writeData(wb, ws2, x = as.integer(tat_df$ano[k]), startRow = rw_k, startCol = 1)
    writeData(wb, ws2, x = tat_df$tat[k], startRow = rw_k, startCol = 3)
    addStyle(wb, ws2, st_meta_cod, rows = rw_k, cols = 1:2, gridExpand = TRUE)
    addStyle(wb, ws2, st_pct_tat,  rows = rw_k, cols = 3,   gridExpand = TRUE)
  }

  # Tabela direita: fórmulas e fatores cumulativos (em rw_fc_nav e rw_fc_out)
  r_tat_cells    <- sprintf("C%d", rw_mh + seq_len(n_tat))
  expr_prod_all  <- paste(sprintf("(1+%s)", r_tat_cells), collapse = "*")
  expr_prod_apos <- if (n_tat > 1) paste(sprintf("(1+%s)", r_tat_cells[-1]), collapse = "*") else "1"

  txt_f_nav <- paste0(paste(sprintf("(1 + TAT %d)", tat_df$ano), collapse = " × "), " - 1")
  txt_f_out <- paste0(
    sprintf("TAT %d", tat_df$ano[1]),
    if (n_tat > 1) paste0(" × ", paste(sprintf("(1 + TAT %d)", tat_df$ano[-1]), collapse = " × ")) else ""
  )

  mergeCells(wb, ws2, rows = rw_fc_nav, cols = 4:5)
  writeData(wb, ws2, x = "Congelamento Passes Navegante", startRow = rw_fc_nav, startCol = 4)
  mergeCells(wb, ws2, rows = rw_fc_nav, cols = 6:8)
  writeData(wb, ws2, x = txt_f_nav, startRow = rw_fc_nav, startCol = 6)
  writeFormula(wb, ws2, x = paste0(expr_prod_all, "-1"), startCol = 9, startRow = rw_fc_nav)
  addStyle(wb, ws2, st_meta_lbl, rows = rw_fc_nav, cols = 4:5, gridExpand = TRUE)
  addStyle(wb, ws2, st_meta_txt, rows = rw_fc_nav, cols = 6:8, gridExpand = TRUE)
  addStyle(wb, ws2, st_meta_pct, rows = rw_fc_nav, cols = 9,   gridExpand = TRUE)

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

  dir.create(paths$docs, recursive = TRUE, showWarnings = FALSE)
  path_xlsx <- file.path(paths$docs, output_filename(ano, mes, "AnexoII_reparticao", "xlsx"))

  tryCatch({
    saveWorkbook(wb, path_xlsx, overwrite = TRUE)
    message(sprintf("  Gravado: docs/%s", basename(path_xlsx)))
  }, error = function(e) {
    warning(sprintf("Não foi possível gravar '%s' (estará aberto no Excel?): %s",
                    basename(path_xlsx), conditionMessage(e)), call. = FALSE)
  })

  invisible(path_xlsx)
}
