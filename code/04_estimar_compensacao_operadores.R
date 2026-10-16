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
#      Compensacoes/code/05_reparticao_B.R) sobre a parte Navegante.
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

# Congelamento (TAT) aplicado à parte Navegante; pôr TRUE para aplicar também
# às parcelas Complemento CM e Viver Cascais.
CONGELAMENTO_TODAS_PARCELAS <- FALSE

MESES_ABREV <- c("jan","fev","mar","abr","mai","jun",
                 "jul","ago","set","out","nov","dez")


# =============================================================================
# Funções auxiliares
# =============================================================================

#' Lê uma lista do Compensacoes (local; se não existir, do GitHub).
ler_lista_compensacoes <- function(nome) {
  local <- file.path(COMPENSACOES_ROOT, "lists", nome)
  src   <- if (file.exists(local)) local else file.path(COMPENSACOES_GITHUB, nome)
  # fill = TRUE: algumas linhas de titulos_nomes.csv têm menos colunas
  fread(src, sep = ";", fill = TRUE, encoding = "UTF-8", na.strings = c("", "NA"))
}

#' Converte "2/3", "3/5" ou "1" em número.
parse_peso <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(s)) return(1)
    p <- suppressWarnings(as.numeric(strsplit(s, "/", fixed = TRUE)[[1]]))
    if (length(p) == 2) p[1] / p[2] else p[1]
  }, numeric(1), USE.NAMES = FALSE)
}

#' Caminho do ficheiro de quotas do Compensacoes para um mês.
caminho_quotas <- function(ano, mes) {
  pasta <- sprintf("%02d_%s_%d", mes, MESES_ABREV[mes], ano)
  file.path(COMPENSACOES_ROOT, "data", pasta, "output",
            sprintf("1_quotas_%s%02d.csv", MESES_ABREV[mes], ano %% 100))
}

#' Lê comNIF + semNIF, mantém só registos OK e agrega por nome_dist.
#' "OK" = registos com compensação calculada e idade elegível
#' (os fora da faixa etária já foram removidos em 02_calcular_compensacoes.R).
agregar_por_titulo <- function(paths, ano, mes) {
  cols <- c("numval", "compensacao", "nome_dist", "age_new")
  ler  <- function(sufixo) {
    f <- file.path(paths$output, output_filename(ano, mes, sufixo))
    fread(f, sep = ";", select = cols, dec = ",")
  }
  dt <- rbindlist(list(ler("comNIF"), ler("semNIF")), use.names = TRUE)

  n0 <- nrow(dt)
  dt <- dt[!is.na(compensacao) & !is.na(nome_dist) &
             !(age_new %in% 3L) & !(age_new >= 25L)]
  message(sprintf("  Registos OK: %s de %s", format(nrow(dt), big.mark = " "),
                  format(n0, big.mark = " ")))

  dt[, .(numval      = sum(numval, na.rm = TRUE),
         compensacao = sum(compensacao, na.rm = TRUE),
         passes      = .N),
     by = nome_dist]
}

#' Associa a cada nome_dist: ticket_name_reduzido, complemento_CM, base_weight,
#' ticket_code de referência (para as quotas) e preco_base.
enriquecer_titulos <- function(agg) {
  tn  <- ler_lista_compensacoes("titulos_nomes.csv")
  tit <- ler_lista_compensacoes("titulos.csv")
  tn[, base_weight_num := parse_peso(base_weight)]
  tn[, complemento_CM  := as.logical(complemento_CM)]

  # Linhas "mestre" de complemento (complemento_CM == TRUE), indexadas pelo
  # nome completo e pelo nome curto. Servem também para as variantes
  # "X+Navegante Complemento 418/sub23", cujo registo não traz o reduzido.
  mestre <- tn[complemento_CM %in% TRUE,
               .(ticket_name_reduzido, complemento_CM, base_weight_num,
                 k1 = ticket_name_completo, k2 = ticket_name_short)]

  resolver <- function(nome) {
    r <- tn[ticket_name_completo == nome][1]
    m <- mestre[k1 == nome | (!is.na(r$ticket_name_short) & k1 == r$ticket_name_short) |
                  (!is.na(r$ticket_name_short) & k2 == r$ticket_name_short)][1]
    if (!is.na(m$ticket_name_reduzido)) {
      list(ticket_name_reduzido = m$ticket_name_reduzido, complemento_CM = TRUE,
           base_weight = m$base_weight_num)
    } else {
      list(ticket_name_reduzido = r$ticket_name_reduzido,
           complemento_CM = isTRUE(r$complemento_CM),
           base_weight = if (is.na(r$base_weight_num)) 1 else r$base_weight_num)
    }
  }
  info <- rbindlist(lapply(agg$nome_dist, resolver))
  agg  <- cbind(agg, info)

  # ticket_code de referência: 1) código no próprio registo; 2) código da linha
  # cujo nome completo == reduzido; 3) titulos.csv (modalidade 418_sub23/Criança)
  cod_nome <- function(nome) tn[ticket_name_completo == nome, ticket_code][1]
  agg[, ticket_code := vapply(nome_dist, cod_nome, numeric(1))]
  agg[is.na(ticket_code) & !is.na(ticket_name_reduzido),
      ticket_code := vapply(ticket_name_reduzido, cod_nome, numeric(1))]
  mod_ok <- c("418_sub23", "Criança", "crianca")
  tit_ref <- tit[modalidade %in% mod_ok][!duplicated(titulo_nome)]
  agg[is.na(ticket_code) & !is.na(ticket_name_reduzido),
      ticket_code := tit_ref$ticket_code[match(ticket_name_reduzido, tit_ref$titulo_nome)]]

  # preco_base a partir de titulos.csv
  agg[, preco_base := tit$preco_base[match(ticket_code, tit$ticket_code)]]

  agg[, viver_cascais := grepl("^Viver Cascais", nome_dist)]
  agg[viver_cascais == TRUE, `:=`(complemento_CM = FALSE, base_weight = 1)]
  agg
}

#' Factor TAT cumulativo desde 2024 até ao ano (igual a 05_reparticao_B.R).
fator_tat <- function(ano) {
  tat <- ler_lista_compensacoes("tat.csv")
  tat[, tat := as.numeric(gsub(",", ".", tat))]
  prod(1 + tat[ano <= ..ano, tat]) - 1
}



#' Factores de congelamento (fórmulas do Excel de referência):
#'   Navegante:    (1+t2024)(1+t2025)(1+t2026) - 1
#'   Outros passes: factor Navegante - t(ano)   [a+b+ab+ac+bc+abc]
fatores_congelamento <- function(ano) {
  tt  <- as.data.frame(ler_lista_compensacoes("tat.csv"))
  v   <- as.numeric(gsub(",", ".", tt$tat))
  ft  <- prod(1 + v[tt$ano <= ano]) - 1
  list(navegante = ft, outros = ft - v[tt$ano == ano])
}

# Colunas de operadores do resumo (ordem do Excel de referência) e respectivos
# códigos no ficheiro de quotas do Compensacoes.
OPERADORES_RESUMO <- c(
  "CP" = "CP", "FT" = "FT", "TT" = "TT", "SL" = "SL", "ML" = "ML",
  "MTS" = "MS", "Carris" = "CA", "TCB" = "TB",
  "AT Cascais" = "CAP", "AT TML - CM" = "CM", "BT Oeste" = "BQ"
)


# =============================================================================
# Função principal
# =============================================================================

estimar_compensacao_operadores <- function(ano, mes, raiz = here_root()) {
  paths <- month_paths(ano, mes, raiz)
  message(sprintf("[04] Estimativa por operador %d/%02d", ano, mes))

  # 1-2. Agregado por título, com reduzido / complemento / base_weight ------
  agg <- agregar_por_titulo(paths, ano, mes)
  agg <- enriquecer_titulos(agg)

  if (any(is.na(agg$ticket_code) & !agg$viver_cascais))
    warning("Títulos sem ticket_code: ",
            paste(agg[is.na(ticket_code) & !viver_cascais, nome_dist], collapse = "; "))

  # 3. Parte Navegante / Complemento CM / Viver Cascais ---------------------
  #    Navegante     = compensacao * base_weight   (ex.: 40 de 60 = 2/3)
  #    Complemento   = compensacao * (1 - base_weight) -> 100% AT TML - CM
  #    Viver Cascais = compensacao                     -> 100% AT Cascais
  agg[, `:=`(
    comp_navegante   = fifelse(viver_cascais, 0, compensacao * base_weight),
    comp_complemento = fifelse(complemento_CM, compensacao * (1 - base_weight), 0),
    comp_cascais     = fifelse(viver_cascais, compensacao, 0),
    teto_passe       = preco_base / base_weight       # ex.: 40/(2/3)=60; Montijo 30/0.6=50
  )]
  if (any(agg$compensacao > agg$teto_passe * agg$passes + 1e-6 & !agg$viver_cascais, na.rm = TRUE))
    warning("Compensação acima do tecto (preco_base/base_weight x passes) em algum título.")

  # Linhas "Navegante": agrega por ticket de quota (passes e validações
  # contam por inteiro; a compensação é só a parte Navegante).
  tit <- agg[viver_cascais == FALSE,
             .(passes = sum(passes), validacoes = sum(numval),
               compensacao_total = sum(comp_navegante),
               titulo = nome_dist[which.max(passes)]),
             by = ticket_code][order(ticket_code)]

  # 4. Quotas do mês -----------------------------------------------------------
  f_quotas <- caminho_quotas(ano, mes)
  if (!file.exists(f_quotas)) stop("Ficheiro de quotas não encontrado: ", f_quotas)
  quotas <- fread(f_quotas, sep = ";", dec = ",", select = c("operador", "ticket_code", "quota"))
  qw <- dcast(quotas, ticket_code ~ operador, value.var = "quota", fill = 0)
  for (nm in names(OPERADORES_RESUMO)) {
    cod <- OPERADORES_RESUMO[[nm]]
    tit[, (paste0("q_", nm)) := qw[[cod]][match(ticket_code, qw$ticket_code)]]
  }
  qcols <- paste0("q_", names(OPERADORES_RESUMO))
  falta <- tit[is.na(get(qcols[1])), ticket_code]
  if (length(falta)) warning("Sem quotas para ticket_code: ", paste(falta, collapse = ", "))
  for (nm in names(OPERADORES_RESUMO))
    tit[, (nm) := compensacao_total * get(paste0("q_", nm))]

  ops <- names(OPERADORES_RESUMO)
  soma <- function(dt) dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = ops]
  linha <- function(rotulo, passes, valid, comp, dist) {
    cbind(data.table(linha = rotulo, passes = passes, validacoes = valid,
                     compensacao_total = comp), dist)
  }
  zeros <- as.data.table(setNames(as.list(rep(0, length(ops))), ops))

  # 5. Outros passes ------------------------------------------------------------
  comp_cm <- agg[complemento_CM == TRUE]
  dist_cm <- copy(zeros); dist_cm[["AT TML - CM"]] <- sum(agg$comp_complemento)
  casc    <- agg[viver_cascais == TRUE]
  dist_ca <- copy(zeros); dist_ca[["AT Cascais"]] <- sum(agg$comp_cascais)

  r_nav <- linha("TOTAL Navegante", sum(tit$passes), sum(tit$validacoes),
                 sum(tit$compensacao_total), soma(tit))
  r_cm  <- linha("Complemento CM", sum(comp_cm$passes), sum(comp_cm$numval),
                 sum(agg$comp_complemento), dist_cm)
  r_ca  <- linha("Viver Cascais", sum(casc$passes), sum(casc$numval),
                 sum(agg$comp_cascais), dist_ca)
  outros <- rbindlist(list(r_cm, r_ca))
  r_out  <- linha("TOTAL outros passes", sum(outros$passes), sum(outros$validacoes),
                  sum(outros$compensacao_total), soma(outros))
  total  <- rbindlist(list(r_nav, r_out))
  r_tot  <- linha("TOTAL", sum(total$passes), sum(total$validacoes),
                  sum(total$compensacao_total), soma(total))

  # 6. Congelamento -----------------------------------------------------------
  fc <- fatores_congelamento(ano)
  cong_nav <- r_nav[, ..ops] * fc$navegante
  cong_out <- r_out[, ..ops] * fc$outros
  r_c1 <- linha("Congelamento Navegante", NA, NA, sum(cong_nav), cong_nav)
  r_c2 <- linha("Congelamento Outros Passes", NA, NA, sum(cong_out), cong_out)
  cong_tot <- round(cong_nav + cong_out, 2)
  r_c3 <- linha("Congelamento TOTAL", NA, NA, sum(cong_tot), cong_tot)
  message(sprintf("  Factores de congelamento: Navegante %.6f | Outros %.6f",
                  fc$navegante, fc$outros))

  # Saídas -------------------------------------------------------------------
  titulos_out <- tit[, c("ticket_code", "titulo", "passes", "validacoes",
                         "compensacao_total", ops), with = FALSE]

  # _resumo.csv: apenas títulos individuais + Complemento CM + Viver Cascais (sem TOTAL*)
  resumo <- rbindlist(list(
    cbind(data.table(linha = as.character(titulos_out$titulo)),
          titulos_out[, c("passes", "validacoes", "compensacao_total", ops), with = FALSE]),
    r_cm, r_ca
  ), use.names = TRUE)
  resumo <- cbind(ano = ano, mes = mes, resumo)

  # _congelamento.csv: linhas de congelamento num CSV separado
  congelamento <- rbindlist(list(r_c1, r_c2, r_c3), use.names = TRUE)
  congelamento[, c("passes", "validacoes") := NULL]
  congelamento <- cbind(ano = ano, mes = mes, congelamento)

  agg_out <- agg[order(-compensacao),
                 .(nome_dist, ticket_name_reduzido, complemento_CM, base_weight,
                   ticket_code, preco_base, passes, numval, compensacao,
                   comp_navegante, comp_complemento, comp_cascais)]

  f_tit  <- file.path(paths$output, output_filename(ano, mes, "comp_por_titulo"))
  f_res  <- file.path(paths$output, output_filename(ano, mes, "passejovem_resumo"))
  f_cong <- file.path(paths$output, output_filename(ano, mes, "passejovem_congelamento"))

  fwrite(agg_out,      f_tit,  sep = ";", dec = ",")
  fwrite(resumo,       f_res,  sep = ";", dec = ",")
  fwrite(congelamento, f_cong, sep = ";", dec = ",")

  message(sprintf("  Gravados: %s, %s, %s",
                  basename(f_tit), basename(f_res), basename(f_cong)))

  print(resumo[, .(linha, passes, validacoes, compensacao_total)])
  print(congelamento[, .(linha, compensacao_total)])
  invisible(list(resumo = resumo, congelamento = congelamento,
                 titulos = agg_out, fatores = fc))
}
