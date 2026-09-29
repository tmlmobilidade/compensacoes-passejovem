# =============================================================================
# tests/verificar_replicacao.R
# Script de validação: compara os resultados gerados em data/
# com os ficheiros de referência em Exemplos/ para os meses disponíveis.
#
# Nota: as correções manuais em data/auxiliar/ (nif_correct.csv e
# data_nascimento_correct.csv) podem causar diferenças de ±1-2 registos
# face aos ficheiros de referência originais, que foram gerados sem correções.
# Isto é esperado e correcto.
#
# Uso:
#   Rscript tests/verificar_replicacao.R
# =============================================================================

suppressPackageStartupMessages(library(data.table))


verificar_mes <- function(ano, mes, nome_mes_exemplo, prefixo_exemplo) {
  cat("\n============================================================\n")
  cat(sprintf("  A VERIFICAR: %d / %02d (%s)\n", ano, mes, nome_mes_exemplo))
  cat("============================================================\n")

  pasta_ex  <- sprintf("Exemplos/%d_%02d_%s", ano, mes, nome_mes_exemplo)
  pasta_our <- sprintf("data/%d_%02d_%s/output", ano, mes, nome_mes_exemplo)

  f_ex_com  <- file.path(pasta_ex, sprintf("%s_%d_comNIF.csv", prefixo_exemplo, ano))
  f_our_com <- file.path(pasta_our, sprintf("%d_%02d_comNIF.csv", ano, mes))
  f_ex_sem  <- file.path(pasta_ex, sprintf("%s_%d_semNIF.csv", prefixo_exemplo, ano))
  f_our_sem <- file.path(pasta_our, sprintf("%d_%02d_semNIF.csv", ano, mes))

  # Verificar se os ficheiros existem
  if (!file.exists(f_our_com)) {
    cat(sprintf("  AVISO: ficheiro comNIF nao encontrado (%s)\n", f_our_com))
    return(FALSE)
  }

  dt_ex_com  <- fread(f_ex_com,  sep = ";", dec = ".")
  dt_our_com <- fread(f_our_com, sep = ";", dec = ".")
  dt_ex_sem  <- fread(f_ex_sem,  sep = ";", dec = ".")
  dt_our_sem <- fread(f_our_sem, sep = ";", dec = ".")

  # --- comNIF ---
  diff_rows_com <- nrow(dt_our_com) - nrow(dt_ex_com)
  diff_comp_com <- sum(dt_our_com$compensacao) - sum(dt_ex_com$compensacao)
  diff_val_com  <- sum(dt_our_com$numval)      - sum(dt_ex_com$numval)

  cat("--- comNIF ---\n")
  cat(sprintf("  Registos:    Referencia = %7d | Gerado = %7d | Diff = %+d\n",
              nrow(dt_ex_com), nrow(dt_our_com), diff_rows_com))
  cat(sprintf("  Compensacao: Referencia = %12.2f EUR | Gerado = %12.2f EUR | Diff = %+.2f EUR\n",
              sum(dt_ex_com$compensacao), sum(dt_our_com$compensacao), diff_comp_com))
  cat(sprintf("  Validacoes:  Referencia = %7d | Gerado = %7d | Diff = %+d\n",
              sum(dt_ex_com$numval), sum(dt_our_com$numval), diff_val_com))

  # --- semNIF ---
  diff_rows_sem <- nrow(dt_our_sem) - nrow(dt_ex_sem)
  diff_comp_sem <- sum(dt_our_sem$compensacao) - sum(dt_ex_sem$compensacao)
  diff_val_sem  <- sum(dt_our_sem$numval)      - sum(dt_ex_sem$numval)

  cat("\n--- semNIF ---\n")
  cat(sprintf("  Registos:    Referencia = %7d | Gerado = %7d | Diff = %+d\n",
              nrow(dt_ex_sem), nrow(dt_our_sem), diff_rows_sem))
  cat(sprintf("  Compensacao: Referencia = %12.2f EUR | Gerado = %12.2f EUR | Diff = %+.2f EUR\n",
              sum(dt_ex_sem$compensacao), sum(dt_our_sem$compensacao), diff_comp_sem))
  cat(sprintf("  Validacoes:  Referencia = %7d | Gerado = %7d | Diff = %+d\n",
              sum(dt_ex_sem$numval), sum(dt_our_sem$numval), diff_val_sem))

  # Consideramos OK se a diferenca total (comNIF + semNIF) em compensacao
  # for inferior a 200 EUR e o numero de registos diferir em menos de 5.
  # (pequenas diferenças são esperadas com correções manuais aplicadas)
  total_diff_comp <- abs(diff_comp_com) + abs(diff_comp_sem)
  total_diff_rows <- abs(diff_rows_com) + abs(diff_rows_sem)
  ok <- total_diff_comp < 200 && total_diff_rows < 5

  if (ok) {
    cat(sprintf("\n  RESULTADO: OK para %d/%02d (dif. <200 EUR e <5 registos)\n", ano, mes))
  } else {
    cat(sprintf("\n  RESULTADO: DISCREPANCIAS SIGNIFICATIVAS em %d/%02d [FALHOU]\n", ano, mes))
  }
  ok
}


# Verificar os meses com dados de referência disponíveis
ok_mai <- verificar_mes(2026, 5, "maio",  "05")
ok_jun <- verificar_mes(2026, 6, "junho", "06")

cat("\n============================================================\n")
if (ok_mai && ok_jun) {
  cat("  SUCESSO: Todos os meses dentro da margem de tolerancia.\n")
  cat("  (Diferenças minimas esperadas devido a correcoes manuais)\n")
} else {
  cat("  ATENCAO: Alguns meses com discrepancias significativas.\n")
}
cat("============================================================\n\n")
