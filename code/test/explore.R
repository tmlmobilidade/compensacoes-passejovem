library(tidyverse)


# load data ---------------------------------------------------------------


julho_comNIF <- read_delim("data/2026_07_julho/output/2026_07_comNIF.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
julho_semNIF <- read_delim("data/2026_07_julho/output/2026_07_semNIF.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
julho_age_nok <- read_delim("data/2026_07_julho/output/2026_07_age_nok.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE) |> select(-1) |> filter(!is.na(year_dist))


# explore -----------------------------------------------------------------

julho_comNIF_dn4 = julho_comNIF |> filter(age_new <= 4)
julho_comNIF_dn24 = julho_comNIF |> filter(age_new >= 24)
# julho_comNIF_dn24 = julho_comNIF_dn24 |> filter(year(birth_date_new)==2001)

# table(julho_comNIF$ticket_operator_code_new)
table(julho_comNIF$emissor)
# AT AML AT Cascais 
# 404193        559 

table(julho_comNIF$nome_imt)
# Navegante Metropolitano      Navegante Metropolitano + Complemento                         Viver Cascais 
# 404087  (404087)                                 106 (106)                                               559 (559) 


sum(julho_comNIF$compensacao) #10725984 (TEM DE DAR 10725984)
sum(julho_semNIF$compensacao) #46772 (TEM DE DAR 46772)
sum(julho_comNIF$compensacao) + sum(julho_semNIF$compensacao) #10772756 (TEM DE DAR 10772756)




# compare -----------------------------------------------------------------
library(readxl)
library(tidyverse)
ANITA_comNIF <- read_excel("Exemplos/2026_07_julho/07_2026_comNIF_AR.xlsx", 
                           sheet = "07_2026") |> select(-1) # remove row.numbers
julho_comNIF <- read_delim("data/2026_07_julho/output/2026_07_comNIF.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE) 

# separar os nok
ANITA_nok = ANITA_comNIF |> filter(`ok/NOK corrigido` != "ok")
ANITA_comNIF = ANITA_comNIF |> filter(`ok/NOK corrigido` == "ok")

ANITA_comNIF_redux = ANITA_comNIF |> select(fiscal_number, age_new, age_calculada, birth_date_corrigida, birth_date_enviada) |> arrange(birth_date_corrigida)
julho_comNIF_redux = julho_comNIF |> select(fiscal_number, age_new, birth_date_new) |> arrange(birth_date_new)

missing = ANITA_comNIF_redux |> filter(!fiscal_number %in% c(julho_comNIF_redux$fiscal_number))
nrow(missing)# 0 

ANITA_semNIF <- read_excel("Exemplos/2026_07_julho/07_2026_semNIF_AR.xlsx", 
                           sheet = "07_2026") |> select(-1) # remove row.numbers
julho_semNIF <- read_delim("data/2026_07_julho/output/2026_07_semNIF.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE) 
# separar os nok
ANITA_nok = ANITA_nok |> bind_rows(ANITA_semNIF |> filter(`ok/NOK` != "ok"))
ANITA_semNIF = ANITA_semNIF |> filter(`ok/NOK` == "ok")


# valores -----------------------------------------------------------------


# comNIF
julho_comNIF_group = julho_comNIF |>
  group_by(nome_imt, ticket_code_new, ticket_name_new) |> 
  summarise(passes = n(),
            validacoes = sum(numval),
            compensacao = sum(compensacao)) |> 
  ungroup()

ANITA_comNIF_group = ANITA_comNIF |> 
  group_by(nome_imt, ticket_code_new, ticket_name_new) |> 
  summarise(passes = n(),
            validacoes = sum(numval),
            compensacao = sum(compensacao)) |> 
  ungroup()

comparacao_group_comNIF = ANITA_comNIF_group |>
  left_join(julho_comNIF_group, by = c("nome_imt", "ticket_code_new", "ticket_name_new")) |> 
  rename_with(~ str_replace(.x, "\\.x$", "_original"), ends_with(".x")) %>%
  rename_with(~ str_replace(.x, "\\.y$", "_rosa"), ends_with(".y")) |> 
  mutate(dif_passes = passes_rosa - passes_original,
         dif_validacoes = validacoes_rosa - validacoes_original,
         dif_compensacao = compensacao_rosa - compensacao_original) |> 
  filter(dif_passes != 0 | dif_validacoes != 0 | dif_compensacao != 0)
nrow(comparacao_group_comNIF)#0

# semNIF

julho_semNIF_group = julho_semNIF |>
  group_by(nome_imt, ticket_code_new, ticket_name_new) |> 
  summarise(passes = n(),
            validacoes = sum(numval),
            compensacao = sum(compensacao)) |> 
  ungroup()

ANITA_semNIF_group = ANITA_semNIF |> 
  group_by(nome_imt, ticket_code_new, ticket_name_new) |> 
  summarise(passes = n(),
            validacoes = sum(numval),
            compensacao = sum(compensacao)) |> 
  ungroup()

comparacao_group_semNIF = ANITA_semNIF_group |>
  left_join(julho_semNIF_group, by = c("nome_imt", "ticket_code_new", "ticket_name_new")) |> 
  rename_with(~ str_replace(.x, "\\.x$", "_original"), ends_with(".x")) %>%
  rename_with(~ str_replace(.x, "\\.y$", "_rosa"), ends_with(".y")) |> 
  mutate(dif_passes = passes_rosa - passes_original,
         dif_validacoes = validacoes_rosa - validacoes_original,
         dif_compensacao = compensacao_rosa - compensacao_original) |> 
  filter(dif_passes != 0 | dif_validacoes != 0 | dif_compensacao != 0)
nrow(comparacao_group_semNIF)
# Diferenças esperadas face à Anita (2 titulares com 2 cartões fundidos pelo mesmo documento):
# - PA515908 (Passaporte): 2 cartões (12 + 28 = 40 validações, 40 € compensação)
# - U0253488 (Passaporte): 2 cartões (1 + 56 = 57 validações, 40 € compensação)
# Total de passes semNIF passa de 2207 para 2205; compensação passa de 46772 € para 46746 €.

# Documentos estranhos e divergências detetadas ----------------------------
julho_docs_estranhos <- read_delim("data/2026_07_julho/output/2026_07_documentos_estranhos.csv",
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
table(julho_docs_estranhos$motivo)
# Casos como 00000, 000000, PCO39594 e 85205 mantêm-se isolados por cartão
# e não foram fundidos erroneamente entre diferentes pessoas.


# explore sub23+1 -----------------------

library(tidyverse)
library(readr)

# 1. Read the verification list and filter age == 24
dt_ver <- read_delim("data/2026_07_julho/output/2026_07_a_verificar.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE) |> 
  mutate(age = as.integer(age)) |>
  filter(age == 24L)

# 2. Read the exported SQL CSV (adjust path/sep if needed)
dt_sql <- read_delim("data/2026_07_julho/input/idades23-1_F_AUX_PROFILE_CARDS_202610261550.csv",
                     delim = ",", escape_double = FALSE, trim_ws = TRUE) |>
  rename_with(tolower)

# Keep one summary row per card_serial_number from SQL
dt_sql_cards <- dt_sql |>
  group_by(serial_number, fiscal_number) |>
  summarise(
    card_profile_id = paste(unique(card_profile_id), collapse = "/"),
    card_date  = max(card_date, na.rm = TRUE),
    .groups    = "drop"
  ) #why 20 were dropped?

# 3. Compare: check if age == 24 cards are in the SQL list
dt_comp <- dt_ver |>
  left_join(dt_sql_cards, by = c("card_serial_number" = "serial_number")) |>
  mutate(in_sql_127_130 = !is.na(card_profile_id))

dt_comp_nif <- dt_ver |>
  left_join(dt_sql_cards, by = "fiscal_number") |>
  mutate(in_sql_127_130 = !is.na(card_profile_id))

# Summary count (In list vs. Not in list)
table(dt_comp$in_sql_127_130)
table(dt_comp_nif$in_sql_127_130)

dt_comp$card_serial_number[dt_comp$in_sql_127_130 == FALSE]





# check odd validation cases ----------------------------------------------

# 1 NIF com mais que um cartao, com mais de 200 validações
julho_all = bind_rows(
  julho_comNIF |> select(fiscal_number, numval, cardserialnumber_new),
  julho_semNIF |> select(doc_number, numval, cardserialnumber_new) |> mutate(cardserialnumber_new = as.character(cardserialnumber_new))
) |>
  arrange(-numval) |> 
  filter(cardserialnumber_new |> str_detect("_"))

hist(julho_all$numval)

julho_all_top10 = julho_all |> slice_head(n = 10)
julho_all_mais200 = julho_all |> filter(numval >=200) |> select(-doc_number)

write.csv(julho_all_mais200, "data/2026_07_julho/output/2026_07_validacoes_mais200.csv", row.names = FALSE)


# agosto

agosto_comNIF = read_delim("data/2026_08_agosto/output/2026_08_comNIF.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE) 

agosto_all = bind_rows(
  agosto_comNIF |> select(fiscal_number, numval, cardserialnumber_new),
  agosto_semNIF |> select(doc_number, numval, cardserialnumber_new) |> mutate(cardserialnumber_new = as.character(cardserialnumber_new))
) |>
  arrange(-numval) |> 
  filter(cardserialnumber_new |> str_detect("_"))

hist(agosto_all$numval)

agosto_all_top10 = agosto_all |> slice_head(n = 10)
agosto_all_mais200 = agosto_all |> filter(numval >=200) |> select(-doc_number)

write.csv(agosto_all_mais200, "data/2026_08_agosto/output/2026_08_validacoes_mais200.csv", row.names = FALSE)

