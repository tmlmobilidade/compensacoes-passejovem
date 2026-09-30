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
nrow(comparacao_group_semNIF)#0

# Está tudo certo!
