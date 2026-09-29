library(tidyverse)


# load data ---------------------------------------------------------------


julho_comNIF <- read_delim("data/2026_07_julho/output/2026_07_comNIF.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE) |> select(-1) # remove row.numbers
julho_semNIF <- read_delim("data/2026_07_julho/output/2026_07_semNIF.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE) |> select(-1) # remove row.numbers
julho_age_nok <- read_delim("data/2026_07_julho/output/2026_07_age_nok.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE) |> select(-1)


# explore -----------------------------------------------------------------

julho_comNIF_dn4 = julho_comNIF |> filter(age_new <= 4)
julho_comNIF_dn24 = julho_comNIF |> filter(age_new >= 24)
# julho_comNIF_dn24 = julho_comNIF_dn24 |> filter(year(birth_date_new)==2001)

# table(julho_comNIF$ticket_operator_code_new)
table(julho_comNIF$emissor)
# AT AML AT Cascais 
# 404047        559 

table(julho_comNIF$nome_imt)
# Navegante Metropolitano      Navegante Metropolitano + Complemento                         Viver Cascais 
# 403971  (403941)                                 106 (106)                                               559 (559) 


sum(julho_comNIF$compensacao) #10.719.178 (TEM DE DAR 10.722.470)
sum(julho_semNIF$compensacao) #50.816 (TEM DE DAR 50.838)
sum(julho_comNIF$compensacao) + sum(julho_semNIF$compensacao) #10.769.994 (TEM DE DAR 10.773.308)




# compare -----------------------------------------------------------------
library(readxl)
ANITA_comNIF <- read_excel("Exemplos/2026_07_julho/07_2026_comNIF_AR.xlsx", 
                           sheet = "07_2026") |> select(-1)
julho_comNIF <- read_delim("data/2026_07_julho/output/2026_07_comNIF.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE) |> select(-1) # remove row.numbers

# separar os nok~
ANITA_nok = ANITA_comNIF |> filter(`ok/NOK corrigido` != "ok")
ANITA_comNIF = ANITA_comNIF |> filter(`ok/NOK corrigido` == "ok")

ANITA_comNIF_redux = ANITA_comNIF |> select(fiscal_number, age_new, age_calculada, birth_date_corrigida, birth_date_enviada) |> arrange(birth_date_corrigida)
julho_comNIF_redux = julho_comNIF |> select(fiscal_number, age_new, birth_date_new) |> arrange(birth_date_new)

missing = ANITA_comNIF_redux |> filter(!fiscal_number %in% c(julho_comNIF_redux$fiscal_number))
# 0 


# valores -----------------------------------------------------------------

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

comparacao_group = ANITA_comNIF_group |>
  left_join(julho_comNIF_group, by = c("nome_imt", "ticket_code_new", "ticket_name_new")) |> 
  rename_with(~ str_replace(.x, "\\.x$", "_original"), ends_with(".x")) %>%
  rename_with(~ str_replace(.x, "\\.y$", "_rosa"), ends_with(".y")) |> 
  mutate(dif_passes = passes_rosa - passes_original,
         dif_validacoes = validacoes_rosa - validacoes_original,
         dif_compensacao = compensacao_rosa - compensacao_original) |> 
  filter(dif_passes != 0 | dif_validacoes != 0 | dif_compensacao != 0)


