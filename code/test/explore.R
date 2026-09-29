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



# test datas --------------------------------------------------------------
library(lubridate)
primeiro_dia_mes <- as.Date(sprintf("%d-%02d-01", 2026, 7))
datas_test = julho_comNIF |> select(fiscal_number, age_new, birth_date_new) |> 
  mutate(age_other = as.integer(floor(as.numeric(primeiro_dia_mes - birth_date_new) / 365.25))) |> 
  mutate(age_dif = age_other-age_new)
table(datas_test$age_dif)

datas_test = datas_test |> 
dt_main[!is.na(birth_date), elegivel := {
  ano_nasc <- as.integer(format(birth_date, "%Y"))
  mes_nasc <- as.integer(format(birth_date, "%m"))
  
  # Idade calculada pelo mês de referência
  # Se fizer 24 em julho, mes_24anos = 7. É elegível se mes <= mes_nasc no ano em que faz 24.
  limite_data <- as.Date(sprintf("%d-%02d-01", ano_nasc + 24, mes_nasc))
  
  primeiro_dia_mes <= limite_data
}]


ano_ref <- 2026
mes_ref <- 7
primeiro_dia_mes <- as.Date(sprintf("%d-%02d-01", ano_ref, mes_ref))

datas_test <- julho_comNIF |> 
  select(fiscal_number, age_new, birth_date_new) |> 
  mutate(
    # Cálculo antigo/aproximado por 365.25 para comparar:
    age_other = as.integer(floor(as.numeric(primeiro_dia_mes - birth_date_new) / 365.25)),
    age_dif   = age_other - age_new,
    
    # ---------------------------------------------------------
    # CRITÉRIO: Elegível até ao final do mês de aniversário dos 24 anos
    # ---------------------------------------------------------
    # Primeiro dia do mês em que faz 24 anos:
    mes_24anos = floor_date(birth_date_new %m+% years(24), unit = "month"),
    
    # Elegível se o mês de referência for menor ou igual ao mês em que atinge 24 anos:
    elegivel = primeiro_dia_mes <= mes_24anos
  )

# Verificar os casos de discrepância
table(datas_test$age_dif)
table(datas_test$elegivel)

# Ver quem faz 24 anos em julho de 2026 e o respetivo estado de elegibilidade
datas_test |> 
  filter(year(birth_date_new) == 2002, month(birth_date_new) %in% c(6, 7, 8)) |> 
  View()



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
