library(tidyverse)


# load data ---------------------------------------------------------------


julho_comNIF <- read_delim("data/2026_07_julho/output/2026_07_comNIF.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE) |> select(-1) # remove row.numbers
julho_semNIF <- read_delim("data/2026_07_julho/output/2026_07_semNIF.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE) |> select(-1) # remove row.numbers



# explore -----------------------------------------------------------------

julho_comNIF_dn4 = julho_comNIF |> filter(age_new <= 4)
julho_comNIF_dn24 = julho_comNIF |> filter(age_new > 23)

table(julho_comNIF$ticket_operator_code_new)
table(julho_comNIF$emissor)
# AT AML AT Cascais 
# 404092        559 

table(julho_comNIF$nome_imt)
# Navegante Metropolitano      Navegante Metropolitano + Complemento                         Viver Cascais 
# 403986  (403941)                                 106 (106)                                               559 (559) 


sum(julho_comNIF$compensacao) #10.719.178 (TEM DE DAR 10.722.470)
sum(julho_semNIF$compensacao) #50.816 (TEM DE DAR 50.838)
sum(julho_comNIF$compensacao) + sum(julho_semNIF$compensacao) #10.769.994 (TEM DE DAR 10.773.308)

