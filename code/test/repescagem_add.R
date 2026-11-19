library(readr)
library(tidyverse)
library(dplyr)
nif_repescagem <- read_csv("data/auxiliar/nif_repescagem.csv", 
                               col_types = cols(fiscal_number = col_character()))


nif_repescagem202610 <- read_delim("data/auxiliar/nif_repescagem202610.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(fiscal_number = col_character()), 
                                   trim_ws = TRUE) |>
  mutate(ano_mes = 202608L)


nif_repescagem_update = nif_repescagem |> 
  bind_rows(nif_repescagem202610) |> 
  distinct(card_serial_number, fiscal_number, .keep_all = TRUE)

write.csv(nif_repescagem_update, "data/auxiliar/nif_repescagem.csv", row.names = FALSE)




# compare if 202610 comes in august db ------------------------------------


apuramento_2026_08_comNIF <- read_delim("data/2026_08_agosto/output/2026_08_comNIF.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

apuramento_2026_08_comNIF = apuramento_2026_08_comNIF |> 
  select(fiscal_number, cardserialnumber_new) |> 
  mutate(fiscal_number = as.character(fiscal_number)) 

nif_compare2608_10 = nif_repescagem202610 |> semi_join(apuramento_2026_08_comNIF) # 204 NIFs are there

nif_compare2608_10_not = nif_repescagem202610 |>
  mutate(card_serial_number = as.character(card_serial_number)) |> 
  anti_join(apuramento_2026_08_comNIF, by= c("card_serial_number" = "cardserialnumber_new")) # 99 card_serials

nif_compare2608_10_not2 = nif_repescagem202610 |>
  anti_join(apuramento_2026_08_comNIF) # 27 NIFs





# compare sem nif between months ------------------------------------------

# casos sem NIF que surgiram em Agosto e não estavam em Julho

agosto_semNIF = read_delim("data/2026_08_agosto/output/2026_08_semNIF.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
julho_semNIF <- read_delim("data/2026_07_julho/output/2026_07_semNIF.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE) 

semNIF_compare0807 = agosto_semNIF |> select(card_serial_number, doc_type, doc_number, age_new) |> 
  anti_join(julho_semNIF |>  select(card_serial_number, doc_type, doc_number, age_new)) |> 
  arrange(doc_type, age_new)
table(semNIF_compare0807$doc_type)
table(semNIF_compare0807$age_new)
hist(semNIF_compare0807$age_new)
semNIF_compare0807$card_serial_number
# dá uma lista de NIFs. copiar para o notepad++ e substituir espaços por vírgulas,
# adicionar vírgula no fim das linhas,
# edit > line operations > join lines
# copiar e meter no sql, no script repescagem_nif_outraBD
# o resultado, copiar em csv, e voltar a correr o script no topo deste.
# atualizar também no dep > public > nif_repescagem2024
