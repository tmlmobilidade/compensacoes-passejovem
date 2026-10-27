library(readr)
library(dplyr)
nif_repescagem2024 <- read_csv("data/auxiliar/nif_repescagem2024.csv", 
                               col_types = cols(fiscal_number = col_character()))


nif_repescagem202610 <- read_delim("data/auxiliar/nif_repescagem202610.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(fiscal_number = col_character()), 
                                   trim_ws = TRUE)


nif_repescagem2024_update =  nif_repescagem2024 |> 
  bind_rows(nif_repescagem202610) |> 
  distinct()

write.csv(nif_repescagem2024_update, "data/auxiliar/nif_repescagem2024_update.csv", row.names = FALSE)
