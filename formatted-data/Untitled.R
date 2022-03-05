#rm(list=ls())
library(tidyverse)

estratos <- read_csv("estratos_onu.csv")
codigos <- read_csv("codigos_identidad.csv", col_names = c("ENT", "Estado"))
datos <- read_csv("tvivienda.csv")  %>% 
  left_join(codigos, by = "ENT")


