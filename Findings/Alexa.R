library(readr)
data_viv <- read_csv("formatted-data/tvivienda_changes.csv")

p2_acces <- read_csv("P2 index/p2_accesibilidad.csv")

p2_hab <- read_csv("P2 index/p2_habitabilidad.csv")

p2_ser <- read_csv("P2 index/p2_servicios.csv")

p2_ubi <- read_csv("P2 index/p2_ubicacion.csv")

p2_sat <- read_csv("P2 index/p2_satisfaccion.csv")

table(data_viv$P4_19_2)

library(tidyverse)

D = data_viv[,c("P4_19_1", "P4_19_2")]
D =  D%>%
  rowwise()%>%
  mutate(antiguedad = sum(c(P4_19_1, P4_19_2),na.rm = T))

data_viv$P4_19_2 = ifelse(data_viv$P4_19_2 == 9, NA, 
                                   data_viv$P4_19_2)

data_viv$P4_19_1 = ifelse(data_viv$P4_19_1 == 98, NA,
                          ifelse(data_viv$P4_19_1  == 99, NA,
                          data_viv$P4_19_1))
data_viv = data_viv%>% 
  rowwise()%>%
  mutate(antiguedad = sum(c(P4_19_1, P4_19_2),na.rm = T))




