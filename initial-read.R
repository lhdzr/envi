setwd("C:/Users/leonh/Documents/Proyectos Personales/Premio INFONAVIT/Github/envi")
library(tidyverse)
source("functions/data-reading-functions.R")


# LECTURA DE DATOS
tdemanda = read.csv("data/conjunto_de_datos_tdemanda_envi_2020/conjunto_de_datos/conjunto_de_datos_tdemanda_envi_2020.csv",encoding = "UTF-8")
thogar = read.csv("data/conjunto_de_datos_thogar_envi_2020/conjunto_de_datos/conjunto_de_datos_thogar_envi_2020.csv",encoding = "UTF-8")
tsdem = read.csv("data/conjunto_de_datos_tsdem_envi_2020/conjunto_de_datos/conjunto_de_datos_tsdem_envi_2020.csv",encoding = "UTF-8")
tviv_seg1 = read.csv("data/conjunto_de_datos_tseg_viv1_envi_2020/conjunto_de_datos/conjunto_de_datos_tseg_viv1_envi_2020.csv",encoding = "UTF-8")
tviv_seg2 = read.csv("data/conjunto_de_datos_tseg_viv2_envi_2020/conjunto_de_datos/conjunto_de_datos_tseg_viv2_envi_2020.csv",encoding = "UTF-8")
masgasto = read.csv("data/conjunto_de_datos_tviv_masgasto_envi_2020/conjunto_de_datos/conjunto_de datos_tviv_masgasto_envi_2020.csv",encoding = "UTF-8")
names(masgasto)[names(masgasto) == 'FOLIO'] <- 'X.U.FEFF.FOLIO'
tvivienda = read.csv("data/conjunto_de_datos_tvivienda_envi_2020/conjunto_de_datos/conjunto_de_datos_tvivienda_envi_2020.csv",encoding = "UTF-8")


# INCLUSIÓN DE 'vid' y 'uid'
tdemanda = set_vid_uid(tdemanda)
thogar = set_vid_uid(thogar)
tsdem = set_vid_uid(tsdem)
tviv_seg1 = set_vid_uid(tviv_seg1)
tviv_seg2 = set_vid_uid(tviv_seg2)
masgasto = set_vid_uid(masgasto)
tvivienda = set_vid_uid(tvivienda)


# ESCRIBIR CSVS
write_csv(tdemanda,"formatted-data/tdemanda.csv")
write_csv(thogar,"formatted-data/thogar.csv")
write_csv(tsdem,"formatted-data/tsdem.csv")
write_csv(tviv_seg1,"formatted-data/tseg_viv1.csv")
write_csv(tviv_seg2,"formatted-data/tseg_viv2.csv")
write_csv(masgasto,"formatted-data/masgasto.csv")
write_csv(tvivienda,"formatted-data/tvivienda.csv")


# CÓDIGOS DE ENTIDAD
ent = read.csv("./conjunto_de_datos_tsdem_envi_2020/catalogos/ent.csv")