# Aplicar factores de expansion 

# Indices PCA
library(readr)
library(tidyverse)

tvivienda <- read_csv("formatted-data/tvivienda.csv")
names(tvivienda)

estados = tvivienda[,c("vid","Entidades")]
#--------------------------------------------------------------------------------------------
CATPCA <- read_csv("VERO/Indices calidad CATPCA/IndicesCATPCA_Estratificados.csv")%>%
  rename(PCA_strata_hab = strata.CATPCAHab...stratumID...,
         PCA_strata_ser =  strata.CATPCASer...stratumID...,
         PCA_strata_ubi =  strata.CATPCAUb...stratumID...,
         PCA_strata_acc  = strata.CATPCAAcc...stratumID...)

estratos = CATPCA[,c("vid","EST_DIS","UPM_DIS","FACTOR")]

CATPCA = left_join(CATPCA, estados, by = "vid")


CATPCA_satisfaccion <- read_csv("VERO/Indice satisfaccion CATPCA/Satisfaction_CATPCA_Estratos.csv")%>%
  rename(PCA_strata_sat = strata.CATPCASer...stratumID...)

CATPCA_satisfaccion = left_join(CATPCA_satisfaccion,estados, by = "vid")
CATPCA_satisfaccion = left_join(CATPCA_satisfaccion,estratos, by = "vid")





#Indices P2 -------------------------------------------------------------------------------------
estratos_P2 <- read_csv("formatted-data/estratos_onu.csv")
names(estratos_P2)

estratos_P2 = left_join(estratos_P2,estratos, by="vid")

estratos_P2 = left_join(estratos_P2, estados, by = "vid")






library(srvyr)

# Aplicar factores de expansion

estratos_p2_srv = as_survey_design(estratos_P2, weights = FACTOR, ids= UPM_DIS, strata = EST_DIS)




















###
tvivienda$Entidades = ifelse(tvivienda$ENT == 1, "Aguascalientes",
                             ifelse(tvivienda$ENT == 2 , "Baja Califonornia",
                                    ifelse(tvivienda$ENT == 3, "Baja Califonornia Sur",
                                           ifelse(tvivienda$ENT == 4, "Campeche",
                                                  ifelse(tvivienda$ENT == 5, "Coahuila",
                                                         ifelse(tvivienda$ENT == 6, "Colima",
                                                                ifelse(tvivienda$ENT == 7 , "Chiapas",
                                                                       ifelse(tvivienda$ENT == 8, "Chihuahua",
                                                                              ifelse(tvivienda$ENT == 9 , "CDMX",
                                                                                     ifelse(tvivienda$ENT == 10,"Durango",
                                                                                            ifelse(tvivienda$ENT == 11, "Guanajuato",
                                                                                                   ifelse(tvivienda$ENT == 12, "Guerrero",
                                                                                                          ifelse(tvivienda$ENT == 13, "Hidalgo",
                                                                                                                 ifelse(tvivienda$ENT == 14, "Jalisco",
                                                                                                                        ifelse(tvivienda$ENT == 15, "Mexico",
                                                                                                                               ifelse(tvivienda$ENT == 16, "Michoacan",
                                                                                                                                      ifelse(tvivienda$ENT == 17, "Morelos",
                                                                                                                                             ifelse(tvivienda$ENT == 18 , "Nayarit",
                                                                                                                                                    ifelse(tvivienda$ENT == 19, "Nuevo León",
                                                                                                                                                           ifelse(tvivienda$ENT == 20, "Oaxaca",
                                                                                                                                                                  ifelse(tvivienda$ENT == 21, "Puebla",
                                                                                                                                                                         ifelse(tvivienda$ENT==22, "Queretaro",
                                                                                                                                                                                ifelse(tvivienda$ENT == 23, "Quintana Roo",
                                                                                                                                                                                       ifelse(tvivienda$ENT == 24, "San Luis",
                                                                                                                                                                                              ifelse(tvivienda$ENT == 25, "Sinaloa",
                                                                                                                                                                                                     ifelse(tvivienda$ENT == 26 , "Sonora",
                                                                                                                                                                                                            ifelse(tvivienda$ENT == 27, "Tabasco",
                                                                                                                                                                                                                   ifelse(tvivienda$ENT == 28 , "Tamaulipas",
                                                                                                                                                                                                                          ifelse(tvivienda$ENT == 29, "Tlaxcala",
                                                                                                                                                                                                                                 ifelse(tvivienda$ENT == 30 , "Veracruz",
                                                                                                                                                                                                                                        ifelse(tvivienda$ENT == 31, "Yucatan",
                                                                                                                                                                                                                                               ifelse(tvivienda$ENT ==32, "Zacatecas",
                                                                                                                                                                                                                                                      tvivienda$ENT))))))))))))))))))))))))))))))))
