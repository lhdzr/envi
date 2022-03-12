# Aplicar factores de expansion 

# Indices PCA
library(readr)
library(tidyverse)

tvivienda <- read_csv("formatted-data/tvivienda.csv")


creditos = tvivienda %>%
  select(vid,P5_1,P5_15_01, P5_15_02,P5_15_03, P5_15_04,P5_15_05, P5_15_06)


# Convertir a 0 porque significa que no tiene credito de infonavit 
creditos[creditos == 2] <- 0 


creditos = creditos%>%
  rowwise()%>%
  mutate(n_creditos = sum(c(P5_15_02,P5_15_03, P5_15_04,P5_15_05, P5_15_06)))

creditos$otro_credito = ifelse(creditos$n_creditos > 0, 2,
                               creditos$n_creditos)


creditos = creditos %>%
  mutate(total = sum(c(P5_15_01,otro_credito)))%>%
  mutate(tipo_credito = ifelse(P5_15_01 == 1,"INFO",
                               ifelse(P5_15_01 == 0 && total > 1 ,"OTRO",
                                      ifelse(total == 0, "NO_CREDITO",
                                             total))))

creditos$tipo_credito[is.na(creditos$tipo_credito)] <- "NO_CREDITO"


# IR ABAJO PARA AGREGAR LOS ESTADOS 
estados = tvivienda[,c("vid","Entidades")]
#--------------------------------------------------------------------------------------------
#Indices PCA
CATPCA <- read_csv("VERO/Indices calidad CATPCA/IndicesCATPCA_Estratificados.csv")%>%
  rename(PCA_strata_hab = strata.CATPCAHab...stratumID...,
         PCA_strata_ser =  strata.CATPCASer...stratumID...,
         PCA_strata_ubi =  strata.CATPCAUb...stratumID...,
         PCA_strata_acc  = strata.CATPCAAcc...stratumID...)

estratos = CATPCA[,c("vid","EST_DIS","UPM_DIS","FACTOR")]
# Agregar estados a PCA
CATPCA = left_join(CATPCA, estados, by = "vid")


CATPCA_satisfaccion <- read_csv("VERO/Indice satisfaccion CATPCA/Satisfaction_CATPCA_Estratos.csv")%>%
  rename(PCA_strata_sat = strata.CATPCASAT...stratumID...)

#Agregar estados a PCA satisfaccion 
CATPCA_satisfaccion = left_join(CATPCA_satisfaccion,estados, by = "vid")


#Indices P2 -------------------------------------------------------------------------------------

estratos_P2 <- read_csv("formatted-data/estratos_onu.csv")


# Agregar estratos  a P2 de acuerdo al vid 
estratos_P2 = left_join(estratos_P2,estratos, by="vid")

# Agregar estados a P2
estratos_P2 = left_join(estratos_P2, estados, by = "vid")

names(estratos_P2)


# Juntar tablas de estatos p2 y pca
juntos = full_join(estratos_P2,CATPCA[,c("vid","PCA_strata_hab","PCA_strata_ser","PCA_strata_ubi","PCA_strata_acc")], by = "vid")

names(CATPCA)

#Juntar tablas satisfacción 
satis = right_join(estratos_P2[,c("vid","estr_satis","EST_DIS","UPM_DIS","FACTOR","Entidades")],CATPCA_satisfaccion[,c("vid","PCA_strata_sat")], by= "vid")

#satis2 = left_join(satis, creditos, by="vid")

#names(satis2)
#
#satis_otrocredito = satis2 %>%
#  filter(tipo_credito == "OTRO")

#satis_info = satis2 %>%
#  filter(tipo_credito == "INFO")

#names(satis2)

#satis_notiene = satis2 %>%
#  filter(Entidades == "Aguscalientes",
#         tipo_credito == "NO_CREDITO")%>%
#  mutate(prop = (length("vid"))/length(estr_satis %in% bueno))
  




#table(satis_otrocredito$estr_satis)

#table(satis_info$estr_satis)

#table(satis_notiene$estr_satis)

names(estratos_P2)
# Aplicar factores de expansion -------------------------------------------------------------------
library(srvyr)

juntos_srv = as_survey_design(juntos, weights = FACTOR, ids= UPM_DIS, strata = EST_DIS)

satis_srv = as_survey_design(satis, weights = FACTOR, ids= UPM_DIS, strata = EST_DIS)
  
options(survey.adjust.domain.lonely = T)
options(survey.lonely.psu = "adjust")

library(scales)
library(stargazer)




juntos2 = juntos%>%
  filter(estr_habit%in% malo,
         estr_serv %in% malo)
names(juntos)
# Tabla vulnerabilidades ----------------------------------------------------------------
tabla_ambiente = juntos_srv%>%
  group_by(Entidades) %>% 
  summarize("Ambiente" = survey_mean(estr_habit %in% malo && estr_serv %in% malo, vartype = NULL),
            "Discapacidad" = survey_mean(estr_acc %in% malo && estr_ubi %in% malo, vartype = NULL),
            "Desempleo" = survey_mean(estr_aseq %in% malo && estr_ubi %in% malo, vartype = NULL),
            "Gentrificacion" = survey_mean(estr_ten %in% malo && estr_ubi %in% bueno, vartype = NULL))

  
tabla_ambiente[2:5] <- sapply(tabla_ambiente[2:5], function(x) percent(x, accuracy=0.01))





# Accesibilidad -----------------------------------------------------------------------
tabla_acces= juntos_srv%>%
  group_by(Entidades) %>% 
  summarize("P2 " = survey_mean(estr_acc %in% "Muy Alta", vartype = NULL),
            "CATPCA" = survey_mean(PCA_strata_acc %in% "Muy Alta", vartype = NULL),
            "P2  " = survey_mean(estr_acc %in% "Alta", vartype = NULL),
            "CATPCA " = survey_mean(PCA_strata_acc %in% "Alta", vartype = NULL),
            "P2   " = survey_mean(estr_acc %in% "Media", vartype = NULL),
            "CATPCA  " = survey_mean(PCA_strata_acc %in% "Media", vartype = NULL),
            "P2" = survey_mean(estr_acc %in% "Baja", vartype = NULL),
            "CATPCA   " = survey_mean(PCA_strata_acc %in% "Baja", vartype = NULL),
            "P2    " = survey_mean(estr_acc %in% "Muy baja", vartype = NULL),
            "CATPCA    "  = survey_mean(PCA_strata_acc %in% "Muy baja", vartype = NULL))


tabla_acces2= juntos_srv%>%
  summarize("P2 " = survey_mean(estr_acc %in% "Muy Alta", vartype = NULL),
            "CATPCA" = survey_mean(PCA_strata_acc %in% "Muy Alta", vartype = NULL),
            "P2  " = survey_mean(estr_acc %in% "Alta", vartype = NULL),
            "CATPCA " = survey_mean(PCA_strata_acc %in% "Alta", vartype = NULL),
            "P2   " = survey_mean(estr_acc %in% "Media", vartype = NULL),
            "CATPCA  " = survey_mean(PCA_strata_acc %in% "Media", vartype = NULL),
            "P2" = survey_mean(estr_acc %in% "Baja", vartype = NULL),
            "CATPCA   " = survey_mean(PCA_strata_acc %in% "Baja", vartype = NULL),
            "P2    " = survey_mean(estr_acc %in% "Muy baja", vartype = NULL),
            "CATPCA    "  = survey_mean(PCA_strata_acc %in% "Muy baja", vartype = NULL))
Entidades = c("Nacional")

tabla_acces2 = cbind(tabla_acces2,Entidades)%>%
  relocate(Entidades, .before = "P2 ")


tabla_acces = rbind(tabla_acces,tabla_acces2)


tabla_acces[2:11] <- sapply(tabla_acces[2:11], function(x) percent(x, accuracy=0.01))
tabla_acces = as.data.frame(tabla_acces)


library(htmlTable)

tabla_acces %>%
  addHtmlTableStyle(pos.caption = "bottom") %>% 
  htmlTable(
          cgroup = list(c("","Indice Accesibilidad"),
                        c("","Muy Alta", "Alta","Media","Baja", "Muy baja")),
          n.cgroup = list(c(1,10),
                          c(1,2,2,2,2,2)),
          caption = "Fuentes: Elaboracion propia",
          total = TRUE)




#Habitabilidad --------------------------------------------------------------
tabla_habi = juntos_srv%>%
  group_by(Entidades) %>% 
  summarize("P2" = survey_mean(estr_habit %in% "Muy Alta", vartype = NULL),
            "CATPCA" = survey_mean(PCA_strata_hab %in% "Muy Alta", vartype = NULL),
            "P2 " = survey_mean(estr_habit %in% "Alta", vartype = NULL),
            "CATPCA " = survey_mean(PCA_strata_hab %in% "Alta", vartype = NULL),
            "P2  " = survey_mean(estr_habit %in% "Media", vartype = NULL),
            "CATPCA  " = survey_mean(PCA_strata_hab %in% "Media", vartype = NULL),
            "P2   " = survey_mean(estr_habit %in% "Baja", vartype = NULL),
            "CATPCA   " = survey_mean(PCA_strata_hab %in% "Baja", vartype = NULL),
            "P2    " = survey_mean(estr_habit %in% "Muy baja", vartype = NULL),
            "CATPCA    " = survey_mean(PCA_strata_hab %in% "Muy baja", vartype = NULL))


tabla_habi2 = juntos_srv%>%
  summarize("P2" = survey_mean(estr_habit %in% "Muy Alta", vartype = NULL),
            "CATPCA" = survey_mean(PCA_strata_hab %in% "Muy Alta", vartype = NULL),
            "P2 " = survey_mean(estr_habit %in% "Alta", vartype = NULL),
            "CATPCA " = survey_mean(PCA_strata_hab %in% "Alta", vartype = NULL),
            "P2  " = survey_mean(estr_habit %in% "Media", vartype = NULL),
            "CATPCA  " = survey_mean(PCA_strata_hab %in% "Media", vartype = NULL),
            "P2   " = survey_mean(estr_habit %in% "Baja", vartype = NULL),
            "CATPCA   " = survey_mean(PCA_strata_hab %in% "Baja", vartype = NULL),
            "P2    " = survey_mean(estr_habit %in% "Muy baja", vartype = NULL),
            "CATPCA    " = survey_mean(PCA_strata_hab %in% "Muy baja", vartype = NULL))


tabla_habi2 = cbind(tabla_habi2,Entidades)%>%
  relocate(Entidades, .before = "P2")


tabla_habi= rbind(tabla_habi,tabla_habi2)



tabla_habi[2:11] <- sapply(tabla_habi[2:11], function(x) percent(x, accuracy=0.01))

tabla_habi %>%
  addHtmlTableStyle(pos.caption = "bottom") %>% 
  htmlTable(
    cgroup = list(c("","Indice Habitabilidad"),
                  c("","Muy Alta", "Alta","Media","Baja", "Muy baja")),
    n.cgroup = list(c(1,10),
                    c(1,2,2,2,2,2)),
    caption = "Fuentes: Elaboracion propia",
    total = TRUE)

# Servicios ----------------------------------------------------
tabla_serv = juntos_srv%>%
  group_by(Entidades) %>% 
  summarize("P2" = survey_mean(estr_serv %in% "Muy Alta", vartype = NULL),
            "CATPCA" = survey_mean(PCA_strata_ser %in% "Muy Alta", vartype = NULL),
            "P2 " = survey_mean(estr_serv %in% "Alta", vartype = NULL),
            "CATPCA " = survey_mean(PCA_strata_ser %in% "Alta", vartype = NULL),
            "P2  " = survey_mean(estr_serv %in% "Media", vartype = NULL),
            "CATPCA  " = survey_mean(PCA_strata_ser %in% "Media", vartype = NULL),
            "P2   " = survey_mean(estr_serv %in% "Baja", vartype = NULL),
            "CATPCA   " = survey_mean(PCA_strata_ser %in% "Baja", vartype = NULL),
            "P2    " = survey_mean(estr_serv %in% "Muy baja", vartype = NULL),
            "CATPCA    " = survey_mean(PCA_strata_ser %in% "Muy baja", vartype = NULL))


tabla_serv2 = juntos_srv%>%
  summarize("P2" = survey_mean(estr_serv %in% "Muy Alta", vartype = NULL),
            "CATPCA" = survey_mean(PCA_strata_ser %in% "Muy Alta", vartype = NULL),
            "P2 " = survey_mean(estr_serv %in% "Alta", vartype = NULL),
            "CATPCA " = survey_mean(PCA_strata_ser %in% "Alta", vartype = NULL),
            "P2  " = survey_mean(estr_serv %in% "Media", vartype = NULL),
            "CATPCA  " = survey_mean(PCA_strata_ser %in% "Media", vartype = NULL),
            "P2   " = survey_mean(estr_serv %in% "Baja", vartype = NULL),
            "CATPCA   " = survey_mean(PCA_strata_ser %in% "Baja", vartype = NULL),
            "P2    " = survey_mean(estr_serv %in% "Muy baja", vartype = NULL),
            "CATPCA    " = survey_mean(PCA_strata_ser %in% "Muy baja", vartype = NULL))


tabla_serv2 = cbind(tabla_serv2,Entidades)%>%
  relocate(Entidades, .before = "P2")


tabla_serv= rbind(tabla_serv,tabla_serv2)

tabla_serv[2:11] <- sapply(tabla_serv[2:11], function(x) percent(x, accuracy=0.01))

tabla_serv %>%
  addHtmlTableStyle(pos.caption = "bottom") %>% 
  htmlTable(
    cgroup = list(c("","Indice Servicios"),
                  c("","Muy Alta", "Alta","Media","Baja", "Muy baja")),
    n.cgroup = list(c(1,10),
                    c(1,2,2,2,2,2)),
    caption = "Fuentes: Elaboracion propia",
    total = TRUE)

# Ubicación --------------------------------------------------------------------------------------

tabla_ubi = juntos_srv%>%
  group_by(Entidades) %>% 
  summarize("P2" = survey_mean(estr_ubi %in% "Muy Alta", vartype = NULL),
            "CATPCA" = survey_mean(PCA_strata_ubi %in% "Muy Alta", vartype = NULL),
            "P2 " = survey_mean(estr_ubi %in% "Alta", vartype = NULL),
            "CATPCA " = survey_mean(PCA_strata_ubi %in% "Alta", vartype = NULL),
            "P2  " = survey_mean(estr_ubi %in% "Media", vartype = NULL),
            "CATPCA  " = survey_mean(PCA_strata_ubi %in% "Media", vartype = NULL),
            "P2   " = survey_mean(estr_ubi %in% "Baja", vartype = NULL),
            "CATPCA   " = survey_mean(PCA_strata_ubi %in% "Baja", vartype = NULL),
            "P2    " = survey_mean(estr_ubi %in% "Muy baja", vartype = NULL),
            "CATPCA    " = survey_mean(PCA_strata_ubi %in% "Muy baja", vartype = NULL))



tabla_ubi2 = juntos_srv%>%
  summarize("P2" = survey_mean(estr_ubi %in% "Muy Alta", vartype = NULL),
            "CATPCA" = survey_mean(PCA_strata_ubi %in% "Muy Alta", vartype = NULL),
            "P2 " = survey_mean(estr_ubi %in% "Alta", vartype = NULL),
            "CATPCA " = survey_mean(PCA_strata_ubi %in% "Alta", vartype = NULL),
            "P2  " = survey_mean(estr_ubi %in% "Media", vartype = NULL),
            "CATPCA  " = survey_mean(PCA_strata_ubi %in% "Media", vartype = NULL),
            "P2   " = survey_mean(estr_ubi %in% "Baja", vartype = NULL),
            "CATPCA   " = survey_mean(PCA_strata_ubi %in% "Baja", vartype = NULL),
            "P2    " = survey_mean(estr_ubi %in% "Muy baja", vartype = NULL),
            "CATPCA    " = survey_mean(PCA_strata_ubi %in% "Muy baja", vartype = NULL))



tabla_ubi2 = cbind(tabla_ubi2,Entidades)%>%
  relocate(Entidades, .before = "P2")


tabla_ubi= rbind(tabla_ubi,tabla_ubi2)


tabla_ubi[2:11] <- sapply(tabla_ubi[2:11], function(x) percent(x, accuracy=0.01))
          

tabla_ubi %>%
  addHtmlTableStyle(pos.caption = "bottom") %>% 
  htmlTable(
    cgroup = list(c("","Indice Ubicacion"),
                  c("","Muy Alta", "Alta","Media","Baja", "Muy baja")),
    n.cgroup = list(c(1,10),
                    c(1,2,2,2,2,2)),
    caption = "Fuentes: Elaboracion propia",
    total = TRUE)


# Satisfaccion-----------------------------------------------------------------------------------------
tabla_sat = satis_srv%>%
  group_by(Entidades) %>% 
  summarize("P2" = survey_mean(estr_satis %in% "Muy Alta", vartype = NULL),
            "CATPCA" = survey_mean(PCA_strata_sat %in% "Muy Alta", vartype = NULL),
            "P2 " = survey_mean(estr_satis %in% "Alta", vartype = NULL),
            "CATPCA " = survey_mean(PCA_strata_sat %in% "Alta", vartype = NULL),
            "P2  " = survey_mean(estr_satis %in% "Media", vartype = NULL),
            "CATPCA  " = survey_mean(PCA_strata_sat %in% "Media", vartype = NULL),
            "P2   " = survey_mean(estr_satis %in% "Baja", vartype = NULL),
            "CATPCA   " = survey_mean(PCA_strata_sat %in% "Baja", vartype = NULL),
            "P2    " = survey_mean(estr_satis %in% "Muy baja", vartype = NULL),
            "CATPCA    " = survey_mean(PCA_strata_sat %in% "Muy baja", vartype = NULL))

tabla_sat2 = satis_srv%>%
  summarize("P2" = survey_mean(estr_satis %in% "Muy Alta", vartype = NULL),
            "CATPCA" = survey_mean(PCA_strata_sat %in% "Muy Alta", vartype = NULL),
            "P2 " = survey_mean(estr_satis %in% "Alta", vartype = NULL),
            "CATPCA " = survey_mean(PCA_strata_sat %in% "Alta", vartype = NULL),
            "P2  " = survey_mean(estr_satis %in% "Media", vartype = NULL),
            "CATPCA  " = survey_mean(PCA_strata_sat %in% "Media", vartype = NULL),
            "P2   " = survey_mean(estr_satis %in% "Baja", vartype = NULL),
            "CATPCA   " = survey_mean(PCA_strata_sat %in% "Baja", vartype = NULL),
            "P2    " = survey_mean(estr_satis %in% "Muy baja", vartype = NULL),
            "CATPCA    " = survey_mean(PCA_strata_sat %in% "Muy baja", vartype = NULL))

tabla_sat2 = cbind(tabla_sat2,Entidades)%>%
  relocate(Entidades, .before = "P2")


tabla_sat= rbind(tabla_sat,tabla_sat2)

tabla_sat[2:11] <- sapply(tabla_sat[2:11], function(x) percent(x, accuracy=0.01))

tabla_sat %>%
  addHtmlTableStyle(pos.caption = "bottom") %>% 
  htmlTable(
    cgroup = list(c("","Indice Satisfaccion"),
                  c("","Muy Alta", "Alta","Media","Baja", "Muy baja")),
    n.cgroup = list(c(1,10),
                    c(1,2,2,2,2,2)),
    caption = "Fuentes: Elaboracion propia",
    total = TRUE)




# NACIONAL  ----------------------------------------------------------------------------------------------------------
bueno <- c("Muy Alta", "Alta", "Media")
malo <- c("Muy baja", "Baja")
names(juntos)

tabla_nacional = juntos_srv%>%
  summarize("Accebilidad P2 " = survey_mean(estr_acc %in% malo, vartype = NULL),
            "Accesibilidad CATPCA" = survey_mean(PCA_strata_acc %in% malo, vartype = NULL),
            "Habitabilidad P2" = survey_mean(estr_habit %in% malo, vartype = NULL),
            "Habitabilidad CATPCA" = survey_mean(PCA_strata_hab %in% malo, vartype = NULL),
            "Servicios P2" = survey_mean(estr_serv %in% malo, vartype = NULL),
            "Servicios CATPCA" = survey_mean(PCA_strata_ser %in% malo, vartype = NULL),
            "Ubicacion P2" = survey_mean(estr_ubi %in% malo, vartype = NULL),
            "Ubicacion CATPCA" = survey_mean(PCA_strata_ubi %in% malo, vartype = NULL))
            
names(satis)

tabla_sat_nacional = satis_srv %>%
  summarize("Satisfaccion P2" = survey_mean(estr_satis %in% malo, vartype = NULL),
            "Satisfaccion CATPCA" = survey_mean(PCA_strata_sat %in% malo, vartype = NULL))


nacional = cbind(tabla_nacional, tabla_sat_nacional)

nacional <- as.data.frame(sapply(nacional, function(x) percent(x, accuracy=0.01)))%>%
  rename("porcentaje" = "sapply(nacional, function(x) percent(x, accuracy = 0.01))")
names(nacional)


vector = c("Accebilidad P2", "Accesibilidad CATPCA","Habitabilidad P2","Habitabilidad CATPCA","Servicios P2",
           "Servicios CATPCA","Ubicacion P2","Ubicacion CATPCA","Satisfaccion P2","Satisfaccion CATPCA")

indice = c("P2", "CATPCA", "P2", "CATPCA", "P2", "CATPCA","P2", "CATPCA", "P2", "CATPCA")

nacional = cbind(nacional, vector, indice)




# Graficar -----------------------------------------------------------------------------




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
                                                                                                                                                    ifelse(tvivienda$ENT == 19, "Nuevo Leon",
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
