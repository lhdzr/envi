rm(list=ls())

library(stratification)
library(tidyverse)
p2_acc <- read_csv("p2_accesibilidad.csv") %>%
  rename(p2_acc = p2distance.2) %>%
  rename(estr_acc = estratos...stratumID...)

p2_habit <- read_csv("p2_habitabilidad.csv")%>%
  rename(p2_habit = p2distance.3) %>%
  rename(estr_habit = strata.DH_2020...stratumID...)

p2_serv <- read_csv("p2_servicios.csv") %>%
  rename(p2_serv = p2distance.2) %>% 
  rename(estr_serv = estratos...stratumID...)


p2_ubi <- read_csv("p2_ubicacion.csv") %>%
  rename(p2_ubi = p2distance.3) %>% 
  rename(estr_ubi = estratos...stratumID...)


# tenencia ----------------------------------------------------------------


ind_ten <- read_csv("indice_tenencia.csv")
estrato_ten <- strata.cumrootf(ind_ten$indice,
                               n = length(ind_ten$indice),
                               Ls = 5)


#aqui pego los estratos (q se encuentran en strata.DH_2020[[stratumID]])
#a el data frame de resultados, como una nueva columna
#observa que los estratos son numericos
assign(paste0("ind_ten"), data.frame(ind_ten, estrato_ten[["stratumID"]])) 


#aqui le pido en orden; que guarde en niveles al df "resultados"
#Que asigne niveles a la columna 13, del recien creado df niveles
#Estos niveles (levels) asignados van de 1 a 5, de muy bajo a muy alto,
#finalmente, junto la columna 13 de niveles con el df resultados y borro niveles

for(i in 1){
  niveles = get(paste0("ind_ten")) 
  levels(niveles[,13]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("ind_ten"), niveles)
  rm(niveles)
}

ind_ten <- ind_ten %>% 
  rename(estr_ten = estrato_ten...stratumID...)


# Estratificación Cultura -------------------------------------------------
library(stratification)
ind_cul <- read_csv("indice_cultural.csv")
estrato_cul <- strata.cumrootf(ind_cul$P6_6,
                               n = length(ind_cul$P6_6),
                               Ls = 2)

#aqui pego los estratos (q se encuentran en strata.DH_2020[[stratumID]])
#a el data frame de resultados, como una nueva columna
#observa que los estratos son numericos
assign(paste0("ind_cul"), data.frame(ind_cul, estrato_cul[["stratumID"]])) 


#aqui le pido en orden; que guarde en niveles al df "resultados"
#Que asigne niveles a la columna 13, del recien creado df niveles
#Estos niveles (levels) asignados van de 1 a 5, de muy bajo a muy alto,
#finalmente, junto la columna 13 de niveles con el df resultados y borro niveles

for(i in 1){
  niveles = get(paste0("ind_cul")) 
  levels(niveles[,3]) = c("Baja","Alta")
  assign(paste0("ind_cul"), niveles)
  rm(niveles)
}

ind_cultura <- ind_cul %>% 
  rename(estr_cul = estrato_cul...stratumID...)





# estrato asequibilidad ---------------------------------------------------
library(stratification)
ind_aseq <- read_csv("prop_aseq.csv") %>% 
  filter(!is.na(prop_gasto),
         prop_gasto <0.8) %>% 
  select(vid, prop_gasto)

estrato_aseq <- strata.cumrootf(ind_aseq$prop_gasto,
                                n = length(ind_aseq$prop_gasto),
                                Ls = 5)


#aqui pego los estratos (q se encuentran en strata.DH_2020[[stratumID]])
#a el data frame de resultados, como una nueva columna
#observa que los estratos son numericos
assign(paste0("ind_aseq"), data.frame(ind_aseq, estrato_aseq[["stratumID"]])) 


#aqui le pido en orden; que guarde en niveles al df "resultados"
#Que asigne niveles a la columna 13, del recien creado df niveles
#Estos niveles (levels) asignados van de 1 a 5, de muy bajo a muy alto,
#finalmente, junto la columna 13 de niveles con el df resultados y borro niveles

for(i in 1){
  niveles = get(paste0("ind_aseq")) 
  levels(niveles[,3]) = c("Muy Alta", "Alta","Media","Baja","Muy Baja")
  assign(paste0("ind_aseq"), niveles)
  rm(niveles)
}

ind_aseq <- ind_aseq %>% 
  rename(estr_aseq = estrato_aseq...stratumID...)


# Estratificacion de satisfaccion -----------------------------------------

library(stratification)
ind_satis <- read_csv("p2_satisfaccion.csv") 


ind_satis <- ind_satis %>% 
  rename(estr_satis = estratos...stratumID...)





# Csv con estratos --------------------------------------------------------

csv <- select(p2_acc, vid, estr_acc) %>% 
  full_join(select(p2_habit,estr_habit, vid), by = "vid") %>% 
  full_join(select(p2_serv, estr_serv, vid), by = "vid") %>% 
  full_join(select(p2_ubi, estr_ubi, vid), by = "vid") %>% 
  full_join(select(ind_aseq, estr_aseq,vid), by = "vid") %>% 
  full_join(select(ind_cultura, estr_cul, vid), by = "vid") %>% 
  full_join(select(ind_ten, estr_ten, vid), by = "vid") %>% 
  full_join(select(ind_satis, estr_satis, vid), by = "vid")



write_csv(csv, "estratos_onu.csv")
