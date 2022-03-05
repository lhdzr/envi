#rm(list=ls())
library(tidyverse)

datos <- read_csv("estratos_onu.csv")
vivienda <- read_csv("tvivienda.csv")

vids <- datos %>% 
  select(vid)


# riesgos ------------------------------------------------------
riesgo_tenencia <- datos %>% 
  filter(estr_ubi %in% c("Muy Alta", "Alta"),
         estr_ten %in% c("Baja", "Muy Baja")) %>% 
  mutate(vid2 = vid) %>% 
  select(riesgo_tenencia = vid2, vid)


riesgo_cc <- datos %>% 
  filter(estr_habit %in% c("Muy Baja", "Baja"),
         estr_serv %in% c("Baja", "Muy Baja")) %>% 
  mutate(vid2 = vid) %>%
  select(riesgo_cc = vid2, vid)

riesgo_disc <- datos %>% 
  filter(estr_acc %in% c("Muy Baja", "Baja"),
         estr_ubi %in% c("Baja", "Muy Baja")) %>% 
  mutate(vid2 = vid) %>%
  select(riesgo_disc = vid2, vid)


riesgo_desempleo <- datos %>% 
  filter(estr_aseq %in% c("Muy Baja", "Baja"),
         estr_ubi %in% c("Baja", "Muy Baja")) %>% 
  mutate(vid2 = vid) %>%
  select(riesgo_desempleo = vid2, vid)


riesgos <- vids %>% 
  full_join(riesgo_cc, by = "vid") %>% 
  full_join(riesgo_desempleo, by = "vid") %>% 
  full_join(riesgo_disc, by = "vid") %>% 
  full_join(riesgo_tenencia, by = "vid") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(across(riesgo_cc:riesgo_tenencia,~replace(.,.!=0,1))) %>% 
  mutate(riesgo_cc = as.numeric(riesgo_cc),
         riesgo_desempleo = as.numeric(riesgo_desempleo),
         riesgo_disc = as.numeric(riesgo_disc),
         riesgo_tenencia = as.numeric(riesgo_tenencia))



# Join con data -----------------------------------------------------------

full_datos <- riesgos %>% 
  full

# Riesgo en tenencia ------------------------------------------------------

ten <- riesgo_tenencia %>% 
  left_join 






