#rm(list=ls())
library(tidyverse)

datos <- read_csv("estratos_onu.csv")
vivienda <- read_csv("tvivienda.csv")

codigos <-
  read_csv("codigos_identidad.csv", col_names = c("ENT", "Estado"))

vivienda <- read_csv("tvivienda.csv")  %>%
  left_join(estratos, by = "vid") %>%
  left_join(codigos, by = "ENT")

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
  full_join(vivienda, by = "vid") %>% 
  left_join(codigos, by = "ENT")

# Riesgo en tenencia ------------------------------------------------------
datos_feif <-
  as_survey_design(full_datos,
                   ids = UPM_DIS,
                   strata = EST_DIS,
                   weights = FACTOR)


vuln <- datos_feif %>% 
  group_by(Estado) %>% 
  summarise(tenencia = survey_mean(riesgo_tenencia ==1, vartype = NULL),
            cc = survey_mean(riesgo_cc ==1, vartype = NULL),
            discapacidad = survey_mean(riesgo_disc ==1, vartype = NULL),
            desempleo = survey_mean(riesgo_desempleo ==1, vartype = NULL))


write_csv(vuln, "riesgos.csv")








