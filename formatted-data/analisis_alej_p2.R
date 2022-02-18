#rm(list=ls())
library(tidyverse)
library(ggplot2)
library(corrplot)


codigos <- read_csv("codigos_identidad.csv", col_names = c("ENT", "Estado"))
datos <- read_csv("tvivienda.csv")  %>% 
  left_join(codigos, by = "ENT")

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



# Join --------------------------------------------------------------------
datos_join <- datos %>% 
  left_join(select(p2_acc, p2_acc, estr_acc, vid), by = "vid") %>% 
  left_join(select(p2_habit, p2_habit, estr_habit, vid), by = "vid") %>% 
  left_join(select(p2_serv, p2_serv, estr_serv, vid), by = "vid") %>% 
  left_join(select(p2_ubi, p2_ubi,estr_ubi, vid), by = "vid")




datos_join %>%
  select(p2_acc, p2_habit, p2_ubi, p2_serv) %>%
  log() %>%
  pairs()

datos_join %>%
  filter((p2_ubi == max(p2_ubi)))

datos_join %>%
  select(p2_acc, p2_habit, p2_serv, p2_ubi) %>%
  cor() %>%
  corrplot(method = "number")

# 
# 
# # Cuantiles acc -----------------------------------------------------------
# 
# cuantiles <- quantile(datos_join$p2_acc)
# 
# datos <- datos_join %>%
#   mutate(
#     cuantil_acc = ifelse(
#       p2_acc < cuantiles[2], 1, ifelse(
#         p2_acc < cuantiles[3], 2, ifelse(
#           p2_acc < cuantiles[4],3, ifelse(
#             p2_acc < cuantiles[5], 4, 0)
#         )
#       )
#     )
#   )


# 
# # Cuantiles Serv ----------------------------------------------------------
# 
# cuantiles <- quantile(datos_join$p2_serv)
# 
# datos <- datos %>%
#   mutate(
#     cuantil_serv = ifelse(
#       p2_serv < cuantiles[2], 1, ifelse(
#         p2_serv < cuantiles[3], 2, ifelse(
#           p2_serv < cuantiles[4],3, ifelse(
#             p2_serv < cuantiles[5], 4, 0 )
#         )
#       )
#     )
#   )
# 
# # Cuantiles habit ----------------------------------------------------------
# 
# cuantiles <- quantile(datos_join$p2_habit)
# 
# datos <- datos %>%
#   mutate(
#     cuantil_habit = ifelse(
#       p2_habit < cuantiles[2], 1, ifelse(
#         p2_habit < cuantiles[3], 2, ifelse(
#           p2_habit < cuantiles[4],3, ifelse(
#             p2_habit < cuantiles[5], 4, 0 )
#         )
#       )
#     )
#   )
# 
# 
# # Cuantiles ubi ----------------------------------------------------------
# 
# cuantiles <- quantile(datos_join$p2_ubi)
# 
# datos <- datos %>%
#   mutate(
#     cuantil_ubi = ifelse(
#       p2_ubi < cuantiles[2], 1, ifelse(
#         p2_ubi < cuantiles[3], 2, ifelse(
#           p2_ubi < cuantiles[4],3, ifelse(
#             p2_ubi < cuantiles[5], 4, 0 )
#         )
#       )
#     )
#   )


# Casos ------------------------------------------------------------
#hago una resta del maximo valor - el resto. La unica situación en la que me va a 
#dar 1, es en el 4-1-1-1

datos %>%
  group_by(ENT) %>%
    mutate(
    viviendas_totales = n()
  ) %>% 
  ungroup()

ventajas <- datos %>%
  mutate(caso_serv = cuantil_serv - cuantil_acc - cuantil_habit - cuantil_ubi) %>%
  mutate(ventaja_serv = if_else(caso_serv >= 0, 1,  0)) %>%
  mutate(caso_ubi = cuantil_ubi - cuantil_acc - cuantil_habit - cuantil_serv) %>%
  mutate(ventaja_ubi = if_else(caso_ubi >= 0, 1,  0)) %>%
  mutate(caso_acc = cuantil_acc - cuantil_serv - cuantil_habit - cuantil_ubi) %>%
  mutate(ventaja_acc = if_else(caso_acc >= 0, 1,  0)) %>%
  mutate(caso_habit = cuantil_habit - cuantil_serv - cuantil_acc - cuantil_ubi) %>%
  mutate(ventaja_habit = if_else(caso_habit >= 0, 1,  0)) %>%
  mutate(suma = cuantil_acc + cuantil_serv + cuantil_habit + cuantil_ubi) %>%
  mutate(ventaja_nada = if_else(suma < 8, 1,  0)) %>%
  mutate(peores = if_else(suma == 4, 1,  0))


pairs(prop_ventajas)


  
#write_csv(serv_bien, "servicios_outliers.csv")

ubi_bien <- datos %>%
  mutate(caso = cuantil_ubi - cuantil_acc - cuantil_habit - cuantil_serv) %>%
  filter(caso >= 0)
#write_csv(ubi_bien, "ubicacion_outliers.csv")

acc_bien <- datos %>%
  mutate(caso = cuantil_acc - cuantil_serv - cuantil_habit - cuantil_ubi) %>%
  filter(caso >= 0)
#write_csv(acc_bien, "accesibilidad_outliers.csv")

habit_bien <- datos %>%
  mutate(caso = cuantil_habit - cuantil_serv - cuantil_acc - cuantil_ubi) %>%
  filter(caso >= 0)
#write_csv(habit_bien, "habitabilidad_outliers.csv")





# Obtengo proporciones (a partir de df ventajas) ----------------------------------------------------
prop_ventajas <- ventajas %>%
  group_by(ENT) %>%
  summarise(
    prop_serv = mean(ventaja_serv == 1),
    prop_ubi = mean(ventaja_ubi == 1),
    prop_acc = mean(ventaja_acc == 1),
    prop_habit = mean(ventaja_habit == 1),
    prop_nada = mean(ventaja_nada == 1),
    prop_peores = mean(peores == 1)
  )





# Mapa de calor de  -------------------------------------------------------

# los cuantiles para estas variables





# Ingreso -----------------------------------------------------------------






# prueba ------------------------------------------------------------------


datos_join %>%
  ggplot() +
  geom_density(aes(x= p2_acc))


datos_join %>%
  ggplot() +
  geom_density(aes(x= p2_habit))

prop_ventajas %>%
  ggplot()+
  geom_density(aes(x= prop_serv))

prop_ventajas %>%
  ggplot()+
  geom_density(aes(x= prop_ubi))

prop_ventajas %>%
  ggplot()+
  geom_density(aes(x= prop_acc))

prop_ventajas %>%
  ggplot()+
  geom_density(aes(x= prop_habit))




# Estratificación tenencia -------------------------------------------------
library(stratification)
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


# Vulnerabilidad tenencia -------------------------------------------------
#Bajo nuestra investigación, una persona que vive en una buena ubicación, pero
#no cuenta con seguridad en la tenencia, se encuentra en una situación vulnerable. 
#Busco esos casos

riesgo_tenencia <- datos_join %>% 
  full_join(select(ind_ten, estr_ten, vid), by ="vid") %>% 
  filter(estr_ubi %in% c("Muy Alta", "Alta"),
         estr_ten %in% c("Baja", "Muy Baja"))


#Aguascalientes es el estado en el que existe mayor riesgo en tenencia.
riesgo_tenencia %>%
  count(Estado)
riesgo_tenencia %>% 
   group_by(TLOC) %>% 
  summarise(n())


# Regresión logistica,  tenencia ------------------------------------------
library(lars)
log_ten_dat <- datos_join %>% 
  mutate(riesgo_ten = ifelse(datos_join$vid %in% riesgo_tenencia$vid, 1, 0))









