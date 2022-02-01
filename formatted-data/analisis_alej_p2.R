rm(list=ls())
library(tidyverse)
library(ggplot2)

datos <- read_csv("tvivienda.csv") 
p2_acc <- read_csv("p2_accesibilidad.csv") %>%
  rename(p2_acc = p2distance.2)
p2_habit <- read_csv("p2_habitabilidad.csv")%>%
  rename(p2_habit = p2distance.3)
p2_serv <- read_csv("p2_servicios.csv") %>%
  rename(p2_serv = p2distance.2)
p2_ubi <- read_csv("p2_ubicacion.csv") %>%
  rename(p2_ubi = p2distance.3)

datos_join <- datos %>% 
  left_join(select(p2_acc, p2_acc, vid), by = "vid") %>% 
  left_join(select(p2_habit, p2_habit, vid), by = "vid") %>% 
  left_join(select(p2_serv, p2_serv, vid), by = "vid") %>% 
  left_join(select(p2_ubi, p2_ubi, vid), by = "vid")



# Cuantiles acc -----------------------------------------------------------

cuantiles <- quantile(datos_join$p2_acc)

datos <- datos_join %>%
  mutate(
    cuantil_acc = ifelse(
      p2_acc < cuantiles[2], 1, ifelse(
        p2_acc < cuantiles[3], 2, ifelse(
          p2_acc < cuantiles[4],3, ifelse(
            p2_acc < cuantiles[5], 4, 0)
        )
      )
    )
  )



# Cuantiles Serv ----------------------------------------------------------

cuantiles <- quantile(datos_join$p2_serv)

datos <- datos %>%
  mutate(
    cuantil_serv = ifelse(
      p2_serv < cuantiles[2], 1, ifelse(
        p2_serv < cuantiles[3], 2, ifelse(
          p2_serv < cuantiles[4],3, ifelse(
            p2_serv < cuantiles[5], 4, 0 )
        )
      )
    )
  )

# Cuantiles habit ----------------------------------------------------------

cuantiles <- quantile(datos_join$p2_habit)

datos <- datos %>%
  mutate(
    cuantil_habit = ifelse(
      p2_habit < cuantiles[2], 1, ifelse(
        p2_habit < cuantiles[3], 2, ifelse(
          p2_habit < cuantiles[4],3, ifelse(
            p2_habit < cuantiles[5], 4, 0 )
        )
      )
    )
  )


# Cuantiles ubi ----------------------------------------------------------

cuantiles <- quantile(datos_join$p2_ubi)

datos <- datos %>%
  mutate(
    cuantil_ubi = ifelse(
      p2_ubi < cuantiles[2], 1, ifelse(
        p2_ubi < cuantiles[3], 2, ifelse(
          p2_ubi < cuantiles[4],3, ifelse(
            p2_ubi < cuantiles[5], 4, 0 )
        )
      )
    )
  )



# Casos ------------------------------------------------------------
#hago una resta del maximo valor - el resto. La unica situación en la que me va a 
#dar 1, es en el 4-1-1-1

serv_bien <- datos %>%
  mutate(caso_serv = cuantil_serv - cuantil_acc - cuantil_habit - cuantil_ubi) %>%
  filter(caso_serv >= 0)
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
