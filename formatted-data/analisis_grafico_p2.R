#rm(list=ls())

library(ggplot2)
library(tidyverse)
library(ggcorrplot)


#Cargo los indices en un solo df
datos <-select(read_csv("p2_accesibilidad.csv"), Accesibilidad = p2distance.2,vid) %>% 
  inner_join(select(read_csv("p2_habitabilidad.csv"),
                    Habitabilidad = p2distance.3, vid), by = "vid" ) %>% 
  inner_join(select(read_csv("p2_satisfaccion.csv"), Satisfacción = p2distance.3,vid), by = "vid" ) %>% 
  inner_join(select(read_csv("p2_servicios.csv"),Servicios = p2distance.2,vid), by = "vid") %>% 
  inner_join(select(read_csv("p2_ubicacion.csv"),Ubicación = p2distance.3,vid), by = "vid") %>% 
  inner_join(select(filter(read_csv("indice_tenencia.csv"), P5_1 !=1), 
                    Tenencia = indice,vid), by = "vid") %>% 
  inner_join(select(read_csv("tvivienda.csv"), ENT, vid, P4_3), by = "vid") %>% 
  inner_join(read_csv("codigos_identidad.csv"), by = "ENT")


# Histogramas de cada uno -------------------------------------------------
#Escribe la variable de interes, las opciones son;
#p2_acces, p2_habit, p2_satis, p2_serv, p2_ubi
var <- datos$Ubicación

ggplot(datos, aes(x = var)) +
  geom_histogram(bins = 30) 



# Correlaciones -----------------------------------------------------------
# Selecciona, igual que antes a las variables de interes

# Correlation matrix
corr <- round(cor(select(datos,!vid)), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "full", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("#ff6978", "#fffcf9", "#b1ede8"), 
           title="Correlograma entre los índices", 
           ggtheme= theme_classic)



# Relaciones lineales -----------------------------------------------------
var1 <- datos$Satisfacción
var2 <- datos$Ubicación



ggplot(datos, aes(x = var2, y = var1))+
  geom_point()




# Indice de indices -------------------------------------------------------
library(p2distance)

reference <- c(min(datos$Accesibilidad), min(datos$Habitabilidad), 
               min(datos$Servicios), min(datos$Ubicación), min(datos$Tenencia))
matriz <- as.matrix(select(datos,Accesibilidad, Habitabilidad,Servicios,Ubicación, Tenencia))

super_indice <- p2distance(matriz, reference)

datos_ind <- cbind(select(datos, vid), super_indice["p2distance"]) %>% 
  inner_join(datos, by = "vid")

ggplot(datos_ind, aes(x = Satisfacción, y = p2distance.2))+
  geom_point()





# regresion ---------------------------------------------------------------
datos <- as_tibble(lapply(datos, function(x) ifelse(x ==0, 0.0000001, x)))

lm <- lm(log(Satisfacción) ~ log(Habitabilidad) + log(Servicios) + log(Ubicación) + 
           log(Accesibilidad) + log(Tenencia) +  Accesibilidad, datos)

summary(lm)

slm <- lm(Satisfacción ~ p2distance.2 ,datos_ind)
summary(slm)



plot(lm)


# Regresion con interaccion -----------------------------------------------
datos$Estado <- as.factor(datos$Estado)
datos$P4_31 <- as.factor(ifelse(datos$P4_3 %in% c(1,2,3,7), "casa", "apartamento"))
mean(is.na(datos$P4_31))

lm <- lm(log(Satisfacción) ~ log(Habitabilidad) + log(Servicios) + log(Ubicación) + 
           log(Accesibilidad) + log(Tenencia)  + Estado, datos)
summary(lm)























