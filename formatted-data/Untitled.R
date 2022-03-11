#rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)

estratos <- read_csv("estratos_onu.csv")
codigos <- read_csv("codigos_identidad.csv", col_names = c("ENT", "Estado"))
datos <- read_csv("tvivienda.csv")  %>% 
  left_join(codigos, by = "ENT")





porcentaje <- c(52.36,48.31,30.14,21.38,7.29,13.98,33.49,39.43,25.59,27.93)
nombres <- c("Accesibilidad P", "Accesibilidad C", "Habitabilidad P", "Habitabilidad C",
             "Servicios P", "Servicios C", "Ubicación P", "Ubicación C", 
             "Satisfacción P", "Satisfacción C")

tabla <- as.data.frame(cbind(nombres, porcentaje))

bien <- tabla %>% 
  mutate(tipo = ifelse(str_detect(tabla$nombres,"P"), "P","C"),
         nombre = str_remove(tabla$nombres, "P|C")) 
bien$porcentaje <- as.numeric(bien$porcentaje)


g<- ggplot(bien)+
  geom_col(aes(x = nombre, y = porcentaje, fill= tipo), position = "dodge")+
  labs(title= "Porcentaje de casas con una baja clasificación, por índice",
       x = "Índice",
       y = "Porcentaje de hogares con baja clasificación") +
  scale_fill_manual(values = c(C = "#F2C1B6", P = "#D9183B" ),
                    labels = c("CATPCA", "DP2"), 
                    name = "Crédito") +
  theme(panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                        size = 2),
        plot.title = element_text(size = 20, face = "bold", color = "black"),
        plot.subtitle =  element_text(size = 10, face = "bold", color = "black")
        )
  
ggsave(g, units = "px", width = 300, height = 300)

  
  
  
  
  
  
  
  

  
  
  





  
  
