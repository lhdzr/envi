rm(list=ls())
library(tidyverse)
library(ggplot2)

datos <- read_csv("tvivienda.csv")
p2_serv <- read_csv("p2_servicios.csv")

datos_join$p2
datos_join <- datos %>%
  left_join(p2_serv, by = "vid")

reg1 <- lm(p2distance.2 ~ P6_10_1 + P5_2  ,datos_join)
summary(reg1)

table(datos$P5_2)
datos_join %>% ggplot(aes(x = P6_10_1, y = p2distance.3))+
  geom_smooth(method = "lm")
