rm(list=ls())
library(tidyverse)
library(ggplot2)

datos <- read_csv("tvivienda.csv")
p2_serv <- read_csv("p2_servicios.csv")



datos2 <- datos %>% filter(!(P6_10_1 == 9))
table(datos2$P6_10_1)

datos_join <- datos2 %>%
  left_join(p2_habit, by = "vid")

reg1 <- lm(p2distance.3 ~ P6_10_3 ,datos_join)
summary(reg1)


datos_join %>% ggplot(aes(x = P6_10_1, y = p2distance.3))+
  geom_smooth(method = "lm")
