#rm(list=ls())
library(tidyverse)
library(ggplot2)
library(reshape2)

#Open data
datos <- read_csv("tvivienda.csv")
satis <- read_csv("p2_satisfaccion.csv")
codigos <- read_csv("codigos_identidad.csv", 
                    col_names = c("codigo", "entidad"))

#Connect satisfaction data to main df
colnames(satis)[18]= "estrato"

datos <- datos %>%
  left_join(satis, by = "vid") %>%
  left_join(codigos, by = c("ENT" = "codigo"))



#See satisfaction by state, both with direct question and with our index
#Direct question
ent_satis <- datos %>%
  group_by(entidad) %>%
  summarise(P6_8_prom = mean(P6_8),
            p2_prom = mean(na.omit(p2distance.3)),
            diferencia = P6_8_prom - p2_prom)

  

ent_satis %>%
  ggplot() +
  geom_col(aes(x=reorder(entidad,desc(diferencia)),y = diferencia)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))+
  xlab("Estado") +
  ylab("Diferencia") +
  labs(title = "Diferencia entre satisfacción reportada y obtenida del índice")



ent_satis %>%
  ggplot() +
  geom_col(aes(x=reorder(entidad, p2_prom),y = p2_prom), alpha = 0.3, fill = "red") +
  geom_col(aes(x=reorder(entidad, p2_prom),y = P6_8_prom), alpha = 0.3, fill = "blue")+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))+
  xlab("Estado") +
  ylab("Diferencia") +
  labs(title = "Diferencia entre satisfacción reportada y obtenida del índice")+
  theme(legend.position = "right")



dfm <- melt(df[,c('ENT','P6_8_prom','p2_prom')],id.vars = 1)          

ggplot(dfm,aes(x = Input,y = value)) +
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  scale_y_log10()
           