setwd("/Users/andreasb/Desktop/TEC/INFONAVIT/Code ")
datos_onu = read.csv("satisfaction_data.csv")
library(tidyverse)
library(dplyr)
datos_onu


#Rename

datos_onu = datos_onu %>% mutate(P6_3_1 = recode(P6_3_1, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head (datos_onu['P6_3_1'])
datos_onu = datos_onu %>% mutate(P6_3_2 = recode(P6_3_2, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head (datos_onu['P6_3_2'])
datos_onu = datos_onu %>% mutate(P6_3_3 = recode(P6_3_3, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head (datos_onu['P6_3_3'])
datos_onu = datos_onu %>% mutate(P6_3_5 = recode(P6_3_5, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head (datos_onu['P6_3_5'])
datos_onu = datos_onu %>% mutate(P6_3_4 = recode(P6_3_4, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head (datos_onu['P6_3_4'])
datos_onu = datos_onu %>% mutate(P6_3_6 = recode(P6_3_6, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head (datos_onu['P6_3_6'])
datos_onu = datos_onu %>% mutate(P6_4_1 = recode(P6_4_1, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head (datos_onu['P6_4_1'])
datos_onu = datos_onu %>% mutate(P6_4_2 = recode(P6_4_2, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head (datos_onu['P6_4_2'])
datos_onu = datos_onu %>% mutate( P6_4_3 = recode( P6_4_3, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head (datos_onu['P6_4_3'])
datos_onu = datos_onu %>% mutate( P6_4_4 = recode( P6_4_4, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head (datos_onu['P6_4_4'])
datos_onu = datos_onu %>% mutate( P6_4_5 = recode( P6_4_5, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head (datos_onu['P6_4_5'])

write.csv(datos_onu, file="satisfaction2.csv")


