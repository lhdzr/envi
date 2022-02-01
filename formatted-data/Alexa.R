library(readr)
data_viv <- read_csv("formatted-data/tvivienda_changes.csv")

# Convertir a NA en la base original 
data_viv$P4_19_2 = ifelse(data_viv$P4_19_2 == 9, NA, 
                                   data_viv$P4_19_2)

data_viv$P4_19_1 = ifelse(data_viv$P4_19_1 == 98, NA,
                          ifelse(data_viv$P4_19_1  == 99, NA,
                          data_viv$P4_19_1))


# Crear variable para generar rangos 
antiguedad_ran = ifelse(data_viv$P4_19_1 < 1, 1,
                        ifelse(data_viv$P4_19_1  >= 1 & data_viv$P4_19_1 <= 5,2 ,
                               ifelse(data_viv$P4_19_1  >= 6 & data_viv$P4_19_1 <= 10,3 ,
                                      ifelse(data_viv$P4_19_1  >= 11 & data_viv$P4_19_1 <= 20,4 ,
                                             ifelse(data_viv$P4_19_1  >= 21 & data_viv$P4_19_1 <= 30,5,
                                                    ifelse(data_viv$P4_19_1  >= 31 & data_viv$P4_19_1 <= 50,6 ,
                                                           ifelse(data_viv$P4_19_1 > 50, 7,
                                                                  data_viv$P4_19_1)))))))

D = data_viv[,c("P4_19_1","P4_19_2")]
D = cbind(D, antiguedad_ran)

library(tidyverse)
D = D%>% 
  rowwise()%>%
  mutate(antiguedad_fin = sum(c(P4_19_2, antiguedad_ran),na.rm = T))

# Cambiar ceros a NA   
D$antiguedad_fin = replace(D$antiguedad_fin,D$antiguedad_fin == 0, NA)

table(D$antiguedad_fin)
sum(is.na(D$antiguedad_fin))

data_viv = cbind(data_viv, D$antiguedad_fin)
data_viv = rename(data_viv,
                  rango_anti = `D$antiguedad_fin` )

setwd("C://Users//alexa//Documents//Reto Infonavit//envi/formatted-data")
write.csv(data_viv, "tvivienda_changes.csv")





library("git2rdata")

root <- "~ Reto Infonavit" 
write_vc(data_viv, file = "formatted-data/tvivienda_changes.csv", root = root)

read_vc(file = "formatted-data/tvivienda_changes.csv", root = root)
root <- git2r::repository("~/my_git_repo") # git repository


write.csv(data_viv, "tvivienda_changes.csv")






ggplot( mapping = aes(x = p2_hab$p2distance.3, y = data_viv$rango_anti)) +
  geom_point()




