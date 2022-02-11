library(readr)
data_viv <- read_csv("tvivienda_changes.csv")

## Hacer variables de rangos para antiguedad de vivienda##----

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


### Agrupar cuantiles de cantidad de cuartos para dormir ###----
quantile(data_viv$P4_10)

q_cuarto <- ifelse(data_viv$P4_10 >=4, 4 ,
                     data_viv$P4_10)

data_viv = cbind(data_viv, q_cuartos)

##Agrupar cuantiles de cantidad de cuartos totales en el hogar ##----
quantile(data_viv$P4_10A)

q_cuartost <- ifelse(data_viv$P4_10A <= 3, 1,
                ifelse(data_viv$P4_10A == 4, 2,
                       ifelse(data_viv$P4_10A == 5, 3,
                              ifelse(data_viv$P4_10A >= 6, 4,
                              data_viv$P4_10A))))
data_viv = cbind(data_viv, q_cuartost)

sum(is.na(data_viv$P4_13))
table(data_viv$P4_13)

## Agrupar area de viviendas ##----

# Convertir a NA en la base original 
data_viv$P4_20_1 = ifelse(data_viv$P4_20_1 == 998, NA,
                          ifelse(data_viv$P4_20_1  == 999, NA,
                                 data_viv$P4_20_1))

data_viv$P4_20_2 = ifelse(data_viv$P4_20_2 == 99, NA,
                          data_viv$P4_20_2)

ran_area <- ifelse(data_viv$P4_20_1<= 60, 1,
                   ifelse(data_viv$P4_20_1 > 60 & data_viv$P4_20_1 <=90 , 2,
                          ifelse(data_viv$P4_20_1 > 90 & data_viv$P4_20_1 <= 120, 3,
                                 ifelse(data_viv$P4_20_1 > 120 & data_viv$P4_20_1 <=160, 4,
                                        ifelse(data_viv$P4_20_1 > 160 & data_viv$P4_20_1 <=200, 5,
                                               ifelse(data_viv$P4_20_1 > 200 & data_viv$P4_20_1 <= 250,6,
                                                      ifelse(data_viv$P4_20_1 >250 & data_viv$P4_20_1 <= 300, 7,
                                                             ifelse(data_viv$P4_20_1 > 300 & data_viv$P4_20_1 <= 500,8,
                                                                    ifelse(data_viv$P4_20_1 > 500, 9,
                                                                           data_viv$P4_20_1)))))))))
df = as.data.frame(cbind(data_viv$P4_20_2, ran_area))
df = df %>%
  rowwise()%>%
  mutate(rango_area = sum(c(V1, ran_area),na.rm = T))

df$rango_area = replace(df$rango_area,df$rango_area == 0, NA)

data_viv <- cbind(data_viv, df$rango_area)

##

sum(is.na(data_viv$P5_15_1))
table(data_viv$P5_15_1)


