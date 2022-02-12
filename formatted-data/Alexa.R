library(readr)
library(tidyverse)
data_viv <- read_csv("tvivienda_changes.csv")
tsdem <- read_csv("tsdem.csv")

## Hacer variables de rangos para antiguedad de vivienda##-------------------------------------
# En la encuesta se les pregunta por la antiguedad en edad y en rangos,
# la manera de homogenizar es convertir la de edad a rangos tmabien

# Convertir a NA en la base original 
data_viv$P4_19_2 = ifelse(data_viv$P4_19_2 == 9, NA, 
                                   data_viv$P4_19_2)

data_viv$P4_19_1 = ifelse(data_viv$P4_19_1 == 98, NA,
                          ifelse(data_viv$P4_19_1  == 99, NA,
                          data_viv$P4_19_1))
#------------------------------------------------------------

# Crear variable de rangos 
antiguedad_ran = ifelse(data_viv$P4_19_1 < 1, 1,
                        ifelse(data_viv$P4_19_1  >= 1 & data_viv$P4_19_1 <= 5,2 ,
                               ifelse(data_viv$P4_19_1  >= 6 & data_viv$P4_19_1 <= 10,3 ,
                                      ifelse(data_viv$P4_19_1  >= 11 & data_viv$P4_19_1 <= 20,4 ,
                                             ifelse(data_viv$P4_19_1  >= 21 & data_viv$P4_19_1 <= 30,5,
                                                    ifelse(data_viv$P4_19_1  >= 31 & data_viv$P4_19_1 <= 50,6 ,
                                                           ifelse(data_viv$P4_19_1 > 50, 7,
                                                                  data_viv$P4_19_1)))))))

#------------------------------------------------------------

# Juntar la nueva varible de rangos con la ya existente 
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
#-------------------------------------------------------------------

#Agregar variable a la base original 
data_viv = cbind(data_viv, D$antiguedad_fin)

data_viv = rename(data_viv,
                  rango_anti = `D$antiguedad_fin` )

#-------------------------------------------------------------------

# Crear varible de cuantiles de cantidad de cuartos en la vivienda y cuartos para dormir 

#Agrupar cuantiles de cantidad de cuartos para dormir
quantile(data_viv$P4_10)

q_cuarto <- ifelse(data_viv$P4_10 >=4, 4 ,
                     data_viv$P4_10)

data_viv = cbind(data_viv, q_cuarto)


#Agrupar cuantiles de cantidad de cuartos totales en el hogar 
quantile(data_viv$P4_10A)

q_cuartost <- ifelse(data_viv$P4_10A <= 3, 1,
                ifelse(data_viv$P4_10A == 4, 2,
                       ifelse(data_viv$P4_10A == 5, 3,
                              ifelse(data_viv$P4_10A >= 6, 4,
                              data_viv$P4_10A))))
data_viv = cbind(data_viv, q_cuartost)
#----------------------------------------------------------------------------


## Agrupar area de viviendas ##----------------------------------------------

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
#---------------------------------------------------------------------------------------------------------------------------

# Hacer df de creditos
creditos = data_viv %>%
  select(vid,P5_15_01, P5_15_02,P5_15_03, P5_15_04,P5_15_05, P5_15_06)

# Cambiar valores para sumar cantidad de creditos 
# 2 la respuesta es que no tiene credito
# 1 si tiene ese credito 
creditos[creditos == 2] <- 0  
creditos[creditos == 2] <- 1

creditos = creditos%>%
  rowwise()%>%
  mutate(n_creditos = sum(c(P5_15_01, P5_15_02,P5_15_03, P5_15_04,P5_15_05, P5_15_06)))

table(creditos$n_creditos)

# Agregar columna de numero de creditos por vivienda 
data_viv = cbind(data_viv, creditos$n_creditos)

#-------------------------------------------------------------------------------------------------------------

# Viviendadas que cuenten con algun subsidio 
subsidio = data_viv%>%
  select(vid, P5_15_08,P5_15_09,P5_15_10)

subsidio[subsidio == 2] <- 0
subsidio[subsidio == 1] <- 1

subsidio = subsidio %>%
  rowwise()%>%
  mutate(n_subsidio = sum(c(P5_15_08,P5_15_09)))

data_viv = cbind(data_viv, subsidio$n_subsidio)

#---------------------------------------------------------------------------------------------------------

tsdem$NIV[tsdem$NIV == 99] <- NA

# Grado promedio de escolaridad por vivienda 
escolaridad = tsdem %>%
  select(vid, NIV)%>%
  group_by(vid)%>%
  summarise(esc_prom = mean(NIV, na.rm = TRUE))

data_viv = cbind(data_viv, escolaridad$esc_prom)

write.csv(data_viv, "tvivienda_changes1.csv")
