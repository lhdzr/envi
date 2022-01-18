#rm(list=ls())
library(tidyverse)
library(p2distance)
library(stratification)
library(gtools)
library(beepr)



# Access to infraestructure and services-----------------------------------------------------------------------
indicadores <- read_csv("datos_onu.csv") %>%
  select(vid,
         P4_12,
         P4_14,
         P4_15,
         P4_16,
         P4_17)

table(indicadores$P4_14)
indicadores_na <- na.omit(indicadores)
minimos <- rep(1,5)

indice <- p2distance(matriz = as.matrix(indicadores[,2:6]),
                     reference_vector = minimos,
                     iterations = 50)

resultados <- cbind(indicadores_na,indice[["p2distance"]])


strata.DH_2020 <- strata.cumrootf(resultados[,7],
                                  n = length(resultados$p2distance.2),
                                  Ls = 5)


assign(paste0("resultados"), data.frame(resultados, strata.DH_2020[["stratumID"]])) 




# Se cambian los levels del grado de marginación  
for(i in 1){
  niveles = get(paste0("resultados")) 
  levels(niveles[,8]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("resultados"), niveles)
  rm(niveles)
}

write.csv(resultados, file = "p2_2.csv")


# 4 -----------------------------------------------------------------------
rm(list=ls())
indicadores <- read_csv("datos_onu.csv") %>%
  select(vid,
         P4_4,
         P4_5,
         P4_6,
         P4_7_1,
         P4_7_2,
         P4_7_3,
         P4_8_1,
         P4_8_2,
         P4_8_3,
         P4_8_4)

mean(is.na(indicadores))

minimos <- rep(1,10)

indice <- p2distance(matriz = as.matrix(indicadores[,2:11]),
                     reference_vector = minimos,
                     iterations = 50)

resultados <- cbind(indicadores,indice[["p2distance"]])


strata.DH_2020 <- strata.cumrootf(resultados[,12],
                                  n = length(resultados$p2distance),
                                  Ls = 5)


assign(paste0("resultados"), data.frame(resultados, strata.DH_2020[["stratumID"]])) 




# Se cambian los levels del grado de marginación  
for(i in 1){
  niveles = get(paste0("resultados")) 
  levels(niveles[,13]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("resultados"), niveles)
  rm(niveles)
}

write.csv(resultados, file = "p2_4.csv")


# 5 -----------------------------------------------------------------------
rm(list=ls())
indicadores <- read_csv("datos_onu.csv") %>%
  select(vid,
         P6_7_1,
         P6_7_2,
         P6_7_3,
         P6_7_4,
         P6_9_1)

mean(is.na(indicadores))

minimos <- rep(1,5)

indice <- p2distance(matriz = as.matrix(indicadores[,2:6]),
                     reference_vector = minimos,
                     iterations = 50)

resultados <- cbind(indicadores,indice[["p2distance"]])


estratos <- strata.cumrootf(resultados[,7],
                                  n = length(resultados$p2distance),
                                  Ls = 5)


assign(paste0("resultados"), data.frame(resultados, estratos[["stratumID"]])) 




# Se cambian los levels del grado de marginación  
for(i in 1){
  niveles = get(paste0("resultados")) 
  levels(niveles[,8]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("resultados"), niveles)
  rm(niveles)
}

write.csv(resultados, file = "p2_4.csv")


# 7 -----------------------------------------------------------------------
rm(list=ls())
indicadores <- read_csv("datos_onu.csv") %>%
  select(vid,
         P6_5_1,
         P6_5_2,
         P6_5_3,
         P6_5_4,
         P6_5_5,
         P6_5_6,
         P6_9_2,
         P6_9_3,
         P6_9_4,
         P6_9_5,
         P6_9_6,
         P6_9_7)

mean(is.na(indicadores))

minimos <- rep(1,12)

indice <- p2distance(matriz = as.matrix(indicadores[,2:13]),
                     reference_vector = minimos,
                     iterations = 50)

resultados <- cbind(indicadores,indice[["p2distance"]])


estratos <- strata.cumrootf(resultados[,14],
                            n = length(resultados$p2distance),
                            Ls = 5)


assign(paste0("resultados"), data.frame(resultados, estratos[["stratumID"]])) 




# Se cambian los levels del grado de marginación  
for(i in 1){
  niveles = get(paste0("resultados")) 
  levels(niveles[,15]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("resultados"), niveles)
  rm(niveles)
}

write.csv(resultados, file = "p2_4.csv")





