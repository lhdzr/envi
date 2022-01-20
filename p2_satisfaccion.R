rm(list=ls())
library(tidyverse)
library(p2distance)
library(stratification)

#Open data
indicadores <- read_csv("satisfaction_data.csv")


#Necessary creation of worst case scenarios
min <- rep(1,11)


#Create p2distance
satis_p2 <- p2distance(matriz = as.matrix(satis[,6:16]),
                       reference_vector = min,
                       iterations = 50)

#Binding of satis with p2 indicator
resultados <- cbind(indicadores,satis_p2[["p2distance"]])


#After obtaining p2 values, strata is created in order to divide the values
#into different groups
estratos <- strata.cumrootf(resultados[,17],
                            n = length(resultados$p2distance.3),
                            Ls = 5)


assign(paste0("resultados"), data.frame(resultados, estratos[["stratumID"]])) 


#Levels are created, from "Very low" to "Very high" 
for(i in 1){
  niveles = get(paste0("resultados")) 
  levels(niveles[,18]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("resultados"), niveles)
  rm(niveles)
}

write_csv(resultados,"p2_satisfaccion")



