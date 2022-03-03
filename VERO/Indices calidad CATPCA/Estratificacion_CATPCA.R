library("dplyr")
library("readr")
library("tidyverse")
library("stratification")


# Estratificacion

datos <- read.csv("datos_onu_names.csv")

habitabilidad <- read.csv("Indicehabitabilidad.csv")
servicios <- read.csv("Indiceservicios.csv")
ubicacion<- read.csv("Indiceubicacion.csv")
accesibilidad<- read.csv("Indiceaccesibilidad.csv")
datosINDICES <- read.csv("INDICES.csv")



datos_indices <- cbind(datosINDICES, habitabilidad$IndiceHABITABILIDAD, servicios$IndiceSERVICIOS, ubicacion$IndiceUBICACION, accesibilidad$IndiceACCESIBILIDAD)


#Datos ONU 
newdata = datos_indices %>% rename(
  IndiceHABITABILIDAD = habitabilidad$IndiceHABITABILIDAD , 
  IndiceSERVICIOS = servicios$IndiceSERVICIOS, 
  IndiceUBICACION = ubicacion$IndiceUBICACION , 
  IndiceACCESIBILIDAD = accesibilidad$IndiceACCESIBILIDAD)



#Estratificacion


#HABITABILIDAD

strata.CATPCAHab <- strata.cumrootf(datos_indices [,5],
                                  n = length(datos_indices),
                                  Ls = 5)

assign(paste0("datos_indices"), data.frame(datos_indices, strata.CATPCAHab[["stratumID"]])) 


for(i in 1){
  niveles = get(paste0("datos_indices")) 
  levels(niveles[,9]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("datos_indices"), niveles)
  rm(niveles)
}

#SERVICIOS

strata.CATPCASer <- strata.cumrootf(datos_indices[,6],
                                    n = length(servicios),
                                    Ls = 5)

assign(paste0("datos_indices"), data.frame(datos_indices, strata.CATPCASer[["stratumID"]])) 


for(i in 1){
  niveles = get(paste0("datos_indices")) 
  levels(niveles[,10]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("datos_indices"), niveles)
  rm(niveles)
}



#UBICACION
strata.CATPCAUb <- strata.cumrootf(datos_indices[,7],
                                    n = length(ubicacion),
                                    Ls = 5)

assign(paste0("datos_indices"), data.frame(datos_indices, strata.CATPCAUb[["stratumID"]])) 


for(i in 1){
  niveles = get(paste0("datos_indices")) 
  levels(niveles[,11]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("datos_indices"), niveles)
  rm(niveles)
}




#ACCESIBILIDAD

strata.CATPCAAcc <- strata.cumrootf(datos_indices[,8],
                                   n = length(accesibilidad),
                                   Ls = 5)

assign(paste0("datos_indices"), data.frame(datos_indices, strata.CATPCAAcc[["stratumID"]])) 


for(i in 1){
  niveles = get(paste0("datos_indices")) 
  levels(niveles[,12]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("datos_indices"), niveles)
  rm(niveles)
}

write.csv(datos_indices, "IndicesCATPCA_Estratificados.csv")

# ANÁLISIS INDICES

library(ggplot2)

matrizCATPCA <-read.csv("matriz_corr_CPCA.csv")
View(matrizCATPCA)



pairs(~datosINDICES$habitabilidad.IndiceHABITABILIDAD+ datosINDICES$servicios.IndiceSERVICIOS +
        datosINDICES$ubicacion.IndiceUBICACION + datosINDICES$accesibilidad.IndiceACCESIBILIDAD, data = datosINDICES)

library(car)
scatterplotMatrix(~datosINDICES$habitabilidad.IndiceHABITABILIDAD+ datosINDICES$servicios.IndiceSERVICIOS +
                    datosINDICES$ubicacion.IndiceUBICACION + datosINDICES$accesibilidad.IndiceACCESIBILIDAD, data = datosINDICES)

ggplot(satisfaccion, aes(x= IndiceSatisfaccion)) + geom_density(lwd =1.2)





