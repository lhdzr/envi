# Estratificacion

datos <- read.csv("datos_onu_names.csv")

habitabilidad <- read.csv("Indicehabitabilidad.csv")
servicios <- read.csv("Indiceservicios.csv")
ubicacion<- read.csv("Indiceubicacion.csv")
accesibilidad<- read.csv("IndiceAcc.csv")
satisfaccion <- read.csv("Satisfaction_CATPCA.csv")




library("tidyverse")

datosINDICES <- cbind(datos, habitabilidad$IndiceHABITABILIDAD, servicios$IndiceSERVICIOS, ubicacion$IndiceUBICACION, accesibilidad$IndiceACCESIBILIDAD)


#Estratificacion
library("stratification")

#HABITABILIDAD

strata.CATPCAHab <- strata.cumrootf(datosINDICES[,48],
                                  n = length(habitabilidad),
                                  Ls = 5)

assign(paste0("datosINDICES"), data.frame(datosINDICES, strata.CATPCAHab[["stratumID"]])) 


for(i in 1){
  niveles = get(paste0("datosINDICES")) 
  levels(niveles[,52]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("datosINDICESs"), niveles)
  rm(niveles)
}

#SERVICIOS

strata.CATPCASer <- strata.cumrootf(datosINDICES[,49],
                                    n = length(servicios),
                                    Ls = 5)

assign(paste0("datosINDICES"), data.frame(datosINDICES, strata.CATPCASer[["stratumID"]])) 


for(i in 1){
  niveles = get(paste0("datosINDICES")) 
  levels(niveles[,53]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("datosINDICES"), niveles)
  rm(niveles)
}



#UBICACION
strata.CATPCAUb <- strata.cumrootf(datosINDICES[,50],
                                    n = length(ubicacion),
                                    Ls = 5)

assign(paste0("datosINDICES"), data.frame(datosINDICES, strata.CATPCAUb[["stratumID"]])) 


for(i in 1){
  niveles = get(paste0("datosINDICES")) 
  levels(niveles[,54]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("datosINDICES"), niveles)
  rm(niveles)
}




#ACCESIBILIDAD

strata.CATPCAAcc <- strata.cumrootf(datosINDICES[,51],
                                   n = length(accesibilidad),
                                   Ls = 5)

assign(paste0("datosINDICES"), data.frame(datosINDICES, strata.CATPCAAcc[["stratumID"]])) 


for(i in 1){
  niveles = get(paste0("datosINDICES")) 
  levels(niveles[,55]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("datosINDICES"), niveles)
  rm(niveles)
}

write.csv(datosINDICES, "IndicesCATPCA_Estratificados.csv")

