#rm(list=ls())
#Load libraries
library(tidyverse)

#Open data
datos_onu <- read_csv("datos_onu.csv")

distinct(datos_onu, FACTOR)

#Application of expansion factor
x <- datos_onu[ unlist( mapply( rep , seq( nrow( datos_onu ) ) , 
                                datos_onu$FACTOR ) ) , ]

#CSV
write_csv(x, "Svy_datos_onu.csv")


