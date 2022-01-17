setwd("C:/Users/leonh/Documents/Proyectos Personales/Premio INFONAVIT/Github/envi")
library(tidyverse)
library(dplyr)
datos_onu = read.csv("formatted-data/datos_onu.csv")
str(datos_onu)

# DEFINE VARIABLES TO REFACTOR
V2R = c(names(datos_onu[,substr(names(datos_onu), 1,4) == 'P4_7' |
                        substr(names(datos_onu), 1,4) == 'P4_8' |
                          substr(names(datos_onu), 1,4) == 'P6_5' |
                          substr(names(datos_onu), 1,4) == 'P6_7' |
                          substr(names(datos_onu), 1,4) == 'P6_9']),
        "P4_12", "P4_13", "P4_14", "P4_15", "P4_16", "P4_17", "P6_6")

# DEFINE THESE VARIABLES AS FACTOR
datos_onu[V2R] = lapply(datos_onu[V2R], factor)
str(datos_onu)


# ELEMENTS WITH OPTIONS SÍ, NO, NO SABE (YES, NO, DOESN'T KNOW) CHOSEN. 
SNNS = names(datos_onu[,substr(names(datos_onu), 1,4) == 'P4_7' | substr(names(datos_onu), 1,4) == 'P4_8'])
head(datos_onu[SNNS])
# REFACTOR VARIABLES SO THAT NO = 1, DOESN'T KNOW = 2, YES = 3.
datos_onu = datos_onu %>% mutate_at(c(SNNS), funs(recode(., '1'=3,'2'=1,'9'=2)))
head(datos_onu[SNNS])

# ELEMENTS WITH 2 CHOICES, THE FACTORS OF WHICH MUST BE REVERSED
E2F = c("P4_16", names(datos_onu[,substr(names(datos_onu), 1,4) == 'P6_7']))
head(datos_onu[E2F])
# REFACTOR VARIABLES SO THAT NO = 1, YES = 2.
datos_onu = datos_onu %>% mutate_at(c(E2F), funs(recode(., '1'=2, '2'=1)))
head(datos_onu[E2F])

# ELEMENTS WITH 3 CHOICES, THE FACTORS OF WHICH MUST BE REVERSED.
E3F = c("P4_12", "P4_13")
head(datos_onu[E3F])
# REVERSE FACTORS
datos_onu = datos_onu %>% mutate(P4_13 = recode(P4_13, '1'=3,'2'=2,'3'=1))
datos_onu = datos_onu %>% mutate(P4_12 = recode(P4_12, '1'=4,'2'=3,'3'=2,'4'=1))
head(datos_onu[E3F])

# ELEMENT WITH 5 CHOICES, THE FACTORS OF WHICH MUST BE REVERSED.
head(datos_onu['P4_14'])
# REVERSE FACTORS
datos_onu = datos_onu %>% mutate(P4_14 = recode(P4_14, '1'=5,'2'=4,'3'=3,'4'=2,'5'=1))
head(datos_onu['P4_14'])

# ELEMENTS WITH 5 CHOICES, ONE OF WHICH IS 'UNSPECIFIED', AND THE FACTORS OF WHICH MUST BE REVERSED.
E5FU = c(names(datos_onu[, substr(names(datos_onu), 1,4) == 'P6_5']), "P6_6")
head(datos_onu[E5FU])
# REFACTOR VARIABLES SO THAT NOTHING = 1, LITTLE = 2, SOME = 3, VERY = 4, UNSPECIFIED = 9.
datos_onu = datos_onu %>% mutate_at(c(E5FU), funs(recode(., '1'=4,'2'=3,'3'=2,'4'=1, '9'=9)))
head(datos_onu[E5FU])

# ELEMENT WITH 6 CHOICES, ONE OF WHICH IS 'UNSPECIFIED', AND THE FACTORS OF WHICH MUST BE REVERSED.
head(datos_onu['P4_15'])
# REFACTOR VARIABLES SO THAT NO SEWAGE = 1, BODY OF WATER = 2, HILL/CRACK = 3, BIODIGESTOR = 4, PUBLIC SEWAGE = 5, UNSPECIFIED = 9.
datos_onu = datos_onu %>% mutate(P4_15 = recode(P4_15, '1'=6,'2'=5,'3'=4,'4'=3,'5'=2, '9'=1))
head(datos_onu['P4_15'])

# ELEMENT WITH 6 CHOICES, ONE OF WHICH IS 'UNSPECIFIED', TWO OF WHICH CANNOT BE PROPERLY RANKED, AND THE FACTORS OF WHICH MUST BE REVERSED.
head(datos_onu['P4_17'])
# REFACTOR VARIABLES SO THAT OTHER FUEL = 1, DON'T COOK = 2, COAL OR LUMBER = 3, GAS OR ELECTRICITY = 4, UNSPECIFIED = 9.
datos_onu = datos_onu %>% mutate(P4_17 = recode(P4_17, '1'=4,'2'=3,'3'=4,'4'=1,'5'=2, '9'=9))
head(datos_onu['P4_17'])

# TYPE OF OWNERSHIP
head(datos_onu['tipo_tenencia'])
# REVERSE VALUES
datos_onu = datos_onu %>% mutate(tipo_tenencia = recode(tipo_tenencia, '1'=6,'2'=5,'3'=4,'4'=3,'5'=2, '666'=1))
head(datos_onu['tipo_tenencia'])

