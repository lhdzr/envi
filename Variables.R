library(tidyverse)
library(readr)
tvivienda <- read_csv("formatted-data/tvivienda.csv")
tsdem <- read_csv("formatted-data/tsdem.csv")


# Overcrowding variable
tvivienda$hacinamiento = tvivienda$P1_1 / tvivienda$P4_10


tvivienda$ran_hacinamiento = ifelse(
  tvivienda$hacinamiento >= 2.5,
  1,
  ifelse(tvivienda$hacinamiento < 2.5, 2,
         tvivienda$hacinamiento)
)

#Process to create householod problems
# Select interest variables
p_est = tvivienda[, c("P4_25_1",
                      "P4_25_2",
                      "P4_25_3",
                      "P4_25_4",
                      "P4_25_5",
                      "P4_25_6",
                      "P4_25_7")]


# Change 2 to 0 and 9 to 0.5
p_est[p_est == 2] <-  0 # 2 means NO problems
p_est[p_est == 9] <-  0.5 # 9 means they don't know




# Sum problems by household
tvivienda$problemas_vivienda = apply(p_est, 1, sum)


# Create variable with recode sequence
tvivienda$tipo_tenencia = ifelse(tvivienda$P5_1 == 1, 3,
                                 ifelse(tvivienda$P5_1 == 2, 4,
                                        ifelse(tvivienda$P5_1 == 3, 4,
                                          ifelse(tvivienda$P5_1 == 4, 2,
                                                 ifelse(tvivienda$P5_1 == 5, 1,
                                                   ifelse(tvivienda$P5_1 == 6,5,
                                                     ifelse(tvivienda$P5_1 == 7, 666,
                                                            tvivienda$P5_1)
                                                   )
                                                 ))
                                        )))


# Select final variables

datos = tvivienda[, c(
  "vid",
  "EST_DIS",
  "UPM_DIS",
  "FACTOR",
  "hacinamiento",
  "ran_hacinamiento",
  "problemas_vivienda",
  "tipo_tenencia",
  "P4_4",
  "P4_5",
  "P4_6",
  "P4_7_1",
  "P4_7_2",
  "P4_7_3",
  "P4_8_1",
  "P4_8_2",
  "P4_8_3",
  "P4_8_4",
  "P4_12",
  "P4_13",
  "P4_14",
  "P4_15",
  "P4_16",
  "P4_17",
  "P6_5_1",
  "P6_5_2",
  "P6_5_3",
  "P6_5_4",
  "P6_5_5",
  "P6_5_6",
  "P6_6",
  "P6_7_1",
  "P6_7_2",
  "P6_7_3",
  "P6_7_4",
  "P6_9_1",
  "P6_9_2",
  "P6_9_3",
  "P6_9_4",
  "P6_9_5",
  "P6_9_6",
  "P6_9_7"
)]
#Change NA to 4. Significa que no tiene baño
datos$P4_12 = ifelse(is.na(datos$P4_12), 4,
                     datos$P4_12)
#Change NA to 6. Significa que no tiene agua
datos$P4_14 = ifelse(is.na(datos$P4_14), 6,
                     datos$P4_14)
#Change NA to 666. No sabe 
datos$P4_15 = ifelse(is.na(datos$P4_15), 666,
                     datos$P4_15)

#Add affordability index to data frame
# datos = inner_join(datos, prop, by = "vid")

# This data frame have only the columns that are included in the index 
# 
#write.csv(datos, "datos_onu.csv")
write.csv(datos,"formatted-data/datos_onu.csv", row.names = FALSE)
# A este data frame hay que agregar el asequibilidad en proporciones, hacer cuantiles 

datos_onu = datos 
##------------------------------------------------------------------------------
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


# ELEMENTS WITH OPTIONS S?, NO, NO SABE (YES, NO, DOESN'T KNOW) CHOSEN. 
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
datos_onu[is.na(datos_onu)] <- 6 #quitar NA

datos_onu = datos_onu %>% mutate(P4_14 = recode(P4_14, '1'=6,'2'=5,'3'=4,'4'=3,'5'=2,'1'=1)) 

head(datos_onu['P4_14'])

# ELEMENTS WITH 5 CHOICES, ONE OF WHICH IS 'UNSPECIFIED', AND THE FACTORS OF WHICH MUST BE REVERSED.
E5FU = c(names(datos_onu[, substr(names(datos_onu), 1,4) == 'P6_5' | substr(names(datos_onu), 1,4) == 'P6_9']), "P6_6")
head(datos_onu[E5FU])
# REFACTOR VARIABLES SO THAT NOTHING = 1, LITTLE = 2, SOME = 3, VERY = 4, UNSPECIFIED = 9.
datos_onu = datos_onu %>% mutate_at(c(E5FU), funs(recode(., '1'=4,'2'=3,'3'=2,'4'=1, '9'=2.5)))
head(datos_onu[E5FU])

# ELEMENT WITH 6 CHOICES, ONE OF WHICH IS 'UNSPECIFIED', AND THE FACTORS OF WHICH MUST BE REVERSED.
head(datos_onu['P4_15'])
# REFACTOR VARIABLES SO THAT NO SEWAGE = 1, BODY OF WATER = 2, HILL/CRACK = 3, BIODIGESTOR = 4, PUBLIC SEWAGE = 5, UNSPECIFIED = 9.
datos_onu = datos_onu %>% mutate(P4_15 = recode(P4_15, '1'=6,'2'=5,'3'=4,'4'=3,'5'=2, '9'=1))
head(datos_onu['P4_15'])

# ELEMENT WITH 6 CHOICES, ONE OF WHICH IS 'UNSPECIFIED', TWO OF WHICH CANNOT BE PROPERLY RANKED, AND THE FACTORS OF WHICH MUST BE REVERSED.
head(datos_onu['P4_17'])
# REFACTOR VARIABLES SO THAT OTHER FUEL = 1, DON'T COOK = 2, COAL OR LUMBER = 3, GAS OR ELECTRICITY = 4, UNSPECIFIED = 9.
datos_onu = datos_onu %>% mutate(P4_17 = recode(P4_17, '1'=4,'2'=3,'3'=4,'4'=1,'5'=2, '9'=2.5))
head(datos_onu['P4_17'])

# TYPE OF OWNERSHIP
head(datos_onu['tipo_tenencia'])
# REVERSE VALUES
datos_onu = datos_onu %>% mutate(tipo_tenencia = recode(tipo_tenencia, '1'=6,'2'=5,'3'=4,'4'=3,'5'=2, '666'=1))
head(datos_onu['tipo_tenencia'])


write.csv(datos_onu, "formatted-data/datos_onu.csv", row.names = FALSE)


cultural = datos_onu[,c("vid","P6_6")]

write.csv(cultural,"KPI/indice_cultural.csv", row.names = FALSE)










