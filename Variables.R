library(tidyverse)
library(readr)
tvivienda <- read_csv("formatted-data/tvivienda.csv")
tsdem <- read_csv("formatted-data/tsdem.csv")

a = tvivienda%>%
  select(c(P5_1,P5_4, P5_6_1,P5_7))
  



#tsdem = read.csv("tsdem.csv")

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
# Change 2 to 0 and 9 to 1.5
p_est[p_est == 2] <-  0
p_est[p_est == 9] <-  1.5

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

# Income and expenses variable
# Select interest variables of data frame
base1 <-
  select(
    tvivienda,
    c(
      vid,
      P5_20_1,
      P5_21_1_1,
      P5_21_1_2,
      P5_20_2,
      P5_21_2_1,
      P5_21_2_2,
      P5_20_3,
      P5_21_3_1,
      P5_21_3_2,
      P5_20_4,
      P5_21_4_1,
      P5_21_4_2,
      P5_20_5,
      P5_21_5_1,
      P5_21_5_2,
      P5_20_6,
      P5_21_6_1,
      P5_21_6_2
    )
  )
# Recode to NA
base1[base1 == 9999888] <- NA
base1[base1 == 9999999] <- NA

# Create variables of expenses by month to each household according to type of loan
base2 <- tvivienda  %>%
  mutate(
    mes_info =  P5_20_1 / ((P5_21_1_1 * 12) + P5_21_1_2),
    mes_fov  =  P5_20_2 / ((P5_21_2_1 * 12) + P5_21_2_2),
    mes_insp =  P5_20_3 / ((P5_21_3_1 * 12) + P5_21_3_2),
    mes_fp   =  P5_20_4 / ((P5_21_4_1 * 12) + P5_21_4_2),
    mes_onl  =  P5_20_5 / ((P5_21_5_1 * 12) + P5_21_5_2),
    mes_fa   =  P5_20_6 / ((P5_21_6_1 * 12) + P5_21_6_2)
  )

# Select important variables
base2 <-
  base2[, c("vid",
            "mes_info",
            "mes_fov",
            "mes_insp",
            "mes_fp",
            "mes_onl",
            "mes_fa")]


# Select variables of expenses per house in 2019 and 2020
tvivienda1 = tvivienda %>%
  select(
    vid,
    P5_30_3_1,
    P5_30_3_2,
    P5_30_3_3,
    P5_30_4_1,
    P5_30_4_2,
    P5_43_3_1,
    P5_43_3_2,
    P5_43_3_3,
    P5_43_4_1,
    P5_43_4_2
  )
#Recide to NA
tvivienda1[tvivienda1 == 999999888] <-  NA
tvivienda1[tvivienda1 == 999999999] <-  NA


# Create  recode variable  of rent income
tvivienda$P5_11.2 = ifelse(tvivienda$P5_11 == 98000,
                           NA,
                           ifelse(
                             tvivienda$P5_11 == 99888,
                             NA,
                             ifelse(tvivienda$P5_11 == 99999, NA,
                                    tvivienda$P5_11)
                           ))
ing_renmen = tvivienda[, c("vid", "P5_11.2")]

# Recode to get monthly income
tsdem$ingreso_m = ifelse(tsdem$P3_4A == 1 ,
                         tsdem$P3_4 * 4,
                         ifelse(
                           tsdem$P3_4A == 2,
                           tsdem$P3_4 * 2,
                           ifelse(
                             tsdem$P3_4A == 3,
                             tsdem$P3_4 * 1,
                             ifelse(tsdem$P3_4A == 4, tsdem$P3_4 / 12,
                                    ifelse(
                                      is.na(tsdem$P3_4A), NA,
                                      ifelse(tsdem$P3_4A == 9, NA,
                                             tsdem$P3_4)
                                    ))
                           )
                         ))


# Summarize to get monthly income by household not by home
ing_viv = tsdem %>%
  group_by(vid) %>%
  summarise(ingreso_viv = sum(ingreso_m, na.rm = TRUE))

# Sum total of income by household
tabla = inner_join(ing_viv, ing_renmen, by = "vid")
tabla = tabla %>%
  rowwise() %>%
  mutate(ingmens_tot = sum(c(ingreso_viv, P5_11.2), na.rm = TRUE))


# Join variables of income and expenses
in_ga = full_join(tabla[, c("vid", "ingmens_tot")], base2, by = "vid")
in_ga = full_join(in_ga, tvivienda1, by = "vid")

# Recode to NA
in_ga$ingmens_tot = ifelse(in_ga$ingmens_tot == 0 , NA,
                           in_ga$ingmens_tot)
# Cambiar a NA las viviendas que tienen ingreso 0 porque luego genera 0 en las proporciones del gasto.


# Get expensing ratio of household
prop = in_ga %>%
  rowwise() %>%
  mutate(gas_tot = sum(c_across(mes_info:P5_43_4_2), na.rm = TRUE)) %>%
  mutate(prop_gasto = gas_tot / ingmens_tot)

# Select important variables
prop = prop[, c("vid", "prop_gasto")]

# Create index of expending ratio
prop$indice_aseq = ifelse(prop$prop_gasto > 0.30 ,
                          1,
                          ifelse(prop$prop_gasto <= 0.30, 2,
                                 ifelse(
                                   is.na(prop$prop_gasto), 666,
                                   prop$prop_gasto
                                 )))
# Recode NA
prop$indice_aseq = ifelse(is.na(prop$indice_aseq), 666,
                          prop$indice_aseq)



# Select final variables

datos = tvivienda[, c(
  "vid",
  "EST_DIS",
  "UPM_DIS",
  "FACTOR",
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
#Change NA to 4
datos$P4_12 = ifelse(is.na(datos$P4_12), 4,
                     datos$P4_12)
#Change NA to 6
datos$P4_14 = ifelse(is.na(datos$P4_14), 6,
                     datos$P4_14)
#Change NA to 666
datos$P4_15 = ifelse(is.na(datos$P4_15), 666,
                     datos$P4_15)

#Add affordability index to data frame
datos = inner_join(datos, prop, by = "vid")

# This data frame have only the columns that are included in the index 
# 
#write.csv(datos, "datos_onu.csv")
write.csv(datos,"formatted-data/datos_onu.csv", row.names = FALSE)
# A este data frame hay que agregar el asequibilidad en proporciones, hacer cuantiles 

##------------------------------------------------------------------------------
# Select satisfaction variables
datos_satisfaccion = tvivienda[, c(
  "P6_3_1",
  "P6_3_2",
  "P6_3_3",
  "P6_3_4",
  "P6_3_5",
  "P6_3_6",
  "P6_4_1",
  "P6_4_2",
  "P6_4_3",
  "P6_4_4",
  "P6_4_5",
  "P6_4_6"
)]
# Recode NA
datos_satisfaccion[is.na(datos_satisfaccion)] <-  8


write.csv(datos_satisfaccion, "datos_satisfaccion.csv")










