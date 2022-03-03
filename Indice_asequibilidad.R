library(tidyverse)
library(readr)
tvivienda <- read_csv("formatted-data/tvivienda.csv")
tsdem <- read_csv("formatted-data/tsdem.csv")

# Income and expenses variable -----------------------------------------------------------------
# Select interest variables of data frame
base1 <-
  select(
    tvivienda,
    c(
      vid,
      P5_20_1, # Cuanto se debe de infonavit
      P5_21_1_1,# Cuanto tiempo le falta para terminar de pagar años-99
      P5_21_1_2,# Cuanto tiempo le falta para terminar de pagar meses -88,99
      P5_20_2, # Cuanto se debe de fovisste
      P5_21_2_1, # Cuanto tiempo le falta para terminar de pagar años -99
      P5_21_2_2, # Cuanto tiempo le falta para terminar de pagar meses -88,99
      P5_20_3, # Cuanto debe de una institución publica
      P5_21_3_1,# Cuanto tiempo le falta años -99
      P5_21_3_2, # Cuanto tiempo le falta meses -88,99
      P5_20_4, # Cuanto se debe institucion privada
      P5_21_4_1,# Cuanto tiempo falta años -99
      P5_21_4_2, # Cuanto tiempo falta meses -88,99
      P5_20_5, #Cuanto se debe de organización no lucrativa
      P5_21_5_1, #Cuanto tiempo falta años -99
      P5_21_5_2, #Cuanto tiempo falta meses -88
      P5_20_6, # Cuanto debe prestamo familiar
      P5_21_6_1, #Cuanto tiempo falta pagar años-88
      P5_21_6_2 #Cuanto tiempo falta pagar meses -88,99
    )
  )
# Recode to NA para los montos que no responde o no sabe 
base1[base1 == 9999888] <- NA
base1[base1 == 9999999] <- NA

# Eliminar los 88 y 99 y poner NA
base1$P5_21_1_1 <- ifelse(base1$P5_21_1_1 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_1_1)

base1$P5_21_1_2 <- ifelse(base1$P5_21_1_2 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_1_2)

base1$P5_21_2_1 <- ifelse(base1$P5_21_2_1 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_2_1)

base1$P5_21_2_2 <- ifelse(base1$P5_21_2_2 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_2_2)

base1$P5_21_3_1 <- ifelse(base1$P5_21_3_1 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_3_1)

base1$P5_21_3_2 <- ifelse(base1$P5_21_3_2 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_3_2)

base1$P5_21_4_1 <- ifelse(base1$P5_21_4_1 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_4_1)

base1$P5_21_4_2 <- ifelse(base1$P5_21_4_2 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_4_2)

base1$P5_21_5_1 <- ifelse(base1$P5_21_5_1 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_5_1)

base1$P5_21_5_2 <- ifelse(base1$P5_21_5_2 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_5_2)

base1$P5_21_6_1 <- ifelse(base1$P5_21_6_1 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_6_1)

base1$P5_21_6_2 <- ifelse(base1$P5_21_6_2 == 88|base1$P5_21_1_1== 99,NA,
                          base1$P5_21_6_2)





# Create variables of expenses by month to each household according to type of loan
gastos_creditos_mes <- base1  %>%
  mutate(
    mes_info =  P5_20_1 / ((P5_21_1_1 * 12) + P5_21_1_2),
    mes_fov  =  P5_20_2 / ((P5_21_2_1 * 12) + P5_21_2_2),
    mes_insp =  P5_20_3 / ((P5_21_3_1 * 12) + P5_21_3_2),
    mes_fp   =  P5_20_4 / ((P5_21_4_1 * 12) + P5_21_4_2),
    mes_onl  =  P5_20_5 / ((P5_21_5_1 * 12) + P5_21_5_2),
    mes_fa   =  P5_20_6 / ((P5_21_6_1 * 12) + P5_21_6_2)
  )

# Select important variables
gastos_creditos_mes <-
  gastos_creditos_mes[, c("vid",
            "mes_info",
            "mes_fov",
            "mes_insp",
            "mes_fp",
            "mes_onl",
            "mes_fa")]


# Select variables of expenses per house in 2019 and 2020
# Dividir los gastos entre los hechos en 2019 y los hechos en 2020 
# Dividir los gastos entre 12 por cada año para tratar de estimar cuanto significa eso mensualmente en el ingreso 
gastos_viv2019 = tvivienda %>%
  select(
    P5_30_3_1,
    P5_30_3_2,
    P5_30_3_3,
    P5_30_4_1,
    P5_30_4_2)

gastos_viv2020 = tvivienda %>%
  select(
    P5_43_3_1,
    P5_43_3_2,
    P5_43_3_3,
    P5_43_4_1,
    P5_43_4_2
  )


#Recide to NA
gastos_viv2019[gastos_viv2019 == 999999888] <-  NA
gastos_viv2019[gastos_viv2019 == 999999999] <-  NA

#Recide to NA
gastos_viv2020[gastos_viv2020 == 999999888] <-  NA
gastos_viv2020[gastos_viv2020 == 999999999] <-  NA

# Dividir los gastos entre 12 
gastos_viv2019 <- gastos_viv2019/12
gastos_viv2020 <- gastos_viv2020/12


gastos_viv <- cbind(gastos_viv2019,gastos_viv2020)
gastos_viv <- cbind(gastos_viv, tvivienda$vid)%>%
  rename(vid = "tvivienda$vid")


# Create  recode variable  of rent income
#tvivienda$P5_11.2 = ifelse(tvivienda$P5_11 == 99888,
#                             NA,
#                             ifelse(tvivienda$P5_11 == 99999, NA,
#                                    tvivienda$P5_11))

#ing_renmen = tvivienda[, c("vid", "P5_11.2")]

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
#tabla = inner_join(ing_viv, ing_renmen, by = "vid")

#tabla = ing_viv %>%
#  rowwise() %>%
#  mutate(ingmens_tot = sum(c(ingreso_viv, P5_11.2), na.rm = TRUE))


# Join variables of income and expenses
ingreso_gasto = full_join(ing_viv, gastos_creditos_mes, by = "vid")

# Recode to NA
ingreso_gasto$ingreso_viv = ifelse(ingreso_gasto$ingreso_viv == 0 , NA,
                           ingreso_gasto$ingreso_viv)
# Cambiar a NA las viviendas que tienen ingreso 0 porque luego genera 0 en las proporciones del gasto.


# Get expensing ratio of household
prop = ingreso_gasto %>%
  rowwise() %>%
  mutate(gas_tot = sum(c_across(mes_info:mes_fa), na.rm = TRUE)) %>%
  mutate(prop_gasto = gas_tot / ingreso_viv)

write_csv(prop, "prop_aseq.csv")

# Select important variables
prop = prop[, c("vid", "prop_gasto")]

# Eliminar outlier
library(naniar)
prop = prop %>% replace_with_na(replace = list(prop_gasto = 125))

# Create index of expending ratio
prop$indice_aseq = ifelse(prop$prop_gasto > 0.30 ,
                          1,
                          ifelse(prop$prop_gasto <= 0.30, 2,
                                   prop$prop_gasto
                                 ))




# Recode NA
#prop$indice_aseq = ifelse(is.na(prop$indice_aseq), 666,
#                          prop$indice_aseq)


# Save affordability index 
write.csv(prop,"KPI/indice_asequibilidad.csv")
