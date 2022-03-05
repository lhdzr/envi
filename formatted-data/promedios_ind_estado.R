#rm(list=ls())
library(tidyverse)
library(srvyr)
library(ggplot2)
library(scales)
library(ggthemes)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
#Bajo las bases de datos
estratos <- read_csv("estratos_onu.csv")
codigos <- read_csv("codigos_identidad.csv", col_names = c("ENT", "Estado"))
datos <- read_csv("tvivienda.csv")  %>% 
  left_join(estratos, by = "vid") %>% 
  left_join(codigos, by = "ENT") 

{
# NO Aplico factores de expansion  -------------------------------------------------------
datos_fe <- as_survey_design(datos, ids = UPM_DIS, strata = EST_DIS, weights = FACTOR)


# Hago los calculos -------------------------------------------------------
tabla_final <- datos_fe %>% 
  group_by(Estado) %>% 
  summarize(mala_acc = survey_mean(estr_acc %in% c("Muy Baja", "Baja"), vartype = NULL),
            mala_habit = survey_mean(estr_habit %in% c("Muy Baja", "Baja"), vartype = NULL),
            mala_serv = survey_mean(estr_serv %in% c("Muy Baja", "Baja"), vartype = NULL),
            mala_ubi = survey_mean(estr_ubi %in% c("Muy Baja", "Baja"), vartype = NULL),
            mala_aseq = survey_mean(estr_aseq %in% c("Muy Baja", "Baja"), vartype = NULL),
            mala_cul = survey_mean(estr_cul %in% c("Muy Baja", "Baja"), vartype = NULL),
            mala_ten = survey_mean(estr_ten %in% c("Muy Baja", "Baja"), vartype = NULL),
            mala_satis = survey_mean(estr_satis %in% c("Muy Baja", "Baja"), vartype = NULL)
            )
#NO
ggplot(tabla_final) +
  geom_col(aes(x = Estado, y = mala_acc))+
  labs(title = "Hogares con baja accesibilidad",
         subtitle = "Porcentaje del total de hogares",
         caption = "Fuente: ENVI 2020",
         x = "Porcentaje", y = "Estado") + 
  theme(
           plot.title = element_text(color = "black", size = 12, 
                                     face = "bold", hjust = 0.5),
           plot.subtitle = element_text(color = "black", hjust = 0.5),
           plot.caption = element_text(color = "black", face = "italic"),
           axis.text.x = element_text(angle = 90))+
  coord_cartesian(ylim = c(0,0.7))} #Grafica de barras

# Preparacion para graficos -----------------------------------------------------
bueno <- c("Muy Alta", "Alta" , "Media")
datos_if <- datos %>% 
  mutate(estr_acc_bueno= ifelse(datos$estr_acc %in% bueno, 1,0),
         estr_habit_bueno = ifelse(estr_habit %in% bueno, 1,0),
         estr_serv_bueno = ifelse(estr_serv %in% bueno, 1,0),
         estr_ubi_bueno = ifelse(estr_ubi %in% bueno, 1,0),
         estr_aseq_bueno = ifelse(estr_aseq %in% bueno, 1,0),
         estr_cul_bueno = ifelse(estr_cul %in% bueno, 1,0),
         estr_ten_bueno = ifelse(estr_ten %in% bueno, 1,0),
         estr_satis_bueno = ifelse(estr_satis %in% bueno, 1,0))


creditos = datos_if%>%
  select(vid,P5_1,P5_15_01, P5_15_02,P5_15_03, P5_15_04,P5_15_05, P5_15_06)


# Convertir a 0 porque significa que no tiene credito de infonavit 
creditos[creditos == 2] <- 0 


creditos = creditos%>%
  rowwise()%>%
  mutate(n_creditos = sum(c(P5_15_02,P5_15_03, P5_15_04,P5_15_05, P5_15_06)))

creditos$otro_credito = ifelse(creditos$n_creditos > 0, 2,
                               creditos$n_creditos)


creditos = creditos %>%
  mutate(total = sum(c(P5_15_01,otro_credito)))%>%
  mutate(tipo_credito = ifelse(P5_15_01 == 1,"INFO",
                               ifelse(P5_15_01 == 0 && total > 1 ,"OTRO",
                                      ifelse(total == 0, "NO_CREDITO",
                                             total))))

creditos$tipo_credito[is.na(creditos$tipo_credito)] <- "NO_CREDITO"

datos_if$credito <- creditos$tipo_credito

datos_feif <- as_survey_design(datos_if, ids = UPM_DIS, strata = EST_DIS, weights = FACTOR)


# GENERALES -----------------------------------------------------------
{acces <- datos_feif %>% 
  select(vid, estr_acc_bueno, Estado) %>% 
  group_by(Estado, estr_acc_bueno) %>% 
  summarize(survey_total(vartype = NULL)) %>%  
  arrange(estr_acc_bueno, coef)
#ssss
suma <- acces %>% 
  mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
  group_by(Estado) %>% 
  summarise(suma = sum(coef))

acces <- acces %>% 
  left_join(suma, by = "Estado")

bueno <- acces %>% 
  filter(estr_acc_bueno == 1)

malo <- acces %>% 
  filter(estr_acc_bueno == 0)

acces$coef <- if_else(acces$estr_acc_bueno == 1, acces$coef, -acces$coef)

acces$estr_acc_bueno <- as.character(acces$estr_acc_bueno)


brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                           big.mark = ",", digits = 2)))




# Plot
ggplot(acces, aes(
  x = reorder(Estado, desc(suma)),
  y = coef,
  fill = estr_acc_bueno
)) +
  geom_bar(stat = "identity", width = .6)   +
  coord_flip() +
  scale_y_continuous(labels = lbls,
                     breaks = brks) +
  labs(
    title = "Condición de Accesibilidad por hogar",
    subtitle = "De acuerdo con el indicador",
    caption = "Fuente: Elaboración propia",
    x = "Estado",
    y = "Cantidad de hogares",
    fill = element_blank()
  )+
  theme_solarized()+
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                    size = 2, linetype = "solid")
  ) +
  scale_fill_manual(values = c("#000000", "#beb7a4")) } #Accesibilidad
{acces <- datos_feif %>% 
    select(vid, estr_habit_bueno, Estado) %>% 
    group_by(Estado, estr_habit_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_habit_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_habit_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_habit_bueno == 0)
  
  acces$coef <- if_else(acces$estr_habit_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_habit_bueno <- as.character(acces$estr_habit_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_habit_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de Habitabilidad por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
  } #Habitabilidad
{acces <- datos_feif %>% 
    select(vid, estr_serv_bueno, Estado) %>% 
    group_by(Estado, estr_serv_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_serv_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_serv_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_serv_bueno == 0)
  
  acces$coef <- if_else(acces$estr_serv_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_serv_bueno <- as.character(acces$estr_serv_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_serv_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de Servicios por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Servicios
{acces <- datos_feif %>% 
    select(vid, estr_ubi_bueno, Estado) %>% 
    group_by(Estado, estr_ubi_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_ubi_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_ubi_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_ubi_bueno == 0)
  
  acces$coef <- if_else(acces$estr_ubi_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_ubi_bueno <- as.character(acces$estr_ubi_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_ubi_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de Ubicación por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Ubicacion
{acces <- datos_feif %>% 
    select(vid, estr_aseq_bueno, Estado) %>% 
    group_by(Estado, estr_aseq_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_aseq_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_aseq_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_aseq_bueno == 0)
  
  acces$coef <- if_else(acces$estr_aseq_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_aseq_bueno <- as.character(acces$estr_aseq_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_aseq_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de asequibilidad por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Asequibilidad
{acces <- datos_feif %>% 
    select(vid, estr_cul_bueno, Estado) %>% 
    group_by(Estado, estr_cul_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_cul_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_cul_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_cul_bueno == 0)
  
  acces$coef <- if_else(acces$estr_cul_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_cul_bueno <- as.character(acces$estr_cul_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_cul_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de cultura por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Cultura
{acces <- datos_feif %>% 
    select(vid, estr_ten_bueno, Estado) %>% 
    group_by(Estado, estr_ten_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_ten_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_ten_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_ten_bueno == 0)
  
  acces$coef <- if_else(acces$estr_ten_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_ten_bueno <- as.character(acces$estr_ten_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_ten_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de tenencia por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Tenencia
{acces <- datos_feif %>% 
    select(vid, estr_satis_bueno, Estado) %>% 
    group_by(Estado, estr_satis_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_satis_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_satis_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_satis_bueno == 0)
  
  acces$coef <- if_else(acces$estr_satis_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_satis_bueno <- as.character(acces$estr_satis_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_satis_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de satisfecho por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Satisfacción






  
  
  
  


# Especificos INFO-------------------------------------------------------------
{acces <- datos_feif %>% 
  select(vid, estr_acc_bueno, Estado, credito) %>% 
  group_by(Estado, estr_acc_bueno, credito) %>% 
  summarize(survey_mean(vartype = NULL)) %>%  
  arrange(estr_acc_bueno, coef) 


bueno <- acces %>% 
  filter(estr_acc_bueno == 1)

malo <- acces %>% 
  filter(estr_acc_bueno == 0) %>% 
  mutate(porc = coef)


brks <- seq(-1, 1, length.out = 11)
lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                  big.mark = ",", digits = 2)))

acces$coef <- if_else(acces$estr_acc_bueno == 1, acces$coef, -acces$coef)

# Plot
ggplot(acces, aes(
  x = Estado,
  y = coef,
  fill = credito
)) +
  geom_bar(stat = "identity", width = .6) +
  geom_text(aes(label= round(coef, digits= 2)), position = position_stack(vjust=0.5)) +
  coord_flip() +
  scale_y_continuous(labels = lbls,
                     breaks = brks)  +
  labs(
    title = "Condición de Accesibilidad por hogar",
    subtitle = "De acuerdo con el indicador",
    caption = "Fuente: Elaboración propia",
    x = "Estado",
    y = "Cantidad de hogares",
    fill = element_blank()
  ) +
  theme_solarized()+
  theme(
    panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                    size = 2, linetype = "solid")
  )
} #Info accesibilidad

{acces <- datos_feif %>% 
    select(vid, estr_habit_bueno, Estado) %>% 
    filter(credito == "INFO") %>% 
    group_by(Estado, estr_habit_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_habit_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_habit_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_habit_bueno == 0)
  
  acces$coef <- if_else(acces$estr_habit_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_habit_bueno <- as.character(acces$estr_habit_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_habit_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de Habitabilidad por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Habitabilidad INFO
{acces <- datos_feif %>% 
    select(vid, estr_serv_bueno, Estado)%>% 
    filter(credito == "INFO") %>% 
    group_by(Estado, estr_serv_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_serv_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_serv_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_serv_bueno == 0)
  
  acces$coef <- if_else(acces$estr_serv_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_serv_bueno <- as.character(acces$estr_serv_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_serv_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de Servicios por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Servicios INFO
{acces <- datos_feif %>% 
    select(vid, estr_ubi_bueno, Estado)%>% 
    filter(credito == "INFO") %>% 
    group_by(Estado, estr_ubi_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_ubi_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_ubi_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_ubi_bueno == 0)
  
  acces$coef <- if_else(acces$estr_ubi_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_ubi_bueno <- as.character(acces$estr_ubi_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_ubi_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de Ubicación por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Ubicacion INFO
{acces <- datos_feif %>% 
    select(vid, estr_aseq_bueno, Estado)%>% 
    filter(credito == "INFO") %>% 
    group_by(Estado, estr_aseq_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_aseq_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_aseq_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_aseq_bueno == 0)
  
  acces$coef <- if_else(acces$estr_aseq_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_aseq_bueno <- as.character(acces$estr_aseq_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_aseq_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de asequibilidad por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Asequibilidad INFO
{acces <- datos_feif %>% 
    select(vid, estr_cul_bueno, Estado)%>% 
    filter(credito == "INFO") %>% 
    group_by(Estado, estr_cul_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_cul_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_cul_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_cul_bueno == 0)
  
  acces$coef <- if_else(acces$estr_cul_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_cul_bueno <- as.character(acces$estr_cul_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_cul_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de cultura por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Cultura INFO
{acces <- datos_feif %>% 
    select(vid, estr_ten_bueno, Estado)%>% 
    filter(credito == "INFO") %>% 
    group_by(Estado, estr_ten_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_ten_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_ten_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_ten_bueno == 0)
  
  acces$coef <- if_else(acces$estr_ten_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_ten_bueno <- as.character(acces$estr_ten_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_ten_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de tenencia por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Tenencia INFO
{acces <- datos_feif %>% 
    select(vid, estr_satis_bueno, Estado)%>% 
    filter(credito == "INFO") %>% 
    group_by(Estado, estr_satis_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_satis_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_satis_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_satis_bueno == 0)
  
  acces$coef <- if_else(acces$estr_satis_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_satis_bueno <- as.character(acces$estr_satis_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_satis_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de satisfecho por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Satisfacción INFO

# Especificos Otro-------------------------------------------------------------
{acces <- datos_feif %>% 
  select(vid, estr_acc_bueno, Estado, credito) %>% 
  filter(credito == "OTRO") %>% 
  group_by(Estado, estr_acc_bueno) %>% 
  summarize(survey_mean(vartype = NULL)) %>%  
  arrange(estr_acc_bueno, coef)


bueno <- acces %>% 
  filter(estr_acc_bueno == 1)

malo <- acces %>% 
  filter(estr_acc_bueno == 0) %>% 
  mutate(porc = coef)


brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                  big.mark = ",", digits = 2)))


acces <-  acces %>% 
  left_join(select(malo,Estado,porc), by = "Estado")

# Plot
ggplot(acces, aes(
  x = reorder(Estado,porc),
  y = coef,
  fill = estr_acc_bueno
)) +
  geom_bar(stat = "identity", width = .6)   +
  coord_flip() +
  scale_y_continuous(labels = lbls,
                     breaks = brks) +
  labs(
    title = "Condición de Accesibilidad por hogar",
    subtitle = "De acuerdo con el indicador",
    caption = "Fuente: Elaboración propia",
    x = "Estado",
    y = "Cantidad de hogares",
    fill = element_blank()
  )+
  theme_solarized()+
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                    size = 2, linetype = "solid")
  ) } #Info accesibilidad
{acces <- datos_feif %>% 
    select(vid, estr_habit_bueno, Estado, credito) %>% 
    filter(credito == "OTRO") %>% 
    group_by(Estado, estr_habit_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_habit_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_habit_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_habit_bueno == 0)
  
  acces$coef <- if_else(acces$estr_habit_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_habit_bueno <- as.character(acces$estr_habit_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_habit_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de Habitabilidad por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Habitabilidad INFO
{acces <- datos_feif %>% 
    select(vid, estr_serv_bueno, Estado, credito)%>% 
    filter(credito == "OTRO") %>% 
    group_by(Estado, estr_serv_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_serv_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_serv_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_serv_bueno == 0)
  
  acces$coef <- if_else(acces$estr_serv_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_serv_bueno <- as.character(acces$estr_serv_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_serv_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de Servicios por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Servicios INFO
{acces <- datos_feif %>% 
    select(vid, estr_ubi_bueno, Estado, credito)%>% 
    filter(credito == "OTRO") %>% 
    group_by(Estado, estr_ubi_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_ubi_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_ubi_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_ubi_bueno == 0)
  
  acces$coef <- if_else(acces$estr_ubi_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_ubi_bueno <- as.character(acces$estr_ubi_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_ubi_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de Ubicación por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Ubicacion INFO
{acces <- datos_feif %>% 
    select(vid, estr_aseq_bueno, Estado, credito)%>% 
    filter(credito == "OTRO") %>% 
    group_by(Estado, estr_aseq_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_aseq_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_aseq_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_aseq_bueno == 0)
  
  acces$coef <- if_else(acces$estr_aseq_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_aseq_bueno <- as.character(acces$estr_aseq_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_aseq_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de asequibilidad por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Asequibilidad INFO
{acces <- datos_feif %>% 
    select(vid, estr_cul_bueno, Estado, credito)%>% 
    filter(credito == "OTRO") %>% 
    group_by(Estado, estr_cul_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_cul_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_cul_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_cul_bueno == 0)
  
  acces$coef <- if_else(acces$estr_cul_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_cul_bueno <- as.character(acces$estr_cul_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_cul_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de cultura por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Cultura INFO
{acces <- datos_feif %>% 
    select(vid, estr_ten_bueno, Estado, credito)%>% 
    filter(credito == "OTRO") %>% 
    group_by(Estado, estr_ten_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_ten_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_ten_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_ten_bueno == 0)
  
  acces$coef <- if_else(acces$estr_ten_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_ten_bueno <- as.character(acces$estr_ten_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_ten_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de tenencia por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Tenencia INFO
{acces <- datos_feif %>% 
    select(vid, estr_satis_bueno, Estado, credito)%>% 
    filter(credito == "OTRO") %>% 
    group_by(Estado, estr_satis_bueno) %>% 
    summarize(survey_total(vartype = NULL)) %>%  
    arrange(estr_satis_bueno, coef)
  #ssss
  suma <- acces %>% 
    mutate(coef = ifelse(coef>0,coef,coef*(-1))) %>% 
    group_by(Estado) %>% 
    summarise(suma = sum(coef))
  
  acces <- acces %>% 
    left_join(suma, by = "Estado")
  
  bueno <- acces %>% 
    filter(estr_satis_bueno == 1)
  
  malo <- acces %>% 
    filter(estr_satis_bueno == 0)
  
  acces$coef <- if_else(acces$estr_satis_bueno == 1, acces$coef, -acces$coef)
  
  acces$estr_satis_bueno <- as.character(acces$estr_satis_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(if_else(brks >0, brks, brks*(-1)), 
                                    big.mark = ",", digits = 2)))
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, desc(suma)),
    y = coef,
    fill = estr_satis_bueno
  )) +
    geom_bar(stat = "identity", width = .6)   +
    coord_flip() +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    labs(
      title = "Condición de satisfecho por hogar",
      subtitle = "De acuerdo con el indicador",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Cantidad de hogares",
      fill = element_blank()
    )+
    theme_solarized()+
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid")
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Satisfacción INFO




            