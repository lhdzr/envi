#rm(list=ls())
library(tidyverse)
library(srvyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(emojifont)


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
  mutate(tipo_credito = ifelse(P5_15_01 == 1,"Infonavit",
                               ifelse(P5_15_01 == 0 && total > 1 ,"Otro",
                                      ifelse(total == 0, "Ninguno",
                                             total))))

creditos$tipo_credito[is.na(creditos$tipo_credito)] <- "Ninguno"

datos_if$credito <- creditos$tipo_credito

datos_feif <- as_survey_design(datos_if, ids = UPM_DIS, strata = EST_DIS, weights = FACTOR)


# Especificos Infonavit-------------------------------------------------------------
{load.emojifont("OpenSansEmoji.ttf")
  acces <- datos_feif %>% 
  select(vid, estr_acc_bueno, Estado, credito) %>% 
  group_by(Estado,credito) %>% 
  summarize(bien = survey_mean(estr_acc_bueno ==1 ,vartype = NULL),
            mal = survey_mean(estr_acc_bueno ==0 ,vartype = NULL)) 

acces <- acces %>% 
  select(Estado, credito,mal) %>% 
  mutate(mal = mal * (-1)) %>% 
  bind_rows(select(acces, Estado, credito, bien)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(coef = bien + mal)


brks <- seq(-1,1,.20)
lbls = as.character(if_else(brks>0,brks*(100),-brks*100))


# Plot
ggplot(acces, aes(
  x = reorder(Estado, coef),
  y = coef,
  fill = credito)
) +
  geom_bar(stat = "identity", width = .8, position = "dodge") +
  coord_flip(ylim = c(-.8,.8))  +
  scale_y_continuous(labels = lbls,
                     breaks = brks)  +
  labs(
    title = "Condición de Accesibilidad por hogar",
    subtitle = "De acuerdo con el indicador DP2",
    caption = "Fuente: Elaboración propia",
    x = "Estado",
    y = "Porcentaje de hogares",
    fill = element_blank()
  ) +
  theme_solarized()+ 
  geom_hline(yintercept = 0, size = .5) +
  scale_fill_manual(values = c("#F2C1B6", "#F28888", "#D9183B"),
                    labels = c("No tiene", "Otro", "Infonavitnavit"),
                    name = "Tipo de crédito")+
  theme(
    panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                    size = 2, linetype = "solid"),
    plot.title = element_text(size = 20, face = "bold", color = "black"),
    plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
    axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                               colour = "black" ))+
  coord_polar()

ggsave("accesibilidad.png", plot = last_plot(), scale = 1.3)
} #Infonavit accesibilidad

{library(emojifont)
  acces <- datos_feif %>% 
    select(vid, estr_habit_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_habit_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_habit_bueno ==0 ,vartype = NULL)) 
  
  acces <- acces %>% 
    select(Estado, credito,mal) %>% 
    mutate(mal = mal * (-1)) %>% 
    bind_rows(select(acces, Estado, credito, bien)) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    mutate(coef = bien + mal)
  
  
  brks <- c(-2,2)
  lbls = c(emoji("warning"), emoji("house_with_garden"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c( "Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8, aes(label= round(if_else(coef>0, coef,-coef), digits= 2)), position = position_stack(vjust=0.5)) +
    coord_flip(ylim = c(-3,3))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    
    labs(
      title = "Condición de Habitabilidad por hogar",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    ) +
    theme_solarized()+ 
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(values = c("#F2C1B6", "#F28888", "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "EmojiOne", size = 20, face = "bold", 
                                 colour = "black" ))
  ggsave("habitabilidad.png", plot = last_plot(), scale = 1.3)
} #Habitabilidad Infonavit
{library(emojifont)
  acces <- datos_feif %>% 
    select(vid, estr_serv_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_serv_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_serv_bueno ==0 ,vartype = NULL)) 
  
  acces <- acces %>% 
    select(Estado, credito,mal) %>% 
    mutate(mal = mal * (-1)) %>% 
    bind_rows(select(acces, Estado, credito, bien)) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    mutate(coef = bien + mal)
  
  
  brks <- c(-2,2)
  lbls = c(emoji("mute"), emoji("speaker"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c( "Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8, aes(label= round(if_else(coef>0, coef,-coef), digits= 2)), position = position_stack(vjust=0.5)) +
    coord_flip(ylim = c(-3,3))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    
    labs(
      title = "Condición de Servicios por hogar",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    ) +
    theme_solarized()+ 
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(values = c("#F2C1B6", "#F28888", "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 20, face = "bold", 
                                 colour = "black" ))
  ggsave("servicios.png", plot = last_plot(), scale = 1.3)
} #Servicios Infonavit
{library(emojifont)
  acces <- datos_feif %>% 
    select(vid, estr_ubi_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_ubi_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_ubi_bueno ==0 ,vartype = NULL)) 
  
  acces <- acces %>% 
    select(Estado, credito,mal) %>% 
    mutate(mal = mal * (-1)) %>% 
    bind_rows(select(acces, Estado, credito, bien)) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    mutate(coef = bien + mal)
  
  
  brks <- c(-2,2)
  lbls = c(emoji("factory"), emoji("city_sunrise"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c( "Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8, aes(label= round(if_else(coef>0, coef,-coef), digits= 2)), position = position_stack(vjust=0.5)) +
    coord_flip(ylim = c(-3,3))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    
    labs(
      title = "Condición de ubicación por hogar",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    ) +
    theme_solarized()+ 
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(values = c("#F2C1B6", "#F28888", "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 20, face = "bold", 
                                 colour = "black" ))
  ggsave("ubicacion.png", plot = last_plot(), scale = 1.3)
} #Ubicacion Infonavit
{library(emojifont)
  acces <- datos_feif %>% 
    select(vid, estr_aseq_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_aseq_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_aseq_bueno ==0 ,vartype = NULL)) 
  
  acces <- acces %>% 
    select(Estado, credito,mal) %>% 
    mutate(mal = mal * (-1)) %>% 
    bind_rows(select(acces, Estado, credito, bien)) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    mutate(coef = bien + mal)
  
  
  brks <- c(-2,2)
  lbls = c(paste0(emoji("heavy_dollar_sign"),emoji("heavy_dollar_sign"),
                  emoji("heavy_dollar_sign")), emoji("heavy_dollar_sign"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c( "Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8, aes(label= round(if_else(coef>0, coef,-coef), digits= 2)), position = position_stack(vjust=0.5)) +
    coord_flip(ylim = c(-3,3))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    
    labs(
      title = "Condición de asequibilidad por hogar",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    ) +
    theme_solarized()+ 
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(values = c("#F2C1B6", "#F28888", "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 20, face = "bold", 
                                 colour = "black" ))
  ggsave("asequibilidad.png", plot = last_plot(), scale = 1.3)
} #Asequibilidad Infonavit
{library(emojifont)
  acces <- datos_feif %>% 
    select(vid, estr_cul_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_cul_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_cul_bueno ==0 ,vartype = NULL)) 
  
  acces <- acces %>% 
    select(Estado, credito,mal) %>% 
    mutate(mal = mal * (-1)) %>% 
    bind_rows(select(acces, Estado, credito, bien)) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    mutate(coef = bien + mal)
  
  
  brks <- c(-2,2)
  lbls = c(emoji("no_pedestrians"), emoji("children_crossing"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c( "Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8, aes(label= round(if_else(coef>0, coef,-coef), digits= 2)), position = position_stack(vjust=0.5)) +
    coord_flip(ylim = c(-3,3))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    
    labs(
      title = "Condición de cultura por hogar",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    ) +
    theme_solarized()+ 
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(values = c("#F2C1B6", "#F28888", "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 20, face = "bold", 
                                 colour = "black" ))
  ggsave("cultura.png", plot = last_plot(), scale = 1.3)
} #Cultura Infonavit
{library(emojifont)
  acces <- datos_feif %>% 
    select(vid, estr_ten_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_ten_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_ten_bueno ==0 ,vartype = NULL)) 
  
  acces <- acces %>% 
    select(Estado, credito,mal) %>% 
    mutate(mal = mal * (-1)) %>% 
    bind_rows(select(acces, Estado, credito, bien)) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    mutate(coef = bien + mal)
  
  
  brks <- c(-2,2)
  lbls = c(emoji("unlock"), emoji("lock"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c( "Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8, aes(label= round(if_else(coef>0, coef,-coef), digits= 2)), position = position_stack(vjust=0.5)) +
    coord_flip(ylim = c(-3,3))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    
    labs(
      title = "Condición de tenencia por hogar",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    ) +
    theme_solarized()+ 
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(values = c("#F2C1B6", "#F28888", "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 20, face = "bold", 
                                 colour = "black" ))
  ggsave("tenencia.png", plot = last_plot(), scale = 1.3)
} #Tenencia Infonavit
{library(emojifont)
  acces <- datos_feif %>% 
    select(vid, estr_satis_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_satis_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_satis_bueno ==0 ,vartype = NULL)) 
  
  acces <- acces %>% 
    select(Estado, credito,mal) %>% 
    mutate(mal = mal * (-1)) %>% 
    bind_rows(select(acces, Estado, credito, bien)) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    mutate(coef = bien + mal)
  
  
  brks <- c(-2,2)
  lbls = c(emoji("disappointed"), emoji("smile"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c( "Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8, aes(label= round(if_else(coef>0, coef,-coef), digits= 2)), position = position_stack(vjust=0.5)) +
    coord_flip(ylim = c(-3,3))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    
    labs(
      title = "Condición de satisfacción por hogar",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    ) +
    theme_solarized()+ 
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(values = c("#F2C1B6", "#F28888", "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 20, face = "bold", 
                                 colour = "black" ))
  ggsave("satisfaccion.png", plot = last_plot(), scale = 1.3)
} #Satisfacción Infonavit





# Especificos Otro-------------------------------------------------------------
{acces <- datos_feif %>% 
  select(vid, estr_acc_bueno, Estado, credito) %>% 
  filter(credito == "Otro") %>% 
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
    filter(credito == "Otro") %>% 
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
} #Habitabilidad Infonavit
{acces <- datos_feif %>% 
    select(vid, estr_serv_bueno, Estado, credito)%>% 
    filter(credito == "Otro") %>% 
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
} #Servicios Infonavit
{acces <- datos_feif %>% 
    select(vid, estr_ubi_bueno, Estado, credito)%>% 
    filter(credito == "Otro") %>% 
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
} #Ubicacion Infonavit
{acces <- datos_feif %>% 
    select(vid, estr_aseq_bueno, Estado, credito)%>% 
    filter(credito == "Otro") %>% 
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
} #Asequibilidad Infonavit
{acces <- datos_feif %>% 
    select(vid, estr_cul_bueno, Estado, credito)%>% 
    filter(credito == "Otro") %>% 
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
} #Cultura Infonavit
{acces <- datos_feif %>% 
    select(vid, estr_ten_bueno, Estado, credito)%>% 
    filter(credito == "Otro") %>% 
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
} #Tenencia Infonavit
{acces <- datos_feif %>% 
    select(vid, estr_satis_bueno, Estado, credito)%>% 
    filter(credito == "Otro") %>% 
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
} #Satisfacción Infonavit



# Graf --------------------------------------------------------------------
library(tidyverse)
library(ggplot2)

p2 <- c(52.36,48.31, 30.14,21.38,7.29,13.98, 33.49,39.43,25.59,27.93)
nombres <- c("Accesibilidad P2", "Accesibilidad CATPCA",  "Habitabilidad P2", 
             "Habitabilidad CATPCA","Servicios P2", "Servicios CATPCA",
             "Ubicación P2","Ubicación CATPCA" ,"Satisfacción P2", "Satisfacción CATPCA")

indice <- as_tibble(cbind(nombres, p2)) %>% 
  mutate(tipo = str_extract(nombres, pattern = "[C|P]{1}"),
         nombre = str_remove(nombres, "P2|CATPCA"),
         p2 = as.numeric(p2)) 


ggplot(indice, aes(x = nombre, y = p2, fill = tipo))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Porcentaje de ....")





# Graficos rene -----------------------------------------------------------

load.emojifont("OpenSansEmoji.ttf")
acces <- datos_feif %>% 
  select(vid, estr_acc_bueno, Estado, credito) %>% 
  group_by(Estado,credito) %>% 
  summarize(bien = survey_mean(estr_acc_bueno ==1 ,vartype = NULL),
            mal = survey_mean(estr_acc_bueno ==0 ,vartype = NULL)) 

acces <- acces %>% 
  select(Estado, credito,mal) %>% 
  mutate(mal = mal * (-1)) %>% 
  bind_rows(select(acces, Estado, credito, bien)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(coef = bien + mal)


brks <- c(-2,2)
lbls = c(emoji("warning"), emoji("wheelchair"))


# Plot
ggplot(acces, aes(
  x = credito,
  y = coef,
  fill = bien >0 )
) +
  geom_bar(stat = "identity", width = .6) +
  geom_text(size = 2.8, aes(label= round(if_else(coef>0, coef,-coef), digits= 2)), position = position_stack(vjust=0.5)) +
  coord_flip(ylim = c(-.8,.8))  +
  scale_y_continuous(labels = lbls,
                     breaks = brks)  +
  facet_wrap(~Estado, ncol = 4)+
  labs(
    title = "Condición de Accesibilidad por hogar",
    subtitle = "De acuerdo con el indicador DP2",
    caption = "Fuente: Elaboración propia",
    x = "Estado",
    y = "Porcentaje de hogares",
    fill = element_blank()
  ) +
  theme_solarized()+ 
  geom_hline(yintercept = 0, size = .5) +
  scale_fill_manual(values = c("#F2C1B6", "#F28888", "#D9183B"),
                    labels = c("No tiene", "Otro", "Infonavit"),
                    name = "Tipo de crédito")+
  theme(
    panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                    size = 2, linetype = "solid"),
    plot.title = element_text(size = 20, face = "bold", color = "black"),
    plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
    axis.text.x = element_text(family = "OpenSansEmoji", size = 20, face = "bold", 
                               colour = "black" ))




# Experimentacion sin wrap ---------------------------------------------------------


{load.emojifont("OpenSansEmoji.ttf")
  acces <- datos_feif %>% 
    select(vid, estr_acc_bueno, Estado, credito) %>% 
    group_by(Estado,credito)  %>% 
    summarize(bien = survey_mean(estr_acc_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_acc_bueno ==0 ,vartype = NULL)) 


  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,.8))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena accesibilidad",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values = c(Ninguno = "#F2C1B6", Otro = "#F28888", Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) 
  
  
  ggsave("accesibilidad.png", plot = last_plot(), width = 2500, height = 4000, units = "px")
} #Info accesibilidad

{library(emojifont)
  acces <- datos_feif %>% 
    select(vid, estr_habit_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_habit_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_habit_bueno ==0 ,vartype = NULL)) 
  
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena Habitabilidad",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values =  c(Ninguno = "#F2C1B6", Otro = "#F28888", Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) 
  
  ggsave("habitabilidad.png", plot = last_plot(), width = 2500, height = 4000, units = "px")
} #Habitabilidad Infonavit

{
  acces <- datos_feif %>% 
    select(vid, estr_serv_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_serv_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_serv_bueno ==0 ,vartype = NULL)) 
  
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  

  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buenos servicios",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values =  c(Ninguno = "#F2C1B6", Otro = "#F28888", Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) 
  
  ggsave("servicios.png", plot = last_plot(), width = 2500, height = 4000, units = "px")
} #Servicios Infonavit

{library(emojifont)
  acces <- datos_feif %>% 
    select(vid, estr_ubi_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_ubi_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_ubi_bueno ==0 ,vartype = NULL)) 
  

  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena ubicación",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values =  c(Ninguno = "#F2C1B6", Otro = "#F28888", Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) 
  
  ggsave("ubicacion.png", plot = last_plot(), width = 2500, height = 4000, units = "px")
} #Ubicacion Infonavit

{
  acces <- datos_feif %>% 
    select(vid, estr_aseq_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_aseq_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_aseq_bueno ==0 ,vartype = NULL)) 
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena asequibilidad",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values =  c(Ninguno = "#F2C1B6", Otro = "#F28888", Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) 
  
  ggsave("asequibilidad.png", plot = last_plot(), width = 2500, height = 4000, units = "px")
} #Asequibilidad Infonavit

{
  acces <- datos_feif %>% 
    select(vid, estr_cul_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_cul_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_cul_bueno ==0 ,vartype = NULL)) 
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,.80))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena adecuación cultural",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values =  c(Ninguno = "#F2C1B6", Otro = "#F28888", Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) 
  
  ggsave("cultura.png", plot = last_plot(), width = 2500, height = 4000, units = "px")
} #Cultura Infonavit

{
  acces <- datos_feif %>% 
    select(vid, estr_ten_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_ten_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_ten_bueno ==0 ,vartype = NULL)) 
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena tenencia",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values =  c(Ninguno = "#F2C1B6", Otro = "#F28888", Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) 
  
  ggsave("tenencia.png", plot = last_plot(), width = 2500, height = 4000, units = "px")
} #Tenencia Infonavit

{
  acces <- datos_feif %>% 
    select(vid, estr_satis_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_satis_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_satis_bueno ==0 ,vartype = NULL)) 
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena satisfacción",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values =  c(Ninguno = "#F2C1B6", Otro = "#F28888", Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) 
  
  ggsave("satisfaccion.png", plot = last_plot(), width = 2500, height = 4000, units = "px")
} #Satisfacción Infonavit



# Con wrap ----------------------------------------------------------------

{load.emojifont("OpenSansEmoji.ttf")
  acces <- datos_feif %>% 
    select(vid, estr_acc_bueno, Estado, credito) %>% 
    group_by(Estado,credito)  %>% 
    summarize(bien = survey_mean(estr_acc_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_acc_bueno ==0 ,vartype = NULL)) 
  
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,.8))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena accesibilidad",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values = c(Ninguno = "#F2C1B6", Otro ="#F28888",Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) +
    facet_wrap(~credito)
  
  
  ggsave("accesibilidad_wrap.png", plot = last_plot(), width = 5000, height = 2000, units = "px")
} #Info accesibilidad

{library(emojifont)
  acces <- datos_feif %>% 
    select(vid, estr_habit_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_habit_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_habit_bueno ==0 ,vartype = NULL)) 
  
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena Habitabilidad",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values = c(Ninguno = "#F2C1B6", Otro ="#F28888",Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) +
    facet_wrap(~credito)
  
  ggsave("habitablidad_wrap.png", plot = last_plot(), width = 5000, height = 2000, units = "px")
} #Habitabilidad Infonavit

{
  acces <- datos_feif %>% 
    select(vid, estr_serv_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_serv_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_serv_bueno ==0 ,vartype = NULL)) 
  
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buenos servicios",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values = c(Ninguno = "#F2C1B6", Otro ="#F28888",Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) +
    facet_wrap(~credito)
  
  ggsave("servicios_wrap.png", plot = last_plot(), width = 5000, height = 2000, units = "px")
} #Servicios Infonavit

{library(emojifont)
  acces <- datos_feif %>% 
    select(vid, estr_ubi_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_ubi_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_ubi_bueno ==0 ,vartype = NULL)) 
  
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena ubicación",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values = c(Ninguno = "#F2C1B6", Otro ="#F28888",Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) +
    facet_wrap(~credito)
  
  ggsave("ubicacion_wrap.png", plot = last_plot(), width = 5000, height = 2000, units = "px")

} #Ubicacion Infonavit

{
  acces <- datos_feif %>% 
    select(vid, estr_aseq_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_aseq_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_aseq_bueno ==0 ,vartype = NULL)) 
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena asequibilidad",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values = c(Ninguno = "#F2C1B6", Otro ="#F28888",Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) +
    facet_wrap(~credito)
  
  ggsave("asequibilidad_wrap.png", plot = last_plot(), width = 5000, height = 2000, units = "px")
} #Asequibilidad Infonavit

{
  acces <- datos_feif %>% 
    select(vid, estr_cul_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_cul_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_cul_bueno ==0 ,vartype = NULL)) 
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,.80))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena adecuación cultural",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values = c(Ninguno = "#F2C1B6", Otro ="#F28888",Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) +
    facet_wrap(~credito)
  
  ggsave("adecuacion_wrap.png", plot = last_plot(), width = 5000, height = 2000, units = "px")
} #Cultura Infonavit

{
  acces <- datos_feif %>% 
    select(vid, estr_ten_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_ten_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_ten_bueno ==0 ,vartype = NULL)) 
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena tenencia",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values = c(Ninguno = "#F2C1B6", Otro ="#F28888",Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) +
    facet_wrap(~credito)
  
  ggsave("tenencia_wrap.png", plot = last_plot(), width = 5000, height = 2000, units = "px")
} #Tenencia Infonavit

{
  acces <- datos_feif %>% 
    select(vid, estr_satis_bueno, Estado, credito) %>% 
    group_by(Estado,credito) %>% 
    summarize(bien = survey_mean(estr_satis_bueno ==1 ,vartype = NULL),
              mal = survey_mean(estr_satis_bueno ==0 ,vartype = NULL)) 
  
  brks <- seq(-1,1,.20)
  lbls = as.character(if_else(brks>0,brks*(100),-brks*100))
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = bien,
    fill = credito)) +
    geom_bar( stat = "identity", width = 0.8, position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0,1))  +
    scale_y_continuous(labels = lbls,
                       breaks = brks)  +
    labs(
      title = "Porcentaje de hogares con buena satisfacción",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_fill_manual(values =c(Ninguno = "#F2C1B6", Otro ="#F28888",Infonavit = "#D9183B"),
                      labels = c("No tiene", "Otro", "Infonavit"),
                      name = "Tipo de crédito")+
    scale_color_manual(name = element_blank(), values = c(Promedio = "black") )+
    theme(
      panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                      size = 2, linetype = "solid"),
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(family = "OpenSansEmoji", size = 8, face = "bold", 
                                 colour = "black" ),
      legend.position = "top",
      axis.title.y = element_blank()) +
    facet_wrap(~credito)
  
  ggsave("satisfaccion_wrap.png", plot = last_plot(), width = 5000, height = 2000, units = "px")
} #Satisfacción Infonavit


            