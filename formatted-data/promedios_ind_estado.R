#rm(list=ls())
library(tidyverse)
library(srvyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(emojifont)


options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

#Bajo las bases de datos
estratos <- read_csv("estratos_onu.csv")
codigos <-
  read_csv("codigos_identidad.csv", col_names = c("ENT", "Estado"))
datos <- read_csv("tvivienda.csv")  %>%
  left_join(estratos, by = "vid") %>%
  left_join(codigos, by = "ENT")

{
  # NO Aplico factores de expansion  -------------------------------------------------------
datos_fe <-
    as_survey_design(datos,
                     ids = UPM_DIS,
                     strata = EST_DIS,
                     weights = FACTOR)
  
  
# Hago los calculos -------------------------------------------------------
tabla_final <- datos_fe %>%
    group_by(Estado) %>%
    summarize(
      mala_acc = survey_mean(estr_acc %in% c("Muy Baja", "Baja"), vartype = NULL),
      mala_habit = survey_mean(estr_habit %in% c("Muy Baja", "Baja"), vartype = NULL),
      mala_serv = survey_mean(estr_serv %in% c("Muy Baja", "Baja"), vartype = NULL),
      mala_ubi = survey_mean(estr_ubi %in% c("Muy Baja", "Baja"), vartype = NULL),
      mala_aseq = survey_mean(estr_aseq %in% c("Muy Baja", "Baja"), vartype = NULL),
      mala_cul = survey_mean(estr_cul %in% c("Muy Baja", "Baja"), vartype = NULL),
      mala_ten = survey_mean(estr_ten %in% c("Muy Baja", "Baja"), vartype = NULL),
      mala_satis = survey_mean(estr_satis %in% c("Muy Baja", "Baja"), vartype = NULL)
    )
#write_csv(tabla_final, "mala_condicion_estado.csv")
  
  #NO
  ggplot(tabla_final) +
    geom_col(aes(x = Estado, y = mala_acc)) +
    labs(
      title = "Hogares con baja accesibilidad",
      subtitle = "Porcentaje del total de hogares",
      caption = "Fuente: ENVI 2020",
      x = "Porcentaje",
      y = "Estado"
    ) +
    theme(
      plot.title = element_text(
        color = "black",
        size = 12,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(color = "black", hjust = 0.5),
      plot.caption = element_text(color = "black", face = "italic"),
      axis.text.x = element_text(angle = 90)
    ) +
    coord_cartesian(ylim = c(0, 0.7))
  } #Grafica de barras

# Preparacion para graficos -----------------------------------------------------
bueno <- c("Muy Alta", "Alta" , "Media")
datos_if <- datos %>%
  mutate(
    estr_acc_bueno = ifelse(datos$estr_acc %in% bueno, 1, 0),
    estr_habit_bueno = ifelse(estr_habit %in% bueno, 1, 0),
    estr_serv_bueno = ifelse(estr_serv %in% bueno, 1, 0),
    estr_ubi_bueno = ifelse(estr_ubi %in% bueno, 1, 0),
    estr_aseq_bueno = ifelse(estr_aseq %in% bueno, 1, 0),
    estr_cul_bueno = ifelse(estr_cul %in% bueno, 1, 0),
    estr_ten_bueno = ifelse(estr_ten %in% bueno, 1, 0),
    estr_satis_bueno = ifelse(estr_satis %in% bueno, 1, 0),
    Estado = Estado
  )




creditos = datos_if %>%
  select(vid,
         P5_1,
         P5_15_01,
         P5_15_02,
         P5_15_03,
         P5_15_04,
         P5_15_05,
         P5_15_06)


# Convertir a 0 porque significa que no tiene credito de infonavit
creditos[creditos == 2] <- 0


creditos = creditos %>%
  rowwise() %>%
  mutate(n_creditos = sum(c(
    P5_15_02, P5_15_03, P5_15_04, P5_15_05, P5_15_06
  )))

creditos$otro_credito = ifelse(creditos$n_creditos > 0, 2,
                               creditos$n_creditos)


creditos = creditos %>%
  mutate(total = sum(c(P5_15_01, otro_credito))) %>%
  mutate(tipo_credito = ifelse(
    P5_15_01 == 1,
    "Infonavit",
    ifelse(
      P5_15_01 == 0 && total > 1 ,
      "Otro",
      ifelse(total == 0, "Ninguno",
             total)
    )
  ))

creditos$tipo_credito[is.na(creditos$tipo_credito)] <- "Ninguno"

datos_if$credito <- creditos$tipo_credito

table(datos_if$credito)

#write_csv(datos_if, "Shiny_Ale/base_inicial_shiny.csv")

datos_feif <-
  as_survey_design(datos_if,
                   ids = UPM_DIS,
                   strata = EST_DIS,
                   weights = FACTOR)


# Especificos Infonavit-------------------------------------------------------------
{
  load.emojifont("OpenSansEmoji.ttf")
acces <- datos_feif %>%
    summarize(
      bien = survey_mean(estr_acc_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_acc_bueno == 0 , vartype = NULL),
      Estado = Estado,
      credito = credito
    )


  acces <- acces %>%
    select(Estado, credito, mal) %>%
    mutate(mal = mal * (-1)) %>%
    bind_rows(select(acces, Estado, credito, bien)) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(coef = bien + mal)
  
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, coef),
    y = coef,
    fill = credito
  )) +
    geom_bar(stat = "identity",
             width = .8,
             position = "dodge") +
    coord_flip(ylim = c(-.8, .8))  +
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
    theme_solarized() +
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(
      values = c("#F2C1B6", "#F28888", "#D9183B"),
      labels = c("No tiene", "Otro", "Infonavitnavit"),
      name = "Tipo de crédito"
    ) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 8,
        face = "bold",
        colour = "black"
      )
    ) +
    coord_polar()
  
  ggsave("accesibilidad.png", plot = last_plot(), scale = 1.3)
} #Infonavit accesibilidad

{
  library(emojifont)
  acces <- datos_feif %>%
    select(vid, estr_habit_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_habit_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_habit_bueno == 0 , vartype = NULL)
    )
  
  acces <- acces %>%
    select(Estado, credito, mal) %>%
    mutate(mal = mal * (-1)) %>%
    bind_rows(select(acces, Estado, credito, bien)) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(coef = bien + mal)
  
  
  brks <- c(-2, 2)
  lbls = c(emoji("warning"), emoji("house_with_garden"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c("Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8,
              aes(label = round(if_else(
                coef > 0, coef, -coef
              ), digits = 2)),
              position = position_stack(vjust = 0.5)) +
    coord_flip(ylim = c(-3, 3))  +
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
    theme_solarized() +
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(
      values = c("#F2C1B6", "#F28888", "#D9183B"),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "EmojiOne",
        size = 20,
        face = "bold",
        colour = "black"
      )
    )
  #ggsave("habitabilidad.png", plot = last_plot(), scale = 1.3)
} #Habitabilidad Infonavit
{
  library(emojifont)
  acces <- datos_feif %>%
    select(vid, estr_serv_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_serv_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_serv_bueno == 0 , vartype = NULL)
    )
  
  acces <- acces %>%
    select(Estado, credito, mal) %>%
    mutate(mal = mal * (-1)) %>%
    bind_rows(select(acces, Estado, credito, bien)) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(coef = bien + mal)
  
  
  brks <- c(-2, 2)
  lbls = c(emoji("mute"), emoji("speaker"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c("Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8,
              aes(label = round(if_else(
                coef > 0, coef, -coef
              ), digits = 2)),
              position = position_stack(vjust = 0.5)) +
    coord_flip(ylim = c(-3, 3))  +
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
    theme_solarized() +
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(
      values = c("#F2C1B6", "#F28888", "#D9183B"),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 20,
        face = "bold",
        colour = "black"
      )
    )
  #ggsave("servicios.png", plot = last_plot(), scale = 1.3)
} #Servicios Infonavit
{
  library(emojifont)
  acces <- datos_feif %>%
    select(vid, estr_ubi_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_ubi_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_ubi_bueno == 0 , vartype = NULL)
    )
  
  acces <- acces %>%
    select(Estado, credito, mal) %>%
    mutate(mal = mal * (-1)) %>%
    bind_rows(select(acces, Estado, credito, bien)) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(coef = bien + mal)
  
  
  brks <- c(-2, 2)
  lbls = c(emoji("factory"), emoji("city_sunrise"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c("Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8,
              aes(label = round(if_else(
                coef > 0, coef, -coef
              ), digits = 2)),
              position = position_stack(vjust = 0.5)) +
    coord_flip(ylim = c(-3, 3))  +
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
    theme_solarized() +
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(
      values = c("#F2C1B6", "#F28888", "#D9183B"),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 20,
        face = "bold",
        colour = "black"
      )
    )
  ggsave("ubicacion.png", plot = last_plot(), scale = 1.3)
} #Ubicacion Infonavit
{
  library(emojifont)
  acces <- datos_feif %>%
    select(vid, estr_aseq_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_aseq_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_aseq_bueno == 0 , vartype = NULL)
    )
  
  acces <- acces %>%
    select(Estado, credito, mal) %>%
    mutate(mal = mal * (-1)) %>%
    bind_rows(select(acces, Estado, credito, bien)) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(coef = bien + mal)
  
  
  brks <- c(-2, 2)
  lbls = c(paste0(
    emoji("heavy_dollar_sign"),
    emoji("heavy_dollar_sign"),
    emoji("heavy_dollar_sign")
  ), emoji("heavy_dollar_sign"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c("Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8,
              aes(label = round(if_else(
                coef > 0, coef, -coef
              ), digits = 2)),
              position = position_stack(vjust = 0.5)) +
    coord_flip(ylim = c(-3, 3))  +
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
    theme_solarized() +
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(
      values = c("#F2C1B6", "#F28888", "#D9183B"),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 20,
        face = "bold",
        colour = "black"
      )
    )
  ggsave("asequibilidad.png", plot = last_plot(), scale = 1.3)
} #Asequibilidad Infonavit
{
  library(emojifont)
  acces <- datos_feif %>%
    select(vid, estr_cul_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_cul_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_cul_bueno == 0 , vartype = NULL)
    )
  
  acces <- acces %>%
    select(Estado, credito, mal) %>%
    mutate(mal = mal * (-1)) %>%
    bind_rows(select(acces, Estado, credito, bien)) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(coef = bien + mal)
  
  
  brks <- c(-2, 2)
  lbls = c(emoji("no_pedestrians"), emoji("children_crossing"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c("Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8,
              aes(label = round(if_else(
                coef > 0, coef, -coef
              ), digits = 2)),
              position = position_stack(vjust = 0.5)) +
    coord_flip(ylim = c(-3, 3))  +
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
    theme_solarized() +
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(
      values = c("#F2C1B6", "#F28888", "#D9183B"),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 20,
        face = "bold",
        colour = "black"
      )
    )
  ggsave("cultura.png", plot = last_plot(), scale = 1.3)
} #Cultura Infonavit
{
  library(emojifont)
  acces <- datos_feif %>%
    select(vid, estr_ten_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_ten_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_ten_bueno == 0 , vartype = NULL)
    )
  
  acces <- acces %>%
    select(Estado, credito, mal) %>%
    mutate(mal = mal * (-1)) %>%
    bind_rows(select(acces, Estado, credito, bien)) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(coef = bien + mal)
  
  
  brks <- c(-2, 2)
  lbls = c(emoji("unlock"), emoji("lock"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c("Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8,
              aes(label = round(if_else(
                coef > 0, coef, -coef
              ), digits = 2)),
              position = position_stack(vjust = 0.5)) +
    coord_flip(ylim = c(-3, 3))  +
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
    theme_solarized() +
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(
      values = c("#F2C1B6", "#F28888", "#D9183B"),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 20,
        face = "bold",
        colour = "black"
      )
    )
  ggsave("tenencia.png", plot = last_plot(), scale = 1.3)
} #Tenencia Infonavit
{
  library(emojifont)
  acces <- datos_feif %>%
    select(vid, estr_satis_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_satis_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_satis_bueno == 0 , vartype = NULL)
    )
  
  acces <- acces %>%
    select(Estado, credito, mal) %>%
    mutate(mal = mal * (-1)) %>%
    bind_rows(select(acces, Estado, credito, bien)) %>%
    mutate_all( ~ replace(., is.na(.), 0)) %>%
    mutate(coef = bien + mal)
  
  
  brks <- c(-2, 2)
  lbls = c(emoji("disappointed"), emoji("smile"))
  
  
  # Plot
  ggplot(acces, aes(
    x = Estado,
    y = coef,
    fill = factor(credito, levels = c("Ninguno", "Otro", "Infonavit"))
  )) +
    geom_bar(stat = "identity", width = .6) +
    geom_text(size = 2.8,
              aes(label = round(if_else(
                coef > 0, coef, -coef
              ), digits = 2)),
              position = position_stack(vjust = 0.5)) +
    coord_flip(ylim = c(-3, 3))  +
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
    theme_solarized() +
    geom_hline(yintercept = 0, size = .5) +
    scale_fill_manual(
      values = c("#F2C1B6", "#F28888", "#D9183B"),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 20,
        face = "bold",
        colour = "black"
      )
    )
  ggsave("satisfaccion.png", plot = last_plot(), scale = 1.3)
} #Satisfacción Infonavit





# Especificos Otro-------------------------------------------------------------
{
  acces <- datos_feif %>%
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
  lbls = paste0(as.character(format(
    if_else(brks > 0, brks, brks * (-1)),
    big.mark = ",",
    digits = 2
  )))
  
  
  acces <-  acces %>%
    left_join(select(malo, Estado, porc), by = "Estado")
  
  # Plot
  ggplot(acces, aes(
    x = reorder(Estado, porc),
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
    ) +
    theme_solarized() +
    theme(
      legend.position = "none",
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      )
    )
} #Info accesibilidad
{
  acces <- datos_feif %>%
    select(vid, estr_habit_bueno, Estado, credito) %>%
    filter(credito == "Otro") %>%
    group_by(Estado, estr_habit_bueno) %>%
    summarize(survey_total(vartype = NULL)) %>%
    arrange(estr_habit_bueno, coef)
  #ssss
  suma <- acces %>%
    mutate(coef = ifelse(coef > 0, coef, coef * (-1))) %>%
    group_by(Estado) %>%
    summarise(suma = sum(coef))
  
  acces <- acces %>%
    left_join(suma, by = "Estado")
  
  bueno <- acces %>%
    filter(estr_habit_bueno == 1)
  
  malo <- acces %>%
    filter(estr_habit_bueno == 0)
  
  acces$coef <-
    if_else(acces$estr_habit_bueno == 1, acces$coef,-acces$coef)
  
  acces$estr_habit_bueno <- as.character(acces$estr_habit_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(
    if_else(brks > 0, brks, brks * (-1)),
    big.mark = ",",
    digits = 2
  )))
  
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
    ) +
    theme_solarized() +
    theme(
      legend.position = "none",
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      )
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Habitabilidad Infonavit
{
  acces <- datos_feif %>%
    select(vid, estr_serv_bueno, Estado, credito) %>%
    filter(credito == "Otro") %>%
    group_by(Estado, estr_serv_bueno) %>%
    summarize(survey_total(vartype = NULL)) %>%
    arrange(estr_serv_bueno, coef)
  #ssss
  suma <- acces %>%
    mutate(coef = ifelse(coef > 0, coef, coef * (-1))) %>%
    group_by(Estado) %>%
    summarise(suma = sum(coef))
  
  acces <- acces %>%
    left_join(suma, by = "Estado")
  
  bueno <- acces %>%
    filter(estr_serv_bueno == 1)
  
  malo <- acces %>%
    filter(estr_serv_bueno == 0)
  
  acces$coef <-
    if_else(acces$estr_serv_bueno == 1, acces$coef,-acces$coef)
  
  acces$estr_serv_bueno <- as.character(acces$estr_serv_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(
    if_else(brks > 0, brks, brks * (-1)),
    big.mark = ",",
    digits = 2
  )))
  
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
    ) +
    theme_solarized() +
    theme(
      legend.position = "none",
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      )
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Servicios Infonavit
{
  acces <- datos_feif %>%
    select(vid, estr_ubi_bueno, Estado, credito) %>%
    filter(credito == "Otro") %>%
    group_by(Estado, estr_ubi_bueno) %>%
    summarize(survey_total(vartype = NULL)) %>%
    arrange(estr_ubi_bueno, coef)
  #ssss
  suma <- acces %>%
    mutate(coef = ifelse(coef > 0, coef, coef * (-1))) %>%
    group_by(Estado) %>%
    summarise(suma = sum(coef))
  
  acces <- acces %>%
    left_join(suma, by = "Estado")
  
  bueno <- acces %>%
    filter(estr_ubi_bueno == 1)
  
  malo <- acces %>%
    filter(estr_ubi_bueno == 0)
  
  acces$coef <-
    if_else(acces$estr_ubi_bueno == 1, acces$coef,-acces$coef)
  
  acces$estr_ubi_bueno <- as.character(acces$estr_ubi_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(
    if_else(brks > 0, brks, brks * (-1)),
    big.mark = ",",
    digits = 2
  )))
  
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
    ) +
    theme_solarized() +
    theme(
      legend.position = "none",
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      )
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Ubicacion Infonavit
{
  acces <- datos_feif %>%
    select(vid, estr_aseq_bueno, Estado, credito) %>%
    filter(credito == "Otro") %>%
    group_by(Estado, estr_aseq_bueno) %>%
    summarize(survey_total(vartype = NULL)) %>%
    arrange(estr_aseq_bueno, coef)
  #ssss
  suma <- acces %>%
    mutate(coef = ifelse(coef > 0, coef, coef * (-1))) %>%
    group_by(Estado) %>%
    summarise(suma = sum(coef))
  
  acces <- acces %>%
    left_join(suma, by = "Estado")
  
  bueno <- acces %>%
    filter(estr_aseq_bueno == 1)
  
  malo <- acces %>%
    filter(estr_aseq_bueno == 0)
  
  acces$coef <-
    if_else(acces$estr_aseq_bueno == 1, acces$coef,-acces$coef)
  
  acces$estr_aseq_bueno <- as.character(acces$estr_aseq_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(
    if_else(brks > 0, brks, brks * (-1)),
    big.mark = ",",
    digits = 2
  )))
  
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
    ) +
    theme_solarized() +
    theme(
      legend.position = "none",
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      )
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Asequibilidad Infonavit
{
  acces <- datos_feif %>%
    select(vid, estr_cul_bueno, Estado, credito) %>%
    filter(credito == "Otro") %>%
    group_by(Estado, estr_cul_bueno) %>%
    summarize(survey_total(vartype = NULL)) %>%
    arrange(estr_cul_bueno, coef)
  #ssss
  suma <- acces %>%
    mutate(coef = ifelse(coef > 0, coef, coef * (-1))) %>%
    group_by(Estado) %>%
    summarise(suma = sum(coef))
  
  acces <- acces %>%
    left_join(suma, by = "Estado")
  
  bueno <- acces %>%
    filter(estr_cul_bueno == 1)
  
  malo <- acces %>%
    filter(estr_cul_bueno == 0)
  
  acces$coef <-
    if_else(acces$estr_cul_bueno == 1, acces$coef,-acces$coef)
  
  acces$estr_cul_bueno <- as.character(acces$estr_cul_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(
    if_else(brks > 0, brks, brks * (-1)),
    big.mark = ",",
    digits = 2
  )))
  
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
    ) +
    theme_solarized() +
    theme(
      legend.position = "none",
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      )
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Cultura Infonavit
{
  acces <- datos_feif %>%
    select(vid, estr_ten_bueno, Estado, credito) %>%
    filter(credito == "Otro") %>%
    group_by(Estado, estr_ten_bueno) %>%
    summarize(survey_total(vartype = NULL)) %>%
    arrange(estr_ten_bueno, coef)
  #ssss
  suma <- acces %>%
    mutate(coef = ifelse(coef > 0, coef, coef * (-1))) %>%
    group_by(Estado) %>%
    summarise(suma = sum(coef))
  
  acces <- acces %>%
    left_join(suma, by = "Estado")
  
  bueno <- acces %>%
    filter(estr_ten_bueno == 1)
  
  malo <- acces %>%
    filter(estr_ten_bueno == 0)
  
  acces$coef <-
    if_else(acces$estr_ten_bueno == 1, acces$coef,-acces$coef)
  
  acces$estr_ten_bueno <- as.character(acces$estr_ten_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(
    if_else(brks > 0, brks, brks * (-1)),
    big.mark = ",",
    digits = 2
  )))
  
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
    ) +
    theme_solarized() +
    theme(
      legend.position = "none",
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      )
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Tenencia Infonavit
{
  acces <- datos_feif %>%
    select(vid, estr_satis_bueno, Estado, credito) %>%
    filter(credito == "Otro") %>%
    group_by(Estado, estr_satis_bueno) %>%
    summarize(survey_total(vartype = NULL)) %>%
    arrange(estr_satis_bueno, coef)
  #ssss
  suma <- acces %>%
    mutate(coef = ifelse(coef > 0, coef, coef * (-1))) %>%
    group_by(Estado) %>%
    summarise(suma = sum(coef))
  
  acces <- acces %>%
    left_join(suma, by = "Estado")
  
  bueno <- acces %>%
    filter(estr_satis_bueno == 1)
  
  malo <- acces %>%
    filter(estr_satis_bueno == 0)
  
  acces$coef <-
    if_else(acces$estr_satis_bueno == 1, acces$coef,-acces$coef)
  
  acces$estr_satis_bueno <- as.character(acces$estr_satis_bueno)
  
  
  brks <- seq(-max(malo$coef), max(bueno$coef), length.out = 5)
  lbls = paste0(as.character(format(
    if_else(brks > 0, brks, brks * (-1)),
    big.mark = ",",
    digits = 2
  )))
  
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
    ) +
    theme_solarized() +
    theme(
      legend.position = "none",
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      )
    ) +
    scale_fill_manual(values = c("#000000", "#beb7a4"))
} #Satisfacción Infonavit



# Grafico P2 Vs CATPCA --------------------------------------------------------------------
library(tidyverse)
library(ggplot2)

p2 <-
  c(52.36, 48.31, 30.14, 21.38, 7.29, 13.98, 33.49, 39.43, 25.59, 27.93)

p2 <- p2/100
nombres <-
  c(
    "Accesibilidad P2",
    "Accesibilidad CATPCA",
    "Habitabilidad P2",
    "Habitabilidad CATPCA",
    "Servicios P2",
    "Servicios CATPCA",
    "Ubicación P2",
    "Ubicación CATPCA" ,
    "Satisfacción P2",
    "Satisfacción CATPCA"
  )

indice <- as_tibble(cbind(nombres, p2)) %>%
  mutate(
    tipo = str_extract(nombres, pattern = "[C|P]{1}"),
    nombre = str_remove(nombres, "P2|CATPCA"),
    p2 = as.numeric(p2)
  )


ggplot(indice, aes(x = nombre, y = p2, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Porcentaje de viviendas con una baja clasificación, por índice",
    caption = "Fuente: Elaboración propia",
    x = "Índice",
    y = "Porcentaje de hogares",
    fill = "Modelo"
  )  +
  scale_fill_manual(
    values = c(
      P = "#590A18",
      C = "#DF5B74"),
    labels = c(
      "DP2",
      "CATPCA"),
    name = "Condición"
  )  +
  theme(
    panel.background = element_rect(
      fill = "#fffffc",
      colour = "#423E37",
      size = 2,
      linetype = "solid"
    ),
    plot.title = element_text(size = 20, face = "bold", color = "black"),
    plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
    axis.text.x = element_text(
      size = 13,
      face = "bold",
      colour = "black",
      angle = 0,
      vjust = 0.5
    ),
    legend.position = "right",
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(colour = "black", face = "bold", size = 15),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.title = element_text(size = 13)
  ) +
  scale_y_continuous(labels = scales::percent)


ggsave(
  "compara_ind.png",
  plot = last_plot(),
  width = 5000,
  height = 2000,
  units = "px"
)


 # Graficos rene -----------------------------------------------------------

load.emojifont("OpenSansEmoji.ttf")
acces <- datos_feif %>%
  select(vid, estr_acc_bueno, Estado, credito) %>%
  group_by(Estado, credito) %>%
  summarize(
    bien = survey_mean(estr_acc_bueno == 1 , vartype = NULL),
    mal = survey_mean(estr_acc_bueno == 0 , vartype = NULL)
  )

acces <- acces %>%
  select(Estado, credito, mal) %>%
  mutate(mal = mal * (-1)) %>%
  bind_rows(select(acces, Estado, credito, bien)) %>%
  mutate_all( ~ replace(., is.na(.), 0)) %>%
  mutate(coef = bien + mal)


brks <- c(-2, 2)
lbls = c(emoji("warning"), emoji("wheelchair"))


# Plot
ggplot(acces, aes(x = credito,
                  y = coef,
                  fill = bien > 0)) +
  geom_bar(stat = "identity", width = .6) +
  geom_text(size = 2.8,
            aes(label = round(if_else(coef > 0, coef, -coef), digits = 2)),
            position = position_stack(vjust = 0.5)) +
  coord_flip(ylim = c(-.8, .8))  +
  scale_y_continuous(labels = lbls,
                     breaks = brks)  +
  facet_wrap( ~ Estado, ncol = 4) +
  labs(
    title = "Condición de Accesibilidad por hogar",
    subtitle = "De acuerdo con el indicador DP2",
    caption = "Fuente: Elaboración propia",
    x = "Estado",
    y = "Porcentaje de hogares",
    fill = element_blank()
  ) +
  theme_solarized() +
  geom_hline(yintercept = 0, size = .5) +
  scale_fill_manual(
    values = c("#F2C1B6", "#F28888", "#D9183B"),
    labels = c("No tiene", "Otro", "Infonavit"),
    name = "Tipo de crédito"
  ) +
  theme(
    panel.background = element_rect(
      fill = "#fffffc",
      colour = "#423E37",
      size = 2,
      linetype = "solid"
    ),
    plot.title = element_text(size = 20, face = "bold", color = "black"),
    plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
    axis.text.x = element_text(
      family = "OpenSansEmoji",
      size = 20,
      face = "bold",
      colour = "black"
    )
  )




# Caracoles ---------------------------------------------------------

## Accesibilidad
{
  acces <- datos_feif %>%
    select(vid, estr_ubi_bueno, Estado) %>%
    group_by(Estado)  %>%
    summarize(
      bien = survey_mean(estr_ubi_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_ubi_bueno == 0 , vartype = NULL)
    )

  
  # ----- This section prepare a dataframe for labels ---- #
  # Get the name and the y position of each label
  label_data <- acces %>% 
    arrange(bien) %>% 
    cbind(id =seq(1:32))
  
  # calculate the ANGLE of the labels
  number_of_bar <- nrow(label_data)
  angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  label_data$vjust <-ifelse(angle < -90, 0.5, 0)
  
  # flip angle BY to make them readable
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  label_data$posicion <- rep(0.3,32)
  
  # Plot
  ggplot(acces, aes(x = reorder(Estado, bien),
                    y = bien)) +
    geom_bar(stat = "identity",
             width = 0.8, fill ="#79d3be", position = position_dodge())  +
    coord_polar(start = 0) +
    geom_text(data =label_data, aes(x=id, y= posicion + 0.1, 
                                    label=Estado, 
                                    vjust= vjust), 
              color="black", fontface="bold",alpha=0.8, size=4, 
              angle= label_data$angle, inherit.aes = FALSE ) +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed")   +
    labs(
      title = "Proporción de viviendas con buena Ubicación",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de viviendas",
      fill = element_blank()
    )  +
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 12,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 8,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      axis.text.y = element_blank()
    )
  

  ggsave(
    "ubicación.png",
    plot = last_plot(),
    width = 3500,
    height = 2000,
    units = "px"
  )

  ## Habitabilidad
  
acces <- datos_feif %>%
    select(vid, estr_habit_bueno, Estado) %>%
    group_by(Estado) %>%
    summarize(
      bien = survey_mean(estr_habit_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_habit_bueno == 0 , vartype = NULL)
    )

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- acces %>% 
  arrange(bien) %>% 
  cbind(id =seq(1:32))

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$vjust <-ifelse(angle < -90, 0.5, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)

label_data$posicion <- rep(0.55,32)

  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = reorder(Estado, bien),
                    y = bien)) +
    geom_bar(stat = "identity",
             width = 0.8, fill ="#F2C1B6", position = position_dodge())  +
    coord_polar(start = 0) +
    geom_text(data =label_data, aes(x=id, y= posicion, 
                                    label=Estado, 
                                    vjust= vjust), 
              color="black", fontface="bold",alpha=0.8, size=3, 
              angle= label_data$angle, inherit.aes = FALSE ) +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed")   +
    labs(
      title = "Proporción de viviendas con buena Habitabilidad",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de viviendas",
      fill = element_blank()
    )  +
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 12,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 8,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      axis.text.y = element_blank()
    )
  
  
  ggsave(
    "habitabilidad.png",
    plot = last_plot(),
    width = 3500,
    height = 2000,
    units = "px"
  )
  
## Servicios

  acces <- datos_feif %>%
    select(vid, estr_serv_bueno, Estado) %>%
    group_by(Estado) %>%
    summarize(
      bien = survey_mean(estr_serv_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_serv_bueno == 0 , vartype = NULL)
    )
  

  # ----- This section prepare a dataframe for labels ---- #
  # Get the name and the y position of each label
  label_data <- acces %>% 
    arrange(bien) %>% 
    cbind(id =seq(1:32))
  
  # calculate the ANGLE of the labels
  number_of_bar <- nrow(label_data)
  angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  label_data$vjust <-ifelse(angle < -90, 0.5, 0)
  
  # flip angle BY to make them readable
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  label_data$posicion <- rep(0.6,32)
  
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = reorder(Estado, bien),
                    y = bien)) +
    geom_bar(stat = "identity",
             width = 0.8, fill ="#F28888", position = position_dodge())  +
    coord_polar(start = 0) +
    geom_text(data =label_data, aes(x=id, y= posicion, 
                                    label=Estado, 
                                    vjust= vjust), 
              color="black", fontface="bold",alpha=0.8, size=3.3, 
              angle= label_data$angle, inherit.aes = FALSE ) +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed")   +
    labs(
      title = "Proporción de viviendas con buenos Servicios",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de viviendas",
      fill = element_blank()
    )  +
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 12,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 8,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      axis.text.y = element_blank()
    )
  
  
  ggsave(
    "servicios.png",
    plot = last_plot(),
    width = 3500,
    height = 2000,
    units = "px"
  )
  
  


#Ubicacion

acces <- datos_feif %>%
    select(vid, estr_ubi_bueno, Estado) %>%
    group_by(Estado) %>%
    summarize(
      bien = survey_mean(estr_ubi_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_ubi_bueno == 0 , vartype = NULL)
    )

  
  # ----- This section prepare a dataframe for labels ---- #
  # Get the name and the y position of each label
  label_data <- acces %>% 
    arrange(bien) %>% 
    cbind(id =seq(1:32))
  
  # calculate the ANGLE of the labels
  number_of_bar <- nrow(label_data)
  angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  label_data$vjust <-ifelse(angle < -90, 0.5, 0)
  
  # flip angle BY to make them readable
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  label_data$posicion <- rep(0.6,32)
  
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = reorder(Estado, bien),
                    y = bien)) +
    geom_bar(stat = "identity",
             width = 0.8, fill ="#F28888", position = position_dodge())  +
    coord_polar(start = 0) +
    geom_text(data =label_data, aes(x=id, y= posicion, 
                                    label=Estado, 
                                    vjust= vjust), 
              color="black", fontface="bold",alpha=0.8, size=3.3, 
              angle= label_data$angle, inherit.aes = FALSE ) +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed")   +
    labs(
      title = "Proporción de viviendas con buenos Servicios",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de viviendas",
      fill = element_blank()
    )  +
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 12,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 8,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      axis.text.y = element_blank()
    )
  
  
  ggsave(
    "servicios.png",
    plot = last_plot(),
    width = 3500,
    height = 2000,
    units = "px"
  )
  

{
  acces <- datos_feif %>%
    select(vid, estr_aseq_bueno, Estado) %>%
    group_by(Estado) %>%
    summarize(
      bien = survey_mean(estr_aseq_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_aseq_bueno == 0 , vartype = NULL)
    )
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = reorder(Estado, bien),
                    y = bien)) +
    geom_bar(stat = "identity",
             width = 0.8, fill ="#DF5B74") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed")   +
    labs(
      title = "Porcentaje de viviendas con buena asequibilidad",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de viviendas",
      fill = element_blank()
    )  +
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 25,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 15,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 14,
        face = "bold",
        colour = "black",
        angle = 90,
        hjust = 1
      ),
      legend.position = "top",
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 14)
    )+
    scale_y_continuous(labels = scales::percent)
  
  
  ggsave(
    "asequibilidad.png",
    plot = last_plot(),
    width = 5000,
    height = 3000,
    units = "px"
  )
} #Asequibilidad Infonavit

{
  acces <- datos_feif %>%
    select(vid, estr_cul_bueno, Estado) %>%
    group_by(Estado) %>%
    summarize(
      bien = survey_mean(estr_cul_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_cul_bueno == 0 , vartype = NULL)
    )
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = reorder(Estado, bien),
                    y = bien)) +
    geom_bar(stat = "identity",
             width = 0.8, fill ="#F00EA8") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed")   +
    labs(
      title = "Porcentaje de viviendas con buena adecuación cultural",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de viviendas",
      fill = element_blank()
    )  +
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 25,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 15,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 14,
        face = "bold",
        colour = "black",
        angle = 90,
        hjust = 1
      ),
      legend.position = "top",
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 14)
    )+
    scale_y_continuous(labels = scales::percent)
  
  
  ggsave(
    "cultura.png",
    plot = last_plot(),
    width = 5000,
    height = 3000,
    units = "px"
  )
} #Cultura Infonavit

{
  acces <- datos_feif %>%
    select(vid, estr_ten_bueno, Estado) %>%
    group_by(Estado) %>%
    summarize(
      bien = survey_mean(estr_ten_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_ten_bueno == 0 , vartype = NULL)
    )
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = reorder(Estado, bien),
                    y = bien)) +
    geom_bar(stat = "identity",
             width = 0.8, fill ="#E6410E") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed")   +
    labs(
      title = "Porcentaje de viviendas con buena tenencia",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de viviendas",
      fill = element_blank()
    )  +
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 25,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 15,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 14,
        face = "bold",
        colour = "black",
        angle = 90,
        hjust = 1
      ),
      legend.position = "top",
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 14)
    )+
    scale_y_continuous(labels = scales::percent)
  
  
  ggsave(
    "tenencia.png",
    plot = last_plot(),
    width = 5000,
    height = 3000,
    units = "px"
  )
} #Tenencia Infonavit

{
  acces <- datos_feif %>%
    select(vid, estr_satis_bueno, Estado) %>%
    group_by(Estado) %>%
    summarize(
      bien = survey_mean(estr_satis_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_satis_bueno == 0 , vartype = NULL)
    )
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = reorder(Estado, bien),
                    y = bien)) +
    geom_bar(stat = "identity",
             width = 0.8, fill ="#D9D02E") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed")   +
    labs(
      title = "Porcentaje de viviendas con buena satisfacción",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Estado",
      y = "Porcentaje de viviendas",
      fill = element_blank()
    )  +
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 25,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 15,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 14,
        face = "bold",
        colour = "black",
        angle = 90,
        hjust = 1
      ),
      legend.position = "top",
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 14)
    )+
    scale_y_continuous(labels = scales::percent)
  
  
  ggsave(
    "satisfaccion.png",
    plot = last_plot(),
    width = 5000,
    height = 3000,
    units = "px"
  )
} #Satisfacción Infonavit



# Con wrap ----------------------------------------------------------------

{
  load.emojifont("OpenSansEmoji.ttf")
  acces <- datos_feif %>%
    select(vid, estr_acc_bueno, Estado, credito) %>%
    group_by(Estado, credito)  %>%
    summarize(
      bien = survey_mean(estr_acc_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_acc_bueno == 0 , vartype = NULL)
    )
  
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = Estado,
                    y = bien,
                    fill = credito)) +
    geom_bar(stat = "identity",
             width = 0.8,
             position = "dodge") +
    geom_hline(aes(yintercept = mean(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0, .8))  +
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
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 8,
        face = "bold",
        colour = "black"
      ),
      legend.position = "top",
      axis.title.y = element_blank()
    ) +
    facet_wrap( ~ credito)
  
  
  ggsave(
    "accesibilidad_wrap.png",
    plot = last_plot(),
    width = 5000,
    height = 2000,
    units = "px"
  )
} #Info accesibilidad

{
  library(emojifont)
  acces <- datos_feif %>%
    select(vid, estr_habit_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_habit_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_habit_bueno == 0 , vartype = NULL)
    )
  
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  
  # Plot
  ggplot(acces, aes(x = Estado,
                    y = bien,
                    fill = credito)) +
    geom_bar(stat = "identity",
             width = 0.8,
             position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0, 1))  +
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
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 8,
        face = "bold",
        colour = "black"
      ),
      legend.position = "top",
      axis.title.y = element_blank()
    ) +
    facet_wrap( ~ credito)
  
  ggsave(
    "habitablidad_wrap.png",
    plot = last_plot(),
    width = 5000,
    height = 2000,
    units = "px"
  )
} #Habitabilidad Infonavit

{
  acces <- datos_feif %>%
    select(vid, estr_serv_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_serv_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_serv_bueno == 0 , vartype = NULL)
    )
  
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  
  
  # Plot
  ggplot(acces, aes(x = Estado,
                    y = bien,
                    fill = credito)) +
    geom_bar(stat = "identity",
             width = 0.8,
             position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0, 1))  +
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
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 8,
        face = "bold",
        colour = "black"
      ),
      legend.position = "top",
      axis.title.y = element_blank()
    ) +
    facet_wrap( ~ credito)
  
  ggsave(
    "servicios_wrap.png",
    plot = last_plot(),
    width = 5000,
    height = 2000,
    units = "px"
  )
} #Servicios Infonavit

{
  library(emojifont)
  acces <- datos_feif %>%
    select(vid, estr_ubi_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_ubi_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_ubi_bueno == 0 , vartype = NULL)
    )
  
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  
  # Plot
  ggplot(acces, aes(x = Estado,
                    y = bien,
                    fill = credito)) +
    geom_bar(stat = "identity",
             width = 0.8,
             position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0, 1))  +
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
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 8,
        face = "bold",
        colour = "black"
      ),
      legend.position = "top",
      axis.title.y = element_blank()
    ) +
    facet_wrap( ~ credito)
  
  ggsave(
    "ubicacion_wrap.png",
    plot = last_plot(),
    width = 5000,
    height = 2000,
    units = "px"
  )
  
} #Ubicacion Infonavit

{
  acces <- datos_feif %>%
    select(vid, estr_aseq_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_aseq_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_aseq_bueno == 0 , vartype = NULL)
    )
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = Estado,
                    y = bien,
                    fill = credito)) +
    geom_bar(stat = "identity",
             width = 0.8,
             position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0, 1))  +
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
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 8,
        face = "bold",
        colour = "black"
      ),
      legend.position = "top",
      axis.title.y = element_blank()
    ) +
    facet_wrap( ~ credito)
  
  ggsave(
    "asequibilidad_wrap.png",
    plot = last_plot(),
    width = 5000,
    height = 2000,
    units = "px"
  )
} #Asequibilidad Infonavit

{
  acces <- datos_feif %>%
    select(vid, estr_cul_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_cul_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_cul_bueno == 0 , vartype = NULL)
    )
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = Estado,
                    y = bien,
                    fill = credito)) +
    geom_bar(stat = "identity",
             width = 0.8,
             position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0, .80))  +
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
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 8,
        face = "bold",
        colour = "black"
      ),
      legend.position = "top",
      axis.title.y = element_blank()
    ) +
    facet_wrap( ~ credito)
  
  ggsave(
    "adecuacion_wrap.png",
    plot = last_plot(),
    width = 5000,
    height = 2000,
    units = "px"
  )
} #Cultura Infonavit

{
  acces <- datos_feif %>%
    select(vid, estr_ten_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_ten_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_ten_bueno == 0 , vartype = NULL)
    )
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = Estado,
                    y = bien,
                    fill = credito)) +
    geom_bar(stat = "identity",
             width = 0.8,
             position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0, 1))  +
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
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 8,
        face = "bold",
        colour = "black"
      ),
      legend.position = "top",
      axis.title.y = element_blank()
    ) +
    facet_wrap( ~ credito)
  
  ggsave(
    "tenencia_wrap.png",
    plot = last_plot(),
    width = 5000,
    height = 2000,
    units = "px"
  )
} #Tenencia Infonavit

{
  acces <- datos_feif %>%
    select(vid, estr_satis_bueno, Estado, credito) %>%
    group_by(Estado, credito) %>%
    summarize(
      bien = survey_mean(estr_satis_bueno == 1 , vartype = NULL),
      mal = survey_mean(estr_satis_bueno == 0 , vartype = NULL)
    )
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  # Plot
  ggplot(acces, aes(x = Estado,
                    y = bien,
                    fill = credito)) +
    geom_bar(stat = "identity",
             width = 0.8,
             position = "dodge") +
    geom_hline(aes(yintercept = median(bien), color = "Promedio"), linetype = "dashed") +
    coord_flip(ylim = c(0, 1))  +
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
    scale_fill_manual(
      values = c(
        Ninguno = "#F2C1B6",
        Otro = "#F28888",
        Infonavit = "#D9183B"
      ),
      labels = c("No tiene", "Otro", "Infonavit"),
      name = "Tipo de crédito"
    ) +
    scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black"
      ),
      plot.subtitle =  element_text(
        size = 10,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(
        family = "OpenSansEmoji",
        size = 8,
        face = "bold",
        colour = "black"
      ),
      legend.position = "top",
      axis.title.y = element_blank()
    ) +
    facet_wrap( ~ credito)
  
  ggsave(
    "satisfaccion_wrap.png",
    plot = last_plot(),
    width = 5000,
    height = 2000,
    units = "px"
  )
} #Satisfacción Infonavit




# Situaciones -------------------------------------------------------------

#Hago el join
ts_dem <- read.csv("tsdem.csv") %>%
  left_join(datos_if, by = "vid")

datos_feif2 <-
  as_survey_design(ts_dem,
                   ids = UPM_DIS.x,
                   strata = EST_DIS.x,
                   weights = FACTOR.x)


#promedio general
promedio <- datos_feif2 %>%
  summarize(
    Satisfacción = survey_mean(estr_satis_bueno == 1, vartype = NULL),
    Ubicación = survey_mean(estr_ubi_bueno == 1, vartype = NULL),
    Accesibilidad = survey_mean(estr_acc_bueno == 1, vartype = NULL),
    Habitabilidad = survey_mean(estr_habit_bueno == 1, vartype = NULL),
    Servicios = survey_mean(estr_serv_bueno == 1, vartype = NULL),
    Asequibilidad = survey_mean(estr_aseq_bueno == 1, vartype = NULL),
    Tenencia = survey_mean(estr_ten_bueno == 1, vartype = NULL),
    Cultura = survey_mean(estr_cul_bueno == 1, vartype = NULL)
  ) %>%
  mutate(nivel = "Promedio")



educacion <- datos_feif2 %>%
  filter(PAREN == 1) %>%
  group_by(nivel = NIV < 3) %>%
  summarize(
    Satisfacción = survey_mean(estr_satis_bueno == 1, vartype = NULL),
    Ubicación = survey_mean(estr_ubi_bueno == 1, vartype = NULL),
    Accesibilidad = survey_mean(estr_acc_bueno == 1, vartype = NULL),
    Habitabilidad = survey_mean(estr_habit_bueno == 1, vartype = NULL),
    Servicios = survey_mean(estr_serv_bueno == 1, vartype = NULL),
    Asequibilidad = survey_mean(estr_aseq_bueno == 1, vartype = NULL),
    Tenencia = survey_mean(estr_ten_bueno == 1, vartype = NULL),
    Cultura = survey_mean(estr_cul_bueno == 1, vartype = NULL)
  ) %>%
  filter(nivel == TRUE) %>%
  mutate(nivel = if_else(nivel == TRUE, "Educación", "NA"))


indigena <- datos_feif2 %>%
  filter(PAREN == 1) %>%
  group_by(nivel = P2_6 == 1) %>%
  summarize(
    Satisfacción = survey_mean(estr_satis_bueno == 1, vartype = NULL),
    Ubicación = survey_mean(estr_ubi_bueno == 1, vartype = NULL),
    Accesibilidad = survey_mean(estr_acc_bueno == 1, vartype = NULL),
    Habitabilidad = survey_mean(estr_habit_bueno == 1, vartype = NULL),
    Servicios = survey_mean(estr_serv_bueno == 1, vartype = NULL),
    Asequibilidad = survey_mean(estr_aseq_bueno == 1, vartype = NULL),
    Tenencia = survey_mean(estr_ten_bueno == 1, vartype = NULL),
    Cultura = survey_mean(estr_cul_bueno == 1, vartype = NULL)
  ) %>%
  filter(nivel == TRUE) %>%
  mutate(nivel = if_else(nivel == TRUE, "Indigena", "NA"))

mujer <- datos_feif2 %>%
  group_by(nivel = SEXO == 2) %>%
  summarize(
    Satisfacción = survey_mean(estr_satis_bueno == 1, vartype = NULL),
    Ubicación = survey_mean(estr_ubi_bueno == 1, vartype = NULL),
    Accesibilidad = survey_mean(estr_acc_bueno == 1, vartype = NULL),
    Habitabilidad = survey_mean(estr_habit_bueno == 1, vartype = NULL),
    Servicios = survey_mean(estr_serv_bueno == 1, vartype = NULL),
    Asequibilidad = survey_mean(estr_aseq_bueno == 1, vartype = NULL),
    Tenencia = survey_mean(estr_ten_bueno == 1, vartype = NULL),
    Cultura = survey_mean(estr_cul_bueno == 1, vartype = NULL)
  ) %>%
  filter(nivel == TRUE) %>%
  mutate(nivel = if_else(nivel == TRUE, "Mujer", "NA"))

mayor <- datos_feif2 %>%
  filter(EDAD != 99) %>%
  group_by(nivel = EDAD > 65) %>%
  summarize(
    Satisfacción = survey_mean(estr_satis_bueno == 1, vartype = NULL),
    Ubicación = survey_mean(estr_ubi_bueno == 1, vartype = NULL),
    Accesibilidad = survey_mean(estr_acc_bueno == 1, vartype = NULL),
    Habitabilidad = survey_mean(estr_habit_bueno == 1, vartype = NULL),
    Servicios = survey_mean(estr_serv_bueno == 1, vartype = NULL),
    Asequibilidad = survey_mean(estr_aseq_bueno == 1, vartype = NULL),
    Tenencia = survey_mean(estr_ten_bueno == 1, vartype = NULL),
    Cultura = survey_mean(estr_cul_bueno == 1, vartype = NULL)
  ) %>%
  filter(nivel == TRUE) %>%
  mutate(nivel = if_else(nivel == TRUE, "Mayor", "NA"))




combinado <- bind_rows(mujer, indigena, educacion, mayor)
combinados <-
  pivot_longer(
    combinado,
    cols = c(
      "Satisfacción",
      "Ubicación",
      "Accesibilidad",
      "Habitabilidad",
      "Servicios",
      "Asequibilidad",
      "Tenencia",
      "Cultura"
    ),
    names_to = c("indice")
  )
promedios <-
  pivot_longer(
    promedio,
    cols = c(
      "Satisfacción",
      "Ubicación",
      "Accesibilidad",
      "Habitabilidad",
      "Servicios",
      "Asequibilidad",
      "Tenencia",
      "Cultura"
    ),
    names_to = c("indice")
  ) %>%
  mutate(promedio = value)

tabla <- combinados %>%
  left_join(select(promedios, indice, promedio), by = "indice") %>% 
  mutate(indice = recode(indice, Accesibilidad = "acc_bien",
                        Asequibilidad = "aseq_bien" ,
                        Cultura = "cul_bien" ,
                        Habitabilidad = "habit_bien",
                        Satisfacción= "satis_bien" ,
                        Servicios= "serv_bien",
                        Tenencia= "ten_bien",
                        Ubicación =  "ubi_bien" ))

write_csv(tabla, "situaciones.csv")
install.packages("plotly")
library(plotly)

graf <- ggplot(tabla, aes(x = indice,
                  y = value,
                  fill = nivel)) +
  geom_bar(stat = "identity",
           width = 0.8,
           position = "dodge") +
  labs(
    title = "Porcentaje de hogares en buen estado, por índice",
    subtitle = "De acuerdo con el indicador DP2",
    caption = "Fuente: Elaboración propia",
    x = "Índice",
    y = "Porcentaje de hogares",
    fill = element_blank()
  )  +
  coord_cartesian(ylim = c(0, 1)) +
  geom_errorbar(
    aes(ymax = promedio, ymin = promedio, color = "Promedio"),
    width = 0.8,
    position = "dodge"
  ) +
  scale_fill_manual(
    values = c(
      Educación = "#590A18",
      Indigena = "#DF5B74",
      Mujer = "#D9183B",
      Mayor = "#A6122D"
    ),
    labels = c(
      "Bajo nivel escolar",
      "Jefe de hogar indígena",
      "Jefe de hogar mujer",
      "Adulto Mayor"
    ),
    name = "Condición"
  ) +
  scale_color_manual(values = c("Promedio" = "#59252E"),
                     name = element_blank()) +
  theme(
    panel.background = element_rect(
      fill = "#fffffc",
      colour = "#423E37",
      size = 2,
      linetype = "solid"
    ),
    plot.title = element_text(size = 20, face = "bold", color = "black"),
    plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
    axis.text.x = element_text(
      size = 10,
      face = "bold",
      colour = "black",
      angle = 0,
      vjust = 0.5
    ),
    legend.position = "top",
    axis.title.y = element_text(size = 13, face = "bold"),
    axis.title.x = element_text(colour = "black", face = "bold"),
    axis.text.y = element_text(face = "bold"),
  ) +
  scale_y_continuous(labels = scales::percent)

ggplotly(graf)

ggsave(
  "indice_gen.png",
  plot = last_plot(),
  width = 5000,
  height = 2000,
  units = "px"
)





# Peor --------------------------------------------------------------------

#promedio general
promedio <- datos_feif2 %>%
  summarize(
    Satisfacción = survey_mean(estr_satis_bueno == 0, vartype = NULL),
    Ubicación = survey_mean(estr_ubi_bueno == 0, vartype = NULL),
    Accesibilidad = survey_mean(estr_acc_bueno == 0, vartype = NULL),
    Habitabilidad = survey_mean(estr_habit_bueno == 0, vartype = NULL),
    Servicios = survey_mean(estr_serv_bueno == 0, vartype = NULL),
    Asequibilidad = survey_mean(estr_aseq_bueno == 0, vartype = NULL),
    Tenencia = survey_mean(estr_ten_bueno == 0, vartype = NULL),
    Cultura = survey_mean(estr_satis_bueno == 0, vartype = NULL)
  ) %>%
  mutate(nivel = "Promedio")


educacion <- datos_feif2 %>%
  filter(PAREN == 1) %>%
  group_by(nivel = NIV < 3) %>%
  summarize(
    Satisfacción = survey_mean(estr_satis_bueno == 0, vartype = NULL),
    Ubicación = survey_mean(estr_ubi_bueno == 0, vartype = NULL),
    Accesibilidad = survey_mean(estr_acc_bueno == 0, vartype = NULL),
    Habitabilidad = survey_mean(estr_habit_bueno == 0, vartype = NULL),
    Servicios = survey_mean(estr_serv_bueno == 0, vartype = NULL),
    Asequibilidad = survey_mean(estr_aseq_bueno == 0, vartype = NULL),
    Tenencia = survey_mean(estr_ten_bueno == 0, vartype = NULL),
    Cultura = survey_mean(estr_satis_bueno == 0, vartype = NULL)
  ) %>%
  filter(nivel == TRUE) %>%
  mutate(nivel = if_else(nivel == TRUE, "Educación", "NA"))


indigena <- datos_feif2 %>%
  filter(PAREN == 1) %>%
  group_by(nivel = P2_6 == 1) %>%
  summarize(
    Satisfacción = survey_mean(estr_satis_bueno == 0, vartype = NULL),
    Ubicación = survey_mean(estr_ubi_bueno == 0, vartype = NULL),
    Accesibilidad = survey_mean(estr_acc_bueno == 0, vartype = NULL),
    Habitabilidad = survey_mean(estr_habit_bueno == 0, vartype = NULL),
    Servicios = survey_mean(estr_serv_bueno == 0, vartype = NULL),
    Asequibilidad = survey_mean(estr_aseq_bueno == 0, vartype = NULL),
    Tenencia = survey_mean(estr_ten_bueno == 0, vartype = NULL),
    Cultura = survey_mean(estr_satis_bueno == 0, vartype = NULL)
  ) %>%
  filter(nivel == TRUE) %>%
  mutate(nivel = if_else(nivel == TRUE, "Indigena", "NA"))

mujer <- datos_feif2 %>%
  group_by(nivel = SEXO == 2) %>%
  summarize(
    Satisfacción = survey_mean(estr_satis_bueno == 0, vartype = NULL),
    Ubicación = survey_mean(estr_ubi_bueno == 0, vartype = NULL),
    Accesibilidad = survey_mean(estr_acc_bueno == 0, vartype = NULL),
    Habitabilidad = survey_mean(estr_habit_bueno == 0, vartype = NULL),
    Servicios = survey_mean(estr_serv_bueno == 0, vartype = NULL),
    Asequibilidad = survey_mean(estr_aseq_bueno == 0, vartype = NULL),
    Tenencia = survey_mean(estr_ten_bueno == 0, vartype = NULL),
    Cultura = survey_mean(estr_satis_bueno == 0, vartype = NULL)
  ) %>%
  filter(nivel == TRUE) %>%
  mutate(nivel = if_else(nivel == TRUE, "Mujer", "NA"))





combinado <- bind_rows(mujer, indigena, educacion, promedio)
combinados <-
  pivot_longer(
    combinado,
    cols = c(
      "Satisfacción",
      "Ubicación",
      "Accesibilidad",
      "Habitabilidad",
      "Servicios",
      "Asequibilidad",
      "Tenencia",
      "Cultura"
    ),
    names_to = c("indice")
  )
promedios <-
  pivot_longer(
    promedio,
    cols = c(
      "Satisfacción",
      "Ubicación",
      "Accesibilidad",
      "Habitabilidad",
      "Servicios",
      "Asequibilidad",
      "Tenencia",
      "Cultura"
    ),
    names_to = c("indice")
  ) %>%
  mutate(promedio = value)

tabla <- combinados %>%
  left_join(promedios, by = "indice") %>%
  select(indice, nivel = nivel.x , value = value.x, promedio)


ggplot(combinados, aes(x = indice,
                       y = value,
                       fill = nivel)) +
  geom_bar(stat = "identity",
           width = 0.8,
           position = "dodge") +
  labs(
    title = "Porcentaje de hogares en buen estado, por índice",
    subtitle = "De acuerdo con el indicador DP2",
    caption = "Fuente: Elaboración propia",
    x = "Índice",
    y = "Porcentaje de hogares",
    fill = element_blank()
  )  +
  scale_fill_manual(
    values = c(
      Educación = "#F2C1B6",
      Indigena = "#F28888",
      Mujer = "#D9183B"
    ),
    labels = c(
      "Bajo nivel escolar",
      "Jefe de hogar indígena",
      "Jefe de hogar mujer"
    ),
    name = "Condición"
  ) +
  theme(
    panel.background = element_rect(
      fill = "#fffffc",
      colour = "#423E37",
      size = 2,
      linetype = "solid"
    ),
    plot.title = element_text(size = 20, face = "bold", color = "black"),
    plot.subtitle =  element_text(size = 10, face = "bold", color = "black"),
    axis.text.x = element_text(
      family = "OpenSansEmoji",
      size = 8,
      face = "bold",
      colour = "black"
    ),
    legend.position = "top",
    axis.title.y = element_blank()
  )




# Por estado, indicador ---------------------------------------------------

{
  acces <- datos_feif %>%
    group_by(Estado) %>% 
    summarize(
      Satisfacción = survey_mean(estr_satis_bueno == 1, vartype = NULL),
      Ubicación = survey_mean(estr_ubi_bueno == 1, vartype = NULL),
      Accesibilidad = survey_mean(estr_acc_bueno == 1, vartype = NULL),
      Habitabilidad = survey_mean(estr_habit_bueno == 1, vartype = NULL),
      Servicios = survey_mean(estr_serv_bueno == 1, vartype = NULL),
      Asequibilidad = survey_mean(estr_aseq_bueno == 1, vartype = NULL),
      Tenencia = survey_mean(estr_ten_bueno == 1, vartype = NULL),
      Cultura = survey_mean(estr_cul_bueno == 1, vartype = NULL)
    )
  
  nacional <-
    pivot_longer(
      acces,
      cols = c(
        "Satisfacción",
        "Ubicación",
        "Accesibilidad",
        "Habitabilidad",
        "Servicios",
        "Asequibilidad",
        "Tenencia",
        "Cultura"
      ),
      names_to = c("indice")
    )
nacional3 <- nacional
nacional2 <- nacional %>% 
  mutate(malo = (1 - value)*-1,
         value = 0) %>% 
  bind_rows(select(nacional3, indice, value)) %>% 
  mutate_all(~replace(., is.na(.),0)) %>% 
  mutate(coef = value + malo)
  

brks <- seq(-1, 1, .20)
lbls = as.character(paste0((if_else(brks > 0, brks * (100), -brks * 100)),"%"))

  ggplot(nacional2, aes(x = reorder(indice,coef), 
                         y = coef)) +
    geom_bar(stat = "identity",
             width = 0.8,
             fill = "#D9183B") +
    coord_flip() +
    geom_hline(aes(yintercept = 0), size = 1) + 
    labs(
      title = "Porcentaje de viviendas a nivel nacional, de acuerdo con su evaluación",
      subtitle = "De acuerdo con el indicador DP2",
      caption = "Fuente: Elaboración propia",
      x = "Índice",
      y = "Porcentaje de hogares",
      fill = element_blank()
    )  +
    scale_y_continuous(labels = lbls,
                       breaks = brks) +
    
    theme(
      panel.background = element_rect(
        fill = "#fffffc",
        colour = "#423E37",
        size = 2,
        linetype = "solid"
      ),
      plot.title = element_text(size = 25, face = "bold", color = "black"),
      plot.subtitle =  element_text(size = 15, face = "bold", color = "black"),
      axis.text = element_text(
        family = "OpenSansEmoji",
        size = 14,
        face = "bold",
        colour = "black"
      ),
      legend.position = "top",
      axis.title = element_text(size = 16, face = "bold")
    ) 
ggsave(
    "indices_nac.png",
    plot = last_plot(),
    width = 5000,
    height = 2000,
    units = "px"
  )
  





# FINAL -------------------------------------------------------------------

acces <- datos_feif %>%
  group_by(Estado)  %>%
  summarize(
    Satisfacción = survey_mean(estr_satis_bueno == 1, vartype = NULL),
    Ubicación = survey_mean(estr_ubi_bueno == 1, vartype = NULL),
    Accesibilidad = survey_mean(estr_acc_bueno == 1, vartype = NULL),
    Habitabilidad = survey_mean(estr_habit_bueno == 1, vartype = NULL),
    Servicios = survey_mean(estr_serv_bueno == 1, vartype = NULL),
    Asequibilidad = survey_mean(estr_aseq_bueno == 1, vartype = NULL),
    Tenencia = survey_mean(estr_ten_bueno == 1, vartype = NULL),
    Cultura = survey_mean(estr_cul_bueno == 1, vartype = NULL))

nacional <-
  pivot_longer(
    acces,
    cols = c(
      "Satisfacción",
      "Ubicación",
      "Accesibilidad",
      "Habitabilidad",
      "Servicios",
      "Asequibilidad",
      "Tenencia",
      "Cultura"
    ),
    names_to = c("indice")
  )


nacional3 <- nacional
nacional2 <- nacional %>% 
  mutate(malo = (1 - value)*-1)


brks <- seq(-1, 1, .20)
lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))

colors <- c("value" = "#D9183B", 
            "malo" = "#F2C1B6")
nacional2 %>% 
  filter(Estado %in% c("Aguascalientes", "Baja California", "Baja California Sur",
                       "Campeche", "CDMX", "Chiapas", "Chihuahua", "Coahuila",
                       "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo",
                       "Jalisco", "Mexico", "Michoacan", "Morelos", "Nayarit",
                       "Nuevo Leon", "Oaxaca")) %>% 
ggplot(aes(x= indice)) +
  geom_bar(aes(y = value, fill = "value"), stat = "identity", width = 0.8) +
  geom_bar(aes(y= malo, fill= "malo"),stat = "identity", width = 0.8)+
  coord_flip() +
  scale_y_continuous(labels = lbls,
                     breaks = brks) +
  scale_fill_manual(
    values = c(
      "value" = "#D9183B",
    "malo" = "#F2C1B6"
    ),
    labels = c("Buena", "Mala"),
    name = "Condición") +
  labs(
    title = "Porcentaje de hogares en buena y mala condición, por estado e indicador",
    subtitle = "De acuerdo con el indicador DP2",
    caption = "Fuente: Elaboración propia",
    x = "Estado",
    y = "Porcentaje de hogares"
  ) +
  theme(
    panel.background = element_rect(
      fill = "#fffffc",
      colour = "#423E37",
      size = 2,
      linetype = "solid"
    ),
    plot.title = element_text(
      size = 20,
      face = "bold",
      color = "black"
    ),
    plot.subtitle =  element_text(
      size = 10,
      face = "bold",
      color = "black"
    ),
    axis.text.x = element_text(
      family = "OpenSansEmoji",
      size = 8,
      face = "bold",
      colour = "black"
    ),
    legend.position = "top",
    axis.title.y = element_blank()
  ) +
  facet_wrap(~ Estado)

ggsave("Final1.png", last_plot(), width = 12, height = 7)

nacional2 %>% 
  filter(!(Estado %in% c("Aguascalientes", "Baja California", "Baja California Sur",
                       "Campeche", "CDMX", "Chiapas", "Chihuahua", "Coahuila",
                       "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo",
                       "Jalisco", "Mexico", "Michoacan", "Morelos", "Nayarit",
                       "Nuevo Leon", "Oaxaca"))) %>% 
  ggplot(aes(x= indice)) +
  geom_bar(aes(y = value, fill = "value"), stat = "identity", width = 0.8) +
  geom_bar(aes(y= malo, fill= "malo"),stat = "identity", width = 0.8)+
  coord_flip() +
  scale_y_continuous(labels = lbls,
                     breaks = brks) +
  scale_fill_manual(
    values = c(
      "value" = "#D9183B",
      "malo" = "#F2C1B6"
    ),
    labels = c("Buena", "Mala"),
    name = "Condición") +
  labs(
    title = "Porcentaje de hogares en buena y mala condición, por estado e indicador",
    subtitle = "De acuerdo con el indicador DP2",
    caption = "Fuente: Elaboración propia",
    x = "Estado",
    y = "Porcentaje de hogares"
  ) +
  theme(
    panel.background = element_rect(
      fill = "#fffffc",
      colour = "#423E37",
      size = 2,
      linetype = "solid"
    ),
    plot.title = element_text(
      size = 20,
      face = "bold",
      color = "black"
    ),
    plot.subtitle =  element_text(
      size = 10,
      face = "bold",
      color = "black"
    ),
    axis.text.x = element_text(
      family = "OpenSansEmoji",
      size = 8,
      face = "bold",
      colour = "black"
    ),
    legend.position = "top",
    axis.title.y = element_blank()
  ) +
  facet_wrap(~ Estado)

ggsave("Final2.png", last_plot(), width = 12, height = 7)





# Plot pero por indice ----------------------------------------------------
nacional2 %>%  
  ggplot(aes(x= Estado)) +
  geom_bar(aes(y = value, fill = "value"), stat = "identity", width = 0.8) +
  geom_bar(aes(y= malo, fill= "malo"),stat = "identity", width = 0.8)+
  coord_flip() +
  scale_y_continuous(labels = lbls,
                     breaks = brks) +
  scale_fill_manual(
    values = c(
      "value" = "#D9183B",
      "malo" = "#F2C1B6"
    ),
    labels = c("Buena", "Mala"),
    name = "Condición") +
  labs(
    title = "Porcentaje de hogares en buena y mala condición, por estado e indicador",
    subtitle = "De acuerdo con el indicador DP2",
    caption = "Fuente: Elaboración propia",
    x = "Estado",
    y = "Porcentaje de hogares"
  ) +
  theme(
    panel.background = element_rect(
      fill = "#fffffc",
      colour = "#423E37",
      size = 2,
      linetype = "solid"
    ),
    plot.title = element_text(
      size = 20,
      face = "bold",
      color = "black"
    ),
    plot.subtitle =  element_text(
      size = 10,
      face = "bold",
      color = "black"
    ),
    axis.text.x = element_text(
      family = "OpenSansEmoji",
      size = 8,
      face = "bold",
      colour = "black"
    ),
    axis.text.y = element_text(
      size = 4.3
    ),
    legend.position = c(0.8,0.16),
    axis.title.y = element_blank()
  ) +
  facet_wrap(~ indice)

ggsave("Final3.png", last_plot(), width = 12, height = 7)







# Plot
#ggplot(acces, aes(x = Estado,
                  y = value)) +
  geom_bar(stat = "identity",
           width = 0.8,
           position = "dodge") +
  geom_hline(aes(yintercept = mean(indice), color = "Promedio"), linetype = "dashed") +
  coord_flip(ylim = c(0, .8))  +
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
  scale_fill_manual(
    values = c(
      Ninguno = "#F2C1B6",
      Otro = "#F28888",
      Infonavit = "#D9183B"
    ),
    ) +
  scale_color_manual(name = element_blank(), values = c(Promedio = "black")) +
  theme(
    panel.background = element_rect(
      fill = "#fffffc",
      colour = "#423E37",
      size = 2,
      linetype = "solid"
    ),
    plot.title = element_text(
      size = 20,
      face = "bold",
      color = "black"
    ),
    plot.subtitle =  element_text(
      size = 10,
      face = "bold",
      color = "black"
    ),
    axis.text.x = element_text(
      family = "OpenSansEmoji",
      size = 8,
      face = "bold",
      colour = "black"
    ),
    legend.position = "top",
    axis.title.y = element_blank()
  ) +
  facet_wrap( ~ indice)
  