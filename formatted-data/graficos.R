# GENERALES -----------------------------------------------------------
{acces <- datos_feif %>% 
  select(vid, estr_acc_bueno, Estado,credito) %>% 
  group_by(Estado, estr_acc_bueno, credito) %>% 
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
  fill = credito
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
    legend.position = "top",
    panel.background = element_rect(fill = "#fffffc", colour = "#423E37",
                                    size = 2, linetype = "solid")
  ) + 
  geom_hline(yintercept = 0, size = .5) +
  scale_fill_manual(values = c("#D9183B", "#F28888", "#F2C1B6"))

} #Accesibilidad
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
