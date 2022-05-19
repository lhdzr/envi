library(shiny)
library(tidyverse)
library(srvyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(fullPage)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session){
  base <- read_csv("base_bien.csv")
  
  opciones <- c("acc_bien", "aseq_bien","cul_bien" , 
                "habit_bien","satis_bien" ,
                "serv_bien", "ten_bien","ubi_bien")
  names(opciones) <- c("Accesibilidad", "Asequibilidad", "Cultura", "Habitabilidad",
                       "Satisfacción", "Servicios", "Tenencia", "Ubicación")
  base_sit <- read_csv("situaciones.csv")
  
  datos_sit <- reactive(
    base_sit %>% 
      filter(indice %in% input$componente)
      
  )
  
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  datos <- reactive(
    base %>% 
      dplyr::select(Estado, credito, input$componente) %>% 
      dplyr::rename(bien = input$componente)
  )
  nombre <- reactive(input$componente)
  
  output$plot <- renderPlot(ggplot(datos(), aes(x = Estado,
                                                y = bien,
                                                fill = credito)) +
                              geom_bar(stat = "identity",
                                       width = 0.8,
                                       position = "dodge") +
                              geom_hline(aes(yintercept = mean(bien), color = "Promedio"), linetype = "dashed") +
                              coord_flip(ylim = c(0, 1))  +
                              scale_y_continuous(labels = lbls,
                                                 breaks = brks)  +
                              labs(
                                title = paste0("Porcentaje de hogares con buena ", nombre()),
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
                                plot.title = element_text(size = 20,
                                                          face = "bold",
                                                          color = "black"),
                                plot.subtitle =  element_text(size = 10,
                                                              face = "bold",
                                                              color = "black"),
                                axis.text.x = element_text(
                                  family = "sans",
                                  size = 8,
                                  face = "bold",
                                  colour = "black"
                                ),
                                legend.position = "top",
                                axis.title.y = element_blank()
                              ) +
                              facet_wrap(~ credito))
  
  #Por situacion
  output$situacion <- renderPlot(ggplot(datos_sit(), aes(x = indice,
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
    scale_y_continuous(labels = scales::percent))
  
  
} )

