library(shiny)
library(shiny)
library(tidyverse)
library(srvyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(shinythemes)
library(shinyWidgets)
library(fullPage)


base <- read_csv("base_bien.csv")

opciones <- c("acc_bien", "aseq_bien","cul_bien" , 
              "habit_bien","satis_bien" ,
              "serv_bien", "ten_bien","ubi_bien")
names(opciones) <- c("Accesibilidad", "Asequibilidad", "Cultura", "Habitabilidad",
                     "Satisfacción", "Servicios", "Tenencia", "Ubicación")

options <- list()

# Define UI for application that draws a histogram
shinyUI(
        pagePiling(
    sections.color = c('#2f2f2f', '#2f2f2f','#2f2f2f','#2f2f2f','#2f2f2f',
                       '#2f2f2f','#2f2f2f','#2f2f2f'),
    opts = options,
    menu = c(
      "Portada" = "portada",
      "Miembros" = "miembros",
      "Introducción" = "intro",
      "Problemática" = "problema",
      "Propuesta" = "propuesta",
      "Resultados" = "resultados",
      "Conclusiones" = "conclusiones",
      "Gracias" = "gracias"
    ),
    pageSectionImage(
      img = "f1.png",
      menu = "portada",
      center = TRUE
    ),
    pageSection(
      center = TRUE,
      menu = "intro"),
    pageSection(
      menu = "problema",
      pageRow(
        height = 2, 
        pageContainer(
          class = "light",
        radioGroupButtons(
          size = "sm",
          "componente",
          label = "Selecciona el componente",
          choices = opciones,
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ))
        )),
    pageRow(
      plotOutput("plot")
    )),
    pageSection(
      menu = "propuesta",
      plotOutput("situacion")
    )
)
)



