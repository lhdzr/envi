library(tidyverse)
library(srvyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(shiny)
library(rsconnect)




# Creacion App ------------------------------------------------------------
base <- read_csv("base_bien.csv")

opciones <- c("acc_bien", "aseq_bien","cul_bien" , 
              "habit_biem","satis_bien" ,
              "serv_bien", "ten_bien","ubi_bien")
names(opciones) <- c("Accesibilidad", "Asequibilidad", "Cultura", "Habitabilidad",
                     "Satisfacción", "Servicios", "Tenencia", "Ubicación")

ui <- fluidPage(
  fluidRow(column(6,
                  selectInput("componente", "Selecciona el componente",
                              choices = opciones )),
           column(6,textOutput("texto"))),
  fluidRow(
    plotOutput("plot")
    
  )
)

server <- function(input, output, session){
  brks <- seq(-1, 1, .20)
  lbls = as.character(if_else(brks > 0, brks * (100), -brks * 100))
  
  datos <- reactive(
    base %>% 
      select(Estado, credito, input$componente) %>% 
      rename(bien = input$componente)
  )
  nombre <- reactive(input$componente)
  
  output$plot <- renderPlot(ggplot(datos(), aes(x = Estado,
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
  
  
}
shinyApp(ui, server)





















