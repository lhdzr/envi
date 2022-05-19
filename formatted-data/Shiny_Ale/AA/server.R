#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  echarts4r::e_common(
    font_family = "Playfair Display",
    theme = "vintage"
  )
  
  output$title <- typed::renderTyped({
    typed::typed(c("Freedom of Fake news^1000", "Freedom of Press Index^500<br>A Visualisation"), typeSpeed = 25, smartBackspace = TRUE)
  })
  
  callModule(mod_ts_server, "ts")
  callModule(mod_map_server, "map")
  
})
