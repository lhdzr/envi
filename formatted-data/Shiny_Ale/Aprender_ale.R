library(shiny)
library(datasets)
library(ggplot2)


ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)



server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$table <- renderTable({
    dataset()
  })
}
shinyApp(ui, server)




ui <- fluidPage(
  textInput("name", "What's your name?"),
  numericInput("age", "How old are you?", value = NA),
  textOutput("greeting"),
  tableOutput("mortgage")
  
)

server <- function(input, output, session){
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
  output$histogram <- renderPlot({
    hist(rnorm(1000))
  }, res = 96)
}

shinyApp(ui, server)




library(shiny)
ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", label = "and y is", min = 1, max = 100, value= 50),
  "then x times y is",
  textOutput("product")
)
server <- function(input, output, session) {
  output$product <- renderText({
    input$x * input$y
  })
}
shinyApp(ui, server)

ui <- fluidPage(
  sliderInput("x", "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", "and y is", min = 1, max = 50, value = 5),
  "then, (x * y) is", textOutput("product"),
  "and, (x * y) + 5 is", textOutput("product_plus5"),
  "and (x * y) + 10 is", textOutput("product_plus10")
)
server <- function(input, output, session) {
  product <- reactive(input$x * input$y)
  output$product <- renderText({
    product()
  })
  output$product_plus5 <- renderText({
    product() + 5
  })
  output$product_plus10 <- renderText({
    product() + 10
  })
}
shinyApp(ui, server)




datasets <- c("economics", "faithfuld", "seals")

ui <- fluidPage(
  selectInput("dataset", "Dataset", choices = datasets),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")})
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$plot <- renderPlot({
    plot(dataset())
  }, res = 96)
}
shinyApp(ui, server)




# Capitulo 2.  ------------------------------------------------------------

ui <- fluidPage(
  textInput("name", "What's your name?"),
  passwordInput("password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3)
)



ui <- fluidPage(
  numericInput("num", "Number one", value = 0, min = 0, max = 100),
  sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
  sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100)
)


ui <- fluidPage(
  dateInput("dob", "When were you born?"),
  dateRangeInput("holiday", "When do you want to go on vacation next?")
)


animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")
ui <- fluidPage(
  selectInput("state", "What's your favourite state?", state.name),
  radioButtons("animal", "What's your favourite animal?", animals)
)




ui <- fluidPage(
  checkboxGroupInput("animal", "What animals do you like?", animals)
)


### Ejercicios
#1
ui <- fluidPage(textInput("nombre", NA, placeholder = "yes"))
shinyApp(ui, server)

#2
ui <- fluidPage(
  sliderInput("hola", "Fecha",min = as.Date("2015-07-01"), max = as.Date("2017-07-01"), 
              value = as.Date("2015-07-01"),
              timeFormat = "%F")
)
shinyApp(ui, server)

#3
ui <- fluidPage(
  sliderInput("hola", "HOLA", min = 0, max = 100, value = 50,
              step = 5, animate = T)
)
shinyApp(ui, server)



## OUTPUTS
ui <- fluidPage(
  textOutput("text"),
  verbatimTextOutput("code")
)
server <- function(input, output, session) {
  output$text <- renderText({
    "Hello friend!"
  })
  output$code <- renderPrint({
    summary(1:10)
  })
}
shinyApp(ui, server)


ui <- fluidPage(
  tableOutput("static"),
  dataTableOutput("dynamic")
)
server <- function(input, output, session) {
  output$static <- renderTable(head(mtcars))
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
}
shinyApp(ui, server)


## Ejercicios
#2
ui <- fluidPage(
  plotOutput("plot", width = "400px")
)
server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5), res = 96, width = 700, height = 300)
}
shinyApp(ui, server)





# Capitulo 3 --------------------------------------------------------------

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)
server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello ", input$name, "!")
  })
}
shinyApp(ui,server)



# Ejercicios
###1
ui <- fluidPage(
  textInput("name", "What is your name?"),
  textOutput("greeting")
)

server1 <- function(input, output, session){
  output$greeting <- renderText({paste0("Hello", input$name)})
}

shinyApp(ui,server1)

#nota
library(ggplot2)
freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3,3)) {
  df <- data.frame(
    x = c(x1,x2),
    g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )
  ggplot(df,aes(x, colour = g)) +
    geom_freqpoly(binwidth =  binwidth, size = 1) +
    coord_cartesian(xlim=xlim)
}

t_test <- function(x1,x2) {
  test <- t.test(x1,x2)
  
  sprintf(
    "p value; %0.3f\n[%0.2f,%0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}


x1 <- rnorm(100, 0, 0.5)
x2 <- rnorm(200, 0.15, 0.9)

freqpoly(x1,x2)
cat(t_test(x1,x2))


ui <- fluidPage(
  fluidRow(
    column(4,
           "Distribution 1",
           numericInput("n1", label = "n", value = 1000, min = 1),
           numericInput("mean1", label = "μ", value = 0, step = 0.1),
           numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4,
           "Distribution 2",
           numericInput("n2", label = "n", value = 1000, min = 1),
           numericInput("mean2", label = "μ", value = 0, step = 0.1),
           numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4,
           "Frequency polygon",
           numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
           sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
    )
  ),
  fluidRow(
    column(9, plotOutput("hist")),
    column(3, verbatimTextOutput("ttest"))
  )
)


server <- function(input, output, session) {
  x1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
  x2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
  output$hist <- renderPlot({
    freqpoly(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  output$ttest <- renderText({
    t_test(x1(), x2())
  })
}

shinyApp(ui, server)



#Controlling timing
ui <- fluidPage(
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 5),
           numericInput("n", label = "n", value = 1e4, min = 0)
    ),
    column(9, plotOutput("hist"))
  )
)
server <- function(input, output, session) {
  x1 <- reactive(rpois(input$n, input$lambda1))
  x2 <- reactive(rpois(input$n, input$lambda2))
  output$hist <- renderPlot({
    freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}

shinyApp(ui,server)


server <- function(input, output, session) {
  timer <- reactiveTimer(500)
  x1 <- reactive({timer()
    rpois(input$n, input$lambda1)
  })
  x2 <- reactive({
    timer()
    rpois(input$n, input$lambda2)
  })
  output$hist <- renderPlot({
    freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}
shinyApp(ui, server)


#Action button
ui <- fluidPage(
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 5),
           numericInput("n", label = "n", value = 1e4, min = 0),
           actionButton("simulate", "Simulate!")
    ),
    column(9, plotOutput("hist"))
  )
)

server <- function(input, output, session) {
  x1 <- eventReactive(input$simulate,{
    rpois(input$n, input$lambda1)
  })
  x2 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda2)
  })
  output$hist <- renderPlot({
    freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}


ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)
server <- function(input, output, session) {
  string <- reactive(paste0("Hello ", input$name, "!"))
  output$greeting <- renderText(string())
  observeEvent(input$name, {
    message("Greeting performed")
  })
}
shinyApp(ui, server)



# Ch. 4 -------------------------------------------------------------------

library(shiny)
library(vroom)
library(tidyverse)

dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}

download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")

selected <- injuries %>% filter(prod_code == 649)
nrow(selected)
selected %>% count(location, wt = weight, sort = TRUE)
selected %>% count(body_part, wt = weight, sort = TRUE)
selected %>% count(diag, wt = weight, sort = TRUE)


summary <- selected %>%
  count(age, sex, wt = weight) %>%
  left_join(population, by = c("age", "sex")) %>%
  mutate(rate = n / population * 1e4)

summary %>%  
  ggplot(aes(age, rate, colour = sex)) +
  geom_line() +
  labs(y = "Estimated number of injuries")






























