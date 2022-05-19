#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
shinyUI(function() {
  
  options <- list()
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    pagePiling(
      sections.color = c('#2f2f2f', '#2f2f2f', '#f9f7f1', '#2f2f2f'),
      opts = options,
      menu = c(
        "Home" = "home",
        "Map" = "map",
        "Series" = "ts",
        "About" = "about"
      ),
      pageSectionImage(
        center = TRUE,
        img = "www/img/reading.jpg",
        menu = "home",
        h1(typed::typedOutput("title"), class = "header shadow-dark"),
        h3(
          class = "light footer",
          "by", tags$a("news-r", href = "https://news-r.org", class = "link"), "with", emo::ji("coffee")
        )
      ),
      pageSection(
        center = TRUE,
        menu = "map",
        mod_map_ui("map")
      ),
      pageSection(
        center = TRUE,
        menu = "ts",
        mod_ts_ui("ts")
      ),
      pageSection(
        center = TRUE,
        menu = "about",
        h1("About", class = "header shadow-dark"),
        h2(
          class = "shadow-light",
          tags$a("The code", href = "https://github.com/news-r/fopi.app", target = "_blank", class = "link"),
          "|",
          tags$a("The API", href = "https://github.com/news-r/fopi", target = "_blank", class = "link")
        ),
        h3(
          class = "light footer",
          "by", tags$a("news-r", href = "https://news-r.org", class = "link"), "with", emo::ji("coffee")
        )
      )
    )
  )
})