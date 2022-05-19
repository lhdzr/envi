run_fopi <- function() {
  utils::data("fopi", package = "fopi")
  
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server), 
    golem_opts = list()
  )
}