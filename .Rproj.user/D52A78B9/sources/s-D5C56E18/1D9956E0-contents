library(shiny)
library(shiny.semantic)
library(tidyverse)
library(leaflet)
source("dropdown_module.R")

ships_path <- reactive({
  here::here("data", "ships.RData")
})

load_ships_data <- reactive({
  load(file = ships_path())
  return(ships_data)
})


ui <- semanticPage(
  title = "Ship's App",
  dropdown_module_ui("Ship_type"),
  theme = "flatly"
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2, shiny.launch.browser = TRUE)
  dropdown_module_server("Ship_type", load_ships_data())
}

shinyApp(ui, server)
