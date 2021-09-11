# To build shiny apps
# https://github.com/Appsilon/shiny.semantic
# remotes::install_github("Appsilon/shiny.semantic", ref = "0.4.0")
# To make a fresh, modern and highly interactive app!
library(shiny)
library(shiny.semantic)
library(tidyverse)

# Module dropdown_input function
vessel_dropdown_module <- function(id, choices = "Select vessel type") {
  dropdown_input("Ship_type", default_text = "Select vessel type", choices = choices)
}

vessel_server_module <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        names_vessels <- unique(data$ship_type)
        update_dropdown_input(session, "Ship_type", choices = names_vessels)
      })
    }
  )
}
# UI section
ui <- semanticPage(
  title = "Ships",
  vessel_dropdown_module("Ship_type", choices = "Select vessel type")
  )


# Server section
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30*1024^2, shiny.launch.browser = TRUE)
  
  # Path of the data
  ships_path <- function() {
    ships_path <- here::here("data", "ships.RData")
    return(ships_path)
  }
  
  # Converted the csv into RData file for smaller space and faster reading capability, data cleaned with janitor::clean_names()! dimension = 3102887x20
  load_ships_data <- function() {
    load(file = ships_path())
    return(ships_data)
  }
  
  # Function to find the distance between last and the first point (last and first point determined by datetime)
  # This data in the end is a smaller dataset which is filtered by vessel type and has the distance calculated in meters
  ships_data_map <- function(ships_data, ship_type_option) {
    ships_data_map <- ships_data %>%
      filter(ship_type == ship_type_option) %>%
      arrange(datetime) %>%
      slice(c(which.min(1:n()), which.max(1:n()))) %>%
      mutate(position = ifelse(row_number(datetime) %in% c(n()), "beginning", "end"))
    return(ships_data_map)
  }
  vessel_server_module(id = "Ship_type", data = load_ships_data())
  
}
shinyApp(ui, server)

# Check styling
# styler::style_file("inst/app.R")
