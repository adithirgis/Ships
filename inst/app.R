# To build shiny apps
library(shiny)
# https://github.com/Appsilon/shiny.semantic
# remotes::install_github("Appsilon/shiny.semantic", ref = "0.4.0")
#  To make a fresh, modern and highly interactive app!
library(shiny.semantic)
# For managing the data
library(tidyverse)

# UI section
ui <- semanticPage(
  selectInput("Ship_type", "Select Vessel Type", "Select")
)


# Reading data and cleaning it
ships_data <- function() {
  # Using tibble here to load faster and it has 398 MB! 3102887x20
  ships_data <- read_csv(here::here("data", "ships.csv")) %>%
    janitor::clean_names()
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


# Server section
server <- function(input, output, session) {
  updateSelectInput(session, "Ship_type", choices = unique(ships_data()$ship_type))
  
}
shinyApp(ui, server)

# Check styling 
# styler::style_file("inst/app.R")
# For session info
# sessionInfo()
