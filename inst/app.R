library(shiny)
library(shiny.semantic)
library(tidyverse)
library(leaflet)

ships_path <- reactive({
  here::here("data/", "ships.RData")
})

load_ships_data <- reactive({
  load(file = ships_path())
  return(ships_data)
})

# dropdown module

dropdown_module_ui <- function(id) {
  fluidRow(
    column(1, selectizeInput(NS(id, "Ship_type"),
                             label = "Select vessel type",
                             choices = "Select"
    )),
    column(1, selectizeInput(NS(id, "Ship_name"),
                             label = "Select vessel name",
                             choices = "Select"
    )),
    column(1, leafletOutput(NS(id, "ship_map"))),
    column(1, verbatimTextOutput(NS(id, "distance_m"))),
    column(1, dataTableOutput(NS(id, "ship_longest_obs")))
  )
}


dropdown_module_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    names_ship_name <- reactive({
      data %>%
        filter(ship_type == input$Ship_type) %>%
        distinct(shipname)
    })
    observe({
      updateSelectizeInput(session, "Ship_type", choices = unique(data$ship_type))
    })
    observeEvent(input$Ship_type, {
      updateSelectizeInput(session, "Ship_name", choices = unique(names_ship_name()$shipname))
    })
    
    shortest_distance_measure <- function(start_lat, start_long, end_lat, end_long) {
      RadE <- 6378.137 # radius of earth in km
      diff_lat_rad <- end_lat * pi / 180 - start_lat * pi / 180 
      diff_lon_rad <- end_long * pi / 180 - start_long * pi / 180 
      a <- sin(diff_lat_rad / 2) * sin(diff_lat_rad / 2) +
        cos(start_lat * pi / 180) * cos(end_lat * pi / 180) *
        sin(diff_lon_rad / 2) * sin(diff_lon_rad / 2)
      cir <- 2 * atan2(sqrt(a), sqrt(1 - a))
      dist_m <- (RadE * cir) * 1000
    }
    
    filtered_data <- reactive({
      data %>%
        filter(ship_type == input$Ship_type & shipname == input$Ship_name) %>%
        arrange(datetime) %>%
        mutate(dist_m = shortest_distance_measure(lag(lat), lag(lon), lat, lon))
    })
    
    ships_data_map <- reactive({
      filtered_data() %>%
        slice(c(which.min(1:n()), which.max(1:n()))) %>%
        mutate(position = ifelse(row_number(datetime) %in% c(n()), "End", "Start"))
    })
    
    ships_distance_map <- reactive({
      data_map <- filtered_data() %>%
        filter(is_parked == 0) # this step is later so that when the ship starts so from 1 to 0 also distance is measured
      dist_ship_m <- sum(data_map$dist_m, na.rm = TRUE)
    })
    
    ships_distance_obs <- reactive({
      filtered_data() %>%
        filter(is_parked == 0) %>%
        filter(dist_m == max(dist_m, na.rm = TRUE)) %>%
        slice(which.max(datetime))
    })
    
    output$ship_map <- renderLeaflet({
      pal <- colorFactor(
        palette = c("green", "red"),
        domain = ships_data_map()$position
      )
      leaflet(ships_data_map()) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircles(
          data = ships_data_map(), lng = ~lon, lat = ~lat, radius = 100,
          color = ~ pal(position)
        ) %>%
        addLegend(position = "bottomright", pal = pal, values = ~position)
    })
    
    output$distance_m <- renderPrint({
      paste(
        "The total distance travelled by a", input$Ship_type, "called", input$Ship_name, "is",
        round(as.numeric(ships_distance_map()), digits = 2), "m"
      )
    })
    
    output$ship_longest_obs <- renderDataTable({
      ships_distance_obs()
    })
  })
}

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
