library(shiny)
library(shiny.semantic)
library(tidyverse)
library(leaflet)
# path of data stored
ships_path <- reactive({
  here::here("data/", "ships.RData")
})
# Load the data
load_ships_data <- reactive({
  load(file = ships_path())
  return(ships_data)
})

# dropdown module
# UI for the module
dropdown_module_ui <- function(id) {
  sidebar_layout(
    sidebar_panel(
      width = 1,
      selectizeInput(NS(id, "Ship_type"),
        width = "300px",
        label = "Select vessel type",
        choices = "Select"
      ),
      selectizeInput(NS(id, "Ship_name"),
        width = "300px",
        label = "Select vessel name",
        choices = "Select"
      )
    ),
    main_panel(
      width = 3,
      leafletOutput(NS(id, "ship_map")),
      verbatimTextOutput(NS(id, "distance_m")),
      dataTableOutput(NS(id, "ship_longest_obs"))
    ),
    min_height = "400px",
    mirrored = FALSE,
    container_style = "background-color: white smoke;",
    area_styles = list(
      sidebar_panel = "border: 1px solid black;",
      main_panel = "border: 1px solid black;"
    )
  )
}

# Server for the module
dropdown_module_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    names_ship_name <- reactive({
      data %>%
        filter(ship_type == input$Ship_type) %>%
        distinct(shipname)
    })
    # Dopdown update
    observe({
      updateSelectizeInput(session, "Ship_type", choices = unique(data$ship_type))
    })
    observeEvent(input$Ship_type, {
      updateSelectizeInput(session, "Ship_name", choices = unique(names_ship_name()$shipname))
    })

    # How to measure distance between two points
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

    # filtering the required data
    filtered_data <- reactive({
      data %>%
        filter(ship_type == input$Ship_type & shipname == input$Ship_name) %>%
        arrange(datetime) %>%
        mutate(dist_m = shortest_distance_measure(lag(lat), lag(lon), lat, lon))
    })

    # Data for map the start and end point
    ships_data_map <- reactive({
      filtered_data() %>%
        slice(c(which.min(1:n()), which.max(1:n()))) %>%
        mutate(position = ifelse(row_number(datetime) %in% c(n()), "End", "Beginning"))
    })

    # Distance note travelled by the ship below the map
    ships_distance_map <- reactive({
      data_map <- filtered_data() %>%
        filter(is_parked == 0) # this step is later so that when the ship starts so from 1 to 0 also distance is measured
      dist_ship_m <- sum(data_map$dist_m, na.rm = TRUE)
    })

    # Longest travelled observation between two consecutive points and give the latest
    ships_distance_obs <- reactive({
      filtered_data() %>%
        filter(is_parked == 0) %>%
        filter(dist_m == max(dist_m, na.rm = TRUE)) %>%
        slice(which.max(datetime))
    })

    # Plot in the map
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

    # Render the note
    output$distance_m <- renderPrint({
      paste(
        "The total distance travelled by a", input$Ship_type, "called", input$Ship_name, "is",
        round(as.numeric(ships_distance_map()), digits = 2), "m"
      )
    })

    # Render table of longest travelled observation
    output$ship_longest_obs <- renderDataTable(
      {
        ships_distance_obs()
      },
      options = list(
        editable = FALSE,
        autoWidth = TRUE,
        dom = "t",
        columnDefs = list(list(width = "200px", targets = "_all")),
        scrollX = TRUE
      )
    )
  })
}

# Define the app
ui <- semanticPage(
  h1("Port and Ships"),
  title = "Ship's App",
  dropdown_module_ui("Ship_type"),
  theme = "flatly"
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2, shiny.launch.browser = TRUE)
  dropdown_module_server("Ship_type", load_ships_data())
}

shinyApp(ui, server)
