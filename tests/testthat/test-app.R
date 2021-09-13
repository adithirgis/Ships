library(testthat)
load("ships.RData")

ships_filter <- ships_data %>%
  filter(ship_type == "Cargo" | ship_type == "Passenger") %>%
  filter(shipname == "KAROLI" | shipname == "ALVSNABBEN 5")

# Testing the note
test_that("returns the output of distance from map's note", {
  testServer(dropdown_module_server, args = list(data = ships_filter), {
    session$setInputs(Ship_type = "Passenger", Ship_name = "ALVSNABBEN 5")
    expect_equal(output$distance_m, "[1] \"The total distance travelled by a Passenger called ALVSNABBEN 5 is 863858.17 m\"")
  })
})

# Testing the longest travelled observation
test_that("returns the output of the rows in the longest distance travelled by a ship", {
  testServer(dropdown_module_server, args = list(data = ships_filter), {
    session$setInputs(Ship_type = "Cargo", Ship_name = "KAROLI")
    expect_equal(nrow(ships_distance_obs()), 1)
  })
})

# Check to see if there is map
test_that("change in the map data", {
  testServer(dropdown_module_server, args = list(data = ships_filter), {
    session$setInputs(Ship_type = "Cargo", Ship_name = "KAROLI")
    session$elapse(300)
    expect_equal(ncol(ships_data_map()), 22)
    output$ship_map
  })
})
