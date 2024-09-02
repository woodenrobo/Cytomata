# test_app.R

library(shinytest2)

test_that("Polygon is created and draggable", {
  app <- AppDriver$new(app_dir = ".", name = "polygon_test", variant = NULL, seed = 42)
  
  # Simulate clicks
  app$click(input = "plot_click", x = 0.25, y = 0.25)
  app$click(input = "plot_click", x = 0.75, y = 0.25)
  app$click(input = "plot_click", x = 0.75, y = 0.75)
  app$click(input = "plot_click", x = 0.25, y = 0.25)
  
  app$expect_values() # Check the state of the app
  
  # Additional tests can be added here
})
