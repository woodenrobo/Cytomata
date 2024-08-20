library(shiny)
library(ggplot2)
library(plotly)
library(sp)  # For point-in-polygon operation

# Sample data
set.seed(42)
data <- data.frame(
  x = rnorm(1000),
  y = rnorm(1000)
)

# UI
ui <- fluidPage(
  titlePanel("Select Points Inside a Polygon"),
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("selected_points"),
      actionButton("clear", "Clear Selection")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store the polygon vertices
  rv <- reactiveValues(polygon = data.frame(x = numeric(), y = numeric()))
  
  # Render the initial blank plot
  output$plot <- renderPlotly({
    ggplot() +
      geom_point(aes(x = data$x, y = data$y)) +
      coord_cartesian(xlim = range(data$x), ylim = range(data$y)) +
      theme_minimal()
  })
  
  # Update the plot with the polygon as the user clicks
  observeEvent(event_data("plotly_click", source = "A"), {
    click_data <- event_data("plotly_click", source = "A")
    if (!is.null(click_data)) {
      rv$polygon <- rbind(rv$polygon, data.frame(x = click_data$x, y = click_data$y))
      
      # Update the plot with the new polygon
      output$plot <- renderPlotly({
        ggplot(data, aes(x = x, y = y)) +
          geom_blank() +
          geom_polygon(data = rv$polygon, aes(x = x, y = y), fill = NA, color = "blue") +
          geom_point(data = data, aes(x = x, y = y)) +
          coord_cartesian(xlim = range(data$x), ylim = range(data$y)) +
          theme_minimal()
      })
    }
  })
  
  # Clear the polygon and reset
  observeEvent(input$clear, {
    rv$polygon <- data.frame(x = numeric(), y = numeric())
    output$plot <- renderPlotly({
      ggplot() +
        geom_blank(aes(x = data$x, y = data$y)) +
        coord_cartesian(xlim = range(data$x), ylim = range(data$y)) +
        theme_minimal()
    })
    output$selected_points <- renderPrint({
      "Polygon cleared."
    })
  })
  
  # Identify points inside the polygon and display them
  observe({
    if (nrow(rv$polygon) >= 3) {
      # Close the polygon by repeating the first point at the end
      polygon_coords <- rbind(rv$polygon, rv$polygon[1,])
      
      # Create a spatial polygon
      sp_polygon <- SpatialPolygons(list(Polygons(list(Polygon(polygon_coords)), ID = "1")))
      
      # Check which points are inside the polygon
      sp_points <- SpatialPoints(data)
      inside <- sp::over(sp_points, sp_polygon)
      
      selected_data <- data[!is.na(inside), ]
      
      output$selected_points <- renderPrint({
        if (nrow(selected_data) > 0) {
          selected_data
        } else {
          "No points selected inside the polygon."
        }
      })
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
