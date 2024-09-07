# app.R
library(shiny)
library(ggplot2)
library(flowWorkspace)
library(jsonlite)

# UI
ui <- fluidPage(
  titlePanel("Cytometry Gating Application"),
  sidebarLayout(
    sidebarPanel(
      actionButton("reset", "Reset")
    ),
    mainPanel(
      plotOutput("scatterplot", click = "plot_click"),
      tags$div(id = "d3_plot"),
      tags$script(src = "https://d3js.org/d3.v7.min.js"),
      tags$script(HTML("
        (function() {
            const margin = {top: 20, right: 20, bottom: 30, left: 40};
            const width = 400 - margin.left - margin.right;
            const height = 400 - margin.top - margin.bottom;

            const svg = d3.select('#d3_plot')
                .append('svg')
                .attr('width', width + margin.left + margin.right)
                .attr('height', height + margin.top + margin.bottom)
                .append('g')
                .attr('transform', `translate(${margin.left},${margin.top})`);

            let xScale, yScale;

            Shiny.addCustomMessageHandler('update_axes', function(message) {
                xScale = d3.scaleLinear()
                .domain(message.x_range)
                .range([0, width]);

                yScale = d3.scaleLinear()
                .domain(message.y_range)
                .range([height, 0]);

                // Create axes
                const xAxis = d3.axisBottom(xScale);
                const yAxis = d3.axisLeft(yScale);

                svg.append('g')
                .attr('class', 'x-axis')
                .attr('transform', `translate(0,${height})`)
                .call(xAxis);

                svg.append('g')
                .attr('class', 'y-axis')
                .call(yAxis);
            });

            Shiny.addCustomMessageHandler('add_point', function(message) {
                svg.append('circle')
                .attr('cx', xScale(message.x))
                .attr('cy', yScale(message.y))
                .attr('r', 5)
                .attr('fill', 'red');
            });

            svg.on('click', function(event) {
                const coords = d3.pointer(event);
                const x = xScale.invert(coords[0]);
                const y = yScale.invert(coords[1]);

                Shiny.setInputValue('d3_click', JSON.stringify({x: x, y: y}));
            });
            })();
    "))
    )
  )
)

# Server
server <- function(input, output, session) {
  # Sample data (replace with your actual data)
  data <- reactive({
    data.frame(
      x = rnorm(1000),
      y = rnorm(1000)
    )
  })
  
  output$scatterplot <- renderPlot({
    ggplot(data(), aes(x, y)) +
      geom_point() +
      theme_void() +
      theme(plot.margin = unit(c(0,0,0,0), "cm"))
  })
  
  observe({
    plot_build <- ggplot_build(ggplot(data(), aes(x, y)) + geom_point())
    x_range <- plot_build$layout$panel_params[[1]]$x.range
    y_range <- plot_build$layout$panel_params[[1]]$y.range
    
    session$sendCustomMessage(
      type = "update_axes",
      message = list(
        x_range = x_range,
        y_range = y_range
      )
    )
  })
  
  observeEvent(input$plot_click, {
    click_data <- input$plot_click
    session$sendCustomMessage(
      type = "add_point",
      message = list(
        x = click_data$x,
        y = click_data$y
      )
    )
  })
  
  observeEvent(input$d3_click, {
    click_coords <- fromJSON(input$d3_click)
    # Process the coordinates in R
    print(paste("Clicked coordinates:", click_coords$x, click_coords$y))
  })
}

shinyApp(ui, server)