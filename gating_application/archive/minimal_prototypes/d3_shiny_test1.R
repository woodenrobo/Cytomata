# app.R

library(shiny)
library(ggplot2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #plot-container {
        position: relative;
        width: 600px;
        height: 600px;
      }
      #d3_output {
        position: absolute;
        top: 0;
        left: 0;
        width: 600px;
        height: 600px;
        pointer-events: none; /* Allow clicks to pass through to the plot */
      }
    ")),
    tags$script(src = "https://d3js.org/d3.v7.min.js"),
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('plot_click', function(coords) {
        console.log('Received coordinates from server:', coords);
        
        let svg = d3.select('#d3_output').select('svg');
        if (svg.empty()) {
          console.log('Creating new SVG element');
          svg = d3.select('#d3_output')
            .append('svg')
            .attr('width', 600)
            .attr('height', 600);
          
          // Create custom axis labels
          svg.append('text')
            .attr('x', 300)
            .attr('y', 620)  // Move below the plot
            .attr('text-anchor', 'middle')
            .text('X Axis Label');
          
          svg.append('text')
            .attr('x', -300)
            .attr('y', -20)  // Move to the left of the plot
            .attr('text-anchor', 'middle')
            .attr('transform', 'rotate(-90)')
            .text('Y Axis Label');
          
          // Create custom ticks
          const xScale = d3.scaleLinear().domain([-1, 6]).range([0, 600]);
          const yScale = d3.scaleLinear().domain([-1, 6]).range([600, 0]);
          
          const xAxis = d3.axisBottom(xScale).ticks(7);
          const yAxis = d3.axisLeft(yScale).ticks(7);
          
          svg.append('g')
            .attr('transform', 'translate(0, 600)')
            .call(xAxis);
          
          svg.append('g')
            .attr('transform', 'translate(0, 0)')
            .call(yAxis);
        }
        
        const plotWidth = 600;  // Adjust to match your plot width
        const plotHeight = 600; // Adjust to match your plot height
        const xOffset = 0;     // Adjust to match your plot's x offset
        const yOffset = 0;     // Adjust to match your plot's y offset
        
        const cx = (coords[0] + 1) / 7 * plotWidth + xOffset;
        const cy = plotHeight - ((coords[1] + 1) / 7 * plotHeight + yOffset);    
        
        console.log('Calculated SVG coordinates:', 'cx =', cx, 'cy =', cy);
        
        const point = svg.append('circle')
          .attr('cx', cx)  // Adjust for plot area and scale
          .attr('cy', cy)  // Adjust for plot area and scale
          .attr('r', 5)
          .attr('fill', 'red');
        
        let line = svg.select('polyline');
        if (line.empty()) {
          console.log('Creating new polyline element');
          line = svg.append('polyline')
            .attr('fill', 'none')
            .attr('stroke', 'blue')
            .attr('stroke-width', 2)
            .attr('points', '');
        }
        
        let points = line.attr('points');
        points += cx + ',' + cy + ' ';
        console.log('Updated polyline points:', points);
        line.attr('points', points);
      });
      "
    ))
  ),
  div(id = "plot-container",
      plotOutput("plot", click = "plot_click", height = "600px", width = "600px"),
      div(id = "d3_output")
  )
)

df <- data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 2, 3, 4, 5))

server <- function(input, output, session) {
  
  # Render a simple ggplot without axis labels, margins, or ticks
  output$plot <- renderPlot({
    print("Rendering plot")
    ggplot(df, aes(x, y)) +
      geom_point() +
      theme_void() +  # Remove axis labels and margins
      theme(
        axis.ticks = element_blank(),  # Remove ticks
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      ) +
      scale_x_continuous(limits = c(-1, 6), expand = c(0, 0)) + 
      scale_y_continuous(limits = c(-1, 6), expand = c(0, 0))
  })
  
  # Observe click events and send coordinates to the client side
  observeEvent(input$plot_click, {
    coords <- c(input$plot_click$x, input$plot_click$y)
    print(paste("Clicked at: x =", coords[1], "y =", coords[2]))
    session$sendCustomMessage("plot_click", coords)
  })
  
}

shinyApp(ui, server)