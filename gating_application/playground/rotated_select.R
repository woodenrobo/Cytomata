library(shiny)

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      $(document).ready(function() {
        function rotateSelect(id, angle) {
          var select = $('#' + id);
          var container = select.parent();
          var label = container.find('label');
          
          // Get the width of the select element
          var selectWidth = select.outerWidth();
          
          container.css({
            'position': 'relative',
            'height': selectWidth + 'px',
            'width': '200px',
            'margin-top': selectWidth + 'px' // Add margin to push content down
          });
          
          label.css({
            'position': 'relative',
            'top': '-20px', // Move label above the rotated select
            'left': '0'
          });
          
          select.css({
            'transform': 'rotate(' + angle + 'deg)',
            'transform-origin': 'left top',
            'position': 'relative',
            'top': '0',
            'left': '0'
          });
        }
        
        // Call the function after a short delay to ensure all elements are rendered
        setTimeout(function() {
          rotateSelect('color', -90);
        }, 100);
      });
    "))
  ),

  fluidRow(
    column(width = 4, offset = 2,
      div(style = "min-height: 200px;",  # Provide space for the rotated select
        selectInput("color", NULL, 
                  choices = c("Red", "Blue", "Green"),
                  selectize = FALSE)
      )
    ),
    column(width = 4,
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  output$result <- renderText({
    paste("Selected color:", input$color)
  })
}

shinyApp(ui = ui, server = server)