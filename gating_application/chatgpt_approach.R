
ui <- fluidPage(
  titlePanel("2D Density Plot with Gating"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      uiOutput("gatingControls"),
      actionButton("applyGating", "Apply Gating"),
      downloadButton("downloadGatedData", "Download Gated Data")
    ),
    mainPanel(
      plotlyOutput("densityPlot"),
      verbatimTextOutput("gatedData")
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    return(df)
  })
  
  output$densityPlot <- renderPlotly({
    req(data())
    df <- data()
    
    p <- ggplot(df, aes(x = df[[1]], y = df[[2]])) +
      geom_density2d() +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(dragmode = "lasso", selectdirection = "any")
  })
  
  output$gatingControls <- renderUI({
    req(data())
    selectInput("gatingMethod", "Select Gating Method:",
                choices = c("Rectangle", "Polygon"),
                selected = "Rectangle")
  })
  
  selected_df <- reactiveVal(data.frame())
  
  observeEvent(input$applyGating, {
    req(input$densityPlot)
    
    selected_points <- event_data("plotly_selected")
    
    if (!is.null(selected_points)) {
      gated_data <- data()[selected_points$pointNumber + 1, ]
      selected_df(gated_data)
      
      output$gatedData <- renderPrint({
        gated_data
      })
    }
  })
  
  output$downloadGatedData <- downloadHandler(
    filename = function() {
      paste("gated_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(selected_df(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)
