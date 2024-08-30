library(shiny)
library(shinyFiles)
library(ggplot2)
library(plotly)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Selecting Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarPanel(
    # Input: Select a file ----
    shinyFilesButton("fcs_files", "Choose .fcs files", "Please select files", multiple = TRUE),
    checkboxInput(inputId = "transform_box", label = "Transform data", value = TRUE),
    numericInput(inputId = "transform_cofactor", label = "Cofactor (default is 5)", value = 5, min = 1, max = 500, step = 1),
    uiOutput("columnSelectUI"),
    uiOutput("gatingControls"),
    actionButton("applyGating", "Apply Gating"),
    actionButton("plotSelected", "Plot Selected Points"),
    downloadButton("downloadGatedData", "Download Gated Data")
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotlyOutput("densityPlot"),
    verbatimTextOutput("gatedData")
  )
)





# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # Set up file system roots
  roots <- c(home = "~")
  
  data <- reactive({
    # Call shinyFiles to handle file selection
    shinyFileChoose(input, "fcs_files", roots = roots, filetypes = c("fcs"))
    # Get the selected file paths
    file_paths <- parseFilePaths(roots, input$fcs_files)
    req(file_paths)
    
    cyto_data <- inject_fcs_gating(file_paths$datapath, filter_features = FALSE, sampling_rate = 1,
                                   asinh_transform = input$transform_box,
                                   cofac = input$transform_cofactor)
    return(cyto_data)
  })
  
  # Dynamically generate column selection UI
  output$columnSelectUI <- renderUI({
    req(data())
    df <- data()
    column_names <- names(df)
    tagList(
      selectInput("xColumn", "Select X Axis:", choices = column_names, selected = column_names[1]),
      selectInput("yColumn", "Select Y Axis:", choices = column_names, selected = column_names[2])
    )
  })
  
  output$densityPlot <- renderPlotly({
    req(data(), input$xColumn, input$yColumn)
    df <- data()
    
    p <- ggplot(df, aes(x = !!sym(input$xColumn), y = !!sym(input$yColumn))) +
      geom_bin2d(bins = 70) +
      scale_fill_continuous(type = "viridis") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(dragmode = ifelse(input$gatingMethod == "Rectangle", "select", "lasso"), selectdirection = "any")
  })
  
  output$gatingControls <- renderUI({
    req(data())
    selectInput("gatingMethod", "Select Gating Method:",
                choices = c("Rectangle", "Polygon"),
                selected = "Rectangle")
  })
  
  selected_df <- reactiveVal(data.frame())
  
  observeEvent(input$applyGating, {
    
    selected_points <- event_data("plotly_selected")

    if (!is.null(selected_points)) {
      # Debugging: Print the selected points data
      print("Selected points:")
      print(selected_points)
      
      # Ensure that `pointNumber` correctly matches your data
      df <- data()
      x_selected <- df[[input$xColumn]][selected_points$pointNumber + 1]
      y_selected <- df[[input$yColumn]][selected_points$pointNumber + 1]
      
      # Filter the original data based on selected points
      gated_data <- df[df[[input$xColumn]] %in% x_selected & df[[input$yColumn]] %in% y_selected, ]
      
      # Debugging: Print the gated data
      print("Gated data:")
      print(head(gated_data))
      
      selected_df(gated_data)
      
      output$gatedData <- renderPrint({
        gated_data
      })
    }
  })
  
  observeEvent(input$plotSelected, {
    req(selected_df())
    
    gated_data <- selected_df()
    
    # Check if the columns exist in the selected data frame
    if (!input$xColumn %in% names(gated_data) || !input$yColumn %in% names(gated_data)) {
      showNotification("Selected columns not found in the gated data.", type = "error")
      return(NULL)
    }
    
    p <- ggplot(gated_data, aes(x = !!sym(input$xColumn), y = !!sym(input$yColumn))) +
      geom_bin2d(bins = 70) +
      scale_fill_continuous(type = "viridis") +
      theme_minimal()
    
    output$densityPlot <- renderPlotly({
      ggplotly(p)
    })
  })

  output$gated_data <- renderPrint({
    selected_df()
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
