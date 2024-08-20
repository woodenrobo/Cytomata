server <- function(input, output, session) {
  
  # Set up file system roots
  roots <- c(home = "~")


  # Parallelized get_density function
  get_density <- function(x, y, n = input$density_res) {
    # Set up the parallel backend
    cl <- makeCluster(detectCores() - 1)  # Use all but one core
    registerDoParallel(cl)

    # Compute the density on the full grid
    dens <- kde2d(x, y, n = n)

    # Split the data into chunks for parallel processing
    chunk_indices <- split(1:length(x), cut(1:length(x), breaks = detectCores() - 1, labels = FALSE))

    # Use foreach to parallelize the calculation
    density_values <- foreach(chunk = chunk_indices, .combine = c, .packages = 'MASS') %dopar% {
      ix <- findInterval(x[chunk], dens$x)
      iy <- findInterval(y[chunk], dens$y)
      ii <- cbind(ix, iy)
      dens$z[ii]
    }

    # Stop the parallel backend
    stopCluster(cl)

    return(density_values)
  }
  
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
      div(style = "display: inline-block;",
        selectInput("xColumn", "Select X Axis:", choices = column_names, selected = column_names[1])
      ),
      div(style = "display: inline-block;",
        selectInput("yColumn", "Select Y Axis:", choices = column_names, selected = column_names[2])
      )
    )
  })

  custom_colorscale <- list(
    c(0, "#131396"), 
    c(0.1, "#0054d1"), 
    c(0.2, "#009ac1"), 
    c(0.3, "#02d7ac"), 
    c(0.4, "#02db58"), 
    c(0.5, "#01d001"), 
    c(0.6, "#5ade02"), 
    c(0.7, "#b3e002"), 
    c(0.8, "#eabb01"), 
    c(0.9, "#FF6600"), 
    c(1, "#eb0303")
  )

  output$densityPlot <- renderPlotly({
    req(data(), input$xColumn, input$yColumn)
    df <- data()

  temp_density <- get_density(df[[input$xColumn]], df[[input$yColumn]])
  temp_density <- (temp_density - min(temp_density)) / (max(temp_density) - min(temp_density))

  # Create the scattergl plot
  fig <- plot_ly(x = ~df[[input$xColumn]], y = ~df[[input$yColumn]], 
                type = 'scattergl', 
                mode = 'markers',
                marker = list(color = temp_density, 
                              colorscale = custom_colorscale,
                              size = input$pointsize,
                              opacity = 1),
                width = input$resolution, height = input$resolution)

  # Add layout details
  fig <- fig %>% layout(
                        xaxis = list(title = input$xColumn),
                        yaxis = list(title = input$yColumn),
                        showlegend = FALSE,
                        dragmode = ifelse(input$gatingMethod == "Rectangle", "select", "lasso"),
                        selectdirection = "any") %>%
                        config(displayModeBar = FALSE)

  fig

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

      # Ensure that `pointNumber` correctly matches your data
      df <- data()
      x_selected <- df[[input$xColumn]][selected_points$pointNumber + 1]
      y_selected <- df[[input$yColumn]][selected_points$pointNumber + 1]
      
      # Filter the original data based on selected points
      gated_data <- df[df[[input$xColumn]] %in% x_selected & df[[input$yColumn]] %in% y_selected, ]
      
      selected_df(gated_data)
      
    }
  })
  
  observeEvent(input$plotSelected, {
    req(selected_df())
    
    gated_data <- selected_df()


    temp_density <- get_density(gated_data[[input$xColumn]], gated_data[[input$yColumn]])
    temp_density <- (temp_density - min(temp_density)) / (max(temp_density) - min(temp_density))

    # Create the scattergl plot
    fig <- plot_ly(x = ~gated_data[[input$xColumn]], y = ~gated_data[[input$yColumn]], 
                  type = 'scattergl', 
                  mode = 'markers',
                  marker = list(color = temp_density, 
                                colorscale = custom_colorscale,
                                size = input$pointsize,
                                opacity = 1),
                  width = input$resolution, height = input$resolution)

    # Add layout details
    fig <- fig %>% layout(
                          xaxis = list(title = input$xColumn),
                          yaxis = list(title = input$yColumn),
                          showlegend = FALSE,
                          dragmode = ifelse(input$gatingMethod == "Rectangle", "select", "lasso"),
                          selectdirection = "any") %>%
                          config(displayModeBar = FALSE)

    output$densityPlot <- renderPlotly({
      fig
    })
  })

  output$gated_data <- renderPrint({
    selected_df()
  })
  
}