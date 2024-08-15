# Define server logic to read selected file ----
server <- function(input, output) {
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
