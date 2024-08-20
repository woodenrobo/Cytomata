
ui <- fluidPage(
  
  # App title ----
  titlePanel("Selecting Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarPanel(width = 2,
    # Input: Select a file ----
    shinyFilesButton("fcs_files", "Choose .fcs files", "Please select files", multiple = TRUE),
    checkboxInput(inputId = "transform_box", label = "Transform data", value = TRUE),
    numericInput(inputId = "transform_cofactor", label = "Cofactor (default is 5)", value = 5, min = 1, max = 500, step = 1),
    numericInput(inputId = "density_res", label = "Density Resolution (10-100)", value = 50, min = 10, max = 100, step = 1),
    numericInput(inputId = "pointsize", label = "Point Size", value = 1, min = 0.1, max = 100, step = 0.1),
    numericInput(inputId = "resolution", label = "Plot Resolution", value = 600, min = 200, max = 3000, step = 1),
    actionButton("applyGating", "Apply Gating"),
    actionButton("plotSelected", "Plot Selected Points")
  ),
  
  # Main panel for displaying outputs ----
  mainPanel( width = 7,
    fluidRow(
      uiOutput("columnSelectUI"),
      uiOutput("gatingControls")),
    plotlyOutput("densityPlot", width = "auto", height = "auto")
  )
)

