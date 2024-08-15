
# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Selecting Files"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      shinyFilesButton("fcs_files", "Choose .fcs files", "Please select files", multiple = TRUE),
      checkboxInput(inputId = "transform_box", label = "Transform data", value = TRUE),
      numericInput(inputId = "transform_cofactor", label = "Cofactor (default is 5)", value = 5, min = 1, max = 500, step = 1),
      uiOutput("gatingControls"),
      actionButton("applyGating", "Apply Gating"),
      downloadButton("downloadGatedData", "Download Gated Data")
    ),
    
  # Main panel for displaying outputs ----
  mainPanel(
      plotlyOutput("densityPlot"),
      verbatimTextOutput("gatedData")
    )
)
)



