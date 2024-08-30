
ui <- fluidPage(
  # Main panel for displaying outputs ----
  mainPanel(
    plotlyOutput("densityPlot"),
    verbatimTextOutput("gatedData")
  )
)

server <- function(input, output, session) {
    data <- reactive({
        
        datapath <- "/home/lev/DOCTORATE/Cytomata_data/small_dev_project_panel_1/fcs/1_raw/210316_Blut_Panel1_CV19.fcs"

        cyto_data <- inject_fcs_gating(datapath, filter_features = FALSE, sampling_rate = 1,
                                    asinh_transform = TRUE,
                                    cofac = 5)
        return(cyto_data)
    })

    
  output$densityPlot <- renderPlotly({
    req(data())
    df <- data()
    get_density <- function(x, y, ...) {
        dens <- MASS::kde2d(x, y, ...)
        ix <- findInterval(x, dens$x)
        iy <- findInterval(y, dens$y)
        ii <- cbind(ix, iy)
        return(dens$z[ii])
    }
    temp_density <- get_density(cyto_data[["Time"]], cyto_data[["106Pd_B2M"]], n = 100)
    p <- ggplot(cyto_data, aes(x = !!sym("Time"), y = !!sym("106Pd_B2M"), color = temp_density)) +
      ggrastr::geom_point_rast() +
      scale_color_gradientn(colours = hsv(h = seq(.6667, 0, length.out = 11))) +
      theme_minimal()
    plot_ly(cyto_data, type = "scattergl")
    ggplotly(p, dynamicTicks = FALSE, tooltip = NULL) %>%
        layout(dragmode = ifelse(input$gatingMethod == "Rectangle", "select", "lasso"), selectdirection = "any") %>%
        config(displayModeBar = FALSE)
  })
}

print(ggplot(cyto_data, aes(x = !!sym("Time"), y = !!sym("106Pd_B2M"))) +
      ggrastr::geom_point_rast() +
      geom_density_2d_filled()
)



temp_density <- get_density(cyto_data[["Time"]], cyto_data[["106Pd_B2M"]], n = 100)
temp_density <- (temp_density - min(temp_density)) / (max(temp_density) - min(temp_density))

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
# Create the scattergl plot
fig <- plot_ly(x = ~cyto_data[["Time"]], y = ~cyto_data[["106Pd_B2M"]], 
               type = 'scattergl', 
               mode = 'markers',
               marker = list(color = temp_density, 
                             colorscale = custom_colorscale,
                             size = 5,
                             opacity = 1))

# Add layout details
fig <- fig %>% layout(title = "Scattergl Plot Colored by Density",
                      xaxis = list(title = "X Axis"),
                      yaxis = list(title = "Y Axis"),
                      showlegend = FALSE)

# Show the plot
fig


get_density <- function(x, y, n = input$density_res) {
    # Set up the parallel backend
    cl <- makeCluster(detectCores() - 1)  # Use all but one core
    registerDoParallel(cl)

    # Compute the density on the full grid
    dens <- ks::kde(cbind(x, y), gridsize = c(100, 100))

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


get_density(x, y, n = 100)


library(MASS)
library(foreach)
library(doParallel)

# Parallelized get_density function
get_density_parallel <- function(x, y, n = 100) {
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

# Example usage
set.seed(0)
x <- rnorm(10000)
y <- rnorm(10000)

density_values <- get_density_parallel(x, y, n = 1000)
