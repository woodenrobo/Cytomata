ctm <- new.env()
# DATA LOADING MODULE ################

#setting paths to data
datapath <- raw_folder
file_names <- dir(datapath, full.names = FALSE, pattern = ".fcs$")
file_paths <- dir(datapath, full.names = TRUE, pattern = ".fcs$")


## injecting data
cs <- load_cytoset_from_fcs(file_paths)


# CHANNEL ANNOTATION CONSISTENCY CHECKS ################
# Extract and print the column names for each cytoframe in the cytoset within the GatingSet
channel_names_list <- lapply(sampleNames(cs), function(sample_name) {
  # Access the cytoframe
  cf <- cs[[sample_name]]
  
  # Get the column names
  channel_names <- colnames(cf)

  # Return the column names
  return(channel_names)
})
# name the list by the sample names for easy reference
names(channel_names_list) <- sampleNames(cs)

channel_names_df <- tibble::enframe(channel_names_list, name = "fcs", value = "values") %>%
  tidyr::unnest(values) %>%
  tidyr::pivot_wider(names_from = values, values_from = values)

# print names of the columns which contain more than one unique value
inconsistent_names <- channel_names_df %>%
                        dplyr::select(-fcs) %>%
                        dplyr::summarise_all(function(x) length(unique(x))) 
inconsistent_names_index <- which(inconsistent_names > 1)
inconsistent_names_cols <- colnames(inconsistent_names)[inconsistent_names_index]

# Extract and print the column names for each cytoframe in the cytoset within the GatingSet
channel_desc_list <- lapply(sampleNames(cs), function(sample_name) {
  # Access the cytoframe
  cf <- cs[[sample_name]]
  channel_desc <- pData(parameters(cf))$desc

  # Return the channel descriptions
  return(channel_desc)
})
# name the list by the sample names for easy reference
names(channel_desc_list) <- sampleNames(cs)

channel_desc_df <- tibble::enframe(channel_desc_list, name = "fcs", value = "values") %>%
  tidyr::unnest(values) %>%
  tidyr::pivot_wider(names_from = values, values_from = values)

# print names of the columns which contain more than one unique value
inconsistent_desc <- channel_desc_df %>%
                        dplyr::select(-fcs) %>%
                        dplyr::summarise_all(function(x) length(unique(x))) 
inconsistent_desc_index <- which(inconsistent_desc > 1)
inconsistent_desc_cols <- colnames(inconsistent_desc)[inconsistent_desc_index]


#stop execution if there are inconsistent column names or descriptions
#will replace this with editable shiny table in the future 
if (length(inconsistent_names_index) > 0 && length(inconsistent_desc_index) > 0) {
    stop("The following channel names are inconsistent across samples: ", paste0(inconsistent_names_cols, collapse = ", "),
         "\nThe following channel descriptions are inconsistent across samples: ", paste0(inconsistent_desc_cols, collapse = ", "))
} else if (length(inconsistent_names_index) > 0) {
    stop("The following channel names are inconsistent across samples: ", paste0(inconsistent_names_cols, collapse = ", "))
} else if (length(inconsistent_desc_index) > 0) {
    stop("The following channel descriptions are inconsistent across samples: ", paste0(inconsistent_desc_cols, collapse = ", "))
}


channel_descriptions <- colnames(channel_desc_df[, !colnames(channel_desc_df) %in% c("fcs")])


# TRANSFORMATION MODULE ################

all_channels <- colnames(cs)
non_marker_channels <- all_channels[grepl("FS|SS|Time|Height|Width|Event|Center|Offset|Residual", all_channels)]
transformable_channels <- setdiff(all_channels, non_marker_channels)

asinh_transform <- TRUE
cofac <- 5
if (asinh_transform == TRUE) {
    asinhTrans <- arcsinhTransform(transformationId = "ln-transformation", a = 0, b = 1 / cofac, c = 0)
    trans_list <- transformList(transformable_channels, asinhTrans)
    cs <- transform(cs, trans_list)
}

#creating a GatingSet
gs <- GatingSet(cs)



# SELECTING SAMPLES MODULE ################
#support for multiple samples

#selecting samples
samples <- sampleNames(gs)[1:2]

#setting ID of the samples
sample_ids <- match(samples, file_names)

# SELECTING GATE MODULE ################
#setting active parent gate
active_parent <- "root"


# SELECTING CHANNELS MODULE ################
#default channels
x_axis <- channel_descriptions[1]
y_axis <- channel_descriptions[10]


# SELECTING AXIS LIMITS MODULE ################
x_limits <- NULL
y_limits <- NULL

# CUSTOMIZING PLOT PRESENTATION ################
scatter_point_size <- 1
plot_resolution <- 650

gate_linewidth <- 2

# PREPARING DATA FOR PLOT ################
#extracting cytosets
if (length(sample_ids) == 1) {
    ff <- cytoframe_to_flowFrame(gs_pop_get_data(gs[sample_ids], parent = active_parent)[[1]])
    exprs <- as.data.frame(ff@exprs)
    colnames(exprs) <- channel_descriptions
} else if (length(sample_ids) > 1) {
    exprs <- data.frame()
    for (samp_id in sample_ids) {
        ff <- cytoframe_to_flowFrame(gs_pop_get_data(gs[samp_id], parent = active_parent)[[1]])
        exprs_temp <- as.data.frame(ff@exprs)
        exprs <- rbind(exprs, exprs_temp)
    }
    colnames(exprs) <- channel_descriptions
}

#setting colors for density scatter
col <- c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598",
            "#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F",
            "#9E0142")
colRamp <- colorRampPalette(col)

#density color calculations
density <- suppressWarnings(densCols(exprs[, x_axis], exprs[, y_axis], nbin = 128, colramp = colRamp))

ctm$main_scatter <- ggplot(exprs, aes(x = !!sym(x_axis), y = !!sym(y_axis))) +
                    scattermore::geom_scattermore(aes(color = density), pointsize = scatter_point_size, pixels = c(plot_resolution, plot_resolution)) +
                    scale_color_identity() +
                    theme_minimal() +
                    theme(legend.position = "none")






# TESTING PLOT INTERACTION ################
library(shiny)
library(ggplot2)
library(scattermore)
library(r2d3)
library(htmlwidgets)

# Assume ctm, exprs, x_axis, y_axis, density, and other variables are defined as in your original code

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #scatter_container { position: relative; }
      #scatter_overlay { position: absolute; top: 0; left: 0; pointer-events: all; }
      .polygon { fill: none; stroke: red; stroke-width: 2px; }
    "))
  ),
  fluidRow(
    column(width = 8,
      div(id = "scatter_container",
        plotOutput("main_scatter", height = "600px", 
                   click = clickOpts(id = "plot_click"),
                   hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce")),
        uiOutput("d3_overlay")
      ),
      actionButton("toggle_mode", "Toggle Brush/Polygon Mode"),
      actionButton("reset", "Reset Selection")
    )
  ),
  fluidRow(
    column(width = 6,
      h4("Selected points"),
      verbatimTextOutput("selected_info")
    ),
    column(width = 6,
      h4("Selection coordinates"),
      verbatimTextOutput("selection_coords")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(mode = "brush", polygon = NULL, selected = NULL)
  
  output$main_scatter <- renderPlot({
    ctm$main_scatter
  })
  
  output$d3_overlay <- renderUI({
    d3Output("scatter_overlay", height = "600px")
  })
  
  output$scatter_overlay <- renderD3({
    req(input$main_scatter_width, input$main_scatter_height)
    r2d3(
      data = list(
        x_range = range(exprs[[x_axis]]), 
        y_range = range(exprs[[y_axis]]),
        width = input$main_scatter_width,
        height = input$main_scatter_height
      ),
      script = "scatter_overlay.js",
      d3_version = "5",
      options = list(
        mode = rv$mode,
        polygon = rv$polygon
      )
    )
  })
  
  observeEvent(input$toggle_mode, {
    rv$mode <- ifelse(rv$mode == "brush", "polygon", "brush")
    rv$polygon <- NULL
    rv$selected <- NULL
  })
  
  observeEvent(input$reset, {
    rv$polygon <- NULL
    rv$selected <- NULL
  })
  
  observeEvent(input$plot_click, {
    if (rv$mode == "polygon") {
      click <- input$plot_click
      session$sendCustomMessage("add_polygon_point", list(x = click$x, y = click$y))
    }
  })
  
  observeEvent(input$polygon_coords, {
    rv$polygon <- input$polygon_coords
    
    # Perform point-in-polygon test
    points_in_polygon <- point.in.polygon(exprs[[x_axis]], exprs[[y_axis]], 
                                          rv$polygon$x, rv$polygon$y)
    rv$selected <- which(points_in_polygon > 0)
    
    output$selected_info <- renderPrint({
      if (length(rv$selected) == 0) return("No points selected")
      head(exprs[rv$selected, ])
    })
    
    output$selection_coords <- renderPrint({
      data.frame(x = rv$polygon$x, y = rv$polygon$y)
    })
  })
  
  observeEvent(input$plot_hover, {
    if (rv$mode == "polygon" && !is.null(rv$polygon)) {
      hover_point <- list(x = input$plot_hover$x, y = input$plot_hover$y)
      session$sendCustomMessage("update_hover_point", hover_point)
    }
  })
}

shinyApp(ui, server)