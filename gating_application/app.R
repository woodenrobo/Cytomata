ctm <- new.env()
# DATA LOADING MODULE ################

#setting paths to data
datapath <- raw_folder
file_names <- dir(datapath, full.names = FALSE, pattern = ".fcs$")
file_paths <- dir(datapath, full.names = TRUE, pattern = ".fcs$")

setwd(gating_app_folder)

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
ctm$gs <- GatingSet(cs)



# SELECTING SAMPLES MODULE ################
#support for multiple samples

#available samples
samples <- sampleNames(ctm$gs)



# SELECTING GATE MODULE ################



# SELECTING AXIS LIMITS MODULE ################
x_limits <- NULL
y_limits <- NULL

# CUSTOMIZING PLOT PRESENTATION ################
# scatter_point_size <- 1
# plot_resolution <- 650


gate_linewidth <- 2



#setting colors for density scatter
col <- c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598",
            "#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F",
            "#9E0142")
col_ramp <- colorRampPalette(col)


# DENSITY SCATTER PLOT FUNCTION ################
density_scatter <- function(exprs = ctm$exprs, x_axis = input$x_channel_select, y_axis = input$y_channel_select, scatter_point_size = input$scatter_point_size, plot_resolution = input$plot_resolution, col_ramp = col_ramp) {
  #density color calculations
  density <- suppressWarnings(densCols(exprs[, x_axis], exprs[, y_axis], nbin = 128, colramp = col_ramp))

  main_scatter <- ggplot(exprs, aes(x = !!sym(x_axis), y = !!sym(y_axis))) +
                      scattermore::geom_scattermore(aes(color = density), pointsize = scatter_point_size, pixels = c(plot_resolution, plot_resolution)) +
                      scale_color_identity() +
                      theme_cowplot() +
                      theme(legend.position = "none",
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank())
  return(main_scatter)
}
  



# TESTING PLOT INTERACTION ################


ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, preset = "yeti"),

  tags$head(
    # CSS for select boxes
    tags$style(HTML("
      .selectize-input { min-height: 20px; padding: 2px 5px; }
      .selectize-dropdown, .selectize-input { font-size: 12px; }
      #y_channel_select { width: 200px; }
      #x_channel_select { width: 200px; }
    ")),
    # CSS for centered select boxes
    tags$style(HTML("
      .centered-select select {
        text-align-last: center;
        text-align: center;
        -ms-text-align-last: center;
        -moz-text-align-last: center;
      }
      .centered-select select option {
        text-align: left;
      }
      /* For Firefox */
      @-moz-document url-prefix() {
        .centered-select select { text-align: center; }
      }
      .y-axis-select {
        transform: rotate(-90deg);
        transform-origin: right center;
        width: 200px;
        position: relative;
        top: 50%;
        left: -180px;  /* Adjust this value as needed */
      }
    ")),

    # CSS for custom icons
    tags$style(HTML("
      .custom-btn {
        width: 50px;
        height: 50px;
        padding: 0;
        background-size: 90%;
        background-position: center;
        background-repeat: no-repeat;
        background-color: transparent !important;
        border: none;
      }
      .custom-btn:hover, .custom-btn:focus, .custom-btn:active {
        background-color: rgba(0,0,0,0.1) !important; /* Slight darkening on hover/focus/active */
        outline: none !important;
        box-shadow: none !important;
      }
      #rectangle { background-image: url('rectangle.png'); }
      #polygon { background-image: url('polygon.png'); }
      #quadrant { background-image: url('quadrant.png'); }
      #interval { background-image: url('interval.png'); }
    "))
  ),

  fluidRow(
    column(width = 2,
      h3("Samples"),
      selectInput("selected_samples", NULL, choices = samples, selected = samples[1], multiple = TRUE),
      uiOutput("sample_switch"),
      # accordion with plot settings
      bs_accordion(id = "accordion") %>%
        bs_set_opts(
          panel_type = "success", use_heading_link = TRUE
        ) %>%
        bs_append(
          title = "Plot Settings",
          content = tagList(
            numericInput("scatter_point_size", "Scatter point size:", value = 1),
            numericInput("plot_resolution", "Plot resolution:", value = 300)
          )
        ) %>%
          bs_set_opts(
          panel_type = "success", use_heading_link = TRUE
        ) %>%
        bs_append(
          title = "Session Settings",
          content = tagList(
            actionButton("save_session", "Save session"),
            actionButton("load_session", "Load session")
          )
        )

    ),
    column(width = 7,
      uiOutput("plotUI")
      ),
    column(width = 3,
      br())
  ),

  fluidRow(
      column(width = 4,
      h4("Brushed points"),
      verbatimTextOutput("brush_info")
    ),
      column(width = 4,
      h4("Rectangle coordinates"),
      verbatimTextOutput("mat")
    ),
    column(width = 4,
      h4("Activated parent gate"),
      verbatimTextOutput("active_parent")
    )
  )
)

server <- function(input, output, session) {


  selected_sample_count <- reactive({
    length(input$selected_samples)
  })

  observeEvent(input$selected_samples, {

    #setting ID of the samples
    ctm$sample_ids <- match(input$selected_samples, file_names)

    #setting default active parent gate
    active_parent <- "root"

    # PREPARING DATA FOR PLOT ################
    #extracting cytosets
    if (selected_sample_count() == 1) {
        ff <- cytoframe_to_flowFrame(gs_pop_get_data(ctm$gs[ctm$sample_ids], parent = active_parent)[[1]])
        ctm$exprs <- as.data.frame(ff@exprs)
        colnames(ctm$exprs) <- channel_descriptions
    } else if (selected_sample_count() > 1) {
        exprs <- data.frame()
        for (samp_id in ctm$sample_ids) {
            ff <- cytoframe_to_flowFrame(gs_pop_get_data(ctm$gs[samp_id], parent = active_parent)[[1]])
            exprs_temp <- as.data.frame(ff@exprs)
            exprs <- rbind(exprs, exprs_temp)
        }
        ctm$exprs <- exprs
        colnames(ctm$exprs) <- channel_descriptions
    }
  })

  # render the sample switch buttons if there is only one sample selected
  output$sample_switch <- renderUI({
    if (selected_sample_count() == 1) {
      div(id = "sample_switch", style = "display:inline-block; position: relative",
        actionButton(inputId = "previous_sample",
                    width = "40px",
                    icon("chevron-left")),
        actionButton(inputId = "next_sample",
                    width = "40px",
                    icon("chevron-right"))
      )
    } else {
      NULL
    }
  })

  # switch between samples
  observeEvent(input$previous_sample, {
    current_index <- match(input$selected_samples, samples)
    if (current_index > 1) {
      updateSelectInput(session, "selected_samples", selected = samples[current_index - 1])
    }
  })

  observeEvent(input$next_sample, {
    current_index <- match(input$selected_samples, samples)
    if (current_index < length(samples)) {
      updateSelectInput(session, "selected_samples", selected = samples[current_index + 1])
    }
  })


  output$plotUI <- renderUI({
    tagList(
      div(style = "display: inline-block; position: relative",
          actionButton(inputId = "rectangle", label = "", class = "custom-btn"),
          actionButton(inputId = "polygon", label = "", class = "custom-btn"),
          actionButton(inputId = "quadrant", label = "", class = "custom-btn"),
          actionButton(inputId = "interval", label = "", class = "custom-btn")
        ),

      div(style = "display: flex; align-items: center;",
        div(style = paste0("width: 30px; position: relative; margin-bottom: ", input$plot_resolution / 2, "px;"),
          div(class = "centered-select y-axis-select",
            selectInput("y_channel_select", NULL, choices = channel_descriptions, selected = channel_descriptions[2], multiple = FALSE, selectize = FALSE, width = "100%")
          )
        ),
        div(style = "flex-grow: 1; margin-left: 10px;",
          plotOutput("main_scatter", width = input$plot_resolution, height = input$plot_resolution,
            click = "scatter_click",
            brush = brushOpts(
              id = "scatter_brush", fill = "#5acef5",
              stroke = "black", opacity = 0.3, delay = 100, delayType = c("debounce")
            )
          )
        )
      ),

      div(style = "display: flex; margin-top: 10px;",
        div(style = paste0("width: 30px; margin-left: ", input$plot_resolution / 2 - 50, "px;"),
          div(class = "centered-select",
            selectInput("x_channel_select", NULL, choices = channel_descriptions, selected = channel_descriptions[1], multiple = FALSE, selectize = FALSE, width = "100%")
          )
        )
      )
    )
  })




  observeEvent(list(input$selected_samples, input$x_channel_select, input$y_channel_select), {
    output$main_scatter <- renderPlot({
      ctm$main_scatter <- density_scatter(exprs = ctm$exprs,
                              x_axis = input$x_channel_select,
                              y_axis = input$y_channel_select,
                              input$scatter_point_size, input$plot_resolution, col_ramp)
      ctm$main_scatter
    })
  })




  observeEvent(input$scatter_brush, {
      output$brush_info <- renderPrint({
          brushedPoints(ctm$exprs, input$scatter_brush)
      })
      components <- c(input$scatter_brush$xmin,
                      input$scatter_brush$xmax,
                      input$scatter_brush$ymin,
                      input$scatter_brush$ymax)
      ctm$mat <- matrix(components, ncol = 2,
                    dimnames = list(c("min", "max"), c(input$x_channel_select, input$y_channel_select)))
      output$mat <- renderPrint({
                          ctm$mat
                  })
      
  })



}

shinyApp(ui, server)
