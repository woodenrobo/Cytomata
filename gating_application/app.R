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

#available samples
samples <- sampleNames(ctm$gs)

#table with channel limits
ctm$axis_lims <- data.frame(channel_descriptions, rep(-1, length(channel_descriptions)),  rep("NA", length(channel_descriptions)))
colnames(ctm$axis_lims) <- c("channel_name", "min", "max")


#setting colors for density scatter
col <- c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598",
            "#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F",
            "#9E0142")
col_ramp <- colorRampPalette(col)



# SHINY APP ################


ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, preset = "yeti"),

  useShinyjs(),

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
      .custom-btn:hover{
        background-color: rgba(0,0,0,0.1) !important; /* Slight darkening on hover/focus/active */
        outline: none !important;
        box-shadow: none !important;
      }
      .custom-btn.active {
        background-color: rgba(0,0,0,0.1) !important;
        outline: none !important;
        box-shadow: none !important;
      }
      #rectangle { background-image: url('rectangle.png'); }
      #polygon { background-image: url('polygon.png'); }
      #quadrant { background-image: url('quadrant.png'); }
      #interval { background-image: url('interval.png'); }
    ")),

    # adding D3 library
    tags$script(src = "https://d3js.org/d3.v7.min.js"),


    # defining plot information container
    tags$script(HTML("
      var plotInfo = {
        x_axis_min: 0,
        x_axis_max: 0,
        x_total: 0,
        y_total: 0,
        y_axis_min: 0,
        y_axis_max: 0,
        x_margin: 0,
        y_margin: 0,
        plot_res: 0,
        d3_x_res: 0,
        d3_y_res: 0,
        cx: 0,
        cy: 0,
        svg: null
      };
    ")),
    
    # JS code to resize the plot container
    tags$script(HTML("
      Shiny.addCustomMessageHandler('plot_resolution', function(plot_resolution) {
        console.log('Received plot resolution from server:', plot_resolution);
        plotInfo.plot_res = plot_resolution;

        plotInfo.d3_x_res = plotInfo.plot_res - plotInfo.x_margin;
        plotInfo.d3_y_res = plotInfo.plot_res - plotInfo.y_margin;
      });
    ")),

    # JS code to handle x axis ranges
    tags$script(HTML("
      Shiny.addCustomMessageHandler('x_axis_range', function(x_range) {
        plotInfo.x_axis_min = x_range[0];
        plotInfo.x_axis_max = x_range[1];
        plotInfo.x_total = plotInfo.x_axis_max - plotInfo.x_axis_min;
        console.log('Received x axis range from server:', plotInfo.x_axis_min, plotInfo.x_axis_max);
      });
    ")),
    
    # JS code to handle y axis ranges
    tags$script(HTML("
      Shiny.addCustomMessageHandler('y_axis_range', function(y_range) {
        plotInfo.y_axis_min = y_range[0];
        plotInfo.y_axis_max = y_range[1];
        plotInfo.y_total = plotInfo.y_axis_max - plotInfo.y_axis_min;
        console.log('Received y axis range from server:', plotInfo.y_axis_min, plotInfo.y_axis_max);
      });
    ")),

    # JS code to handle plot panel margins
    tags$script(HTML("
      Shiny.addCustomMessageHandler('plot_margin', function(plot_margins) {
        plotInfo.x_margin = plot_margins[0];
        plotInfo.y_margin = plot_margins[1];
        console.log('Received plot margins from server:', plotInfo.x_margin, plotInfo.y_margin);

        plotInfo.d3_x_res = plotInfo.plot_res - plotInfo.x_margin;
        plotInfo.d3_y_res = plotInfo.plot_res - plotInfo.y_margin;
      });
    ")),



    # JS code creating the plot body
    tags$script(HTML("
      Shiny.addCustomMessageHandler('plot_done', function(message) {
        // Initialize SVG element if it doesn't exist
        plotInfo.svg = d3.select('#d3_output')
          .append('svg')
          .attr('width', '100%')
          .attr('height', '100%')
          .style('position', 'absolute')
          .style('z-index', '1000');
        console.log('SVG initialized');

      });
    ")),

    # JS code to handle plot clicks
    tags$script(HTML("
      Shiny.addCustomMessageHandler('scatter_click', function(click_coords) {
        console.log('Received coordinates from server:', click_coords);
        
        plotInfo.cx = ((click_coords[0] - plotInfo.x_axis_min) / plotInfo.x_total * plotInfo.d3_x_res);
        plotInfo.cy = plotInfo.d3_y_res - ((click_coords[1] - plotInfo.y_axis_min) / (plotInfo.y_total) * plotInfo.d3_y_res);
        console.log('Calculated point coordinates x:', plotInfo.cx);
        console.log('Calculated point coordinates y:', plotInfo.cy);  

      // Append a circle to the SVG
      plotInfo.svg.append('circle')
        .attr('cx', plotInfo.cx)
        .attr('cy', plotInfo.cy)
        .attr('r', 5)
        .attr('fill', 'red');

        console.log('Circle appended at:', plotInfo.cx, plotInfo.cy);
      });
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
          title = "Session Control",
          content = tagList(
            actionButton("save_session", "Save session"),
            actionButton("load_session", "Load session")
          )
        ) %>%
          bs_set_opts(
          panel_type = "success", use_heading_link = TRUE
        ) %>%
        bs_append(
          title = "Plot Settings",
          content = tagList(
            numericInput("scatter_point_size", "Scatter point size:", value = 1),
            numericInput("plot_resolution", "Plot resolution:", value = 300),
            numericInput("x_axis_min", "X-axis min:", value = -1),
            numericInput("x_axis_max", "X-axis max:", value = NA),
            numericInput("y_axis_min", "Y-axis min:", value = -1),
            numericInput("y_axis_max", "Y-axis max:", value = NA)
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
  rv_ggplot <- reactiveValues(x_range = NULL, y_range = NULL, x_distance = NULL, y_distance = NULL)
  rv <- reactiveValues(current_gate_mode = "off")

  # DENSITY SCATTER PLOT FUNCTION ################
  density_scatter <- function(exprs = ctm$exprs, x_axis = input$x_channel_select, y_axis = input$y_channel_select, scatter_point_size = input$scatter_point_size, plot_resolution = input$plot_resolution, col_ramp = col_ramp) {
    #density color calculations
    density <- suppressWarnings(densCols(exprs[, x_axis], exprs[, y_axis], nbin = 128, colramp = col_ramp))

    main_scatter <- ggplot(exprs, aes(x = !!sym(x_axis), y = !!sym(y_axis))) +
                        scattermore::geom_scattermore(aes(color = density), pointsize = scatter_point_size, pixels = c(plot_resolution, plot_resolution)) +
                        scale_color_identity() +
                        theme_cowplot() +
                        xlim(as.numeric(input$x_axis_min), as.numeric(input$x_axis_max)) +
                        ylim(as.numeric(input$y_axis_min), as.numeric(input$y_axis_max)) +
                        theme(legend.position = "none",
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank())


    # Build the plot
    built <- ggplot_build(main_scatter)

    # Extract x-axis parameters
    rv_ggplot$x_range <- built$layout$panel_params[[1]]$x$continuous_range

    # Extract y-axis parameters
    rv_ggplot$y_range <- built$layout$panel_params[[1]]$y$continuous_range

    # # Set axis margins
    # rv_ggplot$x_distance <- 29
    # rv_ggplot$y_distance <- 24

    #Extract axis margins
    g <- ggplotGrob(main_scatter)
    rv_ggplot$x_distance <- convertUnit(unit(g$widths[6], "null"), "inches", valueOnly = TRUE) * 72 + convertUnit(unit(g$widths[1], "points"), "inches", valueOnly = TRUE) * 72
    rv_ggplot$y_distance <- convertUnit(unit(g$heights[10], "null"), "inches", valueOnly = TRUE) * 72 + convertUnit(unit(g$widths[16], "points"), "inches", valueOnly = TRUE) * 72


    return(main_scatter)
  }

  #counting the number of selected samples
  selected_sample_count <- reactive({
    length(input$selected_samples)
  })

  #observing changes in sample selection and update the data
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


  # render the plot UI
  output$plotUI <- renderUI({
    tagList(
      div(style = "display: inline-block; position: relative",
          actionButton(inputId = "rectangle", label = "", class = "custom-btn"),
          actionButton(inputId = "polygon", label = "", class = "custom-btn"),
          actionButton(inputId = "quadrant", label = "", class = "custom-btn"),
          actionButton(inputId = "interval", label = "", class = "custom-btn")
        ),
      # Y-axis select box
      div(style = "display: flex; align-items: center;",
        div(style = paste0("width: 30px; position: relative; margin-bottom: ", input$plot_resolution / 2, "px;"),
          div(class = "centered-select y-axis-select",
            selectInput("y_channel_select", NULL, choices = channel_descriptions, selected = channel_descriptions[2], multiple = FALSE, selectize = FALSE, width = "100%")
          )
        ),
        # Plot output container
        div(id = "plot_container", 
              style = paste0("position: relative; margin-left: ", 20, "px;
                              width: ", input$plot_resolution, "px;
                              height: ",  input$plot_resolution, "px;"),
          #D3 overlay where the gates will be drawn
          div(id = "d3_output", style = paste0("position: absolute; z-index:2; width: ", c(input$plot_resolution - rv_ggplot$x_distance), "px;
                              height: ",  c(input$plot_resolution - rv_ggplot$y_distance), "px;
                              bottom: ", rv_ggplot$y_distance, "px;
                              left: ", rv_ggplot$x_distance, "px;
                              pointer-events: none;")),
          div(id = "main_scatter", style = paste0("position: absolute; z-index:1; width: ", input$plot_resolution, "px; height: ", input$plot_resolution, "px;"),
          plotOutput("main_scatter", width = input$plot_resolution, height = input$plot_resolution,
            click = if (rv$current_gate_mode %in% c("off", "polygon", "interval")) clickOpts(id = "scatter_click") else NULL,
            dblclick = if (rv$current_gate_mode == "off") dblclickOpts(id = "scatter_dblclick") else NULL,
            brush = if (rv$current_gate_mode == "rectangle") brushOpts(id = "scatter_brush", resetOnNew = TRUE) else NULL,
            hover = if (rv$current_gate_mode %in% c("quadrant", "interval")) hoverOpts(id = "scatter_hover", delay = 0) else NULL
          )
          )
        )
      ),
      # X-axis select box
      div(style = "display: flex; margin-top: 20px;",
        div(style = paste0("width: 30px; margin-left: ", input$plot_resolution / 2 - 50, "px;"),
          div(class = "centered-select",
            selectInput("x_channel_select", NULL, choices = channel_descriptions, selected = channel_descriptions[1], multiple = FALSE, selectize = FALSE, width = "100%")
          )
        )
      )
    )
  })



  # AXES AND API ################

  # observe changes in the selected samples and the selected channels and update the plot output
  # send axes aesthetics to the client side
  observeEvent(list(input$selected_samples, input$x_channel_select, input$y_channel_select), {
    output$main_scatter <- renderPlot({
      ctm$main_scatter <- density_scatter(exprs = ctm$exprs,
                              x_axis = input$x_channel_select,
                              y_axis = input$y_channel_select,
                              input$scatter_point_size, input$plot_resolution, col_ramp)
      ctm$main_scatter
    })

  })

  # observe changes in selected axis channel and set the axis limits from a settings table
  observeEvent(list(input$x_channel_select, input$y_channel_select), {
    x_axis_min <- ctm$axis_lims[ctm$axis_lims$channel_name == input$x_channel_select, "min"]
    x_axis_max <- ctm$axis_lims[ctm$axis_lims$channel_name == input$x_channel_select, "max"]
    y_axis_min <- ctm$axis_lims[ctm$axis_lims$channel_name == input$y_channel_select, "min"]
    y_axis_max <- ctm$axis_lims[ctm$axis_lims$channel_name == input$y_channel_select, "max"]

    updateNumericInput(session, "x_axis_min", value = x_axis_min)
    updateNumericInput(session, "x_axis_max", value = x_axis_max)
    updateNumericInput(session, "y_axis_min", value = y_axis_min)
    updateNumericInput(session, "y_axis_max", value = y_axis_max)

  })

  # observe changes in the axis limits and update the settings table
  observeEvent(list(input$x_axis_min, input$x_axis_max, input$y_axis_min, input$y_axis_max), {
    ctm$axis_lims[ctm$axis_lims$channel_name == input$x_channel_select, "min"] <- input$x_axis_min
    ctm$axis_lims[ctm$axis_lims$channel_name == input$x_channel_select, "max"] <- input$x_axis_max
    ctm$axis_lims[ctm$axis_lims$channel_name == input$y_channel_select, "min"] <- input$y_axis_min
    ctm$axis_lims[ctm$axis_lims$channel_name == input$y_channel_select, "max"] <- input$y_axis_max

  })

  # send x and y axis ranges to the client side
  observe({
    x_range <- rv_ggplot$x_range
    y_range <- rv_ggplot$y_range
    session$sendCustomMessage("x_axis_range", x_range)
    session$sendCustomMessage("y_axis_range", y_range)

    plot_margins <- c(as.numeric(rv_ggplot$x_distance), as.numeric(rv_ggplot$y_distance))
    ctm$plot_margins <- plot_margins
    session$sendCustomMessage("plot_margin", plot_margins)

    session$sendCustomMessage("plot_done", "TRUE")
  })


  # Observe resolution changes and send the new resolution to the plot container
  observeEvent(input$plot_resolution, {
    plot_resolution <- input$plot_resolution
    session$sendCustomMessage("plot_resolution", plot_resolution)
  })


  # EVERYTHING TO DO WITH GATING ################

  # Function to toggle gating modes
  
  toggleGatingMode <- function(mode) {
    if (rv$current_gate_mode == mode) {
      rv$current_gate_mode <- "off"
    } else {
      rv$current_gate_mode <- mode
    }
    print(rv$current_gate_mode)
  }

  # switch between gating modes
  observeEvent(input$rectangle, {
    toggleGatingMode("rectangle")
  })

  observeEvent(input$polygon, {
    toggleGatingMode("polygon")
  })

  observeEvent(input$quadrant, {
    toggleGatingMode("quadrant")
  })

  observeEvent(input$interval, {
    toggleGatingMode("interval")
  })

  # Function to update button appearances
  updateButtonAppearance <- function(mode) {
    lapply(c("rectangle", "polygon", "quadrant", "interval"), function(btn) {
      if (btn == mode && mode != "off") {
        shinyjs::addClass(btn, "active")
      } else {
        shinyjs::removeClass(btn, "active")
      }
    })
  }

  # Update button appearances when mode changes
  observe({
    updateButtonAppearance(rv$current_gate_mode)
  })

  # send current gate mode to the client side
  observe({
    session$sendCustomMessage("gate_mode", rv$current_gate_mode)
  })


  # Observe click events and send coordinates to the client side
  observeEvent(input$scatter_click, {
    click_coords <- c(input$scatter_click$x, input$scatter_click$y)
    print(paste("Clicked at: x =", click_coords[1], "y =", click_coords[2]))
    session$sendCustomMessage("scatter_click", click_coords)
  })

  # Observe double-click events and send coordinates to the client side
  observeEvent(input$scatter_dblclick, {
    dbl_click_coords <- c(input$scatter_dblclick$x, input$scatter_dblclick$y)
    print(paste("DBL Clicked at: x =", dbl_click_coords[1], "y =", dbl_click_coords[2]))
    session$sendCustomMessage("scatter_dbl_click", dbl_click_coords)
  })

  # Observe brush events and send coordinates to the client side
  observeEvent(input$scatter_brush, {
    brush_coords <- c(input$scatter_brush$xmin,
                    input$scatter_brush$xmax,
                    input$scatter_brush$ymin,
                    input$scatter_brush$ymax)
    ctm$mat <- matrix(brush_coords, ncol = 2,
                    dimnames = list(c("min", "max"), c(input$x_channel_select, input$y_channel_select)))
    output$mat <- renderPrint({
                          ctm$mat
                  })
    session$sendCustomMessage("scatter_brush", brush_coords)
  })

  # Observe hover events and send coordinates to the client side
  observeEvent(input$scatter_hover, {
    hover_coords <- c(input$scatter_hover$x, input$scatter_hover$y)
    print(paste("Hovered at: x =", hover_coords[1], "y =", hover_coords[2]))
    session$sendCustomMessage("scatter_hover", hover_coords)
  })

  # Observe brushed events and print the brushed points
  observeEvent(input$scatter_brush, {
      output$brush_info <- renderPrint({
          brushedPoints(ctm$exprs, input$scatter_brush)
      })
  })


  # # CHANGE THIS TO ONLY SAVE THE GATE INFORMATION AND AXIS SETTINGS
  # observeEvent(input$save_session, {
  #   ctm$main_scatter < NULL
  #   ctm$gs <- NULL
  #   ctm$exprs <- NULL
    
  #   save(ctm, file = "ctm.RData")
  # })

  # observeEvent(input$load_session, {
  #   ctm <- load("ctm.RData")
  #   ctm$gs <- GatingSet(cs)
  #   #available samples
  #   samples <- sampleNames(ctm$gs)
  # })



}




shinyApp(ui, server)





