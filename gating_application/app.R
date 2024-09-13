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
colnames(ctm$gs) <- channel_descriptions

#available samples
ctm$samples <- sampleNames(ctm$gs)

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

    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    
    # misc. JS code for the app
    tags$script(src = "misc.js"),

    # adding D3 library
    tags$script(src = "https://d3js.org/d3.v7.min.js"),

    # JS code creating the axes and reinitializing and drawing gating SVG
    # also contains logic for gate interactions
    # also translates coordinates from the plot back to the R backend
    tags$script(src = "d3_graphics.js")
  ),

  fluidRow(
    column(width = 2,
      h3("Samples"),
      selectInput("selected_samples", NULL, choices = ctm$samples, selected = ctm$samples[1], multiple = TRUE),
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
            numericInput("plot_resolution", "Plot resolution:", value = 450),
            numericInput("x_axis_min", "X-axis min:", value = -1),
            numericInput("x_axis_max", "X-axis max:", value = NA),
            numericInput("y_axis_min", "Y-axis min:", value = -1),
            numericInput("y_axis_max", "Y-axis max:", value = NA)
          )
        )
    ),
    column(width = 7,
      div(style = "display: inline-block; position: relative",
            actionButton(inputId = "rectangle", label = "", class = "custom-btn"),
            actionButton(inputId = "polygon", label = "", class = "custom-btn"),
            actionButton(inputId = "quadrant", label = "", class = "custom-btn"),
            actionButton(inputId = "interval", label = "", class = "custom-btn"),
            actionButton(inputId = "switch_channels", label = "", class = "custom-btn-switch")
          ),
      uiOutput("axes"),
      uiOutput("plotUI")
    ),
    column(width = 3,
      uiOutput("gatetreeUI"))
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
      h4("Activate parent gate"),
      uiOutput("active_parent")

    )
  )
)






server <- function(input, output, session) {
  rv_ggplot <- reactiveValues(x_range = NULL, y_range = NULL, m_top = 20, m_right = 20, m_bottom = 30, m_left = 45)
  rv_gates <- reactiveValues(current_gate_mode = "off", active_parent = "root", pop_paths = NULL, gates_found = FALSE, gates_data = NULL, new_gate_name = NULL)
  
  rv_gates$pop_paths <- gs_get_pop_paths(ctm$gs)

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
                              axis.title.y = element_blank()) +
                        theme_void()


    # Build the plot
    ctm$built <- ggplot_build(main_scatter)

    # Extract x-axis parameters
    rv_ggplot$x_range <- ctm$built$layout$panel_params[[1]]$x$continuous_range

    # Extract y-axis parameters
    rv_ggplot$y_range <- ctm$built$layout$panel_params[[1]]$y$continuous_range

    return(main_scatter)
  }

  #counting the number of selected samples
  selected_sample_count <- reactive({
    length(input$selected_samples)
  })

  #observing changes in sample selection and update the data
  observeEvent(list(input$selected_samples, rv_gates$active_parent), {
    #setting ID of the samples
    ctm$sample_ids <- match(input$selected_samples, file_names)


    # PREPARING DATA FOR PLOT ################
    #extracting cytosets
    if (selected_sample_count() == 1) {
        ff <- cytoframe_to_flowFrame(gs_pop_get_data(ctm$gs[ctm$sample_ids], y = rv_gates$active_parent)[[1]])
        ctm$exprs <- as.data.frame(ff@exprs)
        colnames(ctm$exprs) <- channel_descriptions
    } else if (selected_sample_count() > 1) {
        exprs <- data.frame()
        for (samp_id in ctm$sample_ids) {
            ff <- cytoframe_to_flowFrame(gs_pop_get_data(ctm$gs[samp_id], y = rv_gates$active_parent)[[1]])
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
    current_index <- match(input$selected_samples, ctm$samples)
    if (current_index > 1) {
      updateSelectInput(session, "selected_samples", selected = ctm$samples[current_index - 1])
    }
  })

  observeEvent(input$next_sample, {
    current_index <- match(input$selected_samples, ctm$samples)
    if (current_index < length(ctm$samples)) {
      updateSelectInput(session, "selected_samples", selected = ctm$samples[current_index + 1])
    }
  })


  #default channel selection
  current_axes <- reactiveValues(x = channel_descriptions[1], y = channel_descriptions[2])

  # switch channel places button
  observeEvent(input$switch_channels, {
    temp <- current_axes$x
    current_axes$x <- current_axes$y
    current_axes$y <- temp
    updateSelectInput(session, "x_channel_select", selected = current_axes$x)
    updateSelectInput(session, "y_channel_select", selected = current_axes$y)
  })


  # update channel selection
  observeEvent(input$x_channel_select, {
    current_axes$x <- input$x_channel_select
    updateSelectInput(session, "x_channel_select", selected = current_axes$x)
    x_name <- current_axes$x
    session$sendCustomMessage("x_channel_select", x_name)
  })

  observeEvent(input$y_channel_select, {
    current_axes$y <- input$y_channel_select
    updateSelectInput(session, "y_channel_select", selected = current_axes$y)
    y_name <- current_axes$y
    session$sendCustomMessage("y_channel_select", y_name)
  })



  # render the plot UI
  output$plotUI <- renderUI({
    
    tagList(
      
      # Y-axis select box
      div(style = "display: flex; align-items: center;",
        div(style = paste0("width: 30px; position: relative; margin-bottom: ", input$plot_resolution / 2, "px;"),
          div(class = "centered-select y-axis-select",
            selectInput("y_channel_select", NULL, choices = channel_descriptions, selected = current_axes$y, multiple = FALSE, selectize = FALSE, width = "100%")
          )
        ),
        # Plot output container
        div(id = "plot_container", 
              style = paste0("position: relative; margin-left: ", 20, "px;
                width: ", input$plot_resolution, "px;
                height: ",  input$plot_resolution, "px;"),
          
          #Main scatter container with ggplot and D3 gate overlay
          div(id = "main_scatter", 
                style = paste0("position: absolute; z-index:1;
                  width: ", c(input$plot_resolution - rv_ggplot$m_right - rv_ggplot$m_left), "px;
                  height: ", c(input$plot_resolution - rv_ggplot$m_top - rv_ggplot$m_bottom), "px;
                  transform: translate(", rv_ggplot$m_left, "px, ", rv_ggplot$m_top, "px);"),
            div(id = "d3_output_gates", 
                style = paste0("position: absolute; z-index:400;
                  width: ", c(input$plot_resolution - rv_ggplot$m_right - rv_ggplot$m_left), "px;
                  height: ",  c(input$plot_resolution - rv_ggplot$m_top - rv_ggplot$m_bottom), "px;
                  pointer-events: none;")),
            plotOutput("main_scatter", 
              width = c(input$plot_resolution - rv_ggplot$m_right - rv_ggplot$m_left),
              height = c(input$plot_resolution - rv_ggplot$m_top - rv_ggplot$m_bottom),
              click = if (rv_gates$current_gate_mode %in% c("polygon", "quadrant", "interval")) clickOpts(id = "scatter_click") else NULL,
              # dblclick = if (rv_gates$current_gate_mode == "off") dblclickOpts(id = "scatter_dblclick") else NULL,
              brush = if (rv_gates$current_gate_mode == "rectangle") brushOpts(id = "scatter_brush", fill = "lightblue", opacity = 0.2, resetOnNew = TRUE) else NULL,
              hover = if (rv_gates$current_gate_mode %in% c("quadrant", "interval")) hoverOpts(id = "scatter_hover", delay = 0) else NULL
              )
          )
        )
      ),
      # X-axis select box
      div(style = "display: flex; margin-top: 20px;",
        div(style = paste0("width: 30px; margin-left: ", input$plot_resolution / 2 - 50, "px;"),
          div(class = "centered-select",
            selectInput("x_channel_select", NULL, choices = channel_descriptions, selected = current_axes$x, multiple = FALSE, selectize = FALSE, width = "100%")
          )
        )
      )
    )
  })


  #render axes
  output$axes <- renderUI({
    #D3 overlay where the axes are drawn
          div(id = "d3_output", 
                style = paste0("position: absolute; z-index:300; 
                  transform: translate(", c(50), "px, ", c(0), "px);
                  width: ", 0, "px;
                  height: ",  0, "px;
                  pointer-events: none;"))
  })


  output$gatetreeUI <- renderUI({
      plotOutput("gating_tree"
        )
  })

  output$active_parent <- renderUI({
    selectInput("active_parent", NULL, choices = rv_gates$pop_paths, selected = rv_gates$pop_paths[1], multiple = FALSE, selectize = TRUE, width = "100%")
  })


  observeEvent(input$active_parent, {
    rv_gates$active_parent <- input$active_parent
  })

  # AXES AND API ################

  # observe changes in the selected samples and the selected channels and update the plot output
  # send axes aesthetics to the client side
  observeEvent(list(input$selected_samples, input$x_channel_select, input$y_channel_select, rv_gates$active_parent), {
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

  # send plot margins to the client side
  observe({
    margins <- c(rv_ggplot$m_top, rv_ggplot$m_right, rv_ggplot$m_bottom, rv_ggplot$m_left)
    session$sendCustomMessage("plot_margin", margins)
  })


  # send x and y axis ranges to the client side
  observe({
    x_range <- rv_ggplot$x_range
    y_range <- rv_ggplot$y_range
    session$sendCustomMessage("x_axis_range", x_range)
    session$sendCustomMessage("y_axis_range", y_range)
    
    session$sendCustomMessage("plot_done", "plot done")
  })


  # Observe resolution changes and send the new resolution to the plot container
  observeEvent(input$plot_resolution, {
    plot_resolution <- input$plot_resolution
    session$sendCustomMessage("plot_resolution", plot_resolution)

    # session$sendCustomMessage("plot_done", "plot done")
  })





  # EVERYTHING TO DO WITH GATING ################



  # Plotting gate hierarchy
    output$gating_tree <- renderPlot({
      
      labels <- rv_gates$pop_paths
      if (length(labels) > 1) {
        hPlot <- plot(ctm$gs)
        

        names(labels) <- nodes(hPlot)
        nodeAttrs <- list(label=labels)
        attrs <- list(node=list(fillcolor="white", shape="box", width=1,
                                color="gray90", style="rounded"),
                      graph=list(rankdir="TB"))
        index <- which(gs_get_pop_paths(ctm$gs) == "root")


        ctm$hPlot <- plot(hPlot, nodeAttrs=nodeAttrs, attrs=attrs)


        ctm$hPlot
      }
      
    })

          


  # GATING MODES ################

  # Function to toggle gating modes
  
  toggleGatingMode <- function(mode) {
    if (rv_gates$current_gate_mode == mode) {
      rv_gates$current_gate_mode <- "off"
    } else {
      rv_gates$current_gate_mode <- mode
    }
    print(rv_gates$current_gate_mode)
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
    updateButtonAppearance(rv_gates$current_gate_mode)
  })

  # send current gate mode to the client side
  observe({
    session$sendCustomMessage("gate_mode", rv_gates$current_gate_mode)
  })





 # PLOT INTERACTION COORDINATES ################

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

    rectangle_coords <- ctm$mat
    ctm$gate <- rectangleGate(.gate = rectangle_coords)

  })



  # Observe brush events and show modal dialog to name the gate
  observeEvent(input$scatter_brush, {
      showModal(modalDialog(
        title = "Name the gate",
        textInput("gate_name", "Enter gate name:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_gate_name", "OK")
        )
      ))
  })

  # Observe gate name submission and add the gate to the gating tree
  # send the new gate coordinates to the client side
  observeEvent(input$submit_gate_name, {
    print(input$gate_name)
    rv_gates$new_gate_name <- input$gate_name
    print(rv_gates$new_gate_name)

    gate_name <- rv_gates$new_gate_name
    add_gate(gs = ctm$gs, ctm$gate, gate_name, active_parent = rv_gates$active_parent)
    print("gate added")

    toggleGatingMode("rectangle")
    session$sendCustomMessage("scatter_brush", ctm$mat)

    removeModal()
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











  # GATE CREATION ################

  escape_all_special <- function(str) {
    gsub("([^[:alnum:][:space:]])", "\\\\\\1", str)
  }

  add_gate <- function(gs = ctm$gs, gate, gate_name, active_parent) {
    fullname <- paste0(active_parent, "/", gate_name)
    gate_name_query <- paste0("^.*/", escape_all_special(fullname), "$")

    if (length(gate_name_query) > 1) {
      gate_name_query_collapsed <- paste0(gate_name_query, collapse = "|")
      if (sum(grepl(gate_name_query_collapsed, rv_gates$pop_paths, perl = TRUE) > 0)) {
        print("Gate already exists")
      } else {
        gs_pop_add(gs, gate, parent = active_parent, name = gate_name, validityCheck = FALSE)
        recompute(gs)
        rv_gates$pop_paths <- gs_get_pop_paths(gs)
      }
    } else {
      if (sum(grepl(gate_name_query, rv_gates$pop_paths, perl = TRUE) > 0)) {
        print("Gate already exists")
      } else {
        gs_pop_add(gs, gate, parent = active_parent, name = gate_name, validityCheck = FALSE)
        recompute(gs)
        rv_gates$pop_paths <- gs_get_pop_paths(gs)
      }
    }
  } 



  # GATE INFO COLLECTION ################
  gate_collect_info <- function(gs = ctm$gs, pop_paths = rv_gates$pop_paths) {
    gates_info <- NULL
    sample_gates_info <- NULL
    for (sample in seq_along(ctm$samples)) {
        if (length(pop_paths) > 1) {
            for (i in seq(pop_paths)[-1]) {
                gs_get_pop_paths(ctm$gs)
                gate_info <- gh_pop_get_gate(gs[[sample]], pop_paths[i])
                name <- pop_paths[i]
                type <- class(gate_info)[1]
                gate_params <- gate_info@parameters
                axis1 <- names(gate_params[1])
                axis2 <- names(gate_params[2])

                if (type == "rectangleGate") {
                    coords <- rbind(gate_info@min, gate_info@max)
                } else if (type == "polygonGate") {
                    coords <- gate_info@boundaries
                }
                
                sample_gates_info[[i]] <- list(name = name, type = type, axis1 = axis1, axis2 = axis2, coords = coords)
            } 
        gates_info[[sample]] <- sample_gates_info
        }
    }
    names(gates_info) <- sampleNames(gs[seq_along(ctm$samples)])

    return(gates_info)

    ctm$gate_freqs <- gs_pop_get_count_fast(ctm$gs[seq_along(ctm$samples)], "freq")
    ctm$gate_counts <- gs_pop_get_count_fast(ctm$gs[seq_along(ctm$samples)], "count")
  }

  # observe changes in the gating tree and update the gate information
  observe({
    if (length(rv_gates$pop_paths) > 1) {
      rv_gates$gates_data <- gate_collect_info(gs = ctm$gs, pop_paths = rv_gates$pop_paths)
      ctm$gates_data <- rv_gates$gates_data
    }
  })


  # GATE DETECTION ################
  detect_gate <- function(gates_info = rv_gates$gates_data, x_axis = input$x_channel_select, y_axis = input$y_channel_select) {
    channel1_gates <- which(lapply(gates_info[[1]], function(x) x$axis1) %in% c(x_axis, y_axis))
    channel2_gates <- which(lapply(gates_info[[1]], function(x) x$axis2) %in% c(x_axis, y_axis))
    channel1_gates_na <- which(is.na(lapply(gates_info[[1]], function(x) x$axis1)))
    channel2_gates_na <- which(is.na(lapply(gates_info[[1]], function(x) x$axis2)))

    ctm$detected_gates_total <- union(channel1_gates, channel2_gates)

    ctm$detected_gates_biaxial <- intersect(channel1_gates, channel2_gates)
    ctm$detected_gates_mono <- intersect(ctm$detected_gates_total, union(channel1_gates_na, channel2_gates_na))
  }

  # observe changes in the channel selection and gating tree and update the detected gates
  observeEvent(list(input$x_channel_select, input$y_channel_select, rv_gates$gates_data), {
    detect_gate(gates_info = rv_gates$gates_data, x_axis = input$x_channel_select, y_axis = input$y_channel_select)

    # match the detected gate IDs to the gate names
    biaxial_gate_names <- gs_get_pop_paths(ctm$gs)[ctm$detected_gates_biaxial]
    mono_gate_names <- gs_get_pop_paths(ctm$gs)[ctm$detected_gates_mono]

    # get detected gate information
    detected_gate_info_biaxial <- lapply(rv_gates$gates_data[[1]], function(x) x[x$name %in% biaxial_gate_names])
    biaxial_names <- lapply(detected_gate_info_biaxial, function(x) x$name)[-1]
    biaxial_types <- lapply(detected_gate_info_biaxial, function(x) x$type)[-1]
    biaxial_axis1 <- lapply(detected_gate_info_biaxial, function(x) x$axis1)[-1]
    biaxial_axis2 <- lapply(detected_gate_info_biaxial, function(x) x$axis2)[-1]
    biaxial_coords <- lapply(detected_gate_info_biaxial, function(x) x$coords)[-1]

    session$sendCustomMessage("detected_gates_biaxial", list(names = biaxial_names, types = biaxial_types, axis1 = biaxial_axis1, axis2 = biaxial_axis2, coords = biaxial_coords))

    # detected_gate_info_mono <- lapply(ctm$gates_data[[1]], function(x) x[x$name %in% mono_gate_names])
    

  })  

  
  
  # GATE PLOTTING ################

  


  # GATE UPDATES ################



  # GATE ACTIVATION ################




  # GATE DELETION ################






  # SAVING AND LOADING SESSIONS ################

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
