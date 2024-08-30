# environment variables that are saved upon exit from gating
ctm <- new.env()
tempEnv <- new.env()
ctm$gs <- NULL
ctm$file_paths <- NULL
ctm$sample_list <- NULL
ctm$blackB <- "color:white; background-color:black; border-color:black"
ctm$whiteB <- "color:black; background-color:white; border-color:black"
ctm$sliderCol <- "+span>.irs-bar {background: black; border-top: black; border-bottom: black}"


# my color scale in case I need it
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


# functions used in the app

# injecting data
inject_fcs_gating <- function(input, asinh_transform, cofac) {
    # library(flowWorkspace)
    # library(flowCore)
    # library(dplyr)
    cs <- load_cytoset_from_fcs(input)

    
    non_marker_channels <- colnames(cs)
    non_marker_channels <- non_marker_channels[grepl("FS|SS|Time|Height|Width|Event|Center|Offset|Residual", non_marker_channels)]
    transformable_channels <- setdiff(colnames(cs), non_marker_channels)

    if (asinh_transform == TRUE) {
        asinhTrans <- arcsinhTransform(transformationId = "ln-transformation", a = 0, b = 1 / cofac, c = 0)
        trans_list <- transformList(transformable_channels, asinhTrans)
        cs <- transform(cs, trans_list)
    }

    gs <- GatingSet(cs)
    colnames(gs) <- colnames(cs)

    return(gs)
}


# Plots
newPlot <- function(ID, X, Y, parent, typ, axis, font, bgDF=NULL, bg=FALSE) {

  ff <- cytoframe_to_flowFrame(gs_pop_get_data(ctm$gs[ID], parent)[[1]])

  if(length(ff@exprs) > 0) {
   
    ctm$dF <- as.data.frame.array(ff@exprs)
    dfX <- ctm$dF[,X]
    dfX[dfX < 0] <- 0

    
    if(typ != "Histogram") {
      newDotPlot(dfX, X, Y, typ, axis, font, bgDF, xLim)
    } else {
      newHist(Y, dfX, axis, font, xLim)
    }

    index <- which(ctm$fluoCh == X)
    
  } else {
    alert(paste0("The selected sample has 0 events under the selected
                     parent. Please choose another sample or change the
                     parent."))
  }
}

newDotPlot <- function(dfX, X, Y, typ, axis, font, bgDF, xLim) {

  dfY <- ctm$dF[,Y]
  dfY[dfY < 0] <- 0
  
  if (typ != "Contour") {
      if (typ == "Density") {
        col <- c("grey0", "grey20", "grey40", "grey60", "grey80",
                 "grey100")
      } else {
        col <- c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598",
                 "#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F",
                 "#9E0142")
      }
      colRamp <- colorRampPalette(col)
      density <- suppressWarnings(densCols(dfX, dfY, colramp=colRamp))
      plot(dfX, dfY, xaxt="n", yaxt="n", ann=FALSE, pch=20, cex=0.5,
           lwd=0, col=density, xlim=xLim, ylim=yLim, xaxs="i", yaxs="i")
    }
    if (typ == "Contour" || typ == "Pseudocolor + Contour") {

      addContour(dfX, dfY, X, Y, typ, xLim, yLim)
    }

}

addContour <- function(dfX, dfY, X, Y, typ, xLim, yLim) {
  if (nrow(ctm$dF) > 5000) {
    set.seed(1)
    sampledDF <- ctm$dF[sample.int(nrow(ctm$dF), 5000), ]
  } else {
    sampledDF <- ctm$dF
  }
  ctm$z <- kde2d(sampledDF[, X], sampledDF[, Y], n = 100)
  if (typ == "Contour") {
    plot(dfX, dfY, xaxt = "n", yaxt = "n", ann = FALSE, pch = 20, cex = 0.5,
         lwd = 0, col = "black", xlim = xLim, ylim = yLim, xaxs = "i", yaxs = "i")
    for (i in contourLines(ctm$z, nlevels = 12)) {
      if (i$level != 5e-08) {
        polygon(i$x, i$y, col = "white", lwd = 1.2)
      }
    }
  } else {
    contour(ctm$z, drawlabels = FALSE, xaxt = "n", yaxt = "n", add = TRUE,
            ann = FALSE, lwd = 1.5, nlevels = 12)
  }
}

# Gating
detectGate <- function(ID, X, Y, par, typ, obs, show, font, bg = FALSE) {
  popPaths <- gs_get_pop_paths(ctm$gs)
  found <- NULL
  detectedTypes <- NULL
  if (length(popPaths) > 1) {
    for (i in seq(popPaths)[-1]) {
      gatesandPopsInfo <- gs_pop_get_gate(ctm$gs, popPaths[i])[ID][[1]]
      detectedTypes[i] <- class(gatesandPopsInfo)[1]
      infoPar <- gatesandPopsInfo@parameters
      chX <- names(infoPar[1])
      if (typ != "Histogram") {
        if (length(infoPar) > 1) {
          chY <- names(infoPar[2])
        } else {
          chY <- "none"
        }
      } else {
        chY <- "Histogram" }
      if (X == chX && Y == chY
         || X == chY && Y == chX
         || typ == "Histogram" && length(infoPar) == 1 && X == chX) {
        detectedID <- which(popPaths == popPaths[i])
        preDetec <- gs_pop_get_count_fast(ctm$gs[ID], "freq")
        detectedParent <- preDetec[detectedID - 1][,3][[1]]
        if(par == detectedParent) {
          found[i-1] <- gatesandPopsInfo@filterId
        }
      }
    }
    if (obs == "plot" || obs == ">= 4") {
      ctm$found <- found[!is.na(found)]
      detectedTypes <- detectedTypes[!is.na(detectedTypes)]
      if (obs == "plot" && !is.null(ctm$found)) {
        plotGate(ID, X, Y, typ, detectedTypes, show,
                 font, bg)
      }
    }
  }
}

plotGate <- function(ID, X, Y, typ, types, show, font, bg=FALSE) {
  for (i in seq(ctm$found)) {
    coords <- gs_pop_get_gate(ctm$gs, ctm$found[i])[[ID]]
    if(names(coords@parameters[1]) == X) {
      x <- 1
      y <- 2
    } else {
      x <- 2
      y <- 1
    }
    axisXLim <- par("usr")[2]
    axisYLim <- par("usr")[4]
    if (typ != "Histogram") {
      gateHist(coords, x, y, axisXLim, show)
      if (is(coords,"rectangleGate")) {
        rect(xleft=ctm$xLeft, xright=ctm$xRight,
             ybottom=ctm$yBottom, ytop=ctm$yTop, lwd=3)
      }
    } else {
      segments(coords@min[[x]], axisYLim*0.75, coords@max[[x]],
               axisYLim*0.75, lwd=3)
      segments(coords@min[[x]], axisYLim*0.72, coords@min[[x]],
               axisYLim*0.78, lwd=3)
      segments(coords@max[[x]], axisYLim*0.72, coords@max[[x]],
               axisYLim*0.78, lwd=3)
    }
    popPaths <- gs_get_pop_paths(ctm$gs)
    popShortPaths <- gs_get_pop_paths(ctm$gs, path=1)
    subpop <- popPaths[which(popShortPaths == ctm$found[i])]
    gatedPopInfo <- gs_pop_get_count_fast(ctm$gs[ID],
                                          subpopulations=subpop)
    preFreq <- gatedPopInfo[,4][[1]]*100/gatedPopInfo[,5][[1]]
    freq <- paste0(sprintf("%.1f", preFreq), "%")
    gateLeg(ID, X, Y, typ, show, font, bg, freq, coords, x, i)
  }
}

# Support functions for plots
gateLeg <- function(ID, X, Y, typ, show, font, bg, freq, coords, x, i) {
  if(show == TRUE) {
    title <- ctm$found[i]
  } else {
    title <- NULL
  }
  col <- rgb(1,1,1, 0.6)
  axisYLim <- par("usr")[4]
  if(length(ctm$found) == 4) {
    if(bg == TRUE) {
      qXCoords <- c(0.2, 0.8, 0.8, 0.2)
      qYCoords <- c(1.1, 1.1, 0.5, 0.5)
    } else {
      if(names(coords@min)[1] == X) {
        qXCoords <- c(0.1, 0.9, 0.9, 0.1)
        qYCoords <- c(1.05, 1.05, 0.25, 0.25)
      } else {
        qXCoords <- c(0.9, 0.9, 0.1, 0.1)
        qYCoords <- c(0.25, 1.05, 1.05, 0.25)
      }
    }
    if(show == TRUE) {
      if(ctm$found[i] == paste0("Q1: ", X, "- ", Y, "+")
         || ctm$found[i] == paste0("Q2: ", X, "+ ", Y, "+")
         || ctm$found[i] == paste0("Q3: ", X, "+ ", Y, "-")
         || ctm$found[i] == paste0("Q4: ", X, "- ", Y, "-")) {
        title <- substring(ctm$found[i], 1, 2)
      }
    }
    legX <- par("usr")[2]*qXCoords[i]
    legY <- axisYLim*qYCoords[i]
    legend(legX, legY, title=title, legend=freq, cex=1+font/10, bg=col,
           box.lwd=0, x.intersp=-0.5, y.intersp=0.8, text.font=2,
           xjust=0.5, yjust=1.5)
  } else {
    if(typ != "Histogram") {
      legX <- (ctm$xLeft+ctm$xRight)/2
      legY <- (ctm$yBottom+ctm$yTop)/2
      legend(legX, legY, title=title, legend=freq, cex=1+font/10, bg=col,
             box.lwd=0, x.intersp=-0.5, y.intersp=0.8, text.font=2,
             xjust=0.5, yjust=ctm$yJust)
    } else {
      legX <- (coords@min[[x]]+coords@max[[x]])/2
      legY <- axisYLim*0.95
      legend(legX, legY, title=title, legend=freq, cex=1+font/10, bg=col,
             box.lwd=0, x.intersp=-0.5, y.intersp=0.8, text.font=2,
             xjust=0.5)
    }
  }
}

hideTools <- function() {
  hide("rectang", TRUE, "fade")
  hide("polygon", TRUE, "fade")
  hide("quad", TRUE, "fade")
  hide("interval", TRUE, "fade")
}

secStep <- function() {
  delay(500, shinyjs::show("gateOk", TRUE, "fade"))
  delay(500, shinyjs::show("gateCancel", TRUE, "fade"))
  delay(500, shinyjs::show("drawGateName", TRUE, "fade"))
  disable("gateOk")
}


# Ancestry
ancestryGen <- function(typ, previewSamp, bgPop) {
  par(mfrow=c(3, 5), mar=c(4,5,2,1) + 0.1, lwd=2)
  if(typ != "" && previewSamp != "") {
    ID <- match(previewSamp, ctm$sample_list)
    pop <- bgPop
    if(typ == "Backgating" && pop != "") {
      cs <- gs_pop_get_data(ctm$gs[ID], pop)
      ff<- cytoframe_to_flowFrame(cs[[1]])
      bgDF <- as.data.frame.array(ff@exprs)
    } else {
      bgDF <- NULL
    }
    popPaths <- gs_get_pop_paths(ctm$gs)
    gates <- vapply(popPaths[-1], function(x)
      list(gs_pop_get_gate(ctm$gs, x)[[1]]), list(1))
    gateChs <- vapply(gates, function(x)
      list(names(x@parameters)), list(1))
    seq1 <- seq(gateChs)[duplicated(gateChs)]
    seq2 <- seq(gateChs)[duplicated(gateChs, fromLast=TRUE)]
    duplicateIDs <- sort(unique(c(seq1, seq2)))
    data <- gs_pop_get_count_fast(ctm$gs[[1]])
    duplicateParents <- data[,"Parent"][[1]][duplicateIDs]
    if(length(duplicateIDs) > 0) {
      index <- duplicateIDs[duplicated(duplicateParents)]
      plotSeq <- seq(gateChs)[-index]
    } else {
      plotSeq <- seq(gateChs)
    }
    dat <- data.frame(x=numeric(0), y=numeric(0))
    acPlotGen(dat, plotSeq, gates, popPaths, typ, ID, bgDF, pop)
    enable("exportImageAncestry")
  } else {
    disable("bgPop")
  }
}

acPlotGen <- function(dat, plotSeq, gates, popPaths, typ, ID, bgDF, pop) {
  max <- length(plotSeq)/10
  withProgress(message="Generating", detail="plot 0", value=0, max=max, {
    for(i in (plotSeq+1)) {
      dat <- rbind(dat, data.frame(x=rnorm(1), y=rnorm(1)))
      detail <- paste("plot", which((plotSeq+1) == i))
      incProgress(0.1, detail=detail)
      Sys.sleep(0.1)
      gateInfo <- gates[popPaths[i]][[1]]
      xCh <- names(gateInfo@parameters[1])
      if (length(gateInfo@parameters) > 1) {
        yCh <- names(gateInfo@parameters[2])
        type <- typ
      } else {
        yCh <- "Histogram"
        type <- "Histogram"
      }
      shortPath <- gs_get_pop_paths(ctm$gs, path=1)
      index <- which(shortPath == gateInfo@filterId)
      gateFullName <- popPaths[index]
      if(i > 2) {
        stSp <- strsplit(gateFullName, split="/")
        end <- tail(stSp[[1]], 2)[1]
        i2 <- which(shortPath == end)
        parent <- popPaths[i2]
      } else {
        parent <- "root"
      }
      newPlot(ID, xCh, yCh, parent, type, TRUE, 2, bgDF, TRUE)
      detectGate(ID, xCh, yCh, parent, type, "plot", TRUE, 2, TRUE)
      if (parent == "root") {
        parent <- "ungated"
      } else {
        parent <- shortPath[which(popPaths == parent)]
      }
      if (typ == "Backgating" && pop != "") {
        if (!is.null(bgDF)) {
          if (parent == pop) {
            title(main=list(parent, cex=1.5, col="red"))
          } else {
            title(main=list(parent, cex=1.5, col="black"))
          }
        }
      } else {
        title(main=list(parent, cex=1.5, col="black"))
      }
    }
  })
}


# UI
ui <- fluidPage(
  useShinyjs(),
  
  # Sidebar layout
  sidebarPanel(
    id = "Plotleft",

    # Input: Select a file
    shinyFilesButton("fcs_files", "Choose .fcs files", "Please select files", multiple = TRUE),

    # Injection options
    actionButton(inputId = "injectOptMain", label = div(icon("bars"), "File import options")),
    checkboxInput(inputId = "transform_box", label = "Transform data", value = TRUE),
    numericInput(inputId = "transform_cofactor", label = "Cofactor (default is 5)", value = 5, min = 1, max = 500, step = 1),

    # Sample selection
    selectInput(inputId = "samp", label = div(icon("vial"), "Sample"), choices = c("")),


    # tags$style("#customizeAxisY {border-color:black}"),
    #     fluidRow(
    #       column(
    #         width = 8,
    #         selectInput(inputId = "Y",
    #                     label = div(img(src=dataURI(
    #                       file = system.file("icons", "Y.png",
    #                                        package = "BCyto"),
    #                       mime = "image/png"),
    #                       height = "15px"), "Y axis"),
    #                     choices = c(""))
    #       ),
    #       column(
    #         width = 4, style = "margin-top: 25px;",
    #         actionButton(inputId = "customizeAxisY", label = "Scale")
    #       )
    #     ),
    #     tags$style("#customizeAxisX {border-color:black}"),
    #     fluidRow(
    #       column(
    #         width = 8,
    #         selectInput(inputId = "X",
    #                     label = div(img(src=dataURI(
    #                       file = system.file("icons", "X.png",
    #                                        package = "BCyto"),
    #                       mime = "image/png"),
    #                       height = "15px"), "X axis"),
    #                     choices = c(""))
    #       ),
    #       column(
    #         width = 4, style = "margin-top: 25px;",
    #         actionButton(inputId = "customizeAxisX", label = "Scale")
    #       )
    #     ),

    # Plot type selection
    selectInput(inputId = "typ", label = div(icon("chart-area"), "Plot type"), choices = c("Pseudocolor", "Contour", "Density", "Pseudocolor + Contour")),

    # Plot options
    tags$style("#displayOptMain {border-color:black}"),
    actionButton(inputId = "displayOptMain", label = div(icon("bars"), "Display options")),
    numericInput(inputId = "density_res", label = "Density Resolution (10-100)", value = 50, min = 10, max = 100, step = 1),
    numericInput(inputId = "pointsize", label = "Point Size", value = 1, min = 0.1, max = 100, step = 0.1),
    numericInput(inputId = "resolution", label = "Plot Resolution", value = 600, min = 200, max = 3000, step = 1)
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(id = "Plotmiddle",
      width = 7,

      # Row with chevrons and gating tools
      fluidRow(
          # Sample selection chevrons
          column(
            width = 3,
            div(style = "display:inline-block; position:fixed",
                actionButton(inputId = "previousSample",
                             width = "40px",
                             icon("chevron-left"),
                             style = ctm$blackB),
                actionButton(inputId = "nextSample",
                             width = "40px",
                             icon("chevron-right"),
                             style = ctm$blackB))
          ),
          # Gating tool buttons
          column(
            width = 5, offset = 4, align = "left", id = "gatetools",
            div(style = "display:inline-block; position:fixed",
                actionButton(inputId = "rectang",
                             width = "40px",
                             img(src = dataURI(
                               file = system.file("icons",
                                                "Rect.png",
                                                package = "BCyto"),
                               mime = "image/png"),
                               height = "14px"),
                             style = ctm$blackB),
                actionButton(inputId = "polygon",
                             width = "40px",
                             img(src = dataURI(
                               file = system.file("icons",
                                                "Polygon.png",
                                                package = "BCyto"),
                               mime = "image/png"),
                               height = "14px"),
                             style = ctm$blackB),
                actionButton(inputId = "quad",
                             width = "40px",
                             img(src = dataURI(
                               file = system.file("icons",
                                                "Quad.png",
                                                package = "BCyto"),
                               mime = "image/png"),
                               height = "14px"),
                             style = ctm$blackB),
                actionButton(inputId = "interval",
                             width = "40px",
                             img(src = dataURI(
                               file = system.file("icons",
                                                "Interval.png",
                                                package = "BCyto"),
                               mime = "image/png"),
                               height = "7px"),
                             style = ctm$blackB),
            )
          ),
          # Something that seems to appear conditionally
          column(
            width = 5, offset = 3,
            div(style = "display:inline-block; position:fixed",
                textInput(inputId = "drawGateName", label = NULL,
                          placeholder = "Please draw a gate",
                          width = "180px"), )
          ),
          # Something that seems to appear conditionally
          column(
            width = 1, offset = 0,
            div(style = "display:inline-block; position:fixed",
                actionButton(inputId = "gateOk", label = "Ok",
                             style = ctm$blackB),
                actionButton(inputId = "gateCancel", label = "Cancel",
                             style = ctm$whiteB)
            )
          ),
        # Spacing
        br(),
        br(),
        # Selecting axes
        uiOutput("columnSelectUI"),
        # Plot UI
        uiOutput("plotUI"),
        # Saving Plot image
        tags$style("#saveMainPlot {border-color:black}"),
        div(style = "display:inline-block",
            downloadButton(outputId = "saveMainPlot",
                           label = "Export image")),
        # Spacing      
        div(style = "display:inline-block;vertical-align:top;
                    width: 20px;",
            HTML("<br>")),
        # Event counter
        div(style = "display:inline-block", textOutput("events")),
        width = 5
      )
      
  ),

  # Sidebar panel for gating hierarchy
  sidebarPanel(
        id = "Plotright",
        strong(div(icon("sitemap"), "Parent")),
        plotOutput("hierarchy", click = "hierarchy_click",
                   dblclick = "hierarchy_dblclick", height = 450),
        # Row with export image and edit gate buttons
        fluidRow(
          column(width = 5,
                 tags$style("#exportImageGates {border-color:black}"
                 ),
                 downloadButton(outputId = "exportImageGates",
                                label = "Export image"),
          ),
          column(width = 4, offset = 1,
                 tags$style("#editGate {border-color:black}"),
                 actionButton(inputId = "editGate",
                              label = "Edit", icon("fas fa-pen")),
          )
        )
      )

)


server <- function(input, output, session) {
  # Set up file system roots
  roots <- c(home = "~")

  #JS code to prevent special characters in gate names
  runjs("$('#drawGateName').attr('maxlength',18)")
  runjs("$('#drawGateName').bind('keypress', function(event) {
      var regex=new RegExp('^[/]+$');
      if(regex.test(String.fromCharCode(!event.charCode ? event.which:
      event.charCode))) {
          event.preventDefault()
      }
  })")
  

  # reactive value definition
  reactPar <- reactiveValues(d="root")
  reactNameChange <- reactiveValues(d=NULL)
  reactRectangle <- reactiveValues(d=FALSE)
  reactInterval <- reactiveValues(d=FALSE)
  reactPolygon <- reactiveValues(d=FALSE)
  reactQuadrant <- reactiveValues(d=FALSE)
  reactHover <- reactiveValues(d=0)
  hoverCoords <- reactiveValues(d=c(NULL))
  polygonCoords <- reactiveValues(d=data.frame(NULL))
  plotActivator <- reactiveValues(d=0)
  hierarchyActivator <- reactiveValues(d=0)
  bgActivator <- reactiveValues(d=0)
  reactGateFont <- reactiveValues(d=5)
  reactShowGateName <- reactiveValues(d=TRUE)
  reactAxisFont <- reactiveValues(d=15)
  reactShowAxis <- reactiveValues(d=TRUE)





  

    shinyFileChoose(input, "fcs_files", roots = roots, filetypes = c("fcs"))


    observe({
    req(input$fcs_files)
    
    ctm$file_paths <- parseFilePaths(roots, input$fcs_files)

    req(ctm$file_paths$datapath)
    
    ctm$gs <- inject_fcs_gating(ctm$file_paths$datapath,
                                 asinh_transform = input$transform_box,
                                 cofac = input$transform_cofactor)
    ctm$sample_list <- ctm$file_paths$name
    updateSelectInput(session, "samp", choices = ctm$sample_list)
      print(ctm$gs)
      print(input$samp)
      print(ctm$sample_list)
      ID <- match(input$samp, ctm$sample_list)
      print(ID)

      ff <- cytoframe_to_flowFrame(gs_pop_get_data(ctm$gs[ID], parent)[[1]])
      print(ff)
      print(head(ff@exprs))
  })
  


  # Dynamically generate column selection UI
  output$columnSelectUI <- renderUI({
    req(ctm$gs)
    column_names <- colnames(ctm$gs)
    tagList(
      div(style = "display: inline-block;",
        selectInput("xColumn", "Select X Axis:", choices = column_names, selected = column_names[1])
      ),
      div(style = "display: inline-block;",
        selectInput("yColumn", "Select Y Axis:", choices = column_names, selected = column_names[2])
      )
    )
  })


  # ({
  #   # Call shinyFiles to handle file selection
  #   shinyFileChoose(input, "fcs_files", roots = roots, filetypes = c("fcs"))
  #   # Get the selected file paths
  #   ctm$file_paths <- parseFilePaths(roots, input$fcs_files)
  #   req(ctm$file_paths)
    
  #   ctm$gs <- inject_fcs_gating(ctm$file_paths$datapath,
  #                                  asinh_transform = input$transform_box,
  #                                  cofac = input$transform_cofactor)

  #   ctm$sample_list <- ctm$file_paths$name
  #   return(cyto_data)
  # })

  #update axes if changes detected
  observeEvent(input$customizeAxisY, {
    reactAxisCustom$d <- input$Y
  })
  observeEvent(input$customizeAxisX, {
    reactAxisCustom$d <- input$X
  })

  # open plot settings dialogue when button clicked
  observeEvent(input$displayOptMain, {
    showModal(modalDialog(
      tags$style(HTML(paste0("[for=displayAxisFont]
            +span>.irs>.irs-single, [for=displayAxisFont]", ctm$sliderCol))),
      checkboxInput("displayAxis", label="Show axis titles",
                    value=reactShowAxis$d),
      sliderInput("displayAxisFont", label=NULL, min=10, max=25,
                  value=reactAxisFont$d, ticks=FALSE),
      br(),
      tags$style(HTML(paste0("[for=displayGateFont]
            +span>.irs>.irs-single, [for=displayGateFont]", ctm$sliderCol))),
      checkboxInput("displayGateName", label="Show gate name",
                    value=reactShowGateName$d),
      sliderInput("displayGateFont", label=NULL, min=1, max=30,
                  value=reactGateFont$d, ticks=FALSE),
      footer=list(
        actionButton(inputId="reloadplotmodal", label="Ok",
                     style=ctm$blackB),
        actionButton(inputId="cancelModal", label="Cancel",
                     style=ctm$whiteB)
      ),
      easyClose=TRUE,
      size="s"))
    if(length(gs_get_pop_paths(ctm$gs)) > 1) {
      shinyjs::show("displayGateName")
      shinyjs::show("displayGateFont")
    } else {
      hide("displayGateName")
      hide("displayGateFont")
    }
  })

  # reload plot with new settings
  observeEvent(input$reloadplotmodal, {
    plotActivator$d <- plotActivator$d + 1
    removeModal()
    reactShowAxis$d <- input$displayAxis
    reactAxisFont$d <- input$displayAxisFont
    reactShowGateName$d <- input$displayGateName
    reactGateFont$d <- input$displayGateFont
  })

# NOT ADAPTED YET
  observeEvent(input$nextSample, {
    current <- match(input$samp, ctm$sample_list)
    if(current + 1 <= length(ctm$sample_list)) {
      select <- ctm$sample_list[[current + 1]]
      updateSelectInput(inputId="samp", selected=select)
    }
  })
  observeEvent(input$previousSample, {
    current <- match(input$samp, ctm$sample_list)
    if(current > 1) {
      select <- ctm$sample_list[[current - 1]]
      updateSelectInput(inputId="samp", selected=select)
    }
  })

  observeEvent(c(input$samp, input$nextSample, input$previousSample), {
    current <- match(input$samp, ctm$sample_list)
    if(input$samp != "") {
      if(current >= length(ctm$sample_list)) {
        disable("nextSample")
      } else {
        enable("nextSample")
      }
      if(current <= 1) {
        disable("previousSample")
      } else {
        enable("previousSample")
      }
    }
  })

  observeEvent(c(input$samp, input$Y, input$X, reactPar$d, input$typ,
                 input$gateCancel), {
                   reactRectangle$d <- FALSE
                   reactInterval$d <- FALSE
                   reactPolygon$d <- FALSE
                   reactQuadrant$d <- FALSE
                 }
  )

  observeEvent(input$gateOk, {
    "%notin%" <- Negate("%in%")
    if(input$drawGateName %notin% gs_get_pop_paths(ctm$gs, path=1)) {
      if(reactRectangle$d == TRUE) {
        components <- c(input$main_brush$xmin,
                        input$main_brush$xmax,
                        input$main_brush$ymin,
                        input$main_brush$ymax)
        colnames <- c(input$X, input$Y)
        mat <- matrix(components, ncol=2,
                      dimnames=list(c("min", "max"), colnames))
        gate <- rectangleGate(.gate=mat)
      } else if(isolate(reactPolygon$d) == TRUE) {
        mat <- matrix(ncol=2, nrow=nrow(polygonCoords$d))
        for(i in seq(nrow(polygonCoords$d))) {
          mat[i,] <- c(polygonCoords$d$x[i], polygonCoords$d$y[i])
        }
        colnames(mat) <- c(input$X, input$Y)
        gate <- polygonGate(mat)
      } else if(reactInterval$d == TRUE) {
        components <- c(input$main_brush$xmin,
                        input$main_brush$xmax)
        mat <- matrix(components, ncol=1,
                      dimnames=list(c("min", "max"), c(input$X)))
        gate <- rectangleGate(.gate=mat)
      }
      par <- isolate(reactPar$d)
      gs_pop_add(ctm$gs, gate, parent=par, name=input$drawGateName)
      recompute(ctm$gs)
      updateTextInput(inputId="drawGateName", value="")
      reactRectangle$d <- FALSE
      reactInterval$d <- FALSE
      reactPolygon$d <- FALSE
    } else {
      alert("Please choose a different gate name.")
    }
  })

  output$plotUI <- renderUI({
    if(input$typ == "Histogram") {
      if(reactInterval$d == TRUE) {
        brushOpts <- brushOpts("main_brush", fill="lightgrey",
                               stroke="black", opacity=0.4, delay=10,
                               direction="x")
      } else {
        brushOpts <- NULL
      }
      hoverOpts <- NULL
    } else {
      if(reactRectangle$d == TRUE) {
        brushOpts <- brushOpts("main_brush", fill="lightgrey",
                               stroke="black", opacity=0.4, delay=10,
                               direction="xy")
        hoverOpts <- NULL
      } else {
        brushOpts <- NULL
        if(isolate(reactPolygon$d) == TRUE
           || reactQuadrant$d == TRUE) {
          hoverOpts <- hoverOpts(id="mainplot_hover", delay=100,
                                 delayType="debounce",
                                 nullOutside=TRUE)
        } else {
          hoverOpts <- NULL
        }
      }
    }
    plotOutput("mainplot", height=440, click="main_click",
               dblclick="main_dblclick", brush=brushOpts, hover=hoverOpts)
  })

  output$mainplot <- renderPlot({
    par(mar=c(4,6,1,1) + 0.1, lwd=2)
    polygonCoords$d
    input$gateOk
    input$gateCancel
    plotActivator$d
    updateSelectInput(inputId="editGate", label="Edit")
    disable("editGate")
    reactNameChange$d <- NULL
    session$resetBrush("main_brush")
    ID <- match(input$samp, ctm$sample_list)
    par <- isolate(reactPar$d)
    newPlot(ID, input$X, input$Y, par, isolate(input$typ),
            isolate(reactShowAxis$d), isolate(reactAxisFont$d))
    detectGate(ID, input$X, input$Y, par, isolate(input$typ),
               "plot", isolate(reactShowGateName$d),
               isolate(reactGateFont$d))
    if(isolate(reactPolygon$d) == TRUE) {
      click <- isolate({input$main_click})
      if(!is.null(click)) {
        if(abs(click$x) > 0 && abs(click$y) > 0) {
          coords <- polygonCoords$d
          for(i in seq(nrow(coords))) {
            if(i > 1) {
              segments(coords$x[i-1], coords$y[i-1],
                       coords$x[i], coords$y[i],
                       col="red", lwd=2)
            }
            points(coords$x[i], coords$y[i], pch=19, cex=2,
                   col="red")
          }
        }
      }
    } else if(reactQuadrant$d == TRUE) {
      abline(v=hoverCoords$d[1], h=hoverCoords$d[2], lwd=2, col="red")
    } else if(isolate(reactRectangle$d) == FALSE
              && isolate(reactInterval$d) == FALSE
              && isolate(reactPolygon$d) == FALSE
              && isolate(reactQuadrant$d) == FALSE) {
      hide("gateOk", TRUE, "fade")
      hide("gateCancel", TRUE, "fade")
      hide("drawGateName", TRUE, "fade")
      delay(500, shinyjs::show("rectang", TRUE, "fade"))
      delay(500, shinyjs::show("polygon", TRUE, "fade"))
      delay(500, shinyjs::show("quad", TRUE, "fade"))
      delay(500, shinyjs::show("interval", TRUE, "fade"))
      updateTextInput(inputId="drawGateName", value= "",
                      placeholder="Please draw a gate")
      disable("drawGateName")
      polygonCoords$d <- data.frame(NULL)
    }
    output$events <- renderText({
      counts <- gs_pop_get_stats(ctm$gs[input$samp])[,3]
      shownEv <- counts[which(gs_get_pop_paths(ctm$gs) == par)][[1]]
      totalEv <- counts[1][[1]]
      paste0("Plotted events: ",
             format(shownEv, big.mark=","), "/",
             format(totalEv, big.mark=","))
    })
    if(grepl("FS", input$X) || grepl("SS", input$X) || input$X == "Time") {
      disable("customizeAxisX")
    } else {
      enable("customizeAxisX")
    }
    if(grepl("FS", input$Y) || grepl("SS", input$Y) || input$Y == "Time"
       || isolate(input$typ) == "Histogram") {
      disable("customizeAxisY")
    } else {
      enable("customizeAxisY")
    }
  }, width=470)

  observeEvent(c(input$rectang, input$polygon, input$quad, input$interval), {
    if(!is.null(ctm$gs)) {
      detectGate(1, input$X, input$Y, reactPar$d,
                 input$typ, ">= 4", isolate(reactShowGateName$d),
                 isolate(reactGateFont$d))
      if(length(ctm$found) >= 4) {
        showModal(modalDialog(
          "Only 4 gates are allowed on a plot. Would you like
                    to remove the 4 existing gates to draw new ones?",
          footer=list(
            actionButton(inputId="deleteAllModal",
                         label="Delete existing gates",
                         style=ctm$blackB),
            actionButton(inputId="cancelModal", label="Cancel",
                         style=ctm$whiteB)
          ),
          easyClose=FALSE,
          size="s"))
      } else {
        hideTools()
      }
    }
  })

  observeEvent(input$rectang, {
    detectGate(1, input$X, input$Y, reactPar$d,
               input$typ, ">= 4", isolate(reactShowGateName$d),
               isolate(reactGateFont$d))
    if(length(ctm$found) < 4) {
      reactRectangle$d <- TRUE
      secStep()
    }
  })

  observeEvent(input$polygon, {
    detectGate(1, input$X, input$Y, reactPar$d,
               input$typ, ">= 4", isolate(reactShowGateName$d),
               isolate(reactGateFont$d))
    if(length(ctm$found) < 4) {
      reactPolygon$d <- TRUE
      secStep()
    }
  })

  observeEvent(input$quad, {
    detectGate(1, input$X, input$Y, reactPar$d,
               input$typ, ">= 4", isolate(reactShowGateName$d),
               isolate(reactGateFont$d))
    if(length(ctm$found) < 4) {
      reactQuadrant$d <- TRUE
      delay(500, shinyjs::show("gateCancel", TRUE, "fade"))
    }
  })

  observeEvent(input$interval, {
    reactInterval$d <- TRUE
    hideTools()
    secStep()
  })

  observeEvent(input$mainplot_hover, {
    hover <- input$mainplot_hover
    coords <- hoverCoords$d
    if(!is.null(coords[1])) {
      if(max(hover$x, coords[1])/min(hover$x, coords[1]) > 1.1
         || max(hover$y, coords[2])/min(hover$y, coords[2]) > 1.1) {
        reactHover$d <- reactHover$d + 1
        hoverCoords$d[1] <- hover$x
        hoverCoords$d[2] <- hover$y
      }
    } else {
      reactHover$d <- reactHover$d + 1
      hoverCoords$d[1] <- hover$x
      hoverCoords$d[2] <- hover$y
    }
  })

  observeEvent(input$main_click, {
    inputX <- input$main_click$x
    inputY <- input$main_click$y
    if(reactPolygon$d == TRUE) {
      coords <- polygonCoords$d
      if(!is.null(coords)) {
        if(nrow(coords) > 2) {
          maxX <- max(inputX, coords$x[1])
          minX <- min(inputX, coords$x[1])
          maxY <- max(inputY, coords$y[1])
          minY <- min(inputY, coords$y[1])
          if(grepl("FS", input$X) || grepl("SS", input$X)) {
            xVerifier <- maxX/minX < 2.5
          } else {
            xVerifier <- maxX/minX < 1.1
          }
          if(grepl("FS", input$Y) || grepl("SS", input$Y)) {
            yVerifier <- maxY/minY < 2.5
          } else {
            yVerifier <- maxY/minY < 1.1
          }
          if(xVerifier && yVerifier) {
            polygonCoords$d <- rbind(coords, c(coords$x[1],
                                               coords$y[1]))
            updateTextInput(inputId="drawGateName",
                            placeholder="Type gate name")
            enable("drawGateName")
          } else {
            nrowX <- coords$x[nrow(coords)]
            nrowY <- coords$y[nrow(coords)]
            if(nrowX != coords$x[1] && inputY != nrowY) {
              polygonCoords$d <- rbind(coords, c(inputX, inputY))
            }
          }
        } else {
          polygonCoords$d <- rbind(coords, c(inputX, inputY ))
          colnames(polygonCoords$d) <- c("x", "y")
        }
      }
    } else if(reactQuadrant$d == TRUE) {
      mat <- matrix(c(inputX, inputY), ncol=2,
                    dimnames=list(c("value"),
                                  c(input$X, input$Y)))
      gate <- quadGate(.gate=mat)
      names <- c(paste0("Q1: ", input$X, "- ", input$Y, "+"),
                 paste0("Q2: ", input$X, "+ ", input$Y, "+"),
                 paste0("Q3: ", input$X, "+ ", input$Y, "-"),
                 paste0("Q4: ", input$X, "- ", input$Y, "-"))
      gs_pop_add(ctm$gs, gate, parent=reactPar$d, names=names)
      recompute(ctm$gs)
      detectGate(1, input$X, input$Y, reactPar$d,
                 input$typ, "OK", isolate(reactShowGateName$d),
                 isolate(reactGateFont$d))
      updateTextInput(inputId="drawGateName", value="")
      hierarchyActivator$d <- isolate(hierarchyActivator$d) + 1
      reactQuadrant$d <- FALSE
    }
  })

  observeEvent(c(input$main_brush, input$main_click, input$drawGateName), {
    if(reactRectangle$d == TRUE || reactInterval$d == TRUE) {
      if(is.null(input$main_brush)) {
        disable("gateOk")
        updateTextInput(inputId="drawGateName", value="",
                        placeholder="Please draw a gate")
        disable("drawGateName")
      } else {
        if(input$drawGateName == "") {
          disable("gateOk")
          updateTextInput(inputId="drawGateName",
                          placeholder="Type gate name")
          enable("drawGateName")
        } else {
          enable("gateOk")
        }
      }
    }
    if(reactPolygon$d == TRUE) {
      if(input$drawGateName == "") {
        disable("gateOk")
      } else {
        enable("gateOk")
      }
    }
  })

  observeEvent(input$main_dblclick, {
    popPaths <- gs_get_pop_paths(ctm$gs)
    if(length(popPaths) > 1) {
      inputX <- input$main_dblclick$x
      inputY <- input$main_dblclick$y
      popGates <- vapply(popPaths[-1], function(x)
        list(gs_pop_get_gate(ctm$gs, x)[1][[1]]),
        list(length(popPaths[-1])))
      clickableGate <- list()
      for(i in seq(popGates)) {
        channels <- names(popGates[[i]]@parameters)
        if(length(channels) == 1) {
          channels[2] <- ctm$Y
        }
        popParent <- gs_pop_get_parent(ctm$gs, popGates[[i]]@filterId)
        if(input$typ != "Histogram") {
          if(channels[1] == input$X
             && channels[2] == input$Y
             && popParent == reactPar$d) {
            clickableGate[[i]] <- popGates[[i]]
          }
        } else {
          if(channels[1] == input$X
             && popParent == reactPar$d) {
            clickableGate[[i]] <- popGates[[i]]
          }
        }
      }
      updateParent <- FALSE
      singlePath <- gs_get_pop_paths(ctm$gs, path=1)
      clickableGate <- unlist(clickableGate)
      for(i in clickableGate) {
        index <- which(singlePath == i@filterId)
        if(is(i, "rectangleGate")) {
          if(length(names(i@parameters)) > 1) {
            if(inputX >= i@min[[1]]
               && inputX <= i@max[[1]]
               && inputY >= i@min[[2]]
               && inputY <= i@max[[2]]) {
              updateParent <- TRUE
              reactPar$d <- popPaths[index]
            }
          } else {
            if(inputX >= i@min[[1]]
               && inputX <= i@max[[1]]) {
              updateParent <- TRUE
              reactPar$d <- popPaths[index]
            }
          }
        }
        if(is(i, "polygonGate")) {
          if(inputX >= min(i@boundaries[,1])
             && inputX <= max(i@boundaries[,1])
             && inputY >= min(i@boundaries[,2])
             && inputY <= max(i@boundaries[,2])) {
            updateParent <- TRUE
            reactPar$d <- popPaths[index]
          }
        }
      }
      if(updateParent == TRUE) {
        if(reactPar$d != "root") {
          gate <- gs_pop_get_gate(ctm$gs, reactPar$d)
          channels <- names(gate[[1]]@parameters)
        }
        popParents <- gs_pop_get_count_fast(ctm$gs[1], "freq")[,3][[1]]
        index <- which(popParents == reactPar$d)
        gatesWithThisParent <- popGates[index]
        if(length(gatesWithThisParent) > 0) {
          channels <- names(gatesWithThisParent[[1]]@parameters)
        }
        updateSelectInput(inputId="X", selected=channels[1])
        if(length(channels) == 2) {
          if(input$typ == "Histogram") {
            updateSelectInput(inputId="typ",
                              selected="Pseudocolor")
          }
          updateSelectInput(inputId="Y", selected=channels[2])
          if(input$X == channels[1]
             && input$Y == channels[2]) {
            plotActivator$d <- plotActivator$d + 1
          }
        } else {
          if(input$typ != "Histogram") {
            updateSelectInput(inputId="Y", selected="")
            updateSelectInput(inputId="typ", selected="Histogram")
          }
          if(input$X == channels) {
            plotActivator$d <- plotActivator$d + 1
          }
        }
      }
    }
  })

  output$saveMainPlot <- downloadHandler(
    function() {
      paste0(substr(input$samp, 1, nchar(input$samp) - 4), ".png") },
    function(file) {
      png(file, units="in", height=6, width=6.44, res=300)
      par(mar=c(4,6,1,1) + 0.1, lwd=2)
      ID <- match(input$samp, ctm$sample_list)
      newPlot(ID, input$X, input$Y, reactPar$d,
              input$typ, isolate(reactShowAxis$d),
              isolate(reactAxisFont$d))
      detectGate(ID, input$X, input$Y, reactPar$d,
                 input$typ, "plot", isolate(reactShowGateName$d),
                 isolate(reactGateFont$d))
      dev.off()
    }
  )

  ##right----
  output$hierarchy <- renderPlot({
    input$gateOk
    hierarchyActivator$d
    input$deleteAllModal
    if(length(gs_get_pop_paths(ctm$gs)) > 1) {
      hPlot <- plot(ctm$gs)
      labels <- gs_get_pop_paths(ctm$gs, path=1)
      labels[1] <- "ungated"
      for(i in seq(labels)) {
        if(substr(labels[i], 1, 1) == "Q" && nchar(labels[i]) >= 13) {
          labels[i] <- substr(labels[i], 1, 2)
        }
      }
      names(labels) <- nodes(hPlot)
      nodeAttrs <- list(label=labels)
      attrs <- list(node=list(fillcolor="white", shape="box", width=1,
                              color="gray90", style="rounded"),
                    graph=list(rankdir="TB"))
      index <- which(gs_get_pop_paths(ctm$gs) == reactPar$d)
      if(is.null(reactNameChange$d)) {
        colour <- "black"
        names(colour) <- nodes(hPlot)[index]
      } else {
        if(reactPar$d == reactNameChange$d) {
          colour <- "red"
          names(colour) <- nodes(hPlot)[index]
        } else {
          colour <- c("black", "red")
          i2 <- which(gs_get_pop_paths(ctm$gs) == reactNameChange$d)
          names(colour) <- c(nodes(hPlot)[index], nodes(hPlot)[i2])
        }
      }
      nodeAttrs$color <- colour
      ctm$hPlot <- plot(hPlot, nodeAttrs=nodeAttrs, attrs=attrs)
      plot(hPlot, nodeAttrs=nodeAttrs, attrs=attrs)
      js$enableTab("ancestryTab")
      js$enableTab("overlayTab")
      js$enableTab("prolifTab")
      js$enableTab("tsneTab")
      js$enableTab("resultTab")
    } else {
      nodes <- c("ungated", "B")
      edgeL <- list(ungated="B", B="ungated")
      graphNEL <- new("graphNEL", nodes=nodes, edgemode="directed",
                      edgeL=edgeL)
      attrs <- list(node=list(fillcolor="white", shape="rectangle",
                              width=1))
      edgeAttrs <- list(color=c("ungated~B"="white"))
      nodeAttrs <- list(color=c("B"="white"), fontcolor=c("B"="white"))
      ctm$hPlot <- plot(Rgraphviz::agopen(graphNEL, "", attrs=attrs,
                              edgeAttrs=edgeAttrs, nodeAttrs=nodeAttrs))
    }
    pops <- gs_get_pop_paths(ctm$gs, path=1)[-1]
    if(ctm$loadingFile == FALSE) {
      updateSelectInput(inputId="bgPop", choices=c("", pops[-1]))
      updateSelectInput(inputId="ovP", choices=c("", pops))
    }
    updateSelectInput(inputId="tSPar", choices=c("", pops))
    updateSelectInput(inputId="rsParent", choices=c("", pops))
    ctm$loadingFile <- FALSE
  })

  observeEvent(input$hierarchy_dblclick, {
    popPaths <- gs_get_pop_paths(ctm$gs)
    if(length(popPaths) > 1) {
      agNode <- ctm$hPlot@AgNode
      xy <- c(seq_len(length(agNode)))
      dataF <- data.frame(x=xy, y=xy)
      for(i in seq(agNode)) {
        rownames(dataF)[i] <- popPaths[i]
        dataF$x[i] <- agNode[[i]]@center@x
        dataF$y[i] <- agNode[[i]]@center@y
        dataF$short[i] <- agNode[[i]]@txtLabel@labelText
      }
      selected <- nearPoints(dataF, input$hierarchy_dblclick,
                             xvar="x", yvar="y", threshold=25,
                             maxpoints=1, addDist=TRUE)
      if(length(rownames(selected)) > 0) {
        popGates <- vapply(popPaths[-1], function(x)
          list(gs_pop_get_gate(ctm$gs, x)[1][[1]]),
          list(length(popPaths[-1])))
        popParents <- gs_pop_get_count_fast(ctm$gs[1], "freq")[,3][[1]]
        checkingPop <- rownames(selected)
        if(checkingPop != reactPar$d) {
          reactPar$d <- checkingPop
          if(reactPar$d != "root") {
            gate <- gs_pop_get_gate(ctm$gs, reactPar$d)
            channels <- names(gate[[1]]@parameters)
          }
          index <- which(popParents == reactPar$d)
          gatesWithThisParent <- popGates[index]
          if(length(gatesWithThisParent) > 0) {
            channels <- names(gatesWithThisParent[[1]]@parameters)
          }
          updateSelectInput(inputId="X", selected=channels[1])
          if(length(channels) == 2) {
            if(input$typ == "Histogram") {
              updateSelectInput(inputId="typ",
                                selected="Pseudocolor")
            }
            updateSelectInput(inputId="Y", selected=channels[2])
            if(input$X == channels[1]
               && input$Y == channels[2]) {
              plotActivator$d <- plotActivator$d + 1
            }
          } else {
            if(input$typ != "Histogram") {
              updateSelectInput(inputId="Y",
                                selected="")
              updateSelectInput(inputId="typ",
                                selected="Histogram")
            }
            if(input$X == channels) {
              plotActivator$d <- plotActivator$d + 1
            }
          }
        }
      }
    }
  })

  observeEvent(input$hierarchy_click, {
    popPaths <- gs_get_pop_paths(ctm$gs)
    if(length(popPaths) > 1) {
      agNode <- ctm$hPlot@AgNode
      xy <- c(seq_len(length(agNode)))
      dataF <- data.frame(x=xy, y=xy)
      for(i in seq(agNode)) {
        rownames(dataF)[i] <- popPaths[i]
        dataF$x[i] <- agNode[[i]]@center@x
        dataF$y[i] <- agNode[[i]]@center@y
        dataF$short[i] <- agNode[[i]]@txtLabel@labelText
      }
      selected <- nearPoints(dataF, input$hierarchy_click,
                             xvar="x", yvar="y", threshold=25,
                             maxpoints=1, addDist=TRUE)
      if(length(rownames(selected)) > 0
         && rownames(selected) != "ungated") {
        reactNameChange$d <- rownames(selected)
        updateSelectInput(inputId="editGate",
                          label=HTML("<span style=
                                         'color: red'>Edit</span>"))
        enable("editGate")
      } else {
        updateSelectInput(inputId="editGate", label="Edit")
        disable("editGate")
        reactNameChange$d <- NULL
      }
    }
  })

  observeEvent(input$editGate, {
    index <- which(gs_get_pop_paths(ctm$gs) == reactNameChange$d)
    path <- gs_get_pop_paths(ctm$gs, path=1)[index]
    showModal(modalDialog(
      if(substr(path, 1, 1) != "Q" || substr(path, 3, 4) != ": ") {
        paste0("Please type a new name for gate '", path, "':")
      } else{
        "Quadrant gates can't have their names changed."
      },
      br(),
      br(),
      if(substr(path, 1, 1) != "Q" || substr(path, 3, 4) != ": ") {
        textInput(inputId="newNameTextBox", label=NULL,
                  width="180px")
      },
      footer=list(
        if(substr(path, 1, 1) != "Q" || substr(path, 3, 4) != ": ") {
          actionButton(inputId="changeNameModal", label="Ok",
                       style=ctm$blackB)
        },
        actionButton(inputId="cancelModal", label="Cancel",
                     style=ctm$whiteB),
        actionButton(inputId="deleteGate", label="Delete gate",
                     style=ctm$blackB)
      ),
      easyClose=TRUE,
      size="m"))
  })

  observeEvent(input$deleteGate, {
    rootPlotLoad <- FALSE
    popPaths <- gs_get_pop_paths(ctm$gs, path=1)
    index <- which(gs_get_pop_paths(ctm$gs) == reactNameChange$d)
    gatetoDelete <- popPaths[index]
    i2 <- which(gs_get_pop_paths(ctm$gs) == reactPar$d)
    showingParent <- popPaths[i2]
    popsAfter <- gh_pop_get_descendants(ctm$gs[[1]], showingParent, path=1)
    popsAfterIDs <- vapply(popsAfter, function(x)
      which(popPaths == x), integer(1))
    popsBefore <- popPaths[-popsAfterIDs]
    gs_pop_remove(ctm$gs, gatetoDelete)
    if(gatetoDelete == showingParent || gatetoDelete %in% popsBefore) {
      reactPar$d <- "root"
    }
    plotActivator$d <- plotActivator$d + 1
    hierarchyActivator$d <- isolate(hierarchyActivator$d) + 1
    removeModal()
    updateSelectInput(inputId="editGate",
                      label=HTML("<span style=
                                     'color: black'>Edit</span>"))
  })

  observeEvent(c(input$newNameTextBox, input$editGate), {
    if(!is.null(input$newNameTextBox)) {
      if(input$newNameTextBox == "") {
        disable("changeNameModal")
      } else {
        enable("changeNameModal")
      }
    }
  })

  observeEvent(input$changeNameModal, {
    popPaths <- gs_get_pop_paths(ctm$gs, path=1)
    if(input$newNameTextBox %in% popPaths) {
      alert("Please choose a different gate name.")
    } else {
      index <- which(gs_get_pop_paths(ctm$gs) == reactNameChange$d)
      gs_pop_set_name(ctm$gs, popPaths[index], input$newNameTextBox)
      removeModal()
      if(reactNameChange$d == reactPar$d) {
        reactPar$d <- paste0("/", input$newNameTextBox)
      }
      plotActivator$d <- plotActivator$d + 1
    }
  })

  observeEvent(input$deleteAllModal, {
    for(i in ctm$found) {
      gs_pop_remove(ctm$gs, i)
    }
    removeModal()
    plotActivator$d <- plotActivator$d + 1
  })

  observeEvent(input$cancelModal, {
    reactAxisCustom$d <- NULL
    removeModal()
  })

  output$exportImageGates <- downloadHandler(
    "Gate hierarchy.png",
    function(file) {
      png(file, units="in", height=6, width=6.44, res=300)
      par(mar=c(4,6,1,1) + 0.1, lwd=2)
      plot(ctm$hPlot)
      dev.off()
    }
  )
}



# Launch Shiny app ----
shinyApp(ui, server)
