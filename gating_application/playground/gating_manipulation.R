
  # Get the gate hierarchy
  ctm$pop_paths <- gs_get_pop_paths(ctm$gs)

  # GATE CREATION ################
  # active_parent <- "/CD3+ CD4+ T cells Polygon"

  add_gate <- function(gs = ctm$gs, gate, gate_name, active_parent) {
    fullname <- paste0(active_parent, "/", gate_name)
    gate_name_query <- paste0("^.*/", escape_all_special(fullname), "$")

    if (length(gate_name_query) > 1) {
      gate_name_query_collapsed <- paste0(gate_name_query, collapse = "|")
      if (sum(grepl(gate_name_query_collapsed, ctm$pop_paths, perl = TRUE) > 0)) {
        print("Gate already exists")
      } else {
        gs_pop_add(gs, gate, parent = active_parent, name = gate_name, validityCheck = FALSE)
        recompute(gs)
        ctm$pop_paths <- gs_get_pop_paths(gs)
      }
    } else {
      if (sum(grepl(gate_name_query, ctm$pop_paths, perl = TRUE) > 0)) {
        print("Gate already exists")
      } else {
        gs_pop_add(gs, gate, parent = active_parent, name = gate_name, validityCheck = FALSE)
        recompute(gs)
        ctm$pop_paths <- gs_get_pop_paths(gs)
      }
    }
  } 


  escape_all_special <- function(str) {
    gsub("([^[:alnum:][:space:]])", "\\\\\\1", str)
  }

  # Saving rectangle gate
  rectangle_coords <- data.frame("142Nd_CD19" = c(-0.8, 1), "115In_CD3" = c(3, 6), check.names = FALSE)
  gate <- rectangleGate(.gate = rectangle_coords)
  gate_name <- "CD3+ CD4+ T cells"

  add_gate(gs = ctm$gs, gate, gate_name, active_parent = "root")


  # Saving polygon gate
  polygon_coords <- data.frame("142Nd_CD19" = c(-0.8, -0.9, -0.8, 1, 2, 2, 1), "115In_CD3" = c(3, 4, 5, 6, 5, 3, 3), check.names = FALSE)
  gate <- polygonGate(.gate = polygon_coords)
  gate_name <- "CD3+ CD4+ T cells Polygon"

  add_gate(gs = ctm$gs, gate, gate_name, active_parent = "root")


  # Saving quadrant gate
  quadrant_coords <- data.frame("142Nd_CD19" = c(1.5), "115In_CD3" = c(3), check.names = FALSE)
  gate <- quadGate(.gate = quadrant_coords)
  gate_name <- c(paste0("Q1: ", "142Nd_CD19", "- ", "115In_CD3", "+"),
                 paste0("Q2: ", "142Nd_CD19", "+ ", "115In_CD3", "+"),
                 paste0("Q3: ", "142Nd_CD19", "+ ", "115In_CD3", "-"),
                 paste0("Q4: ", "142Nd_CD19", "- ", "115In_CD3", "-"))

  add_gate(gs = ctm$gs, gate, gate_name, active_parent = "root")

  # Saving interval gate
  interval_coords <- data.frame("142Nd_CD19" = c(1.5), check.names = FALSE)
  interval1 <- rbind(interval_coords, -Inf)
  interval2 <- rbind(interval_coords, Inf)
  gate1 <- rectangleGate(.gate = interval1)
  gate2 <- rectangleGate(.gate = interval2)
  gate_name1 <- paste0("142Nd_CD19", "-")
  gate_name2 <- paste0("142Nd_CD19", "+")

  add_gate(gs = ctm$gs, gate = gate1, gate_name = gate_name1, active_parent = "root")
  add_gate(gs = ctm$gs, gate = gate2, gate_name = gate_name2, active_parent = "root")





  # GATE INFO COLLECTION ################
  gate_collect_info <- function(gs = ctm$gs, pop_paths = ctm$pop_paths) {
    gates_info <- NULL
    sample_gates_info <- NULL
    for (sample in ctm$sample_ids) {
        if (length(pop_paths) > 1) {
            for (i in seq(pop_paths)[-1]) {
                gs_get_pop_paths(ctm$gs, path = 4)
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
    names(gates_info) <- sampleNames(gs[ctm$sample_ids])

    return(gates_info)

    ctm$gate_freqs <- gs_pop_get_count_fast(ctm$gs[ctm$sample_ids], "freq")
    ctm$gate_counts <- gs_pop_get_count_fast(ctm$gs[ctm$sample_ids], "count")
  }

  gates_info <- gate_collect_info(gs = ctm$gs, pop_paths = ctm$pop_paths)


  # GATE TREE PLOTTING ################

  #placeholder for the gate tree
  library(graph)

  hPlot <- plot(ctm$gs)
  labels <- gs_get_pop_paths(ctm$gs, path=1)

  names(labels) <- nodes(hPlot)
  nodeAttrs <- list(label=labels)
  attrs <- list(node=list(fillcolor="white", shape="box", width=1,
                          color="gray90", style="rounded"),
                graph=list(rankdir="TB"))
  index <- which(gs_get_pop_paths(ctm$gs) == "root")


  ctm$hPlot <- plot(hPlot, nodeAttrs=nodeAttrs, attrs=attrs)
  plot(hPlot, nodeAttrs=nodeAttrs, attrs=attrs)

    



  # GATE DETECTION ################

  x_axis = "142Nd_CD19"
  y_axis = "115In_CD3"

  detect_gate <- function(gates_info, x_axis, y_axis) {
    channel1_gates <- which(lapply(gates_info[[1]], function(x) x$axis1) %in% c(x_axis, y_axis))
    channel2_gates <- which(lapply(gates_info[[1]], function(x) x$axis2) %in% c(x_axis, y_axis))
    channel1_gates_na <- which(is.na(lapply(gates_info[[1]], function(x) x$axis1)))
    channel2_gates_na <- which(is.na(lapply(gates_info[[1]], function(x) x$axis2)))

    ctm$detected_gates_total <- union(channel1_gates, channel2_gates)

    ctm$detected_gates_biaxial <- intersect(channel1_gates, channel2_gates)
    ctm$detected_gates_mono <- intersect(detected_gates_total, union(channel1_gates_na, channel2_gates_na))

  }
  



  # GATE PLOTTING ################

  plot_gate <- function(gs = ctm$gs, gate_name, sample_id) {
    
  }


  # GATE UPDATES ################

  update_gate <- function(gs = ctm$gs, gate, gate_name, active_parent) {
    gate_name_query <- paste0("^.*/", escape_all_special(gate_name), "$")
    if (length(gate_name_query) > 1) {
      gate_name_query_collapsed <- paste0(gate_name_query, collapse = "|")
      if (sum(grepl(gate_name_query_collapsed, ctm$pop_paths, perl = TRUE)) == length(gate_name_query)) {
        gate_list <- c()
        for (sel_sample in ctm$sample_ids) {
          gate_list <- c(gate_list, gate)
        }
        names(gate_list) <- sampleNames(gs[ctm$sample_ids])
        for (g in seq(length(gate_name))) {
          gs_pop_set_gate(gs[ctm$sample_ids], gate_name[g], gate_list)
          recompute(gs[ctm$sample_ids], gate_name[g]) 
        }
      } else {
        print("Gate is not present in the hierarchy and will not be updated")
      }
    } else {
      if (sum(grepl(gate_name_query, ctm$pop_paths, perl = TRUE) > 0)) {
        gate_list <- c()
        for (sel_sample in ctm$sample_ids) {
          gate_list <- c(gate_list, gate)
        }
        names(gate_list) <- sampleNames(gs[ctm$sample_ids])

        gs_pop_set_gate(gs[ctm$sample_ids], gate_name, gate_list)
        recompute(gs[ctm$sample_ids], gate_name) 

      } else {
        print("Gate is not present in the hierarchy and will not be updated")
      }
    }
    
  }


  # Updating rectangle gate
  rectangle_coords <- data.frame("142Nd_CD19" = c(-1, 1.5), "115In_CD3" = c(3, 6.5), check.names = FALSE)
  gate <- rectangleGate(.gate = rectangle_coords)
  gate_name <- "CD3+ CD4+ T cells"
  
  update_gate(gs = ctm$gs, gate, gate_name, active_parent = "root")
  gates_info <- gate_collect_info(gs = ctm$gs, pop_paths = ctm$pop_paths)

  # Updating polygon gate
  polygon_coords <- data.frame("142Nd_CD19" = c(-0.7, -0.9, -0.8, 1, 2, 2, 1), "115In_CD3" = c(2, 4, 5, 6, 5, 3, 3), check.names = FALSE)
  gate <- polygonGate(.gate = polygon_coords)
  gate_name <- "CD3+ CD4+ T cells Polygon"

  update_gate(gs = ctm$gs, gate, gate_name, active_parent = "root")
  gates_info <- gate_collect_info(gs = ctm$gs, pop_paths = ctm$pop_paths)

  # Updating quadrant gate
  quadrant_coords <- data.frame("142Nd_CD19" = c(1.3), "115In_CD3" = c(2), check.names = FALSE)
  gate <- quadGate(.gate = quadrant_coords)
  gate_name <- c(paste0("Q1: ", "142Nd_CD19", "- ", "115In_CD3", "+"),
                 paste0("Q2: ", "142Nd_CD19", "+ ", "115In_CD3", "+"),
                 paste0("Q3: ", "142Nd_CD19", "+ ", "115In_CD3", "-"),
                 paste0("Q4: ", "142Nd_CD19", "- ", "115In_CD3", "-"))
  
  update_gate(gs = ctm$gs, gate, gate_name, active_parent = "root")
  gates_info <- gate_collect_info(gs = ctm$gs, pop_paths = ctm$pop_paths)


  # Updating interval gate
  interval_coords <- data.frame("142Nd_CD19" = c(1.3), check.names = FALSE)
  interval1 <- rbind(interval_coords, -Inf)
  interval2 <- rbind(interval_coords, Inf)
  gate1 <- rectangleGate(.gate = interval1)
  gate2 <- rectangleGate(.gate = interval2)
  gate_name1 <- paste0("142Nd_CD19", "-")
  gate_name2 <- paste0("142Nd_CD19", "+")

  update_gate(gs = ctm$gs, gate = gate1, gate_name = gate_name1, active_parent = "root")
  update_gate(gs = ctm$gs, gate = gate2, gate_name = gate_name2, active_parent = "root")
  gates_info <- gate_collect_info(gs = ctm$gs, pop_paths = ctm$pop_paths)


  # GATE ACTIVATION ################
  # rv_gates <- list(active_parent = "root")
  rv_gates$active_parent <- "CD3+ CD4+ T cells"



  # GATE RENAMING ################
  
  old_name <- "/CD3+ CD4+ T cells" #FULL PATH
  new_name <- "CD3+ CD4+ T cells rectangle" #NOT FULL PATH

  rename_gate <- function(gs = ctm$gs, gate_name, new_name) {
    tryCatch({
            gs_pop_set_name(gs, old_name, new_name)
        },
        error = function(e) {
        })

    ctm$pop_paths <- gs_get_pop_paths(ctm$gs)
  }

  rename_gate(gs = ctm$gs, old_name, new_name)
  gates_info <- gate_collect_info(gs = ctm$gs, pop_paths = ctm$pop_paths)



  # GATE DELETION ################

  selected_node <- "/CD3+ CD4+ T cells rectangle"

  deletion_type <- "single"
  deletion_type <- "branch"
  deletion_type <- "children"

  delete_gate <- function(gs = ctm$gs, selected_node = selected_node, type = deletion_type) {
    all_gates <- ctm$pop_paths
    for (sample in ctm$sample_ids) {
      children <- gh_pop_get_descendants(gs[[sample]], selected_node)
      if (type == "single") {
          gh_pop_remove(gs[[sample]], node = selected_node)
      } else if (type == "branch") {
          for (node in c(selected_node, children)) {
            gh_pop_remove(gs[[sample]], node = node)
          }
      } else if (type == "children") {
          for (node in children) {
            gh_pop_remove(gs[[sample]], node = node)
          }
      }
    }
   
    rv_gates$active_parent <- "root"
    recompute(gs) 
    ctm$pop_paths <- gs_get_pop_paths(ctm$gs)
  }


  delete_gate(gs = ctm$gs, selected_node = selected_node, type = deletion_type)
  gates_info <- gate_collect_info(gs = ctm$gs, pop_paths = ctm$pop_paths)



  


  # GATE COPY ################

  selected_node <- "root"

  copy_type <- "single"
  copy_type <- "branch"
  copy_type <- "children"

  target_parent <- "/CD3+ CD4+ T cells Polygon"

  copy_gate <- function(gs = ctm$gs, selected_node = selected_node, target_node = target_parent, type = copy_type) {
    all_gates <- ctm$pop_paths
    children <- gh_pop_get_descendants(gs[[1]], selected_node)
    children_types <- lapply(gates_info[[1]], function(x) x$type)

    if (type == "single") {
        gate <- gs_pop_get_gate(gs, selected_node)
        add_gate(gs, gate = gate, gate_name = selected_node, active_parent = target_node)

    } else if (type == "branch") {
        for (node in c(selected_node, children)) {
          gate <- gs_pop_get_gate(gs, node)
          add_gate(gs, gate = gate, gate_name = node, active_parent = target_node)
        }
    } else if (type == "children") {
        for (node in children) {
          gate <- gs_pop_get_gate(gs, node)
          add_gate(gs, gate = gate, gate_name = node, active_parent = target_node)
        }
    }
    recompute(gs)
    ctm$pop_paths <- gs_get_pop_paths(ctm$gs)
    gates_info <- gate_collect_info(gs = ctm$gs, pop_paths = ctm$pop_paths)

  }


  copy_gate(gs = ctm$gs, selected_node = selected_node, target_node = target_parent, type = copy_type)



  # # Saving rectangle gate
  # rectangle_coords <- data.frame("142Nd_CD19" = c(-0.8, 1), "115In_CD3" = c(3, 6), check.names = FALSE)
  # gate <- rectangleGate(.gate = rectangle_coords)
  # gate_name <- "CD3+ CD4+ T cells"

  # add_gate(gs = ctm$gs, gate, gate_name, active_parent = "root")


  # # Saving polygon gate
  # polygon_coords <- data.frame("142Nd_CD19" = c(-0.8, -0.9, -0.8, 1, 2, 2, 1), "115In_CD3" = c(3, 4, 5, 6, 5, 3, 3), check.names = FALSE)
  # gate <- polygonGate(.gate = polygon_coords)
  # gate_name <- "CD3+ CD4+ T cells Polygon"

  # add_gate(gs = ctm$gs, gate, gate_name, active_parent = "root")


  # # Saving quadrant gate
  # quadrant_coords <- data.frame("142Nd_CD19" = c(1.5), "115In_CD3" = c(3), check.names = FALSE)
  # gate <- quadGate(.gate = quadrant_coords)
  # gate_name <- c(paste0("Q1: ", "142Nd_CD19", "- ", "115In_CD3", "+"),
  #                paste0("Q2: ", "142Nd_CD19", "+ ", "115In_CD3", "+"),
  #                paste0("Q3: ", "142Nd_CD19", "+ ", "115In_CD3", "-"),
  #                paste0("Q4: ", "142Nd_CD19", "- ", "115In_CD3", "-"))

  # add_gate(gs = ctm$gs, gate, gate_name, active_parent = "root")

  # # Saving interval gate
  # interval_coords <- data.frame("142Nd_CD19" = c(1.5), check.names = FALSE)
  # interval1 <- rbind(interval_coords, -Inf)
  # interval2 <- rbind(interval_coords, Inf)
  # gate1 <- rectangleGate(.gate = interval1)
  # gate2 <- rectangleGate(.gate = interval2)
  # gate_name1 <- paste0("142Nd_CD19", "-")
  # gate_name2 <- paste0("142Nd_CD19", "+")

  # add_gate(gs = ctm$gs, gate = gate1, gate_name = gate_name1, active_parent = "root")
  # add_gate(gs = ctm$gs, gate = gate2, gate_name = gate_name2, active_parent = "root")
