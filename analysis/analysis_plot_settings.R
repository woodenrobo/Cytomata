#increase the size of all labels, text and legends
figure_object_scaling <- as.numeric(settings$value[settings$setting == "figure_object_scaling"])
figure_text_scaling <- as.numeric(settings$value[settings$setting == "figure_text_scaling"])





#COLOR PALETTE SETTINGS
automatic_palette <- as.numeric(settings$value[settings$setting == "automatic_palette"])


if (automatic_palette < 1) {
    #create a list of custom palletes FOR EACH GROUPING COLUMN

    if (grepl("anca", project_name, ignore.case = TRUE)) {
        # ANCA
        custom_palette <- list(
            c("HC" = "#27317d", "MPO" = "#a14c06", "PR3" = "#b31f1f"),
            c("HC" = "#27317d", "ANCA" = "#a93f15")
        )
    } else if (grepl("iacs", project_name, ignore.case = TRUE)) {
        # IACS
        custom_palette <- list(
            c("HC" = "#27317d", "S1.1" = "#914100"),
            c("yes" = "#008562", "no" = "#850b00"),
            c("S1.1" = "#914100", "S1.2" = "#697c00"),
            c("HC" = "#27317d", "S1.1" = "#914100", "S1.2" = "#697c00")
        )
    } else if (grepl("iahp", project_name, ignore.case = TRUE)) {
        # IAHP
        custom_palette <- list(
            c("HC" = "#27317d", "S1.1" = "#914100", "S1.2" = "#697c00"),
            c("HC" = "#27317d", "S1.1" = "#914100"),
            c("HC" = "#27317d", "S1.2" = "#697c00"),
            c("S1.1" = "#914100", "S1.2" = "#697c00")
        )
    } else if (grepl("dc10", project_name, ignore.case = TRUE)) {
        # DC10
        custom_palette <- list(
            c(
                "DC_0h" = "#00008B",      # Dark blue
                "DC_0_5h" = "#0000CD",    # Medium blue
                "DC_4h" = "#4169E1",      # Royal blue
                "DC_24h" = "#6495ED",     # Cornflower blue
                
                "DC_LPS_0h" = "#006400",    # Dark green
                "DC_LPS_0_5h" = "#228B22",  # Forest green
                "DC_LPS_4h" = "#32CD32",    # Lime green
                "DC_LPS_24h" = "#90EE90",   # Light green
                
                "DC10_0h" = "#8B4513",     # Saddle brown
                "DC10_0_5h" = "#CD853F",   # Peru
                "DC10_4h" = "#ddae72",     # Burlywood
                "DC10_24h" = "#fad18b",    # Moccasin
                
                "DC10_LPS_0h" = "#8B0000",   # Dark red
                "DC10_LPS_0_5h" = "#B22222", # Firebrick
                "DC10_LPS_4h" = "#CD5C5C",   # Indian red
                "DC10_LPS_24h" = "#f06c6c"   # Light coral
            ),
            c(
                "DC_0h" = "#00008B",      # Dark blue
                "DC10_0h" = "#8B4513"     # Saddle brown
            ),
            c(
                "DC_0_5h" = "#0000CD",    # Medium blue
                "DC10_0_5h" = "#CD853F"   # Peru
            ),
            c(
                "DC_4h" = "#4169E1",      # Royal blue
                "DC10_4h" = "#ddae72"     # Burlywood
            ),
            c(
                "DC_24h" = "#6495ED",     # Cornflower blue
                "DC10_24h" = "#fad18b"    # Moccasin
            ),
            c(
                "DC_LPS_0h" = "#006400",    # Dark green
                "DC10_LPS_0h" = "#8B0000"   # Dark red
            ),
            c(
                "DC_LPS_0_5h" = "#228B22",  # Forest green
                "DC10_LPS_0_5h" = "#B22222" # Firebrick
            ),
            c(
                "DC_LPS_4h" = "#32CD32",    # Lime green
                "DC10_LPS_4h" = "#CD5C5C"   # Indian red
            ),
            c(
                "DC_LPS_24h" = "#90EE90",   # Light green
                "DC10_LPS_24h" = "#f06c6c"   # Light coral
            )
        )
    } else {
        # Default palette if no specific project is matched
        stop("No custom palette defined for the project. Please check the project name or define a custom palette.")
    }
    
}
#DEFAULT THEMES PLACEHOLDER
