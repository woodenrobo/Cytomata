#increase the size of all labels, text and legends
figure_object_scaling <- as.numeric(settings$value[settings$setting == "figure_object_scaling"])
figure_text_scaling <- as.numeric(settings$value[settings$setting == "figure_text_scaling"])





#COLOR PALETTE SETTINGS
automatic_palette <- as.numeric(settings$value[settings$setting == "automatic_palette"])


if (automatic_palette < 1) {
    #create a list of custom palletes FOR EACH GROUPING COLUMN
    # ANCA
    # custom_palette <- list(c("HC" = "#27317d", "MPO" = "#a14c06", "PR3" = "#b31f1f"), c("HC" = "#27317d", "ANCA" = "#a93f15"))
    # NKSG IACS
    # custom_palette <- list(c("HC" = "#27317d", "S1.1" = "#914100"),
    # c("yes" = "#008562", "no" = "#850b00"),
    # c("S1.1" = "#914100", "S1.2" = "#697c00"),
    # c("HC" = "#27317d", "S1.1" = "#914100", "S1.2" = "#697c00")
    # )
    # TOMI DC10

    custom_palette <- list(
        # for grouping_var = "group"
        c(
            "DC_0h" = "#00008B",      # Dark blue
            "DC_0.5h" = "#0000CD",    # Medium blue
            "DC_4h" = "#4169E1",      # Royal blue
            "DC_24h" = "#6495ED",     # Cornflower blue
            
            "DC+LPS_0h" = "#006400",    # Dark green
            "DC+LPS_0.5h" = "#228B22",  # Forest green
            "DC+LPS_4h" = "#32CD32",    # Lime green
            "DC+LPS_24h" = "#90EE90",   # Light green
            
            "DC-10_0h" = "#8B4513",     # Saddle brown
            "DC-10_0.5h" = "#CD853F",   # Peru
            "DC-10_4h" = "#ddae72",     # Burlywood
            "DC-10_24h" = "#fad18b",    # Moccasin
            
            "DC-10+LPS_0h" = "#8B0000",   # Dark red
            "DC-10+LPS_0.5h" = "#B22222", # Firebrick
            "DC-10+LPS_4h" = "#CD5C5C",   # Indian red
            "DC-10+LPS_24h" = "#f06c6c"   # Light coral
        ),
        # for grouping_var = "paired_0"
        c( 
            "DC_0h" = "#00008B",      # Dark blue

            "DC-10_0h" = "#8B4513"     # Saddle brown
        ),
        # for grouping_var = "paired_0_5"
        c( 
            "DC_0.5h" = "#0000CD",    # Medium blue

            "DC-10_0.5h" = "#CD853F"   # Peru
        ),
        # for grouping_var = "paired_4"
        c( 
            "DC_4h" = "#4169E1",      # Royal blue

            "DC-10_4h" = "#ddae72"     # Burlywood
        ),
        # for grouping_var = "paired_24"
        c( 
            "DC_24h" = "#6495ED",     # Cornflower blue

            "DC-10_24h" = "#fad18b"    # Moccasin
        ),
        # for grouping_var = "paired_0_LPS"
        c( 
            "DC+LPS_0h" = "#006400",    # Dark green

            "DC-10+LPS_0h" = "#8B0000"   # Dark red
        ),
        # for grouping_var = "paired_0_5_LPS"
        c( 
            "DC+LPS_0.5h" = "#228B22",  # Forest green

            "DC-10+LPS_0.5h" = "#B22222" # Firebrick
        ),
        # for grouping_var = "paired_4_LPS"
        c( 
            "DC+LPS_4h" = "#32CD32",    # Lime green

            "DC-10+LPS_4h" = "#CD5C5C"   # Indian red
        ),
        # for grouping_var = "paired_24_LPS"
        c( 
            "DC+LPS_24h" = "#90EE90",   # Light green

            "DC-10+LPS_24h" = "#f06c6c"   # Light coral
        )
    )
}
#DEFAULT THEMES PLACEHOLDER
