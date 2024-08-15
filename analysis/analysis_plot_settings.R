#increase the size of all labels, text and legends
figure_object_scaling <- as.numeric(settings$value[settings$setting == "figure_object_scaling"])
figure_text_scaling <- as.numeric(settings$value[settings$setting == "figure_text_scaling"])





#COLOR PALETTE SETTINGS
automatic_palette <- as.numeric(settings$value[settings$setting == "automatic_palette"])


if (automatic_palette < 1) {
    #create a list of custom palletes for each grouping column here
    custom_palette <- list(c("small_mouse" = "red", "big_mouse" = "blue"), c("small_mouse" = "red", "big_mouse" = "blue"), c("small_mouse" = "red", "big_mouse" = "blue"))
}

#DEFAULT THEMES PLACEHOLDER
