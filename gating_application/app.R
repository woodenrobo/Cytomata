
source(paste0(path_to_cytomata, "gating_application/", "ui.R"))
source(paste0(path_to_cytomata, "gating_application/", "server.R"))



# Launch Shiny app ----
shinyApp(ui, server)
