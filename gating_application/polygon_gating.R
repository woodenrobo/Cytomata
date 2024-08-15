library(shiny)
library(leaflet)
library(leaflet.extras)
library(sp)

cities <- structure(list(AccentCity = c("Saint Petersburg", "Harare", "Qingdao", 
                                        "Addis Abeba", "Xian", "Anshan", "Rongcheng", "Kinshasa", "New York", 
                                        "Sydney", "Lubumbashi", "Douala", "Bayrut", "Luanda", "Ludhiana"
), Longitude = c(30.264167, 31.0447222, 120.371944, 38.749226, 
                 108.928611, 122.99, 116.364159, 15.3, -74.0063889, 151.205475, 
                 27.466667, 9.7, 35.5097222, 13.233174, 75.85), Latitude = c(59.894444, 
                                                                             -17.8177778, 36.098611, 9.024325, 34.258333, 41.123611, 23.528858, 
                                                                             -4.3, 40.7141667, -33.861481, -11.666667, 4.0502778, 33.8719444, 
                                                                             -8.836804, 30.9)), class = "data.frame", row.names = c(NA, -15L
                                                                             ), .Names = c("AccentCity", "Longitude", "Latitude"))

cities_coordinates <- SpatialPointsDataFrame(cities[,c("Longitude","Latitude")],cities)

ui <- fluidPage(
  leafletOutput("mymap"),
  textOutput("selected_cities")
)


server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(0,0,2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data=cities,lat=~Latitude,lng=~Longitude,label=~AccentCity) %>%
      addDrawToolbar(
        targetGroup='draw',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        circleOptions = TRUE)  %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE)) 
  })
  
  output$selected_cities <- renderText({
    #use the draw_stop event to detect when users finished drawing
    req(input$mymap_draw_stop)
    print(input$mymap_draw_new_feature)
    feature_type <- input$mymap_draw_new_feature$properties$feature_type
    
    if(feature_type %in% c("rectangle","polygon")) {
      
      #get the coordinates of the polygon
      polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
      
      #transform them to an sp Polygon
      drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
      
      #use over from the sp package to identify selected cities
      selected_cities <- cities_coordinates %over% SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
      
      #print the name of the cities
      cities[which(!is.na(selected_cities)),"AccentCity"]
    } else if(feature_type=="circle") {
      #get the coordinates of the center of the cirle
      center_coords <- matrix(c(input$mymap_draw_new_feature$geometry$coordinates[[1]],input$mymap_draw_new_feature$geometry$coordinates[[2]]),ncol=2)
      
      #calculate the distance of the cities to the center
      dist_to_center <- spDistsN1(cities_coordinates,center_coords,longlat=TRUE)
      
      #select the cities that are closer to the center than the radius of the circle
      cities[dist_to_center < input$mymap_draw_new_feature$properties$radius/1000,"AccentCity"]
    }
    
    
  })
  
}

shinyApp(ui, server)