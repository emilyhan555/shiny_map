server <- function(input,output){
        
     dat <- reactive({
       
       data = accident1.df
       
       if (input$x == "FALSE") {
         data <- data[data$Fatality == 'FALSE',]
       } else {
         data <- data[data$Fatality == 'TRUE',]
       }
       
       if (input$y == 'FALSE') {
         data <- data[data$Hit.And.Run == 'FALSE',]
       } else {
         data <- data[data$Hit.And.Run == 'TRUE',]
       }
       
       if (input$z) {
         data <- data[data$Date == input$z,]
       }
       
     }) 
    
    
    output$AccidentMap <- renderLeaflet({
      df <- dat()
      map <- leaflet(data = df) %>% addTiles() %>%
        addMarkers(lng = ~Longitude, 
                   lat = ~Latitude,
                   clusterOptions = markerClusterOptions(),
                   popup = as.character(accident1.df$Street.Name)) 
      map
    })
    
  
    output$AccidentTable <- DT::renderDataTable({
      req(input$x, input$y, input$z)
      data = dat()[,c(1,2,4,7,8,11,12)]
     
      DT::datatable(data, 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    })
    
}
