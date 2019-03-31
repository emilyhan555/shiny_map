accident.df <- read.csv("Vehicle_Accident_Data.csv")
str(accident.df)

accident.df$Location <- gsub("[()]","",accident.df$Location)
library(stringr)
location.df <- str_split_fixed(accident.df$Location, ",", 2)
date <- str_split_fixed(accident.df$Crash.Date.Time," ",2)

accident.df$Longitude <- location.df[,2]
accident.df$Latitude <- location.df[,1]
accident.df$Date <- date[,1]
accident.df$Time <- date[,2]

names(accident.df)
accident1.df <- accident.df[,c(-2,-4,-6,-9,-11,-12,-15)]

accident1.df <- na.omit(accident1.df)
sapply(accident1.df, function(x){sum(is.na(x))})
accident1.df[accident1.df == "" | accident1.df == "0"] <- NA
accident1.df <- na.omit(accident1.df)
sapply(accident1.df, function(x){sum(is.na(x))})

accident1.df$Latitude <- as.numeric(accident1.df$Latitude)
accident1.df$Longitude <- as.numeric((accident1.df$Longitude))
accident1.df$Date <- as.Date(accident1.df$Date, format = "%m/%d/%Y")




library(shiny)
library(leaflet)
library(DT)
ui <- fluidPage(
    titlePanel("Vehicle Accident Map"),
    
    sidebarLayout(
      sidebarPanel(
        dateInput(inputId = "z",label = "Date",value = "10-10-2018",
                format = "mm-dd-yyyy", weekstart = "0", 
                startview = "month"),
        selectInput(inputId = "x",label = "Fatality",choices = unique(accident1.df$Fatality)),
        selectInput(inputId = "y",label = "Hit And Run", choices = unique(accident1.df$Hit.And.Run))
        ),
      
    mainPanel(
      leafletOutput(outputId = "AccidentMap", width = "100%", height = 400),
      br(),
      DT::dataTableOutput(outputId = 'AccidentTable')
      
      )
    )
)

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
      data = accident1.df[,c(1,2,4,7,8,11,12)]
      
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
      
      DT::datatable(data, 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    })
    
}

shinyApp(ui = ui, server = server)




