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
