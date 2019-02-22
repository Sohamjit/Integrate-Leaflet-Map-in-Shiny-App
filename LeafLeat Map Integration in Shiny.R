library(leaflet)
library(shiny)
library(data.table)

Famous_People = as.data.table(read.csv("https://gist.githubusercontent.com/Brian23ldu/4e2b0543760c167294acb7b5805e3623/raw/849dd0f4abc10cd9c069391693035b5f58658ad9/History_Famous_People.csv"))


Famous_Peopl_ARTS = Famous_People[Domain == "ARTS", ]
Famous_Peopl_INSTITUTIONS = Famous_People[Domain == "INSTITUTIONS", ]
Famous_Peopl_HUMANITIES = Famous_People[Domain == "HUMANITIES", ]
Famous_Peopl_BUSINESSLAW = Famous_People[Domain == "BUSINESS & LAW", ]
Famous_Peopl_SPORTS = Famous_People[Domain == "SPORTS", ]
Famous_Peopl_PUBLICFIGURE = Famous_People[Domain == "PUBLIC FIGURE", ]
Famous_Peopl_EXPLORATION = Famous_People[Domain == "EXPLORATION", ]
Famous_Peopl_SCIENCETECHNOLOGY = Famous_People[Domain == "SCIENCE & TECHNOLOGY", ]

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10 , left = 50,
              selectInput("var","Select the Measure", 
              choices = c("HPI" = 15 , "Average Page Views" = 13,
                          "Total Page Views" = 23 , "Page Views in English"=20 ,
                          "Page Views in Non English" = 21), selected = 20
  ))
)

server = server <- function(input, output) {
  
  output$map <- renderLeaflet({
    colm = as.numeric(input$var)
    leaflet(Famous_People) %>% # Add default OpenStreetMap map tiles
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      addCircleMarkers(data =Famous_Peopl_ARTS, lng=~LON, lat=~LAT, 
                       popup = ~paste(
                         "<h3 style = 'color : red'>Famous People Round the Globe</h3>","<b>Name : </b>",Name ,
                         "<br>","<b>Country : </b>",Country.Name ,
                         "<br>","<b>Occupation : </b>",Occupation ,
                         "<br>","<b>HPI  : </b>", HPI, sep = " "),
                       radius = ~(Famous_Peopl_ARTS[[colm]])/100000000 , group = "Arts", color = "green") %>%
      addCircleMarkers(data =Famous_Peopl_INSTITUTIONS, lng=~LON, lat=~LAT, 
                       popup = ~paste(
                         "<h3 style = 'color : red'>Famous People Round the Globe</h3>","<b>Name : </b>",Name ,
                         "<br>","<b>Country : </b>",Country.Name ,
                         "<br>","<b>Occupation : </b>",Occupation ,
                         "<br>","<b>HPI  : </b>", HPI, sep = " "),
                       radius = ~(Famous_Peopl_INSTITUTIONS[[colm]])/10000000, group = "Institutions" , color = "pink") %>%
      addCircleMarkers(data =Famous_Peopl_HUMANITIES, lng=~LON, lat=~LAT, 
                       popup = ~paste(
                         "<h3 style = 'color : red'>Famous People Round the Globe</h3>","<b>Name : </b>",Name ,
                         "<br>","<b>Country : </b>",Country.Name ,
                         "<br>","<b>Occupation : </b>",Occupation ,
                         "<br>","<b>HPI  : </b>", HPI, sep = " "),
                       radius = ~(Famous_Peopl_HUMANITIES[[colm]])/100000000 , group = "Humanities", , color = "yellow") %>%
      addCircleMarkers(data =Famous_Peopl_BUSINESSLAW, lng=~LON, lat=~LAT, 
                       popup = ~paste(
                         "<h3 style = 'color : red'>Famous People Round the Globe</h3>","<b>Name : </b>",Name ,
                         "<br>","<b>Country : </b>",Country.Name ,
                         "<br>","<b>Occupation : </b>",Occupation ,
                         "<br>","<b>HPI  : </b>", HPI, sep = " "),
                       radius = ~(Famous_Peopl_BUSINESSLAW[[colm]])/10000000, group = "Business & Law" , color = "red") %>%
      addCircleMarkers(data =Famous_Peopl_SPORTS, lng=~LON, lat=~LAT, 
                       popup = ~paste(
                         "<h3 style = 'color : red'>Famous People Round the Globe</h3>","<b>Name : </b>",Name ,
                         "<br>","<b>Country : </b>",Country.Name ,
                         "<br>","<b>Occupation : </b>",Occupation ,
                         "<br>","<b>HPI  : </b>", HPI, sep = " "),
                       radius = ~(Famous_Peopl_SPORTS[[colm]])/100000000 , group = "Sports", color = "black") %>%
      addCircleMarkers(data =Famous_Peopl_PUBLICFIGURE, lng=~LON, lat=~LAT, 
                       popup = ~paste(
                         "<h3 style = 'color : red'>Famous People Round the Globe</h3>","<b>Name : </b>",Name ,
                         "<br>","<b>Country : </b>",Country.Name ,
                         "<br>","<b>Occupation : </b>",Occupation ,
                         "<br>","<b>HPI  : </b>", HPI, sep = " "),
                       radius = ~(Famous_Peopl_PUBLICFIGURE[[colm]])/10000000, group = "Public Figure" , color = "violet") %>%
      addCircleMarkers(data =Famous_Peopl_EXPLORATION, lng=~LON, lat=~LAT, 
                       popup = ~paste(
                         "<h3 style = 'color : red'>Famous People Round the Globe</h3>","<b>Name : </b>",Name ,
                         "<br>","<b>Country : </b>",Country.Name ,
                         "<br>","<b>Occupation : </b>",Occupation ,
                         "<br>","<b>HPI  : </b>", HPI, sep = " "),
                       radius = ~(Famous_Peopl_EXPLORATION[[colm]])/100000000 , group = "Exploration", color = "turquoise") %>%
      addCircleMarkers(data =Famous_Peopl_SCIENCETECHNOLOGY, lng=~LON, lat=~LAT, 
                       popup = ~paste(
                         "<h3 style = 'color : red'>Famous People Round the Globe</h3>","<b>Name : </b>",Name ,
                         "<br>","<b>Country : </b>",Country.Name ,
                         "<br>","<b>Occupation : </b>",Occupation ,
                         "<br>","<b>HPI  : </b>", HPI, sep = " "),
                       radius = ~(Famous_Peopl_SCIENCETECHNOLOGY[[colm]])/10000000, group = "Science & Technology" , color = "brown") %>%
      addLayersControl( overlayGroups = c("Arts","Institutions", "Humanities ","Exploration", 
                                          "Science & Technology" , "Sports", "Business & Law" , "Public Figure"))
  } ) 
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  