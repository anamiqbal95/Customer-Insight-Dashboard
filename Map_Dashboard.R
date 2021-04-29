rm(list = ls())
cat("\014")

## pre processing
{
hyp_latlon <- read.csv("hyp_latlon.csv")
hyp_airbus<-subset(hyp_latlon,hyp_latlon$Respondant=="Airbus")
hyp_boeing<-subset(hyp_latlon,hyp_latlon$Respondant=="Boeing")
hyp_boeing$Work.Package

unique(hyp_airbus$Work.Package)
hyp_airbus$Colour = NA
hyp_airbus$Colour[hyp_airbus$Work.Package == "Propulsion"] = "blue"
hyp_airbus$Colour[hyp_airbus$Work.Package == "Structures"] = "green"
hyp_airbus$Colour[hyp_airbus$Work.Package == "Materials"] = "lawngreen"
hyp_airbus$Colour[hyp_airbus$Work.Package == "Systems"] = "gray"

unique(hyp_boeing$Work.Package)
hyp_boeing$Colour = NA
hyp_boeing$Colour[hyp_boeing$Work.Package == "Propulsion"] = "blue"
hyp_boeing$Colour[hyp_boeing$Work.Package == "Structures"] = "green"
hyp_boeing$Colour[hyp_boeing$Work.Package == "Aerospace Support"] = "lawngreen"
hyp_boeing$Colour[hyp_boeing$Work.Package == "Systems"] = "gray"


## for hover text
labs <- lapply(seq(nrow(hyp_airbus)), function(i) {
  paste0("Supplier: ", hyp_airbus[i, "UK.Suppliers..names.cleaned."], '<p>',
         "Work Package: ", hyp_airbus[i, "Work.Package"] ) 
})

labs_1 <- lapply(seq(nrow(hyp_boeing)), function(i) {
  paste0("Supplier: ", hyp_boeing[i, "UK.Suppliers..names.cleaned."], '<p>',
         "Work Package: ", hyp_boeing[i, "Work.Package"] ) 
})
}

## dashboard
{
  library(shiny)
  library(shinydashboard)
  library( htmltools )
  library(rvest)
  library(XML)
  library(measurements)
  library(ggplot2)
  library(ggrepel)
  library(leaflet)
  library(maps)
  
  ui <- dashboardPage(
    
    dashboardHeader(title = span("", 
                                 span("Airbus & Boeing", 
                                      style = "color: white; font-size: 28px;font-weight: bold")), 
                    titleWidth = 250),
    
    dashboardSidebar(sidebarMenu(
      menuItem(
        "Maps", 
        tabName = "maps", 
        icon = icon("globe"),
        menuSubItem("Airbus", tabName = "airbus", icon = icon("map")),
        menuSubItem("Boeing", tabName = "boeing", icon = icon("map"))
      ))),
    
    dashboardBody(
      
      tags$style(type = "text/css", "#airbus_map {height: calc(85vh - 80px) !important;}"),
      tags$style(type = "text/css", "#boeing_map {height: calc(85vh - 80px) !important;}"),
      
      tabItems(
        tabItem(
          tabName = "airbus",
          box(
            title = "Airbus",
            collapsible = TRUE,
            width = "100%",
            height = "100%",
            leafletOutput("airbus_map")
          )
        ),
        tabItem(
          tabName = "boeing",
          box(
            title = "Boeing",
            collapsible = TRUE,
            width = "100%",
            height = "100%",
            leafletOutput("boeing_map")
          )
        )
      ))
    
  )
  
  server <- function(input, output) {
    
    output$airbus_map <- renderLeaflet({
      
      
      leaflet(hyp_airbus) %>% 
        
        addTiles()%>%
        
        addLegend("bottomright",values = hyp_airbus$Colours,
                  colors = unique(hyp_airbus$Colour),
                  title = "Work Package",
                  labels = unique(hyp_airbus$Work.Package),
                  opacity = 1)%>%
        
        setView(max(hyp_airbus$lon), max(hyp_airbus$lat), zoom = 30)%>%
        
        fitBounds(lng1 = min(hyp_airbus$lon),lat1 = min(hyp_airbus$lat),
                  lng2 = max(hyp_airbus$lon),lat2 = max(hyp_airbus$lat))%>%
        
        addCircleMarkers(lng = ~hyp_airbus$lon,
                         lat = ~hyp_airbus$lat,
                         radius = 15,
                         color = hyp_airbus$Colour,
                         label = lapply(labs, htmltools::HTML)
                           )
      
    })
    
    
    output$boeing_map <- renderLeaflet({
      
      leaflet(hyp_boeing$UK.Suppliers..names.cleaned.) %>% 
        
        addTiles()%>% 
        
        addLegend("bottomright",values = hyp_boeing$Colours,
                  colors = unique(hyp_boeing$Colour),
                  title = "Work Package",
                  labels = unique(hyp_boeing$Work.Package),
                  opacity = 1)%>%
        
        fitBounds(lng1 = min(hyp_boeing$lon),lat1 = min(hyp_boeing$lat),
                  lng2 = max(hyp_boeing$lon),lat2 = max(hyp_boeing$lat))%>%
        
        addCircleMarkers(lng = hyp_boeing$lon,
                         lat = hyp_boeing$lat,
                         radius = 15,
                         color = hyp_boeing$Colour,
                         label = lapply(labs_1, htmltools::HTML)
        )
      
      
      
      
    })
    
    
  }
  
  shinyApp(ui, server)
}
