rm(list = ls())
cat("\014")

## loading packages
{
  library(shiny)
  library(shinydashboard)
  library(htmltools)
  library(rvest)
  library(XML)
  library(measurements)
  library(ggplot2)
  library(ggrepel)
  library(plotly)
  library(tableHTML)
  library(shinycssloaders)
  library(shinyWidgets)
  library(dplyr)
  library(ggpubr)
  library(tidyverse)
  library(lubridate)
  library(dplyr)
}

# reading csv file
akris_data <- read.csv("Dataset.csv") 
summary(akris_data)

akris_data$Value[1:100] <- rep(20, 100)

# for getting top three value boxes
value_by_medium <- aggregate(akris_data$Value, by=list(akris_data$Medium), FUN=sum)
colnames(value_by_medium) <- c("Medium", "Value")

# for getting mean value by location bar chart
agg = aggregate(akris_data$Value,
                by = list(akris_data$Geographical.segments),
                FUN = mean)
colnames(agg) <- c("Location", "MeanValue")
agg$MeanValue <- round(agg$MeanValue, 0)
agg$MeanValue <- prettyNum(agg$MeanValue, 
                           big.mark = ",")

# for getting total value by year
akris_data$Year <- substr(akris_data$Date, 1,4)
value_by_year <- aggregate(akris_data$Value, by=list(akris_data$Year), FUN=sum)
colnames(value_by_year) <- c("Year", "Value")

# for getting total value by month
akris_data$Month <- substr(akris_data$Date, 6,7)
value_by_month <- aggregate(akris_data$Value, by=list(akris_data$Month), FUN=mean)
colnames(value_by_month) <- c("Month", "MeanValue")


# ui
ui <- dashboardPage(
  dashboardHeader(title = span("", 
                               span("Data Manager", 
                                    style = "color: white; font-size: 42px;font-weight: bold")), 
                  titleWidth = 1500),
  
  dashboardSidebar(
    
    collapsed = TRUE
    
  ),
  
  dashboardBody(
    
    # for getting top three value boxes i.e total value by each medium
    fluidRow(withSpinner(
      valueBoxOutput("progressBox2")),
      valueBoxOutput("approvalBox2"),
      valueBoxOutput("progressBox")
    ),
    
    # user will choose the year and get the corresponding total value and single highest achieved value for that year
    fluidRow(box(title = "Select Year:", width = 4 ,background = "blue",
                 height = 115,  
                 solidHeader = TRUE,status = "info",
                 selectInput(
                   inputId =  "year", 
                   label = "", 
                   choices = value_by_year$Year
                 )),
             
             box(title = "Total Value Achieved for Selected Year:",background = "blue", width = 4, 
                 height = 115, solidHeader = TRUE,
                 status = "info",
                 textOutput("info_box")),
             
             box(title = "Maximum Value Achieved for Selected Year:",background = "blue", width = 4, 
                 height = 115, solidHeader = TRUE,
                 status = "info",
                 textOutput("info_box1"))
             
    ),
    
    # for adjusting font size and color of info boxes
    tags$style("#info_box {font-size:40px;
               color:white;
               display:block;
               font-weight: bold;}"),
    
    tags$style("#info_box1 {font-size:40px;
               color:white;
               display:block;
               font-weight: bold;}"),
    
    
   # for plotting mean value achieved by each location
    fluidRow(box(title = "Mean Value by Location:", 
                 width = 4, plotlyOutput("bar"), 
                 solidHeader = TRUE, 
                 status = "info"),
             
             # for getting mean value achieved in each month
             box(title = "Mean Value by Month:", 
                 width = 8,plotlyOutput("distribution"),
                 solidHeader = TRUE, 
                 status = "info")
    )
    
  )
)

#server 
server <- function(input, output) {
 
  # total value achieved by email
  output$progressBox2 <- renderValueBox({
    valueBox(
      paste0(prettyNum(value_by_medium$Value[value_by_medium$Medium == "email"], big.mark = ",")), 
      "Total Value by Email", icon = icon("thumbs-up"),
      color = "blue"
    )
  })
  
  # total value achieved by referral
  output$approvalBox2 <- renderValueBox({
    valueBox(
      paste0(prettyNum(value_by_medium$Value[value_by_medium$Medium == "referral"], big.mark = ",")), 
      "Total Value by Referral", icon = icon("thumbs-up"),
      color = "blue"
    )
  })
  
  # total value achieved by automated_email
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(prettyNum(value_by_medium$Value[value_by_medium$Medium == "automated_email"], big.mark = ",")), 
      "Total Value by Automated Email", icon = icon("thumbs-down"),
      color = "blue"
    )
  })
  

  # total value achieved for year selected by user
  output$info_box <- renderText(paste0( " ",
                                        value = paste0(prettyNum(value_by_year$Value[value_by_year$Year == input$year], 
                                                                 big.mark = ",")) 
  ))
  
  # highest value achieved for year selected by user
  output$info_box1 <- renderText(paste0( " ",
                                         value = paste0(prettyNum(max(akris_data$Value[akris_data$Year == input$year]), 
                                                                  big.mark = ",")) 
  ))
  
  
  
  # plotting bar graph of overall mean value achieved at each location
  output$bar <- renderPlotly({
    
    ggplot(data=agg, aes(x=Location, y=MeanValue)) +
      geom_bar(stat="identity",width = 0.8, fill = "dodgerblue3")+
      ggtitle(label = "")+
      theme_bw() +
      theme(
        axis.line = element_line(colour = "dodgerblue4"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        title = element_text(face = "bold", color = "dodgerblue4"),
        axis.title = element_text(face = "bold", color = "dodgerblue4"),
        text=element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) +
      guides(fill=FALSE) 
    
  })
  
  # plotting bar graph of overall mean value achieved in each month
  output$distribution <- renderPlotly({
    
    ggplot(data=value_by_month, aes(x=Month, y=MeanValue)) +
      geom_bar(stat="identity",width = 0.8, fill = "dodgerblue4")+
      ggtitle(label = "")+
      theme_bw() +
      theme(
        axis.line = element_line(colour = "dodgerblue4"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        title = element_text(face = "bold", color = "dodgerblue4"),
        axis.title = element_text(face = "bold", color = "dodgerblue4"),
        text=element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) +
      guides(fill=FALSE) 
    
  })
  
}

shinyApp(ui, server)
