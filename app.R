#Source the datafile (updates it)
#source("Data_save.R")
# load the required packages
library(shiny)
require(shinydashboard)
library(tidyverse)
library(scholar)
library(timevis)
library(tm)
library(wordcloud)
library(rsconnect)
library(readr)
###########################

#Build shinyapp
#UI
ui <- dashboardPage(
  
  dashboardHeader(
    
    title = "Dr Matthew J Grainger",
    
    titleWidth = 300
    
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Academic CV", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("My website", icon = icon("send",lib='glyphicon'), 
               href = "https://uncertainecologist.netlify.com/")
    )
  ),
  
  dashboardBody(
    
    tabsetPanel(
      
      id = "tabs",
      
      tabPanel(
        
        title = "My academic CV",
        
        value = "page1",
        fluidRow(
          box("My timeline", timevisOutput("timeline"), width=12)
        ),
        fluidRow(
          infoBoxOutput("value1"),
          infoBoxOutput("value2"),
          infoBoxOutput("value3")
        ),
        fluidRow(
          box(
            title = "Citations over time"
            ,status = "primary"
            ,solidHeader = TRUE 
            ,collapsible = TRUE 
            ,plotOutput("cites", height = "650px")
          ),
          box(
            title="Wordcloud of Abstracts"
            ,status = "primary"
            ,solidHeader = TRUE 
            ,collapsible = TRUE
            ,plotOutput("Words", height = "650px")
          )
        ),
        fluidRow(
          box(title="My publications"
              ,status = "primary"
              ,solidHeader = TRUE 
              ,collapsible = TRUE
              ,selectInput("sort_on", 
                           "Choose variable to sort on",
                           choices = c("Title" = "Title",
                                       "Year" = "Year",
                                       "Journal"="Journal"))
              ,tableOutput('table')
          ),
          box(title="My predicted H-Index"
              ,status = "primary"
              ,solidHeader = TRUE 
              ,collapsible = TRUE
              ,plotOutput('predictH')))
      )
    )
  )
)


# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values of boxes
profile<-readRDS("Profile.RDS")
pubs<-readRDS("pubs.RDS")
predH<-readRDS("predH.RDS")
cite.yr<-readRDS("citeyr.RDS")

d<-readRDS("d.RDS")
timeline_dat<-readRDS("timeline_dat.RDS")
pubstab<-pubs %>% 
    select(title, journal, number, year) %>% 
    mutate(year=round(year)) %>% 
    rename("Title"=title, "Journal"=journal, "Journal & Page numbers"=number, "Year"=year)
  
#reactive data
sortTable <- reactive({
    pubstab[do.call(order, pubstab[as.character(input$sort_on)]),]
  })
  
  
  
  WoSpapers <- read_delim("WoSpapers.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  
  #predict_h_index(GS_id)
  
  output$timeline <- renderTimevis({
    timevis(timeline_dat)
  })
  #creating the valueBoxOutput content
  output$value1 <- renderInfoBox({
    infoBox("Affiliation:", profile$affiliation, 
            icon = icon("briefcase",lib='font-awesome')
            ,color = "purple")})
  output$value2 <- renderInfoBox({
    infoBox('Total citations:', profile$total_cites, 
            icon = icon("book-reader",lib='font-awesome')
            ,color = "purple")})
  output$value3 <- renderInfoBox({
    infoBox("H-Index:", profile$h_index, 
            icon = icon("hospital-symbol",lib='font-awesome')
            ,color = "purple")})
  
  #creating the plotOutput content
  
  output$cites <- renderPlot({
    ggplot(cite.yr, aes(year,cites)) + 
      geom_bar(stat='identity',fill=colors()[35])+
      ylab("number of citations")+
      xlab("Year")+
      scale_x_continuous(
        breaks = c(2005,2006,2007,2008,2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017,2018,2019,2020))+
      theme_classic()
  })
  output$Words<-renderPlot({
    wordcloud(d$word, d$freq, scale = c(3,1),min.freq=8,colors=brewer.pal(8, "Dark2"))
  } )
  output$table <- renderTable(sortTable(), digits = 0)
  output$predictH<-renderPlot({
    ggplot(predH, aes(years_ahead,h_index, colour="red")) + 
      geom_point()+
      geom_smooth()+
      ylab("Potential H index")+
      xlab("Years ahead")+
      theme_classic()+
      theme(legend.position = "none")
  })
  
  
}

#Launch the App
shinyApp(ui, server)