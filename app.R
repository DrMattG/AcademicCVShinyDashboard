# load the required packages
library(shiny)
require(shinydashboard)
library(tidyverse)
library(scholar)
library(timevis)
library(tm)
library(wordcloud)
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
  GS_id<-"aZ4E5I4AAAAJ&hl"
  profile<-get_profile(GS_id)
  cite.yr<-get_citation_history(GS_id)
  pubs<-get_publications(GS_id)
  
  
  
  pubstab<-pubs %>% 
    select(title, journal, number, year) %>% 
    mutate(year=round(year)) %>% 
    rename("Title"=title, "Journal"=journal, "Journal & Page numbers"=number, "Year"=year)
  
  #reactive data
  sortTable <- reactive({
    pubstab[do.call(order, pubstab[as.character(input$sort_on)]),]
  })
  
  predH<-predict_h_index(GS_id)
  
  WoSpapers <- read_delim("WoSpapers.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  # Define the folder where the text files are
  a <-Corpus(VectorSource(WoSpapers$AB))
  a
  # Preprocessing text
  a <- tm_map(a, removeNumbers) # Not necessary if numbers are important for you
  a <- tm_map(a, removePunctuation)
  a <- tm_map(a , stripWhitespace)
  a <- tm_map(a, tolower)
  # Stopwords are words such as "we" "the" "and" "so", etc. You can add your own words to the list
  a <- tm_map(a, removeWords, c(stopwords("english"), "can", "also", "may"))
  # a <- tm_map(a, stemDocument, language = "english") # You can also do steamming if you want
  
  # Computing the term document matrix
  tdm <- TermDocumentMatrix(a)
  
  # Transforming data for wordcloud
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing=TRUE)
  myNames <- names(v)
  d <- data.frame(word=myNames, freq=v)
  # Making and displaying the cloud
  #wordcloud(d$word, d$freq, min.freq=5,colors=brewer.pal(8, "Dark2"))
  
  timeline_dat <- data.frame(
    id      = 1:4,
    content = c("University of Pretoria","World Pheasant Association", "Newcastle University",
                "Norwegian Institute for Nature Research"),
    start   = c("2009-01-01","2011-04-01", "2012-12-01",
                "2019-01-04"),
    end     = c("2012-01-01","2012-11-01", "2018-12-31",NA)
  )
  
  
  
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
        breaks = c(2005,2006,2007,2008,2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))+
      theme_classic()
  })
  output$Words<-renderPlot({
    wordcloud(d$word, d$freq, scale = c(3,1),min.freq=3,colors=brewer.pal(8, "Dark2"))
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