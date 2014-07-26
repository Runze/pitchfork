library(shiny)

shinyUI(fluidPage(theme='bootstrap.min.css',
   tags$style(type='text/css',
              'label {font-size: 12px;}',
              '.recalculating {opacity: 1.0;}'
   ),
  
  titlePanel('Most Recent Tour Announcement on Pitchfork'),
  
  sidebarPanel(
    #initialize all the input to 'Loading...' until data are updated (see server.R)
    selectInput('city',
                label = 'Select a city (see result on the city tab)',
                choices = 'Loading...',
                selected = 'Loading...'),
    
    selectInput('artist', 
                label = 'Select an artist (see result on the artist tab)',
                choices = 'Loading...',
                selected = 'Loading...'),
    
    h6(a('www.runzemc.com', href='http://www.runzemc.com'))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel('City', htmlOutput('header1'), dataTableOutput('city_table')),
      tabPanel('Artist', htmlOutput('header2'), dataTableOutput('artist_table'))
    ),
    p('* Data are automatically updated from pitchfork per each run of the app.')
  )
))
