library(shiny)

shinyServer(function(input, output, session) {
  #reactive function used to update the data every time it is run
  tours_input = reactive({ 
    web_crawl()
  })
  
  #observe function used to wait until data are downloaded to update the select input
  observe({
    #get a list of unique artists and cities to provide input options
    unique_artist = as.character(sort(unique(tours_input()$artist)))
    unique_city = as.character(sort(unique(tours_input()$city)))
    
    #set default city to los angeles whenever it is included (because i live here)
    la = grep('los angeles', unique_city, ignore.case = T)
    if (la > 0) {
      default_city = unique_city[la]
    } else {
      default_city = unique_city[1]
    }
    
    #based on the newly-obtained data, update the input options
    updateSelectInput(session, 'city', choices = unique_city, selected = default_city)
    updateSelectInput(session, 'artist', choices = unique_artist, selected = unique_artist[1])
  })
  
  output$header1 = renderUI({
    h1 = paste('Upcoming Shows in', input$city, '*', sep = ' ')
    HTML('<h4>', h1, '<h4>')
  })
  
  output$header2 = renderUI({
    h2 = paste('Upcoming Shows Announced by', input$artist, '*', sep = ' ')
    HTML('<h4>', h2, '<h4>')
  })
  
  #create table to show all the upcoming shows for a specific city
  subset_data_city = function() {
    if (input$city != 'Loading...') {
      dat = subset(tours_input(), city == input$city, select = -city)
      dat = dat[order(dat$event_dt, dat$artist), ]
      names(dat) = c('Artist', 'Event Date', 'Venue', 'Find More on Songkick')
      
      return(dat)
    }
  }
  
  output$city_table = renderDataTable(subset_data_city(), options = list(bFilter = F, iDisplayLength = 10, bAutoWidth = F))
  
  #create table to show all the upcoming shows for a specific artist
  subset_data_artist = function() {
    if (input$artist != 'Loading...') {
      dat = subset(tours_input(), artist == input$artist, select = -artist)
      dat = dat[order(dat$event_dt), ]
      names(dat) = c('Event Date', 'City', 'Venue', 'Find More on Songkick')
      
      return(dat)
    }
  }
  
  output$artist_table = renderDataTable(subset_data_artist(), options = list(bFilter = F, iDisplayLength = 10, bAutoWidth = F))
})