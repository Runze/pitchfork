library(shiny)
library(RCurl)
library(XML)
library(stringr)
library(lubridate)

#note this global file is not very necessary for my case here as it just contains a web crawl function
#i could've just included it in the server file but i didn't want it to be too long
web_crawl = function() {
  tours = data.frame(artist = character(), tour = character())
  
  for (i in 1:2) {
    #only crawl the first 2 pages as they cover most, if not all, of the upcoming shows
    link = paste0('http://pitchfork.com/news/tours/', i)
    url = getURL(link, followlocation = T)
    
    #find the part that includes the tours info (readHTMLTable does not work here as it is not formatted as a table)
    loc_1 = str_locate(url, '<li class="first "> <h1>')
    loc_2 = str_locate(url, '<div class="pagination">')
    url_sub = substr(url, loc_1[2]-3, loc_2[1]-2)
    url_list = strsplit(url_sub, '<h1>')
    
    #extract the relevant data from the identified block
    for (j in 1:length(url_list[[1]])) {
      if (url_list[[1]][j] != '' & is.na(str_locate(url_list[[1]][j], 'Artists:')[1])) {
        s = url_list[[1]][j]
        
        #check if the current string includes tour-related information
        loc_3 = str_locate(s, 'Announce|Extend|Add|Plot|Expand|Join|Tour') #key words used to indicate tour annoucement
        if (!is.na(loc_3[1])) {
          #extract artist information
          artist = substr(s, 1, loc_3[1]-2)
          
          #extract tour information
          s = strsplit(s, '<br.*?>')
          s = lapply(s, function(x) gsub('<.*?>', '', x))
          
          #find the rows that start with numbers (indicating dates)
          d = grep('^[0-9]{2}-[0-9]{2}', s[[1]])
          tour = s[[1]][d]
          
          #combine artist and tour information and append to the data.frame
          df = data.frame(cbind(rep(artist, length(tour)), tour))
          names(df) = c('artist', 'tour')
          tours = rbind(tours, df)
        }
      }
    }
  }
  
  #extract and process dates
  event_dt = substr(tours$tour, 1, 5)
  split_dt = unlist(strsplit(event_dt, '-'))
  event_mm = as.numeric(split_dt[2 * (1:length(event_dt)) - 1])
  event_dd = as.numeric(split_dt[2 * (1:length(event_dt))])
  
  #because the dates provided on the website do not have year in it, the following rule is implemented to interpret years
  #get current year
  event_yy = rep(year(today()), length(event_dt))
  #assume that if the announced tour date predates the current date by 6 months, the tour occurs in the following year
  event_yy[event_mm - month(today()) <= -6] = event_yy[event_mm - month(today()) <= -6] + 1
  
  #compare with today's date to show only upcoming tours
  predate =  event_yy*10000 + event_mm*100 + event_dd <
    year(today())*10000 + month(today())*100 + day(today())
  
  #put it together
  #formatC is used to pad 0 in front of single digits
  #note i have to process all the dates as characters this way because there seems to be a bug in data.table
  #that once deployed, shows dates incorrectly
  event_mm = formatC(event_mm, width = 2, format = 'd', flag = '0')
  event_dd = formatC(event_dd, width = 2, format = 'd', flag = '0')
  event_dt = paste(event_yy, event_mm, event_dd, sep = '-')
  
  tours$event_dt = event_dt
  tours = subset(tours, predate == F)
  
  #now that date is extracted, remove date from the tour
  loc_4 = str_locate(tours$tour, '[[:alpha:]]|/.')
  tours$tour = substr(tours$tour, loc_4[, 1], nchar(as.character(tours$tour)))
  
  #extract location info
  loc_5 = str_locate(tours$tour, '-|–')
  tours$city = as.character(substr(tours$tour, 1, loc_5[, 1]-1))
  
  #standardize city names to the extent possible
  #the following cleaning was performed at the time the app was created
  #hopefully there won't be too many new inconsistencies in the future...
  tours$city = gsub('The Netherlands', 'Netherlands', tours$city)
  tours$city = gsub('Holland', 'Netherlands', tours$city)
  tours$city = gsub('George, Washington', 'George, WA', tours$city)
  tours$city = gsub('Glasgow, England', 'Glasgow, Scotland', tours$city)
  tours$city = gsub('Grafenhainichen, Germany', 'Gräfenhainichen, Germany', tours$city)
  tours$city = gsub('Hamburg$', 'Hamburg, Germany', tours$city)
  tours$city = gsub('Liège, Belgium', 'Liege, Belgium', tours$city)
  tours$city = gsub('Holland', 'Netherlands', tours$city)
  tours$city = gsub('New York City, NY', 'New York, NY', tours$city)
  tours$city = gsub('Toronto, ON', 'Toronto, Ontario', tours$city)
  tours$city = gsub('Switzerand', 'Switzerland', tours$city)
  tours$city = gsub('Tonsberg, Norway', 'Tønsberg, Norway', tours$city)
  tours$city = gsub('Engalnd', 'England', tours$city)
  tours$city = gsub('[^[:alnum:]]', ' ', tours$city)
  
  #extract venue
  tours$venue = as.character(substr(tours$tour, loc_5[, 1]+1, nchar(as.character(tours$tour))))
  tours$venue = gsub('[^[:alnum:]]', ' ', tours$venue)
  
  #clean up the table
  tours = subset(tours, select = -tour)
  
  #function to remove leading, trailing, and excessive white spaces
  clean_space = function(x) {
    x = gsub('^ +', '', x)
    x = gsub(' +$', '', x)
    x = gsub(' +', ' ', x)
    return(x)
  }
  
  tours$artist = clean_space(tours$artist)
  tours$city = clean_space(tours$city)
  tours$venue = clean_space(tours$venue)
  
  #attach link to songkick's artist page
  sk = 'http://www.songkick.com/search?utf8=%E2%9C%93&type=initial&query='
  artist = gsub(' ', '+', tours$artist)
  sk_link = paste0(sk, artist, '&commit=')
  tours$link = paste0("<a href='",  sk_link, "' target='_blank'>Link</a>") 
  
  return(tours)
}