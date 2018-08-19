#-----------------------------------------------------
# Statistical Thinking - Assignment 1
# Archel Aguilar - 98081767
# Web scrapting of movie sales - from https://www.boxofficemojo.com/weekly/chart/?yr=2018&wk=31&p=.htm
#-----------------------------------------------------

# install.packages("pacman")
# library(pacman)

install.packages("httr")
library(httr)

install.packages("rvest")
library(rvest)

install.packages("dplyr")
library(dplyr)

install.packages("magrittr")
library(magrittr)

install.packages("lubridate")
library(lubridate)



#p_load(httr, rvest, XML, dplyr)

getYear = "2018"
getWeek = "31"


base_url = "https://www.boxofficemojo.com/weekly/chart/"
query_params = list(yr=getYear, wk=getWeek)

resp = GET(url = base_url, query=query_params)

str(resp)

resp_html = content(resp)


#using rvest functions
install.packages("rvest")
library(rvest)

page_html = read_html(resp)

write_xml(page_html, file="resp_html.html")

title = page_html %>% 
  html_nodes("title") %>%
  html_text()


#need to install this to access the list using extract2 function
install.packages("magrittr")
library(magrittr)


myColNames = c("ThisWeek", "LastWeek", "Title", "Studio", "WeeklyGross", "PctChange", "TheatreCount", "TheatreChange", "Average", "TotalGross", "Budget", "WeekNum")


weeklyBoxOffice = page_html %>%
  html_nodes("table") %>%
  extract2(5) %>% 
  html_table() %>%
  setNames(myColNames) %>%
  filter(row_number()!=1) %>% 
  filter(row_number()!=n()) %>%
  mutate(calYear=getYear, calWeek=getWeek)


#test errors

  tryCatch(
    {
      page_html %>%
        html_nodes("table") %>%
        extract2(5) %>% 
        html_table() %>%
        setNames(myColNames) %>%
        filter(row_number()!=1) %>% 
        filter(row_number()!=n()) %>%
        mutate(calYear=getYear, calWeek=getWeek)
    }
    ,
    error=function(e) return(NULL)
  )



#------------------
# make it a function.

getWeeklyBoxOffice = function(theYear, theWeek, priceAdj) {
  
  if (theYear <= year(now()) && theWeek <= week(now())-2) {
  
    tryCatch(
      {  
        base_url = "https://www.boxofficemojo.com/weekly/chart/"
        query_params = list(yr=theYear, wk=theWeek, adjust_yr=priceAdj)
        
        myResp = GET(url = base_url, query=query_params)
        
        myPage_html = read_html(myResp)
        
        myColNames = c("ThisWeek", "LastWeek", "Title", "Studio", "WeeklyGross", "PctChange", "TheatreCount", "TheatreChange", "Average", "TotalGross", "Budget", "WeekNum")
        
          myWeeklyBoxOffice = myPage_html %>%
          html_nodes("table") %>%
          extract2(5) %>%
          html_table() %>%
          setNames(myColNames) %>%
          filter(row_number()!=1) %>% 
          filter(row_number()!=n()) %>%
          mutate(calYear=theYear, calWeek=theWeek)
        
          return(myWeeklyBoxOffice)
      },
          error=function(e) return(NULL)
    )
  }
  else {
    return(NULL) 
  }
}
         
        

df1 = getWeeklyBoxOffice("2018", "31", "2018")
df2 = getWeeklyBoxOffice("2018", "30", "2018")

View(df)


#---------------------------------------------------------
#can now use rbind to concatenate rows.
#write a function to get data from start date to now

install.packages("lubridate")
library(lubridate)

# week(ymd("2014-03-16", "2014-03-17","2014-03-18", '2014-01-01'))
# 
# startDate = "2018-01-01"


getWeeklyBoxOfficeByDate = function(startDate, priceAdj) {
  
  countWeeks = floor(interval(startDate, now()) / duration(num=1, units="weeks"))
  
  weekDates = ymd(startDate) + weeks(x = seq.int(from = 0, to = countWeeks, by = 1))
  
  yearWeeks = data.frame(yr=year(weekDates), wk=week(weekDates))
  
  mydf = NULL
  fulldf = NULL
  for (x in 1:nrow(yearWeeks)) {
    myYear = yearWeeks[x,1]
    myWeek = yearWeeks[x,2]
    
    #wait 2 sec before next request to avoid spamming
    pause(2)
    
    mydf = getWeeklyBoxOffice(myYear, myWeek, priceAdj)
    
    if(!is.null(mydf)) {
      if(!is.null(fulldf)) {
        
        fulldf = rbind(fulldf, mydf)  
        
      } else {
        
        fulldf = mydf
        
      }
      
    }
  }
  return(fulldf)  

}

getdf = getWeeklyBoxOfficeByDate("2015-01-01", "2018")

write.csv(getdf, file="WeeklyBoxOfficeResults2015-2018.csv")

#---------------------------------------------------------

#---------------------------------
# Access OMDB API for movie ratings
# eg. http://www.omdbapi.com/?t=ex+machina
# KEY: 
#Here is your key: f635a606
#Please append it to all of your API requests,
#
#OMDb API: http://www.omdbapi.com/?i=tt3896198&apikey=f635a606

#---------------------------------

apiKey = "f635a606"
getTitle = "Ghost in the shell" 


omdb_url = "http://www.omdbapi.com/"
omdb_params = list(t=getTitle, apikey=apiKey)

oresp = GET(url = omdb_url, query=omdb_params)

str(oresp)

http_type(oresp)
#[1] "application/json"


# Examine returned text with content()
content(oresp, as="text")

# Parse response with content()
content(oresp, as="parsed")

# Parse returned text with fromJSON()
install.packages("jsonlite")
library(jsonlite)
movieList = fromJSON(content(oresp, as="text"))

#fromJSON(content(oresp, as="text"), simplifyDataFrame = TRUE)
str(movieList$Ratings)

install.packages("rlist")
library(rlist)

list.select(movieList, Website)

install.packages("tibble")
library(tibble)

moviedf = enframe(movieList)
View(moviedf)

install.packages("tidyr")
library(tidyr)

#pivot the table
movieResult = moviedf %>%
    spread(name, value)

View(movieResult)

getMovieRatings = function (gTitle) {
  
  tryCatch(
    {
      #my API Key
      apiKey = "f635a606"
      
      f_omdb_url = "http://www.omdbapi.com/"
      f_omdb_params = list(t=gTitle, apikey=apiKey)
      
      f_oresp = GET(url = f_omdb_url, query=f_omdb_params) 
      
      f_movieList = fromJSON(content(f_oresp, as="text"))
      
      f_moviedf = enframe(f_movieList)
      
      #pivot the table
      f_movieResult = f_moviedf %>%
        spread(name, value)
      
      return(f_movieResult)
      
    },
    error=function(e) return(NULL)
  )  
    
}

x = getMovieRatings("Ghost in the Shell")


#view titles and sum weekly gross (strip $ and commas)
moviesbyTitle = getdf %>%
  group_by(Title, Studio) %>%
  summarize(calWeeklyGross = sum(as.numeric(gsub("[\\$,]", "", WeeklyGross))),
            totTheatreCount = sum(as.numeric(gsub("[\\$,]", "", TheatreCount))),
            totGross = max(as.numeric(gsub("[\\$,]", "", TotalGross))),
            totBudget = max(as.numeric(gsub("[\\$,]", "", Budget))* 100000),
            WeeksOn = max(WeekNum)
            )

write.csv(moviesbyTitle, "MoviesByTitle.csv")



