#-----------------------------------------------------
# Statistical Thinking - Assignment 1
# Archel Aguilar - 98081767
# Web scrapting of movie sales - from https://www.boxofficemojo.com/weekly/chart/?yr=2018&wk=31&p=.htm
#-----------------------------------------------------

install.packages("pacman")
library(pacman)

install.packages("httr")
library(httr)
# 

install.packages("XML")
library(XML)

install.packages("dplyr")
library(dplyr)


p_load(httr, rvest, XML, dplyr)

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
      filter(row_number()!=n())
    }
    ,
    error=function(e) return(NULL)
  )



#------------------
# make it a function.



getWeeklyBoxOffice = function(theYear, theWeek) {
  
  #if val < week(now())-2 then error
  if (theYear <= year(now()) && theWeek <= week(now())-2) {
  
    tryCatch(
      {  
        base_url = "https://www.boxofficemojo.com/weekly/chart/"
        query_params = list(yr=theYear, wk=theWeek)
        
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
         
        

df1 = getWeeklyBoxOffice("2018", "31")
df2 = getWeeklyBoxOffice("2018", "30")

View(df)

#can now use rbind to concatenate rows.
#write a function to get data from start date to now

install.packages("lubridate")
library(lubridate)

week(ymd("2014-03-16", "2014-03-17","2014-03-18", '2014-01-01'))


startDate = "2018-01-01"



getWeeklyBoxOfficeByDate = function(startDate) {
  
  countWeeks = floor(interval(startDate, now()) / duration(num=1, units="weeks"))
  
  weekDates = ymd(startDate) + weeks(x = seq.int(from = 0, to = countWeeks, by = 1))
  
  yearWeeks = data.frame(yr=year(weekDates), wk=week(weekDates))
  
  mydf = NULL
  fulldf = NULL
  for (x in 1:nrow(yearWeeks)) {
    myYear = yearWeeks[x,1]
    myWeek = yearWeeks[x,2]
    
    mydf = getWeeklyBoxOffice(myYear, myWeek)
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

getdf = getWeeklyBoxOfficeByDate("2018-07-19")



