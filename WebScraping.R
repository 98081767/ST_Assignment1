#-----------------------------------------------------
# Statistical Thinking - Assignment 1
# Archel Aguilar - 98081767
# Web scrapting of movie sales - from https://www.boxofficemojo.com/weekly/chart/?yr=2018&wk=31&p=.htm
#-----------------------------------------------------

install.packages("pacman")
library(pacman)

# install.packages("httr")
# library(httr)
# 


p_load(httr, rvest, XML, dplyr)

getYear = "2018"
getWeek = "31"


base_url = "https://www.boxofficemojo.com/weekly/chart/"
query_params = list(yr=getYear, wk=getWeek)

resp = GET(url = base_url, query=query_params)

str(resp)

resp_html = content(resp)


#using rvest functions
# install.packages("rvest")
# library(rvest)

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
  filter(row_number()!=n())


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
        filter(row_number()!=n()) 
      
        return(myWeeklyBoxOffice)
    },
        error=function(e) return(NULL)
  )
}
         
        

df = getWeeklyBoxOffice("2018", "31")



