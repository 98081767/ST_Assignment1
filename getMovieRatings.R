# ----------------------------------------
# Assignment 2 - Get Movie Ratings
#------------------------------------------

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

install.packages("jsonlite")
library(jsonlite)

install.packages("rlist")
library(rlist)

install.packages("tibble")
library(tibble)

install.packages("tidyr")
library(tidyr)


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

# apiKey = "f635a606"
# getTitle = "Life (2017)" 
# 
# 
# omdb_url = "http://www.omdbapi.com/"
# omdb_params = list(t=getTitle, apikey=apiKey)
# 
# oresp = GET(url = omdb_url, query=omdb_params)
# 
# str(oresp)
# 
# http_type(oresp)
# #[1] "application/json"
# 
# 
# # Examine returned text with content()
# content(oresp, as="text")
# 
# # Parse response with content()
# content(oresp, as="parsed")
# 
# # Parse returned text with fromJSON()
# install.packages("jsonlite")
# library(jsonlite)
# movieList = fromJSON(content(oresp, as="text"))
# 
# #fromJSON(content(oresp, as="text"), simplifyDataFrame = TRUE)
# str(movieList$imdbID)
# 
# movieList$Title
# 
# rtscore = movieList$Ratings %>%
#   filter(Source == "Rotten Tomatoes") %>%
#   select(Value) %>%
#   mutate_all(funs(as.numeric(gsub("[\\%]", "", .)))) %>%
#   .$Value
# 
# 
# movieDF = NULL
# 
# movieDF$Title = movieList$Title
# movieDF$Year = movieList$Year
# movieDF$Rated = movieList$Rated
# movieDF$Released = movieList$Released
# movieDF$Runtime = movieList$Runtime
# movieDF$Genre = movieList$Genre
# movieDF$Director = movieList$Director
# movieDF$Writer = movieList$Writer
# movieDF$Actors = movieList$Actors
# movieDF$Plot = movieList$Plot
# movieDF$Language = movieList$Language
# movieDF$Country = movieList$Country
# movieDF$Awards = movieList$Awards
# movieDF$Poster = movieList$Poster
# if (!is.null(movieList$imdbRating)) {
#   movieDF$IMDBRating = as.numeric(movieList$imdbRating) * 10
# } else {
#   movieDF$IMDBRating = NA
# }
# if (!is.null(movieList$Ratings)) {
#   movieDF$RTRating = movieList$Ratings %>%
#     filter(Source == "Rotten Tomatoes") %>%
#     select(Value) %>%
#     mutate_all(funs(as.numeric(gsub("[\\%]", "", .)))) %>%
#     .$Value
#   if (length(movieDF$RTRating)==0){
#     movieDF$RTRating = NA
#   }
#   
# } else {
#   movieDF$RTRating = NA
# }
# if (!is.null(movieList$Metascore)) {
#   movieDF$Metacritic = as.numeric(movieList$Metascore)
# } else {
#   movieDF$Metacritic = NA
# }
# if (!is.null(movieList$imdbVotes)) {
#   movieDF$IMDBVotes = as.numeric(gsub("[\\$,]", "", movieList$imdbVotes))
# } else {
#   movieDF$IMDBVotes = NA
# }
# movieDF$imdbID = movieList$imdbID
# movieDF$Type = movieList$Type
# movieDF$DVD = movieList$DVD
# movieDF$Production = movieList$Production
# movieDF$Website = movieList$Website
# 
# 
# movieDF = as.data.frame(movieDF)
# 
# list.select(movieList, Website)
# 
# moviedf = enframe(movieList)
# View(moviedf)
# 
# #pivot the table
# movieResult = moviedf %>%
#   spread(name, value)
# 
# View(movieResult)


#---------------GET MOVIE RATINGS FUNCTION------------------

getMovieRatings = function (gTitle) {
  
  #tryCatch(
  #  {
  #my API Key
  #NOTE: 1000 request limit per day
  apiKey = "f635a606"
  
  f_omdb_url = "http://www.omdbapi.com/"
  f_omdb_params = list(t=gTitle, apikey=apiKey)
  
  print(gTitle)
  
  #create dataframe
  f_movieDF = NULL

  f_oresp = GET(url = f_omdb_url, query=f_omdb_params) 
  
  if (http_type(f_oresp) == "application/json") {
  
      f_movieList = fromJSON(content(f_oresp, as="text"))
      
      if (f_movieList$Response == "True") {
        
        f_movieDF$Search = gTitle
        f_movieDF$Title = f_movieList$Title
        
        if(!is.null(f_movieDF$Year)) {
          f_movieDF$Year = f_movieList$Year
        } else {
          f_movieDF$Year = NA
        }
        
        if(!is.null(f_movieList$Rated)) {
          f_movieDF$Rated = f_movieList$Rated
        } else {
          f_movieDF$Rated = NA
        }
        
        if (!is.null(f_movieList$Released)) {
          f_movieDF$Released = dmy(f_movieList$Released)
        } else {
          f_movieDF$Released = NA
        }
        
        if (!is.null(f_movieList$Runtime)) {
          f_movieDF$Runtime = f_movieList$Runtime
        } else {
          f_movieDF$Runtime = NA
        }
        
        if (!is.null(f_movieList$Genre)) {
          f_movieDF$Genre = f_movieList$Genre
        } else {
          f_movieDF$Genre = NA
        }
        
        if (!is.null(f_movieList$Director)) {
          f_movieDF$Director = f_movieList$Director
        } else {
          f_movieDF$Director = NA
        }
        
        if (!is.null(f_movieList$Writer)) {
          f_movieDF$Writer = f_movieList$Writer
        } else {
          f_movieDF$Writer = NA
        }
        
        if (!is.null(f_movieList$Actors)) {
          f_movieDF$Actors = f_movieList$Actors
        } else {
          f_movieDF$Actors = NA
        }
        
        if (!is.null(f_movieList$Plot)) {
          f_movieDF$Plot = f_movieList$Plot
        } else {
          f_movieDF$Plot = NA
        }
        
        if (!is.null(f_movieList$Language)) {
          f_movieDF$Language = f_movieList$Language
        } else {
          f_movieDF$Language = NA
        }
        
        if (!is.null(f_movieList$Country)) {
          f_movieDF$Country = f_movieList$Country
        } else {
          f_movieDF$Country = NA
        }  
        
        if (!is.null(f_movieList$Awards)) {
          f_movieDF$Awards = f_movieList$Awards
        } else {
          f_movieDF$Awards = NA  
        }
        
        if (!is.null(f_movieList$Poster)) {
          f_movieDF$Poster = f_movieList$Poster
        } else {
          f_movieDF$Poster = NA
        }
      
        if (!is.null(f_movieList$imdbRating)) {
          f_movieDF$IMDBRating = as.numeric(f_movieList$imdbRating) * 10
        } else {
          f_movieDF$IMDBRating = NA
        }
        
        if (!is.null(f_movieList$Ratings) & length(f_movieList$Ratings)!=0) {
          f_movieDF$RTRating = f_movieList$Ratings %>%
            filter(Source == "Rotten Tomatoes") %>%
            select(Value) %>%
            mutate_all(funs(as.numeric(gsub("[\\%]", "", .)))) %>%
            .$Value
          if (length(f_movieDF$RTRating)==0){
            f_movieDF$RTRating = NA
          }  
        } else {
          f_movieDF$RTRating = NA
        }
        
        if (!is.null(f_movieList$Metascore)) {
          f_movieDF$Metacritic = as.numeric(f_movieList$Metascore)
        } else {
          f_movieDF$Metacritic = NA
        }
        
        if (!is.null(f_movieList$imdbVotes)) {
          f_movieDF$IMDBVotes = as.numeric(gsub("[\\$,]", "", f_movieList$imdbVotes))
        } else {
          f_movieDF$IMDBVotes = NA
        }
        
        if (!is.null(f_movieList$imdbID)) {
          f_movieDF$imdbID = f_movieList$imdbID
        } else {
          f_movieList$imdbID = NA
        }
        
        if (!is.null(f_movieList$Type)) {
          f_movieDF$Type = f_movieList$Type  
        } else {
          f_movieDF$Type = NA
        }
        
        if (!is.null(f_movieList$DVD)) {
          f_movieDF$DVD = dmy(f_movieList$DVD)
        } else {
          f_movieDF$DVD = NA
        }
        
        if (!is.null(f_movieList$Production)) {
          f_movieDF$Production = f_movieList$Production  
        } else {
          f_movieDF$Production = NA
        }
        
        if (!is.null(f_movieList$Website)) {
          f_movieDF$Website = f_movieList$Website
        } else {
          f_movieDF$Website = NA
        }
        
        f_movieDF = as.data.frame(f_movieDF)
        
      } else {
        # movie response == False
        print(f_movieList$Error)
      }
  }
  return(f_movieDF)
  
  # },
  #  error=function(e) return(NULL)
  #)  
  
}
#-------------------------------------------------------------------

#---------------GET MOVIE RATINGS BY LIST  FUNCTION------------------
getMovieRatingsByList = function(vTitles) {
  #NOTE: 1000 request limit per day
  fullDF = NULL
  movieDF = NULL
  for (x in 1:length(vTitles)) {
    
    #wait 1 sec before next request to avoid spamming
    Sys.sleep(1)
    movieDF = getMovieRatings(vTitles[x]) 
    
    if(!is.null(movieDF)) {
      if(!is.null(fullDF)) {
        fullDF = rbind(fullDF, movieDF)  
      } else {
        fullDF = movieDF
      }
    }
    
  }
  return(fullDF)
}
#-------------------------------------------------------------------

# x = getMovieRatings("Li'l Quinquin")
# 
# View(x)

myMovieList = c("Ghost in the Shell", "Radio Dreams", "Ready Player One", "Storks", "Thor: Ragnarok", "Worlds Apart")
# 
# myMovieList = c("Ghost in the Shell", "Li'l Quinquin")
# 
y = getMovieRatingsByList(myMovieList)


#-------------get titles

movieList = read.csv("MoviesByTitle.csv")

#day 1
m1 = movieList[1:200,]
movieRatingsDF = getMovieRatingsByList(as.character(m1$Title))
write.csv(movieRatingsDF, "MovieRatings1.csv")
m2 = movieList[201:500,]
movieRatingsDF = getMovieRatingsByList(as.character(m2$Title))
write.csv(movieRatingsDF, "MovieRatings2.csv")
m3 = movieList[501:1000,]
movieRatingsDF = getMovieRatingsByList(as.character(m3$Title))
write.csv(movieRatingsDF, "MovieRatings3.csv")

#day 2
m4 = movieList[1001:1010,]
movieRatingsDF = getMovieRatingsByList(as.character(m4$Title))
write.csv(movieRatingsDF, "MovieRatings4.csv")
m5 = movieList[1011:1500,]
movieRatingsDF = getMovieRatingsByList(as.character(m5$Title))
write.csv(movieRatingsDF, "MovieRatings5.csv")
m6 = movieList[1501:2000,]
movieRatingsDF = getMovieRatingsByList(as.character(m6$Title))
write.csv(movieRatingsDF, "MovieRatings6.csv")

#day 3
m7 = movieList[2001:2010,]
movieRatingsDF = getMovieRatingsByList(as.character(m7$Title))
write.csv(movieRatingsDF, "MovieRatings7.csv")
m8 = movieList[2011:2500,]
movieRatingsDF = getMovieRatingsByList(as.character(m8$Title))
write.csv(movieRatingsDF, "MovieRatings8.csv")
m9 = movieList[2501:3500,]
movieRatingsDF = getMovieRatingsByList(as.character(m9$Title))
write.csv(movieRatingsDF, "MovieRatings9.csv")

# 
# nrow(movieList)
# 
# 
#test = movieList[1201:1202,]
#movieRatingsDF = getMovieRatingsByList(as.character(test$Title))
# 
# 
# 
movieRatingsDF = getMovieRatingsByList(as.character(m4$Title))
write.csv(movieRatingsDF, "MovieRatings4.csv")



mratings = read.csv("G:/Team Drives/STDS - AT2/MovieRatingsFull.csv")

View(mratings)

msales = read.csv("G:/Team Drives/STDS - AT2/MoviesByTitle.csv")

View(msales)

mcombined = left_join(msales, mratings, by=c("Title"))

write.csv(mcombined, "MovieListCombined.csv")
