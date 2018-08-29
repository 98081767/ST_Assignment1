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

install.packages("naniar")
library(naniar)

install.packages("ggplot2")
library(ggplot2)


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

getMovieRatings = function (stype, gTitle) {
  
  #tryCatch(
  #  {
  #my API Key
  #NOTE: 1000 request limit per day
  apiKey = "f635a606" #98081767@student.uts.edu.au
  #apiKey = "5b07fbf5" #Archel.J.Aguilar@gmail.com
  
  f_omdb_url = "http://www.omdbapi.com/"
  if (stype == "id") {
    f_omdb_params = list(i=gTitle, apikey=apiKey)
    print(gTitle)
  } else {
    f_omdb_params = list(t=gTitle, apikey=apiKey)
    print(gTitle)
  }
  
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
#------------------------------------------------------------------

#---------------GET MOVIE RATINGS BY LIST  FUNCTION------------------
getMovieRatingsByList = function(vTitles) {
  #NOTE: 1000 request limit per day
  fullDF = NULL
  movieDF = NULL
  for (x in 1:length(vTitles)) {
    
    #wait 1 sec before next request to avoid spamming
    Sys.sleep(1)
    movieDF = getMovieRatings("title", vTitles[x]) 
    
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



#---------------UPDATE MOVIE RATINGS FUNCTION------------------
updateMovieRatingsByList = function(dfMovies, rIds) {
  #NOTE: 1000 request limit per day

  for (x in rIds) {
    movieDF = NULL
    print(x)
    
    
    if(!is.null(dfMovies)) {
        #wait 1 sec before next request to avoid spamming
        Sys.sleep(1)
      
        print(paste(dfMovies[x,]$Title,dfMovies[x,]$imdbID, sep="-"))
        
        movieDF = getMovieRatings("id", dfMovies[x,]$imdbID) 
        
        if(!is.null(movieDF)) {
          print(paste("Prod:", as.character(movieDF$Production)))
          
          #update record
          dfMovies[x,]$Year = as.character(movieDF$Year)
          dfMovies[x,]$Rated = as.character(movieDF$Rated)
          dfMovies[x,]$Released = as.character(movieDF$Released)
          dfMovies[x,]$Runtime = as.character(movieDF$Runtime)
          dfMovies[x,]$Genre = as.character(movieDF$Genre)
          dfMovies[x,]$Director = as.character(movieDF$Director)
          dfMovies[x,]$Writer = as.character(movieDF$Writer)
          dfMovies[x,]$Actors = as.character(movieDF$Actors)
          dfMovies[x,]$Plot = as.character(movieDF$Plot)
          dfMovies[x,]$Language = as.character(movieDF$Language)
          dfMovies[x,]$Country = as.character(movieDF$Country)
          dfMovies[x,]$Awards = as.character(movieDF$Awards)
          dfMovies[x,]$Poster = as.character(movieDF$Poster)
          dfMovies[x,]$Released = as.character(movieDF$Released)
          dfMovies[x,]$IMDBRating = as.integer(movieDF$IMDBRating)
          dfMovies[x,]$RTRating = as.integer(movieDF$RTRating)
          dfMovies[x,]$Metacritic = as.integer(movieDF$Metacritic)
          dfMovies[x,]$IMDBVotes = as.integer(movieDF$IMDBVotes)
          dfMovies[x,]$Type = as.character(movieDF$Type)
          dfMovies[x,]$DVD = as.character(movieDF$DVD)
          dfMovies[x,]$Production = as.character(movieDF$Production)
          dfMovies[x,]$Website = as.character(movieDF$Website)
        }
    }
    
  }
  
  return(dfMovies)
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

# movieList = read.csv("MoviesByTitle.csv")
# 
# #day 1
# m1 = movieList[1:200,]
# movieRatingsDF = getMovieRatingsByList(as.character(m1$Title))
# write.csv(movieRatingsDF, "MovieRatings1.csv")
# m2 = movieList[201:500,]
# movieRatingsDF = getMovieRatingsByList(as.character(m2$Title))
# write.csv(movieRatingsDF, "MovieRatings2.csv")
# m3 = movieList[501:1000,]
# movieRatingsDF = getMovieRatingsByList(as.character(m3$Title))
# write.csv(movieRatingsDF, "MovieRatings3.csv")
# 
# #day 2
# m4 = movieList[1001:1010,]
# movieRatingsDF = getMovieRatingsByList(as.character(m4$Title))
# write.csv(movieRatingsDF, "MovieRatings4.csv")
# m5 = movieList[1011:1500,]
# movieRatingsDF = getMovieRatingsByList(as.character(m5$Title))
# write.csv(movieRatingsDF, "MovieRatings5.csv")
# m6 = movieList[1501:2000,]
# movieRatingsDF = getMovieRatingsByList(as.character(m6$Title))
# write.csv(movieRatingsDF, "MovieRatings6.csv")
# 
# #day 3
# m7 = movieList[2001:2010,]
# movieRatingsDF = getMovieRatingsByList(as.character(m7$Title))
# write.csv(movieRatingsDF, "MovieRatings7.csv")
# m8 = movieList[2011:2500,]
# movieRatingsDF = getMovieRatingsByList(as.character(m8$Title))
# write.csv(movieRatingsDF, "MovieRatings8.csv")
# m9 = movieList[2501:3004,]
# movieRatingsDF = getMovieRatingsByList(as.character(m9$Title))
# write.csv(movieRatingsDF, "MovieRatings9.csv")

# 
# nrow(movieList)
# 
# 
#test = movieList[1201:1202,]
#movieRatingsDF = getMovieRatingsByList(as.character(test$Title))
# 
# 
# 
# movieRatingsDF = getMovieRatingsByList(as.character(m4$Title))
# write.csv(movieRatingsDF, "MovieRatings4.csv")

#mratings = read.csv("G:/Team Drives/STDS - AT2/MovieRatingsFull.csv")

mratings = read.csv("MovieRatingsFull.csv", stringsAsFactors =FALSE)
#View(mratings)

#msales = read.csv("G:/Team Drives/STDS - AT2/MoviesByTitle.csv")

msales = read.csv("MoviesByTitle.csv", stringsAsFactors =FALSE)
#View(msales)

mcombined = left_join(msales, mratings, by=c("Title"))

write.csv(mcombined, "MovieListCombined.csv")

#-----------------------------------------------------
# GET TMDB data
#----------------------------------------------------
install.packages("TMDb")
library(TMDb)

mytoken = "b64d8b75244a0edfd7217267d2a65734"

getIMDBID = function (mydf, myapikey) {
  
  for (x in 1:nrow(mydf)) {
    myTitle = mydf[x,]$Title
    myYear = mydf[x,]$startYear
    print(paste(myTitle, myYear, sep="-"))
    
    
    myCont = search_movie(api_key=myapikey, myTitle, primary_release_year=myYear) 
    
    if (myCont$total_results == 1) {
      
      myMovieDetails = movie(api_key=myapikey, id=myCont$results$id)
      myIMDBID = myMovieDetails$imdb_id
      
      mydf[x, ]$imdbID = myIMDBID
      
    } else {
      print(paste("total results:", myCont$total_results, sep=" "))
    }
  }
  
  return(mydf)
}



# tmdbCont = search_movie(api_key = mytoken, "13 Hours: The Secret Soldiers of Benghazi", primary_release_year="2016")
# str(tmdbCont$results)

tmdbResults =  movie(api_key=mytoken, id="0001")

tmdbResults$imdb_id


mcombined = read.csv("MovieListCombined.csv", stringsAsFactors = FALSE)

missingMovies = mcombined %>%
  filter(is.na(X)) %>%
  mutate(startYear = as.character(startYear)) %>%
  select(X.1, Title, startYear, imdbID) 

str(missingMovies)


missingIMDB = getIMDBID(missingMovies, mytoken)



#--------------------------------------------------
#update IMDB for missing movies
mcombined[missingIMDB$X.1, ]$imdbID = missingIMDB$imdbID

missingIMDB %>%
  filter(!is.na(imdbID)) %>% View

write.csv(mcombined, "MovieListCombined.csv")
#--------------------------------------------------


#get list of movies that need to be updated
vIds = mcombined %>%
  filter(is.na(X) & !is.na(imdbID) & imdbID!="") %>%
  mutate(imdbID = as.character(imdbID)) %>%
  select(X.1, Title, startYear, imdbID) 


myIds = as.character(vIds$X.1)
#myIds = c("12", "18")


newcombined = updateMovieRatingsByList(mcombined, myIds)

newcombined %>%
  filter(is.na(X)) %>% View

write.csv(newcombined, "MovieListCombined.csv")

#-----------------------------------------------------
# EDA
#----------------------------------------------------


mcombined = read.csv("MovieListCombined.csv", stringsAsFactors = FALSE)



test = mcombined %>%
  #filter(Title=="12 Strong") %>%
  mutate(totBudget = trimws(totBudget)) %>%
  replace_with_na(replace = list(totBudget=c("NA"))) %>%
  filter(!is.na(totBudget)) #%>% View

options(digits=10)
test$totBudget = as.numeric(test$totBudget)

ggplot(test, aes(x=totBudget, y=totGross)) +
  geom_jitter(alpha=0.3)

ggplot(test, aes(x=cut(totBudget, breaks=11), y=totGross)) +
  geom_jitter(alpha=0.3) 

ggplot(test, aes(x=cut(totBudget, breaks=15), y=totGross)) +
  geom_boxplot() 

#abline shows that majority of movies make more than budget

ggplot(test, aes(x=totBudget, y=totGross)) +
  geom_jitter(alpha=0.3) +
  scale_x_log10() +
  scale_y_log10()+
  geom_abline(intercept = 0, colour="red") +
  geom_smooth(method="lm")

ggplot(test, aes(x=cut(log(totBudget), breaks=11), y=log(totGross))) +
  geom_jitter(alpha=0.3) 

ggplot(test, aes(x=cut(log(totBudget), breaks=15), y=log(totGross))) +
  geom_boxplot()
  


cor(log(test$totBudget), log(test$totGross))


movie.mod = lm(totGross ~ totBudget, data=test)

summary(movie.mod)


movie.mod = lm(totGross ~ totBudget, data=test)

summary(movie.mod)



#mutate(totBudget = formatC(as.numeric(totBudget), digits=10, format="d")) %>% 

summary(mcombined)

test %>% 
  filter(!is.na(totBudget)) %>% 
  mutate(totProfit = totGross - totBudget) %>%
  group_by(startYear) %>%
  summarise(avgBudget=mean(totBudget), 
            avgGross = mean(totGross),
            avgProfit = mean(totProfit),
            sumBudget = sum(totBudget),
            sumGross = sum(totGross)
            ) %>% View
