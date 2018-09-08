#--------------------------------------------
# Assignment 2 - EDA
#--------------------------------------------

install.packages("dplyr")
library(dplyr)

install.packages("magrittr")
library(magrittr)

install.packages("lubridate")
library(lubridate)

install.packages("tibble")
library(tibble)

install.packages("tidyr")
library(tidyr)


install.packages("ggplot2")
library(ggplot2)

install.packages("naniar")
library(naniar)

install.packages("purrr")
library(purrr)

install.packages("Amelia")
library(Amelia)


mcombined = read.csv("MovieListCombined.csv", stringsAsFactors = FALSE)

#-----------------------------------------------------------------------------
#STAGE 1 - DATA CLEANSING
#-----------------------------------------------------------------------------

View(mcombined)



na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available")

#----------------------Initial cleaning of data
cdata = mcombined %>% 
  mutate(totBudget = trimws(totBudget)) %>% 
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  #split Genre to different columns
  mutate(Genre = strsplit(as.character(Genre), ",")) %>% 
  unnest(Genre) %>% 
  mutate(GenreTrue = 1,
         Genre = paste("G", Genre, sep="_")) %>% 
  arrange(Genre) %>% 
  spread(Genre, GenreTrue) %>%
  mutate(Awards = !is.na(Awards),
         Runtime = as.integer(gsub(" min", "", as.character(Runtime))),
         TMDBbudget = replace(TMDBbudget, TMDBbudget<=1200, NA), #clean incorrect data
         totBudget = replace(totBudget, is.na(totBudget), TMDBbudget[is.na(totBudget)]),
         WeeksOn = replace(WeeksOn, WeeksOn == "-", 1) #dash means week 1
         ) %>% 
  arrange(X.1) %>% 
  select(X.1, 
         imdbID,
         TMDBID,
         Title, 
         Studio, 
         totTheatreCount, 
         totGross, 
         totBudget, 
         #TMDBbudget,
         WeeksOn, 
         startYear, 
         startWeek,
         Runtime,
         Rated,
         Awards,
         Language,
         IMDBRating,
         RTRating,
         Metacritic,
         Production,
         Country,
         starts_with("G_"),
         -G_NA
  ) #%>% View

write.csv(cdata, "MovieClean1.csv")

#----------------------Split Language into columns
cdata2 = cdata %>%
  #split Language to different columns
  mutate(Language = 
            strsplit(gsub(" ", "", as.character(Language)), ",")
      ) %>% 
  mutate(Language = lapply(Language, FUN=unique)) %>%
  unnest(Language) %>% 
  mutate(
    Language = paste("L", Language, sep="_"),
    LanguageTrue = 1
  ) %>% 
  arrange(Language) %>% 
  spread(Language, LanguageTrue) %>% 
  arrange(X.1) 


write.csv(cdata2, "MovieClean2.csv")
  
  
  #split Country to different columns
  # mutate(Country = strsplit(as.character(Country), ",")) %>% 
  # unnest(Country) %>% 
  # mutate(CountryTrue = 1,
  #        Country = paste("C", Country, sep="_")) %>% 
  # arrange(Country) %>% 
  # spread(Country, CountryTrue) %>% 
  # arrange(X.1) %>% View

mclean = read.csv("MovieClean2.csv", stringsAsFactors = FALSE)


#missingness maps
missmap(mclean)

mclean %>% 
  select(X.1, 
         Title, 
         Studio, 
         totTheatreCount, 
         totGross, 
         totBudget, 
         WeeksOn, 
         startYear, 
         startWeek,
         Runtime,
         Rated,
         Awards,
         #Language,
         IMDBRating,
         RTRating,
         Metacritic,
         Production
         #Country
         #starts_with("G_"),
         #starts_with("L_"),
  ) %>%
    missmap()

str(mclean)

#---------------------Convert features to correct type
mclean = mclean %>%
  mutate(
    Studio = as.factor(Studio),
    Rated = as.factor(Rated),
    Production = as.factor(Production)
  )

#STAGE 2 - DATA UNDERSTANDING
#check distribution of each variable

#------Movies by Year
mclean %>% 
  group_by(startYear) %>%
  summarise(total=n(),
            avgWeeksOn=mean(WeeksOn),
            medWeeksOn=median(WeeksOn),
            avgRunTime=mean(Runtime, na.rm = TRUE),
            medRunTime=median(Runtime, na.rm = TRUE),
            avgIMDBRating=mean(IMDBRating, na.rm = TRUE),
            medIMDBRating=median(IMDBRating, na.rm = TRUE)
            ) %>%
  filter(startYear %in% c(2015:2017))

#     startYear total avgWeeksOn medWeeksOn avgRunTime medRunTime avgIMDBRating medIMDBRating
#         <int> <int>      <dbl>      <dbl>      <dbl>      <dbl>         <dbl>         <dbl>
#   1      2015   673       6.37          7       105.        102          64.9            66
#   2      2016   720       6.27          7       103.        101          65.1            66
#   3      2017   705       6.4           7       104.        101          65.6            66



#-------Weeks On by Year
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(WeeksOn >= 0) %>% 
  group_by(startYear) %>%
  summarise(avgWeeksOn=mean(WeeksOn),
            medWeeksOn=median(WeeksOn),
            minWeeksOn=min(WeeksOn),
            maxWeeksOn=max(WeeksOn)
            )
#       startYear avgWeeksOn medWeeksOn minWeeksOn maxWeeksOn
#         <int>      <dbl>      <dbl>      <dbl>      <dbl>
#   1      2015       6.37          7          1        113
#   2      2016       6.27          7          1         99
#   3      2017       6.48          7          1         59
#Notes: The average number of weeks in cinema is 6 weeks. This is fairly consistent for all years. 

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(WeeksOn >= 0) %>% 
  ggplot(aes(y=WeeksOn, x=as.factor(startYear))) +
    geom_boxplot()


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(WeeksOn >= 0) %>% 
  ggplot(aes(x=WeeksOn)) +
  geom_histogram(binwidth = 3) +
  facet_wrap(~startYear)


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(WeeksOn >= 0) %>% 
  ggplot(aes(x=WeeksOn, fill=as.factor(startYear))) +
  geom_density(alpha=.2)


#investigate outliers
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(WeeksOn >= 25) 
#Studios 
#- IMAX - specialised cinema for large film format.
#- KL (Kino Lorber) are specalised cinemas for documentaries and specialize in art house, low budget



#------Studio
mclean %>%
  group_by(Studio) %>%
  summarise(total=n()) %>%
  arrange(desc(total))






#check outliers
#check missing data - impute
#preliminary EDA (break out factors)

#STAGE 3 - DAT PREPARATION
#check missing data - impute
#inclusion/exclusion report
#aggregate / merge


#STAGE 4 - MODELLING
#check collinearily of variables
#check interactions between variables. Transformation to log if required.
#split train / test
#run models
#cross validation
#prdict 
#plot prediction vs actual
#segment data for highest profit and repeat models





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


