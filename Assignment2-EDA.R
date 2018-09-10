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

#----------------START EDA HERE---------------------------


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


#------Box office sales (totGross)
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  group_by(startYear) %>%
  summarise(total=n(),
            avgGross = mean(totGross, na.rm=TRUE),
            medGross = median(totGross, na.rm=TRUE),
            maxGross = max(totGross, na.rm=TRUE),
            minGross = min(totGross, na.rm=TRUE),
            iqrGross = IQR(totGross, na.rm=TRUE),
            Q1Gross = quantile(totGross, 1/4, na.rm=TRUE),
            Q3Gross = quantile(totGross, 3/4, na.rm=TRUE)
            )

#       startYear total  avgGross medGross   maxGross minGross iqrGross Q1Gross Q3Gross
#         <int> <int>     <dbl>    <dbl>      <dbl>    <dbl>    <dbl>   <dbl>   <dbl>
#   1      2015   673 18200660.   175900 1009345800      100  3191300   24300 3215600
#   2      2016   720 16867940.   138550  557324700      100  3420300   26700 3447000
#   3      2017   705 16242887.   179400  627476700      400  2473400   21800 2495200



salesBreaks = c(0, 1000, 10000, 50000, 100000, 200000, 500000, 1000000, 5000000, 10000000, 50000000, 1000000000)

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(y=totGross, x=as.factor(startYear))) +
  geom_boxplot() +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks)
#NOTES:
#- most movies gross between 20,000 and 3M


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=totGross)) +
  geom_histogram() +
  scale_x_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~startYear)
#NOTES: There are two peaks for grossing films, peak around $50,000 and $5M


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=totGross, fill=as.factor(startYear))) +
  geom_density(alpha=.2) +
  scale_x_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


#-------Budget
# Gross refers to gross earnings in U.S. dollars. On average, the movie's
# distributor receives a little more than half of the final gross (often
# referred to as the "rentals") with the remainder going to the exhibitor (i.e.,
# movie theater). The money split varies from movie to movie, but, generally,
# the contract favors the distributor in early weeks and shifts to the exhibitor
# later on.

mclean %>% 
  select(Title,
         totGross, 
         totBudget
  ) %>%
  missmap()


mclean %>% 
  select(Title,
         totGross, 
         totBudget
  ) %>%
  summary()

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  group_by(startYear) %>%
  summarise(total=n(),
            avgBudget = mean(totBudget, na.rm=TRUE),
            medBudget = median(totBudget, na.rm=TRUE),
            maxBudget = max(totBudget, na.rm=TRUE),
            minBudget = min(totBudget, na.rm=TRUE),
            iqrBudget = IQR(totBudget, na.rm=TRUE),
            Q1Budget = quantile(totBudget, 1/4, na.rm=TRUE),
            Q3Budget = quantile(totBudget, 3/4, na.rm=TRUE)
  )
#       startYear total avgBudget medBudget maxBudget minBudget iqrBudget Q1Budget Q3Budget
#         <int> <int>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>    <dbl>    <dbl>
#   1      2015   673 36894031.  18000000 250000000     50000  33075000  6925000 40000000
#   2      2016   720 37406028.  16000000 250000000     15000  34800000  5200000 40000000
#   3      2017   705 45248465.  25000000 500000000     75000  40227010  9772990 50000000

budgetBreaks = c(0, 1000, 10000, 50000, 100000, 200000, 500000, 1000000, 5000000, 10000000, 50000000, 1000000000)

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(y=totBudget, x=as.factor(startYear))) +
  geom_boxplot() +
  scale_y_log10(labels = scales::dollar, breaks=budgetBreaks)
#NOTES:
#- movie budget centre is $16M to $25M, and range between $500,000 and $50M


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=totBudget)) +
  geom_histogram() +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~startYear)


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=totBudget, fill=as.factor(startYear))) +
  geom_density(alpha=.2) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
#A majority of the movie budget is over $5M. Is it because the other movie budgets weren't provided?
#	- Is it due to certain studios only? 
# - Is budget generally provided for top grossing films? 


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  mutate(topGross = totGross >= 5000000) %>% 
  ggplot(aes(x=totBudget, fill=as.factor(topGross))) +
  geom_density(alpha=.2) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
#- Top grossing films generally have higher budget but graph shows that non top grossing films also has budgets provided. 

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  mutate(topGross = totGross >= 5000000) %>% 
  group_by(topGross) %>%
  summarise(num=n())
# topGross   num
# <lgl>    <int>
#   1 FALSE     1636
#   2 TRUE       461
#   3 NA           1

#is budget generally provided by certain studios?
#-It appears the top 10 are well known, provide the most movies and generally have higher budgets
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  mutate(hasBudget = !is.na(totBudget)) %>%
  group_by(Studio) %>%
  summarise(num=n(),
            numBudget = sum(hasBudget),
            avgBudget = mean(totBudget, na.rm=TRUE),
            medBudget = median(totBudget, na.rm=TRUE),
            maxBudget = max(totBudget, na.rm=TRUE),
            medTheatre = median(totTheatreCount, na.rm=TRUE)
            ) %>% 
  arrange(desc(numBudget)) %>% View


#correlation between sales and budget

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(totBudget)) %>%
  summarise(corr = cor(totGross, totBudget, use="complete.obs"))
#0.684632

# show correlation between budget and box office sales
# - the red line shows the break even point. Most of the movies are below the red line meaning they aren't even breaking even. 
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(totBudget)) %>%
  ggplot(aes(y=totGross, x=totBudget)) +
    geom_point(alpha=0.5) + 
    geom_abline(intercept = 0, colour="red") +
    geom_abline(intercept = 0.5, slope=1, colour="green") +
    geom_smooth(method="lm", se=TRUE) +
    scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
    scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  

#get studios that provide budget for more than 10 films  
studioWRec = mclean %>%
    filter(startYear %in% c(2015:2017)) %>%
    filter(!is.na(totBudget)) %>%
    mutate(hasBudget = !is.na(totBudget)) %>%
    group_by(Studio) %>%
    summarise(numBudget = sum(hasBudget)) %>% 
    filter(numBudget >= 10) %>%
    select(Studio) 


studioWRec = as.character(studioWRec$Studio)

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(totBudget)) %>%
  filter(Studio %in% studioWRec) %>%
  ggplot(aes(y=totGross, x=totBudget)) +
  geom_point() + 
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0.5, slope=1, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ Studio)
#- graph shows that some have a negative correlation between budget and sales. 



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
  filter(WeeksOn >= 25) %>% View
#Studios 
#- IMAX - specialised cinema for large film format.
#- KL (Kino Lorber) are specalised cinemas for documentaries and specialize in art house, low budget
#- Gathr (Gathr Films) - Theatrical On Demand cinema


#filter out movies from specialist studios
#NOTES: 
# - Still some outliers with weeks greater than 20
# - Movie showing range between 1 to 9 weeks
'%!in%' <- function(x,y)!('%in%'(x,y))
mclean %>% 
  filter(startYear %in% c(2015:2017)) %>%
  filter(Studio %!in% c("Imax", "KL", "Gathr")) %>%
  filter(WeeksOn >=1) %>%
  ggplot(aes(y=WeeksOn, x=as.factor(startYear))) +
  geom_boxplot()


#outlier movies
mclean %>% 
  filter(startYear %in% c(2015:2017)) %>%
  filter(Studio %!in% c("Imax", "KL", "Gathr")) %>%
  filter(WeeksOn >= 20) %>% 
  select(Title)
  

#------Start Week
# In the United States, summer vacation lasts two to three months. The dates
# vary depending on the location of the school district, with two major formats.
# One is from late May-mid June to early September (in most northern states),
# the other major format lasting from late May to mid August (in most southern
# and western states). (Excluding some districts, as some schools may end late
# June and begin early September).
#
# Summer vacation or break lasts for about 12 weeks, starting anywhere from late
# May to mid June, and ending anywhere from late August to Labor Day, the first
# Monday in September. This often depends on the region – for example, most
# schools in the Northeastern United States end in June and start the Wednesday
# after Labor Day (Teachers report back on Tuesday), while the majority of
# schools in the Southern United States have schools end in May and start again
# in August.

#summer vacation - week 20-24 to 34-35
#north/south - week 22-35


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=startWeek)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~startYear)


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(y=startWeek, x=as.factor(startYear))) +
  geom_boxplot()


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=startWeek, fill=as.factor(startYear))) +
  geom_density(alpha=.2)
#NOTES:
#- Looks like two peak release periods - Week 15 and week 40
#-https://en.wikipedia.org/wiki/Dump_months
#- The dump months are what the film community calls the two periods of the year
#when there are lowered commercial and critical expectations for most new
#releases from American filmmakers and distributors. Domestic audiences during
#these periods are smaller than the rest of the year, so no tentpole movies are
#released. January[1] and February are usually most commonly described this way,
#with August and September sometimes included.
#- this seems to line up with when movies are released.

#correlation with Box office sales
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=startWeek, y=log(totGross))) +
  geom_jitter()
  

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  group_by(startYear) %>%
  summarise(corr = cor(startWeek, totGross, use="complete.obs"))
# startYear   corr
# <int>  <dbl>
#   1      2015 0.0385
#   2      2016 0.0344
#   3      2017 0.0302
  
cor(mclean$startWeek, mclean$totGross, use="complete.obs")
#NOTES: There is no correlation between gross sales and start week 
# - but when are the high grossing films released?


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  mutate(topGross = totGross >= 5000000) %>%
  ggplot(aes(x=startWeek, fill=as.factor(topGross))) +
  geom_density(alpha=.2)


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  mutate(topGross = totGross >= 5000000) %>%
  ggplot(aes(y=startWeek, x=as.factor(topGross))) +
  geom_boxplot()


#--------IMDB Rating



mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  select(IMDBRating) %>%
  summary()

# IMDBRating   
# Min.   :21.00  
# 1st Qu.:59.00  
# Median :66.00  
# Mean   :65.21  
# 3rd Qu.:72.00  
# Max.   :98.00  
# NA's   :345   
  
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(y=IMDBRating, x=as.factor(startYear))) +
  geom_boxplot()


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=IMDBRating)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~startYear)


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=IMDBRating, fill=as.factor(startYear))) +
  geom_density(alpha=.2)



mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(IMDBRating)) %>%
  summarise(corr = cor(totGross, IMDBRating, use="complete.obs"))
#0.1057696

# show correlation between IMDB Rating and box office sales
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(IMDBRating)) %>%
  ggplot(aes(y=totGross, x=IMDBRating)) +
  geom_jitter(alpha=0.5) + 
  geom_smooth(method="lm", se=TRUE) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#add awards as a facet.
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(IMDBRating)) %>%
  ggplot(aes(y=totGross, x=IMDBRating, color=Awards)) +
  geom_jitter(alpha=0.5) + 
  geom_smooth(method="lm", se=TRUE) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(y=IMDBRating, x=as.factor(Awards))) +
  geom_boxplot() +
  facet_wrap(~ startYear)




#what if i get data from 2018
mclean %>%
  filter(startYear %in% c(2018)) %>%
  filter(!is.na(IMDBRating)) %>%
  ggplot(aes(y=totGross, x=IMDBRating)) +
  geom_jitter(alpha=0.5) + 
  geom_smooth(method="lm", se=TRUE) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

mclean %>%
  filter(startYear %in% c(2018)) %>%
  filter(!is.na(IMDBRating)) %>%
  summarise(corr = cor(totGross, IMDBRating, use="complete.obs"))
#1 0.1531358


#Rotten tomatoes
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(RTRating)) %>%
  summarise(corr = cor(totGross, RTRating, use="complete.obs"))
#-0.00191799

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=RTRating, fill=as.factor(startYear))) +
  geom_density(alpha=.2)


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(RTRating)) %>%
  ggplot(aes(y=totGross, x=RTRating)) +
  geom_jitter(alpha=0.5) + 
  geom_smooth(method="lm", se=TRUE) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(y=RTRating, x=as.factor(Awards))) +
  geom_boxplot() + 
  facet_wrap(~ startYear)


#Metacritic
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(Metacritic)) %>%
  summarise(corr = cor(totGross, Metacritic, use="complete.obs"))
#-0.00191799

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=Metacritic, fill=as.factor(startYear))) +
  geom_density(alpha=.2)


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(Metacritic)) %>%
  ggplot(aes(y=totGross, x=Metacritic)) +
  geom_jitter(alpha=0.5) + 
  geom_smooth(method="lm", se=TRUE) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(y=Metacritic, x=as.factor(Awards))) +
  geom_boxplot() + 
  facet_wrap(~ startYear)


#-------Runtime

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  select(Runtime) %>%
  summary()

# Runtime     
# Min.   :  2.0  
# 1st Qu.: 91.0  
# Median :101.0  
# Mean   :104.2  
# 3rd Qu.:116.0  
# Max.   :319.0  
# NA's   :332 

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(y=Runtime, x=as.factor(startYear))) +
  geom_boxplot()


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=Runtime)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~startYear)


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(x=Runtime, fill=as.factor(startYear))) +
  geom_density(alpha=.2)



mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(Runtime)) %>%
  summarise(corr = cor(totGross, Runtime, use="complete.obs"))
#0.1792978

# show correlation between Runtime and box office sales
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(Runtime)) %>%
  ggplot(aes(y=totGross, x=Runtime)) +
  geom_jitter(alpha=0.5) + 
  geom_smooth(method="lm", se=TRUE) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



#---------Awards

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(y=totGross, x=as.factor(Awards))) +
  geom_boxplot() + 
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) 

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(Awards)) %>%
  summarise(corr = cor(totGross, Awards, use="complete.obs"))

# 0.141846 - can't do correlation on non linear relationship

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(Awards)) %>%
  ggplot(aes(x=totGross, fill=as.factor(Awards))) +
  geom_density(alpha=.2) +
  scale_x_log10(labels = scales::dollar, breaks=salesBreaks) 


#-------Rated
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  select(Rated) %>%
  table()

# APPROVED         G NOT RATED    PASSED        PG     PG-13         R     TV-14     TV-MA     TV-PG     TV-Y7   UNRATED 
#       6        10       293         1       131       326       491        15        23        10         1        93 


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  ggplot(aes(y=totGross, x=as.factor(Rated))) +
  geom_boxplot() + 
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) 


#-------Genre

#genre by sales
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  ggplot(aes(y=totGross, x=as.factor(Genre))) +
  geom_boxplot() + 
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#genre by budget
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  ggplot(aes(y=totBudget, x=as.factor(Genre))) +
  geom_boxplot() +
  scale_y_log10(labels = scales::dollar, breaks=budgetBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(totBudget)) %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  ggplot(aes(y=totGross, x=totBudget)) +
  geom_point(alpha=0.5) + 
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0.5, slope=1, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~Genre)




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


