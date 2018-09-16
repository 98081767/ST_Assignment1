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


install.packages("plotly")
library(plotly)

install.packages("Metrics")
library(Metrics)


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


mclean2 = mclean %>%
  rename(
    ID = X.1,
    Theatres = totTheatreCount, 
    Sales = totGross, 
    Budget = totBudget, 
    Weeks_Showing = WeeksOn, 
    Year = startYear, 
    Opening_Week = startWeek,
    IMDB_Rating = IMDBRating,
    RottenTomatoes_Rating = RTRating,
    Metacritic_Rating = Metacritic
  ) 


#studios
mclean2 %>%
  group_by(Studio) %>%
  summarise(countm = n()) %>%
  nrow()


#missingness maps
missmap(mclean)

mclean %>% 
  mutate(Genre = rowSums(select(.,c(G_Action:G_Western)), na.rm=TRUE)) %>% 
  mutate(Genre = ifelse(Genre==0, NA, 1)) %>% 
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
         Production,
         Genre
         #Country
         #starts_with("G_"),
         #starts_with("L_"),
  ) %>%
  rename(
    ID = X.1,
    Theatres = totTheatreCount, 
    Sales = totGross, 
    Budget = totBudget, 
    Weeks_Showing = WeeksOn, 
    Year = startYear, 
    Opening_Week = startWeek,
    IMDB_Rating = IMDBRating,
    RottenTomatoes_Rating = RTRating,
    Metacritic_Rating = Metacritic
  ) %>%
    missmap(y.cex = 0.5, margins=c(10,5))

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
#check outliers
#check missing data - impute (IMDBRating, Runtime, Rated,)
#preliminary EDA (break out factors)


###############################################################################
#------Movies by Year
###############################################################################
mclean2 %>% 
  group_by(Year) %>%
  summarise(total=n(),
            avgWeeksOn=mean(Weeks_Showing),
            medWeeksOn=median(Weeks_Showing),
            avgRunTime=mean(Runtime, na.rm = TRUE),
            medRunTime=median(Runtime, na.rm = TRUE),
            avgIMDBRating=mean(IMDB_Rating, na.rm = TRUE),
            medIMDBRating=median(IMDB_Rating, na.rm = TRUE)
            ) %>%
  filter(Year %in% c(2015:2017))

#     startYear total avgWeeksOn medWeeksOn avgRunTime medRunTime avgIMDBRating medIMDBRating
#         <int> <int>      <dbl>      <dbl>      <dbl>      <dbl>         <dbl>         <dbl>
#   1      2015   673       6.37          7       105.        102          64.9            66
#   2      2016   720       6.27          7       103.        101          65.1            66
#   3      2017   705       6.4           7       104.        101          65.6            66

###############################################################################
#------Box office sales (totGross)
###############################################################################
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  group_by(Year) %>%
  summarise(total=n(),
            avgGross = mean(Sales, na.rm=TRUE),
            medGross = median(Sales, na.rm=TRUE),
            maxGross = max(Sales, na.rm=TRUE),
            minGross = min(Sales, na.rm=TRUE),
            iqrGross = IQR(Sales, na.rm=TRUE),
            Q1Gross = quantile(Sales, 1/4, na.rm=TRUE),
            Q3Gross = quantile(Sales, 3/4, na.rm=TRUE)
            ) %>%
  summarise(mean(medGross))

#       startYear total  avgGross medGross   maxGross minGross iqrGross Q1Gross Q3Gross
#         <int> <int>     <dbl>    <dbl>      <dbl>    <dbl>    <dbl>   <dbl>   <dbl>
#   1      2015   673 18200660.   175900 1009345800      100  3191300   24300 3215600
#   2      2016   720 16867940.   138550  557324700      100  3420300   26700 3447000
#   3      2017   705 16242887.   179400  627476700      400  2473400   21800 2495200



salesBreaks = c(0, 1000, 10000, 50000, 100000, 200000, 500000, 1000000, 5000000, 10000000, 50000000, 1000000000)
budgetBreaks = c(0, 1000, 10000, 50000, 100000, 200000, 500000, 1000000, 5000000, 10000000, 50000000, 1000000000)

mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(y=Sales, x=as.factor(Year))) +
  geom_boxplot() +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) + 
  labs(x="Year")
#NOTES:
#- most movies gross between 20,000 and 3M


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(x=Sales)) +
  geom_histogram() +
  theme_minimal() +
  annotate("rect", xmin=10000, xmax=10000000, ymin=0, ymax=Inf, fill="blue", alpha=0.1) +
  annotate("rect", xmin=20000000, xmax=100000000, ymin=0, ymax=Inf, fill="green", alpha=0.1) +
  scale_x_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~Year) + 
  labs(y="# Movies")
#NOTES: There are two peaks for grossing films, peak around $50,000 and $5M


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(x=Sales, fill=as.factor(Year))) +
  geom_density(alpha=.2) +
  scale_x_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

###############################################################################
#-------Budget
###############################################################################
# Gross refers to gross earnings in U.S. dollars. On average, the movie's
# distributor receives a little more than half of the final gross (often
# referred to as the "rentals") with the remainder going to the exhibitor (i.e.,
# movie theater). The money split varies from movie to movie, but, generally,
# the contract favors the distributor in early weeks and shifts to the exhibitor
# later on.

mclean2 %>% 
  select(Title,
         Sales, 
         Budget
  ) %>%
  missmap()


mclean2 %>% 
  select(Title,
         Sales, 
         Budget
  ) %>%
  summary()

mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  group_by(Year) %>%
  summarise(total=n(),
            avgBudget = mean(Budget, na.rm=TRUE),
            medBudget = median(Budget, na.rm=TRUE),
            maxBudget = max(Budget, na.rm=TRUE),
            minBudget = min(Budget, na.rm=TRUE),
            iqrBudget = IQR(Budget, na.rm=TRUE),
            Q1Budget = quantile(Budget, 1/4, na.rm=TRUE),
            Q3Budget = quantile(Budget, 3/4, na.rm=TRUE)
  )
#       startYear total avgBudget medBudget maxBudget minBudget iqrBudget Q1Budget Q3Budget
#         <int> <int>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>    <dbl>    <dbl>
#   1      2015   673 36894031.  18000000 250000000     50000  33075000  6925000 40000000
#   2      2016   720 37406028.  16000000 250000000     15000  34800000  5200000 40000000
#   3      2017   705 45248465.  25000000 500000000     75000  40227010  9772990 50000000



mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(y=Budget, x=as.factor(Year))) +
  geom_boxplot() +
  scale_y_log10(labels = scales::dollar, breaks=budgetBreaks)
#NOTES:
#- movie budget centre is $16M to $25M, and range between $500,000 and $50M


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(x=Budget)) +
  geom_histogram() +
  theme_minimal() +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  annotate("rect", xmin=5000000, xmax=50000000, ymin=0, ymax=Inf, fill="blue", alpha=0.1) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~Year) +
  labs(y="# Movies")
  



mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(x=Budget, fill=as.factor(Year))) +
  geom_density(alpha=.2) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
#A majority of the movie budget is over $5M. Is it because the other movie budgets weren't provided?
#	- Is it due to certain studios only? 
# - Is budget generally provided for top grossing films? 


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  mutate(Budget = Budget >= 5000000) %>% 
  ggplot(aes(x=Budget, fill=as.factor(Year))) +
  geom_density(alpha=.2) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
#- Top grossing films generally have higher budget but graph shows that non top grossing films also has budgets provided. 

mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  mutate(topGross = Sales >= 5000000) %>% 
  group_by(topGross) %>%
  summarise(num=n())
# topGross   num
# <lgl>    <int>
#   1 FALSE     1636
#   2 TRUE       461
#   3 NA           1

#is budget generally provided by certain studios?
#-It appears the top 10 are well known, provide the most movies and generally have higher budgets
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  mutate(hasBudget = !is.na(Budget)) %>%
  group_by(Studio) %>%
  summarise(num=n(),
            numBudget = sum(hasBudget),
            avgBudget = mean(Budget, na.rm=TRUE),
            medBudget = median(Budget, na.rm=TRUE),
            maxBudget = max(Budget, na.rm=TRUE),
            medTheatre = median(Theatres, na.rm=TRUE)
            ) %>% 
  arrange(desc(numBudget)) %>% View


#correlation between sales and budget

mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Budget)) %>%
  summarise(corr = cor(Sales, Budget, use="complete.obs"))
#0.684632

# show correlation between budget and box office sales
# - the red line shows the break even point. Most of the movies are below the red line meaning they aren't even breaking even. 

mclean2 %>%
  filter(Budget > 50000000 & Sales < 50000) %>% View

outl = c(2367, 1319)


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Budget)) %>%
  mutate(Sales_Exceeds_Budget = Sales > Budget) %>%
  mutate(leverage = ID %in% outl) %>% 
  ggplot(aes(y=Sales, x=Budget)) +
    theme_minimal() +  
    geom_jitter(alpha=0.5, aes(color=Sales_Exceeds_Budget)) + 
    geom_text(nudge_y=-0.1, aes(label=ifelse(leverage, Title, ""))) +
    geom_abline(intercept = 0, colour="red") +
    geom_abline(intercept = 0, slope=1.66, colour="green") +
    geom_smooth(method="lm", se=TRUE) +
    scale_x_log10(labels = scales::dollar, breaks=budgetBreaks, limits=c(10000, 1000000000)) +
    scale_y_log10(labels = scales::dollar, breaks=salesBreaks, limits=c(10000, 1000000000)) +
    #expand_limits(x=c(0, 1000000000), y=c(0, 1000000000)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
    


# out3 = train %>%
#   filter(totBudget > 1000000 & totGross < 5000) %>% 
#   select(X.1)
# 
# 
# outl = rbind(out1, out2, out3)
# 
# 
# train %>%
#   mutate(leverage = X.1 %in% outl$X.1) %>% 
#   filter(startYear %in% c(2015:2017)) %>%
#   ggplot(aes(y=totGross, x=totBudget)) +
#   geom_point(alpha=0.5, aes(color=leverage)) + 
#   geom_text(aes(label=ifelse(leverage, Title, ""))) +



mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Budget)) %>%
  mutate(Sales_Exceeds_Budget = Sales > Budget) %>%
  group_by(Sales_Exceeds_Budget) %>%
  summarise(count = n())


movie3.mod = lm(log(Sales) ~ log(Budget), data=mclean2)
summary(movie3.mod)




#get studios that provide budget for more than 10 films  
studioWRec = mclean2 %>%
    filter(Year %in% c(2015:2017)) %>%
    filter(!is.na(Budget)) %>%
    mutate(hasBudget = !is.na(Budget)) %>%
    group_by(Studio) %>%
    summarise(numBudget = sum(hasBudget)) %>% 
    filter(numBudget >= 10) %>%
    select(Studio) 


studioWRec = as.character(studioWRec$Studio)

mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Budget)) %>%
  filter(Studio %in% studioWRec) %>%
  ggplot(aes(y=Sales, x=Budget)) +
  geom_point() + 
  #theme_minimal() +
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0, slope=1.66, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~ Studio)
#- graph shows that some have a negative correlation between budget and sales. 


#difference in means test between movies with budget and no budget

mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  mutate(hasBudget = !is.na(Budget)) %>%
  ggplot(aes(x=as.factor(hasBudget), y=Sales))+
  geom_boxplot() + 
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks)


#check if normally distributed
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  mutate(hasBudget = !is.na(Budget)) %>%
  ggplot(aes(fill=as.factor(hasBudget), x=Sales))+
  geom_density(alpha=.2) +
  scale_x_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  #filter(!is.na(Sales)) %>%
  mutate(hasBudget = ifelse(!is.na(Budget), "HasBudget", "NoBudget")) %>%
  select(Sales, hasBudget) %>% 
  t.test(Sales ~ hasBudget, data=., var.equal=TRUE)

# Two Sample t-test
# 
# data:  Sales by hasBudget
# t = 19.805, df = 2095, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   46377787 56572014
# sample estimates:
#   mean in group HasBudget  mean in group NoBudget 
# 52260623.8                785723.4 



###############################################################################
#-------Weeks On by Year
###############################################################################
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




mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Budget)) %>%
  #mutate(Sales_Exceeds_Budget = Sales > Budget) %>%
  #mutate(leverage = ID %in% outl) %>% 
  ggplot(aes(y=Sales, x=Weeks_Showing)) +
  theme_minimal() +  
  geom_jitter(alpha=0.5) + 
  #geom_text(nudge_y=-0.1, aes(label=ifelse(leverage, Title, ""))) +
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0, slope=1.66, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks, limits=c(10000, 1000000000)) +
  #expand_limits(x=c(0, 1000000000), y=c(0, 1000000000)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


  
###############################################################################
#------Start Week
###############################################################################
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


###############################################################################
#--------IMDB Rating
###############################################################################


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  select(IMDB_Rating, RottenTomatoes_Rating, Metacritic_Rating) %>%
  summary()

# IMDBRating   
# Min.   :21.00  
# 1st Qu.:59.00  
# Median :66.00  
# Mean   :65.21  
# 3rd Qu.:72.00  
# Max.   :98.00  
# NA's   :345   
  
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(y=IMDB_Rating, x=as.factor(Year))) +
  geom_boxplot()


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(x=IMDB_Rating)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~Year)


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  select(ID, IMDB_Rating, RottenTomatoes_Rating, Metacritic_Rating) %>%
  gather(RatingType, Score, -ID) %>% 
  ggplot(aes(x=Score)) +
  theme_minimal() + 
  geom_histogram(binwidth = 2) +
  labs(y="# Movies") +
  facet_wrap(~RatingType) 



mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(x=IMDB_Rating, fill=as.factor(Year))) +
  geom_density(alpha=.2)



mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(IMDB_Rating)) %>%
  summarise(corr = cor(Sales, IMDB_Rating, use="complete.obs"))
#0.1057696

# show correlation between IMDB Rating and box office sales
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(IMDB_Rating)) %>%
  ggplot(aes(y=Sales, x=IMDB_Rating)) +
  geom_jitter(alpha=0.5) + 
  geom_smooth(method="lm", se=TRUE) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#add awards as a facet.
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(IMDB_Rating)) %>%
  ggplot(aes(y=Sales, x=IMDB_Rating, color=Awards)) +
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


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  mutate(hasRating = ifelse(!is.na(IMDB_Rating), "HasRating", "NoRating")) %>%
  select(Sales, hasRating) %>% 
  t.test(Sales ~ hasRating, data=., var.equal=TRUE)


#There is a significant different so we have to reject the null hypothesis
# Two Sample t-test
# 
# data:  Sales by hasRating
# t = 3.0043, df = 2095, p-value = 0.002694
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   3702930 17625637
# sample estimates:
#   mean in group HasRating  mean in group NoRating 
# 18834292                 8170008 


###############################################################################
#Rotten tomatoes
###############################################################################
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

###############################################################################
#Metacritic
###############################################################################
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

###############################################################################
#-------Runtime
###############################################################################

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


###############################################################################
#---------Awards
###############################################################################

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

###############################################################################
#-------Rated
###############################################################################
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  select(Rated) %>%
  table()

# APPROVED         G NOT RATED    PASSED        PG     PG-13         R     TV-14     TV-MA     TV-PG     TV-Y7   UNRATED 
#       6        10       293         1       131       326       491        15        23        10         1        93 


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  group_by(Rated) %>%
  summarise(count = n())


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  mutate(Rated = replace(Rated, Rated %in% c("PG", "PG-13", "TV-PG", "TV-14"), "PG")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("G", "TV-Y7"), "G")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("R", "TV-MA"), "MA")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("NOT RATED", "UNRATED", "APPROVED", "PASSED"), "NOT RATED")) %>%
  ggplot(aes(x=0, fill=factor(Rated))) +
    geom_bar(position = "fill")



mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  mutate(Rated = replace(Rated, Rated %in% c("PG", "PG-13", "TV-PG", "TV-14"), "PG")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("G", "TV-Y7"), "G")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("R", "TV-MA"), "MA")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("NOT RATED", "UNRATED", "APPROVED", "PASSED"), "NOT RATED")) %>%
  group_by(Rated) %>%
  summarise(count = n()) 


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Rated)) %>% 
  mutate(Rated = replace(Rated, Rated %in% c("PG", "PG-13", "TV-PG", "TV-14"), "PG")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("G", "TV-Y7"), "G")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("R", "TV-MA"), "MA")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("NOT RATED", "UNRATED", "APPROVED", "PASSED"), "NOT RATED")) %>%
  mutate(Rated = factor(Rated, levels=c("G", "PG", "MA", "NOT RATED"))) %>% 
  ggplot(aes(y=Sales, x=as.factor(Rated))) +
  theme_minimal() + 
  geom_boxplot() + 
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  labs(x="Classification Ratings")


#chisquared test
cs = mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Rated)) %>% 
  mutate(Rated = replace(Rated, Rated %in% c("PG", "PG-13", "TV-PG", "TV-14"), "PG")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("G", "TV-Y7"), "G")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("R", "TV-MA"), "MA")) %>%
  mutate(Rated = replace(Rated, Rated %in% c("NOT RATED", "UNRATED", "APPROVED", "PASSED"), "NOT RATED")) %>%
  mutate(Rated = factor(Rated, levels=c("G", "PG", "MA", "NOT RATED"))) %>% 
  group_by(Rated) %>%
  summarise(count = n()) %>%
  spread(Rated, count) %>%
  chisq.test()

cs

cs$observed
cs$expected
cs$residuals


###############################################################################
#-------Genre
###############################################################################


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  group_by(Genre) %>%
  summarise(count = n())


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  ggplot(aes(x=Genre)) +
    geom_bar() + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(y="# movies")





#genre by sales
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  ggplot(aes(y=Sales, x=as.factor(Genre))) +
  geom_boxplot() + 
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Genre")







#genre by budget
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  ggplot(aes(y=totBudget, x=as.factor(Genre))) +
  geom_boxplot() +
  scale_y_log10(labels = scales::dollar, breaks=budgetBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Budget)) %>%
  gather(Genre, GValid, G_Action:G_Western) %>% 
  filter(GValid == 1) %>% 
  ggplot(aes(y=Sales, x=Budget)) +
  #theme_minimal() + 
  geom_point(alpha=0.5) + 
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0, slope=1.66, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks, limits=c(10000, 1000000000)) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks, limits=c(10000, 1000000000)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~Genre)



mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Budget)) %>%
  mutate(Sales_Exceeds_Budget = Sales > Budget) %>%
  mutate(leverage = ID %in% outl) %>% 
  ggplot(aes(y=Sales, x=Budget)) +
  theme_minimal() +  
  geom_jitter(alpha=0.5, aes(color=Sales_Exceeds_Budget)) + 
  geom_text(nudge_y=-0.1, aes(label=ifelse(leverage, Title, ""))) +
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0, slope=1.66, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks, limits=c(10000, 1000000000)) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks, limits=c(10000, 1000000000)) +
  #expand_limits(x=c(0, 1000000000), y=c(0, 1000000000)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


###############################################################################
#------Total Theatre count
###############################################################################

mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  select(Theatres) %>%
  summary()

# totTheatreCount
# Min.   :    0  
# 1st Qu.:   15  
# Median :   78  
# Mean   : 2979  
# 3rd Qu.: 1282  
# Max.   :47135


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(Theatres == 0) %>%
  View


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(y=Theatres, x=as.factor(Year))) +
  geom_boxplot()


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(x=Theatres)) +
  geom_histogram() +
  facet_wrap(~Year)


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(x=Theatres, fill=as.factor(Year))) +
  geom_density(alpha=.2)


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Theatres)) %>%
  summarise(corr = cor(Sales, Theatres, use="complete.obs"))
#0.8334217


# show correlation between totTheatreCount and box office sales
tbreaks = c(0, 10, 100, 1000, 10000, 100000)

mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Theatres)) %>%
  ggplot(aes(y=Sales, x=Theatres)) +
  theme_minimal() +
  geom_jitter(alpha=0.5, width=0.5) + 
  geom_smooth(method="lm", se=TRUE) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  scale_x_log10(labels = scales::comma, breaks=tbreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#check wide release
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Theatres)) %>%
  mutate(wide = Theatres >= 600) %>%
  filter(wide == TRUE) %>%
  summarise(corr = cor(Sales, Theatres, use="complete.obs"))

mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Theatres)) %>%
  mutate(wide = Theatres >= 600) %>%
  ggplot(aes(y=Sales, x=Theatres, color=wide)) +
  geom_jitter(alpha=0.5, width=0.5) + 
  geom_smooth(method="lm", se=TRUE) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  scale_x_log10(labels = scales::comma, breaks=tbreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Theatres)) %>%
  filter(Studio %in% studioWRec) %>%
  mutate(wide = Theatres >= 600) %>%
  ggplot(aes(y=Sales, x=Theatres, color=wide)) +
  #theme_minimal() + 
  geom_jitter(alpha=0.5, width=0.5) + 
  geom_smooth(method="lm", se=TRUE) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  scale_x_log10(labels = scales::comma, breaks=tbreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~Studio)




mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Budget)) %>%
  plot_ly(z= ~log(Sales), x= ~log(Budget), y= ~log(Theatres), opacity=0.6) %>%
    add_markers() #%>%
    #add_surface(x= ~x, y= ~y, z= ~plane0, showscale=FALSE)
  

testdata = mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Budget)) %>%
  filter(!is.na(Theatres)) %>%
  mutate(wide = Theatres >= 600) 

  

movie1.mod = lm(log(Sales) ~ log(Budget) + log(Theatres), data=testdata)
summary(movie1.mod)

movie2.mod = lm(log(Sales) ~ log(Theatres), data=testdata)
summary(movie2.mod)

movie3.mod = lm(log(Sales) ~ log(Budget), data=testdata)
summary(movie3.mod)

movie5.mod = lm(log(Sales) ~ log(Budget) + log(Theatres) + IMDBRating, data=testdata)
summary(movie5.mod)

movie6.mod = lm(log(Sales) ~ log(Theatres) + wide, data=testdata)
summary(movie6.mod)


#with NA budget included (won't work because the model removes missing data - need to impute)
testdata2 = mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  #filter(!is.na(Budget)) %>%
  filter(!is.na(Sales)) %>% 
  filter(Theatres !=0) %>%
  mutate(wide = Theatres >= 600) 

write.csv(testdata2, "summary.csv")


movie12.mod = lm(log(Sales) ~ log(Theatres), data=testdata2)
summary(movie12.mod)


movie15.mod = lm(log(Sales) ~ log(Theatres) + IMDB_Rating, data=testdata2)
summary(movie15.mod)

movie16.mod = lm(log(Sales) ~ log(Theatres) + wide, data=testdata2)
summary(movie16.mod)




# show correlation between totTheatreCount and totBudget to check for collinearity
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Theatres)) %>%
  filter(!is.na(Budget)) %>%
  summarise(corr = cor(log(Budget), log(Theatres), use="complete.obs"))
#0.6068515

  
mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(!is.na(Theatres)) %>%
  filter(!is.na(Budget)) %>%
  ggplot(aes(y=Theatres, x=Budget)) +
  geom_jitter(alpha=0.5, width=0.5) + 
  geom_smooth(method="lm", se=TRUE) +
  scale_y_log10(labels=scales::comma, breaks=tbreaks) +
  scale_x_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


movie4.mod = lm(log(Theatres) ~ log(Budget), data=testdata)
summary(movie4.mod)


# install.packages("modelr")
# library(modelr)
# install.packages("broom")
# library(broom)

# grid = mclean %>%
#   filter(startYear %in% c(2015:2017)) %>%
#   data_grid(
#     totGross = seq_range(totGross, n=10),
#     totBudget = seq_range(totBudget, n=10),
#     totTheatreCount = seq_range(totTheatreCount, n=10)
#   )
# 
# testdata = mclean %>%
#   filter(startYear %in% c(2015:2017)) 
#   
#   
# movie.mod = lm(totGross ~ totBudget + totTheatreCount, data=testdata)
# summary(movie.mod)
# 
# tidy_plane = movie.mod %>%
#   augment(newdata = grid)

# x1 = unique(mclean$totBudget)
# 
# plane0 = tidy_plane %>%
#    pull(.fitted) %>%
#    matrix(nrow = length(3000), byrow = TRUE)



###############################################################################
#------Language
###############################################################################

#language by sales
mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  gather(Language, LValid, L_Aboriginal:L_Zulu) %>% 
  filter(LValid == 1) %>% 
  ggplot(aes(y=totGross, x=as.factor(Language))) +
  geom_boxplot() + 
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  gather(Language, LValid, L_Aboriginal:L_Zulu) %>% 
  filter(LValid == 1) %>%
  group_by(Language) %>%
  summarise(totCount = n()) %>%
  ggplot(aes(x=totCount)) +
    geom_histogram(binwidth = 5)


#list of langugage with movies over 20
keyLang = mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  gather(Language, LValid, L_Aboriginal:L_Zulu) %>% 
  filter(LValid == 1) %>%
  group_by(Language) %>%
  summarise(totCount = n()) %>%
  filter(totCount >= 20) %>% 
  filter(Language != "L_NA") %>% 
  select(Language)
  

keyLang = keyLang$Language


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  gather(Language, LValid, L_Aboriginal:L_Zulu) %>% 
  filter(LValid == 1) %>%
  filter(Language %in% keyLang) %>% 
  ggplot(aes(y=totGross, x=as.factor(Language))) +
  geom_boxplot() + 
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


langLong = mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  gather(Language, LValid, L_Aboriginal:L_Zulu) %>% 
  filter(LValid == 1) %>%
  filter(Language %in% keyLang) 

lang.aov = aov(totGross ~ Language, data=langLong)

summary(lang.aov)

#                 Df    Sum Sq   Mean Sq F value Pr(>F)  
#   Language      13 9.952e+16 7.656e+15   1.775 0.0415 *
#   Residuals   2340 1.009e+19 4.313e+15 

# just barely significant that a difference in language causes a change in sales.




# #genre by budget
# mclean %>%
#   filter(startYear %in% c(2015:2017)) %>%
#   gather(Genre, GValid, G_Action:G_Western) %>% 
#   filter(GValid == 1) %>% 
#   ggplot(aes(y=totBudget, x=as.factor(Genre))) +
#   geom_boxplot() +
#   scale_y_log10(labels = scales::dollar, breaks=budgetBreaks) +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# 
# 
# mclean %>%
#   filter(startYear %in% c(2015:2017)) %>%
#   filter(!is.na(totBudget)) %>%
#   gather(Genre, GValid, G_Action:G_Western) %>% 
#   filter(GValid == 1) %>% 
#   ggplot(aes(y=totGross, x=totBudget)) +
#   geom_point(alpha=0.5) + 
#   geom_abline(intercept = 0, colour="red") +
#   geom_abline(intercept = 0, slope=1.66, colour="green") +
#   geom_smooth(method="lm", se=TRUE) +
#   scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
#   scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
#   facet_wrap(~Genre)



###############################################################################
#------Studio
###############################################################################
mclean %>%
  group_by(Studio) %>%
  summarise(total=n()) %>%
  arrange(desc(total))



#----check missing data for chosen model


mclean %>% 
  select(X.1, 
         Title, 
         totTheatreCount, 
         totGross, 
         totBudget,
         IMDBRating
  ) %>%
  missmap()


mclean %>% 
  select(X.1, 
         Title, 
         totTheatreCount, 
         totGross, 
         totBudget,
         IMDBRating
  ) %>%
  summary()


mclean %>% 
  select(X.1, 
         Title, 
         totTheatreCount, 
         totGross, 
         totBudget,
         IMDBRating,
         RTRating,
         Metacritic,
         imdbID
  ) %>%
  filter(!is.na(IMDBRating)) %>% 
  filter(!is.na(totGross)) %>%
  filter(!is.na(totBudget)) %>%
  summarise(count=n())
#down to 872 counts

mclean %>% 
  select(X.1, 
         Title, 
         totTheatreCount, 
         totGross, 
         totBudget,
         IMDBRating
  ) %>%
  filter(!is.na(IMDBRating)) %>% 
  filter(!is.na(totGross)) %>%
  filter(!is.na(totBudget)) %>%
  missmap()





#STAGE 3 - DATA PREPARATION
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


#####################################################################################
#  --------------MODEL 1
######################################################################################

train = mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  #filter(!is.na(IMDBRating)) %>% 
  filter(Theatres !=0) %>%
  filter(!is.na(Sales)) %>%
  filter(!is.na(Budget))

movie.form = as.formula("log(Sales) ~ log(Budget) + log(Theatres)")

movie.mod = lm(movie.form, data=train)
summary(movie.mod)

plot(movie.mod)

#check outliers:
out1 = train %>%
  filter(row_number(ID) %in% c(614, 437, 271, 3, 110)) %>% 
  select(ID)


out2 = train %>%
  filter(Budget < 200000 & Sales > 1000000) %>% 
  select(ID)

out3 = train %>%
  filter(Budget > 1000000 & Sales < 5000) %>% 
  select(ID)


outl = rbind(out1, out2, out3)
  
  
train %>%
  mutate(leverage = ID %in% outl$ID) %>% 
  filter(Year %in% c(2015:2017)) %>%
  ggplot(aes(y=Sales, x=Budget)) +
  geom_point(alpha=0.5, aes(color=leverage)) + 
  geom_text(aes(label=ifelse(leverage, Title, ""))) +
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0, slope=1.66, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


train$pred = predict(movie.mod , newdata=train)
rmse(train$pred, log(train$Sales))
#0.6651602


train %>%
ggplot(aes(x=pred, y=log(totGross))) +
  geom_point() +
  geom_abline(color="red")



test = mclean2 %>%
  filter(Year %in% c(2018)) %>%
  filter(Theatres !=0) %>%
  filter(!is.na(Sales)) %>%
  filter(!is.na(Budget))


test$pred = predict(movie.mod, newdata=test)
rmse(test$pred, log(test$Sales))
test.rho = cor(test$pred, log(test$Sales))
test.rho2 = test.rho^2

test %>%
  ggplot(aes(x=pred, y=log(Sales))) +
  geom_point() +
  geom_abline(color="red")


install.packages("vtreat")
library(vtreat)


cvtrain = mclean2 %>%
  filter(Theatres !=0) %>%
  filter(!is.na(Sales)) %>%
  filter(!is.na(Budget))


splitPlan = kWayCrossValidation(nRows=nrow(cvtrain), nSplits = 3, NULL, NULL)

k = 3 # Number of folds
cvtrain$pred.cv = 0 
for(i in 1:k) {
  split = splitPlan[[i]]
  model.cv = lm(movie.form, data = cvtrain[split$train,])
  cvtrain$pred.cv[split$app] = predict(model.cv, newdata = cvtrain[split$app,])
}

cvtrain %>%
  ggplot(aes(x=pred.cv, y=log(Sales))) +
  geom_point() +
  geom_abline(color="red")

rmse(cvtrain$pred.cv, log(cvtrain$Sales))
#0.9352242
cvtrain.rho = cor(cvtrain$pred.cv, log(cvtrain$Sales))
cvtrain.rho2 = cvtrain.rho^2
cvtrain.rho2
#0.8930827



#####################################################################################
#  --------------MODEL 2
######################################################################################

train2 = mclean2 %>%
  filter(Year %in% c(2015:2017)) %>%
  filter(Theatres !=0) %>%
  filter(!is.na(Sales)) 
  #filter(!is.na(Budget))

movie.form2 = as.formula("log(Sales) ~ log(Theatres)")

movie.mod2 = lm(movie.form2, data=train2)
summary(movie.mod2)

plot(movie.mod2)


train2$pred = predict(movie.mod2 , newdata=train2)
rmse(train2$pred, log(train2$Sales))
#0.8392449


train2 %>%
  ggplot(aes(x=pred, y=log(Sales))) +
  geom_point() +
  geom_abline(color="red")



test2 = mclean2 %>%
  filter(Year %in% c(2018)) %>%
  filter(Theatres !=0) %>%
  filter(!is.na(Sales)) 


test2$pred = predict(movie.mod2, newdata=test2)
rmse(test2$pred, log(test2$Sales))
#0.8752811
test.rho = cor(test2$pred, log(test2$Sales))
test.rho2 = test.rho^2
test.rho2

test2 %>%
  ggplot(aes(x=pred, y=log(Sales))) +
  geom_point() +
  geom_abline(color="red")


install.packages("vtreat")
library(vtreat)


cvtrain2 = mclean2 %>%
  filter(Theatres !=0) %>%
  filter(!is.na(Sales)) 


splitPlan = kWayCrossValidation(nRows=nrow(cvtrain2), nSplits = 3, NULL, NULL)

k = 3 # Number of folds
cvtrain2$pred.cv = 0 
for(i in 1:k) {
  split = splitPlan[[i]]
  model.cv = lm(movie.form2, data = cvtrain2[split$train,])
  cvtrain2$pred.cv[split$app] = predict(model.cv, newdata = cvtrain2[split$app,])
}

cvtrain2 %>%
  ggplot(aes(x=pred.cv, y=log(Sales))) +
  geom_point() +
  geom_abline(color="red")

rmse(cvtrain2$pred.cv, log(cvtrain2$Sales))
#1.007917
cvtrain2.rho = cor(cvtrain2$pred.cv, log(cvtrain2$Sales))
cvtrain2.rho2 = cvtrain2.rho^2
cvtrain2.rho2
#0.9032991


#check collinearity
cvtrain %>%
  select(Sales,
         Budget,
         Theatres,
         IMDB_Rating) %>%
  mutate(Sales = log(Sales),
         Budget = log(Budget),
         Theatres = log(Theatres)
         ) %>%
  pairs()


install.packages("corrplot")
library(corrplot)

corrcheck = cvtrain %>%
  select(Sales,
         Budget,
         Theatres,
         IMDB_Rating) %>%
  mutate(Sales = log(Sales),
         Budget = log(Budget),
         Theatres = log(Theatres)
  ) 

corrplot.mixed(cor(corrcheck))




#now find out what is going on with movies above the red line.


mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(IMDBRating)) %>% 
  filter(!is.na(totGross)) %>%
  filter(!is.na(totBudget)) %>%
  mutate(good = totGross > totBudget) %>% 
  ggplot(aes(y=totGross, x=totBudget)) +
  geom_point(alpha=0.5, aes(color=good)) + 
  #geom_text(aes(label=ifelse(leverage, Title, ""))) +
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0, slope=1.66, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


mgood = mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(IMDBRating)) %>% 
  filter(!is.na(totGross)) %>%
  filter(!is.na(totBudget)) %>%
  mutate(good = totGross > totBudget) %>%
  filter(good == TRUE)


mgood %>%
  ggplot(aes(y=totGross, x=totBudget)) +
  geom_point(alpha=0.5, aes(color=good)) + 
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0, slope=1.66, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



mgood.form = as.formula("log(totGross) ~ log(totBudget)")

mgood.mod = lm(mgood.form, data=mgood)
summary(mgood.mod)

plot(mgood.mod)

#check high leverage
mgood %>%
  mutate(leverage = row_number(X.1) %in% c(207, 136, 198)) %>% 
  # X.1       Title
  # 1 1629        NH10
  # 2 2282 The D Train
  # 3 2367 The Gallows
  ggplot(aes(y=totGross, x=totBudget)) +
  geom_point(alpha=0.5, aes(color=leverage)) + 
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0, slope=1.66, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



good.corr = mgood %>%
  select(totGross,
         totBudget,
         totTheatreCount,
         IMDBRating) %>%
  mutate(totGross = log(totGross),
         totBudget = log(totBudget),
         totTheatreCount = log(totTheatreCount)
  ) 

corrplot.mixed(cor(good.corr))


#check poor movies:
mbad = mclean %>%
  filter(startYear %in% c(2015:2017)) %>%
  filter(!is.na(IMDBRating)) %>% 
  filter(!is.na(totGross)) %>%
  filter(!is.na(totBudget)) %>%
  mutate(good = totGross > totBudget) %>%
  filter(good == FALSE)


mbad %>%
  ggplot(aes(y=totGross, x=totBudget)) +
  geom_point(alpha=0.5, aes(color=good)) + 
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0, slope=1.66, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



mbad.form = as.formula("log(totGross) ~ log(totBudget)")

mbad.mod = lm(mbad.form, data=mbad)
summary(mbad.mod)

plot(mbad.mod)


#check high leverage
mbad %>%
  mutate(leverage = row_number(X.1) %in% c(359, 256, 160)) %>%  
  filter(leverage == TRUE) %>%
  select (X.1, imdbID, Title, totTheatreCount, totBudget, totGross)
#   X.1                                               Title  totTheatreCount  totBudget   totGross
# 1 1303                                         Lazer Team              50   2,480,421  1,281,800
# 2 2150 Tad the Lost Explorer and the Secret of King Midas             100  10,778,400      5,900
# 3 2967                                         Wolf Totem               2  38,000,000    224,100
  ggplot(aes(y=totGross, x=totBudget)) +
  geom_point(alpha=0.5, aes(color=leverage)) + 
  geom_abline(intercept = 0, colour="red") +
  geom_abline(intercept = 0, slope=1.66, colour="green") +
  geom_smooth(method="lm", se=TRUE) +
  geom_text(aes(label=ifelse(leverage, Title, ""))) +
  scale_x_log10(labels = scales::dollar, breaks=budgetBreaks) +
  scale_y_log10(labels = scales::dollar, breaks=salesBreaks) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))





