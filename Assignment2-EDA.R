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


mcombined = read.csv("MovieListCombined.csv", stringsAsFactors = FALSE)

#-----------------------------------------------------------------------------
#STAGE 1 - DATA CLEANSING
#-----------------------------------------------------------------------------

View(mcombined)

install.packages("naniar")
library(naniar)

na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available")


cdata = mcombined %>%
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  #split Genre to different columns
  mutate(Genre = strsplit(as.character(Genre), ",")) %>% 
  unnest(Genre) %>% 
  mutate(GenreTrue = 1,
         Genre = paste("G", Genre, sep="_")) %>% 
  arrange(Genre) %>% 
  spread(Genre, GenreTrue) %>%
  mutate(Awards = !is.na(Awards)) %>% 
  arrange(X.1) %>% 
  select(X.1, 
         Title, 
         Studio, 
         totTheatreCount, 
         totGross, 
         totBudget, 
         WeeksOn, 
         startYear, 
         startWeek,
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
  )

write.csv(cdata, "MovieClean.csv")


cdata2 = cdata %>%
  #split Language to different columns
  #mutate(LanguageX = 
  #          strsplit(gsub(" ", "", as.character(Language)), ",")  
  #        
  #    ) %>% 
  #unnest(Language) %>% 
  select(X.1, Language) %>%
  mutate(Language = gsub(" ", "", as.character(Language))) %>%
  separate(Language, c("L1", "L2", "L3", "L4"), sep=",") %>% 
  gather(LKey, LTerm, -X.1) %>% 
  mutate(
     LTerm = paste("L", LTerm, sep="_"),
     LanguageTrue = 1
         ) %>% 
  #group_by(X.1) %>% 
  # mutate(row_id =1:n()) %>%
  # ungroup() %>%
  spread(LTerm, LanguageTrue) %>% 
  arrange(X.1) %>% View
  

write.csv(cdata2, "MovieClean.csv")
  
  
  #split Country to different columns
  mutate(Country = strsplit(as.character(Country), ",")) %>% 
  unnest(Country) %>% 
  mutate(CountryTrue = 1,
         Country = paste("C", Country, sep="_")) %>% 
  arrange(Country) %>% 
  spread(Country, CountryTrue) %>% 
  arrange(X.1) %>% View


  

write.csv(cdata, "MovieClean.csv")


val = c("a, b, c,a")
strsplit(val,",")





#STAGE 2 - DATA UNDERSTANDING
#check distribution of each variable
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


