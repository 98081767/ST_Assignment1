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
