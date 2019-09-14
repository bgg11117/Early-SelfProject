#Movie.Data <- read.csv("../input/movie_metadata.csv", header = TRUE, stringsAsFactors = FALSE)

Movie <- read.csv('movie_metadata.csv',header = TRUE, stringsAsFactors = FALSE)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

Movie <- Movie[!duplicated(Movie$movie_title),]
#Add profit and ROI
Movie <- Movie %>% mutate(profit = gross-budget,ROI = (profit/gross)*100)

profit20 <- Movie %>% arrange(desc(profit)) %>% 
                      top_n(20, profit)
#Plot
ggplot(profit20, aes(x=budget/1000000, y=profit/1000000)) +
  geom_point(size = 1) + geom_text_repel(aes(label = movie_title), size = 2) + 
  xlab("Budget $million") + ylab("Profit $million") +
  ggtitle("20 Most Profitable Movies")

#Top 20 based on ROI
roi20 <- Movie %>% 
  filter(budget >10000) %>% 
  arrange(desc(ROI)) %>% 
  top_n(20, ROI)

#Plot
ggplot(roi20, aes(x=budget/1000000, y=profit/1000000)) + 
  geom_point(size = 1) + geom_text_repel(aes(label = movie_title), size = 2)  + 
  xlab("Budget $million") + ylab("Profit $million") + 
  ggtitle("top 20 Movies Based on ROI")

#Top 20 most profitable directors
#use as.numric in the sum to avoid integer overflow
directors20 <- Movie %>% 
  group_by(director_name) %>%
  select(director_name, budget, gross, profit) %>%
  na.omit() %>% 
  summarise(films = n(), budget = sum(as.numeric(budget)), 
            gross = sum(as.numeric(gross)), profit = sum(as.numeric(profit))) %>% 
  arrange(desc(profit)) %>% 
  top_n(20, profit)

#Plot
ggplot(directors20, aes(x=films, y=profit/1000000)) +
  geom_point(size = 1) + geom_text_repel(aes(label = director_name), size = 2) + 
  xlab("Number of Films") + ylab("Profit $millions") + 
  ggtitle("Most Profitable Directors")

#Top 20 most profitable directors - average per film
avgdirectors20 <- directors20 %>% 
  mutate(avg_per_film = profit/films) %>%
  arrange(desc(avg_per_film)) %>% 
  top_n(20, avg_per_film)

#Plot
ggplot(avgdirectors20, aes(x=films, y=avg_per_film/1000000)) + 
  geom_point(size = 1) + geom_text_repel(aes(label = director_name), size = 2) + 
  xlab("Number of Films") + ylab("Avg Profit per Film $millions") + 
  ggtitle("Most Profitable Directors - Avg per Film")


#Deeper analysis on Steven Speilberg
spielberg <- Movie %>% 
  filter(director_name == "Steven Spielberg") %>% 
  select(title_year, profit, movie_title) %>% 
  na.omit() %>% 
  arrange(desc(title_year))

#Plot
ggplot(spielberg, aes(x=title_year, y=profit/1000000)) + geom_point() + 
  geom_text_repel(aes(label = movie_title), size = 2) + xlab("Year") +
  ylab("Profit per Film $millions") + ggtitle("Steven Spielberg Films") + 
  geom_hline(yintercept = 0, linetype = 3, alpha = 0.9) +
  geom_hline(yintercept = 100, linetype = 2, alpha = 0.6)

#Deeper analysis on Nolan
nolan <- Movie %>% 
  filter(director_name == "Christopher Nolan") %>% 
  select(title_year, profit, movie_title) %>% 
  na.omit() %>% 
  arrange(desc(title_year))

#Plot
ggplot(nolan, aes(x=title_year, y=profit/1000000)) + geom_line() + 
  geom_text_repel(aes(label = movie_title), size = 5) + xlab("Year") +
  ylab("Profit per Film $millions") + ggtitle("Christopher Nolan Films") + 
  geom_hline(yintercept = 0, linetype = 3, alpha = 0.9) +
  geom_hline(yintercept = 100, linetype = 2, alpha = 0.6)

#data processing

Movie <- read.csv('movie_metadata.csv',header = TRUE, stringsAsFactors = TRUE)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

Movie <- Movie[!duplicated(Movie$movie_title),]
#Add profit and ROI
Movie <- Movie %>% mutate(profit = gross-budget,ROI = (profit/gross)*100)

Movie$content_rating[Movie$content_rating == 'GP'] <- 'PG'
Movie$content_rating[Movie$content_rating == 'TV-14'] <- 'PG-13'
Movie$content_rating[Movie$content_rating == 'TV-G'] <- 'G'
Movie$content_rating[Movie$content_rating == 'TV-PG'] <- 'PG'
Movie$content_rating[Movie$content_rating == 'X'] <- 'R'
Movie$content_rating[Movie$content_rating == 'M'] <- 'PG'
Movie$content_rating <- droplevels(Movie$content_rating)

avgbox <- Movie %>% group_by(content_rating)%>%
  select(content_rating,budget , gross , profit , imdb_score)%>%
  na.omit() %>%
  summarise(films = n() , avgbudget = mean(as.numeric(budget)),
            avggross = mean(as.numeric(gross)), avgimdb_score = mean(as.numeric(imdb_score)))%>% 
  filter(content_rating %in% c("G",'PG','PG-13','R','NC-17','TV-14')) %>%
  arrange(desc(avggross))

#The column you are trying to summarize with geom_bar() is already summarized. 
#You need to change stat=stat_count (default in geom_bar()) to identity.geom_bar(stat="identity")

ggplot(aes(x = content_rating , y= avggross),data = avgbox )+
  geom_bar(stat="identity")+xlab('content-rating') +
  ylab('avgbox')+
  ggtitle('Avgbox by content-rating')

#-----------------------------------
library(plotly)
library(dtplyr)
library(formattable)
library(ggplot2)
library(stats)
library(graphics)
movie <- read.csv('movie_metadata.csv',header=T,stringsAsFactors = F)
str(movie)
dim(movie)

temp <- movie %>% select(movie_title,title_year)
temp <- temp %>% group_by(title_year) %>% summarise(n=n())
temp <- na.omit(temp)
temp <- movie %>% select(content_rating,imdb_score)
temp <- temp %>% group_by(content_rating)%>% summarise(score = mean(imdb_score))
p <- plot_ly(
  x = temp$content_rating,
  y = temp$score,
  name = "Avg score by Rating",
  type = "bar")
p