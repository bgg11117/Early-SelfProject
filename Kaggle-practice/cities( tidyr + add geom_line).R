library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(readr)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

df <- read_csv("./input/new/cities_r2.csv")
str(df)
df %>% group_by(state_name) %>% summarise(total_cities = n_distinct(name_of_city)) %>%
  arrange(desc(total_cities)) %>% 
  ggplot(aes(reorder(x = state_name, -total_cities), y = total_cities))+
  geom_bar(aes(fill = state_name), stat = "identity")+  theme_few()+
  ggtitle('Number of Cities Per State')+xlab('States') + ylab("Total Cities")+
  theme(axis.text.x = element_text(angle=90, hjust=1),  legend.position = "none")

df_literate<-df %>%   group_by(state_name) %>% 
  summarise(female = sum(literates_female), male = sum(literates_male))

mdata<-gather(df_literate, key,value,-state_name)

ggplot(mdata,aes(reorder(x = state_name, -value), y = value, group = key,color=key))+
  geom_line()+ theme_few()+
  ggtitle("Total Number of literates(Male Vs Female)")+xlab("State")+
  ylab("Total Number of Literates")+
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(df,aes(x = name_of_city, group = 1 ))+
  geom_line(aes(y = effective_literacy_rate_female, colour = "Female")) +
  geom_line(aes(y = effective_literacy_rate_male, colour = "Male")) + theme_few()+
  ggtitle("Literacy Rate Comparison(Male Vs Female)")+xlab("Cities")+
  ylab("Literacy Rate")+  theme(axis.text.x = element_text(angle=90, hjust=1))