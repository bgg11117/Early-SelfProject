rm(list=ls())

library(dplyr)
library(DT)
library(plotly)
library(tidyr)
library(wesanderson)
library(data.table)
library(ggplot2)
library(ggrepel)

videogame <- read.csv("./input/vgsales.csv")
summary(videogame)

#Feature Engineering
videogame <- videogame[!(videogame$Year %in% c("N/A", "2017", "2020","Adventure")),]
videogame <- videogame %>% gather(Region, Revenue, c(7:10))
videogame$Region <- as.factor(videogame$Region)

#theme
theme1 <- function() {
  return(theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4), 
               plot.title = element_text(size = 15, vjust = 2),
               panel.background = element_blank(),
               axis.line.x = element_line(color="black", size = 0.25),
               axis.line.y = element_line(color="black", size = 0.25),
               axis.ticks = element_blank(),
               axis.title.x = element_text(size = 12, vjust = -0.35)))
}

theme2 <- function() {
  return(theme(axis.text.x = element_text(size = 10, vjust = 0.4), 
         plot.title = element_text(size = 15, vjust = 2),
         panel.background = element_blank(),
         axis.line.x = element_line(color="black", size = 0.25),
         axis.line.y = element_line(color="black", size = 0.25),
         axis.ticks = element_blank(),
         axis.title.x = element_text(size = 12, vjust = -0.35)))
}

## Colors
mycolors <- c("#771C19", "#AA3929", "#8E9CA3", "#556670", "#000000",
              "#E25033", "#F27314", "#F8A31B", "#E2C59F", "#B6C5CC")


#Revenue & releases
  
revenue_by_year <- videogame %>% 
  group_by(Year) %>%
  summarize(Global_Sales= sum(Global_Sales))

ggplot(revenue_by_year, aes(Year, Global_Sales,group=1)) + 
  geom_line(color='blue') +
  theme1() +
  ggtitle("Video Game Global_Sales by Year")

ggplot(videogame, aes(Year)) + 
  geom_bar(fill = "blue") +
  theme1()+
  ggtitle("Video Game Releases by Year")

#Top Genre by Revenue each Year

topg <- videogame %>% 
  group_by(Year, Genre) %>% 
  summarize(Revenue = sum(Global_Sales)) %>%
  arrange(desc(Year))%>%
  top_n(1)

datatable(topg)

ggplot(topg, aes(Year, Revenue, fill = Genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top Genre by Revenue each Year") +
  theme1() +
  theme(legend.position = "top") 

#Top Publisher by Rev each Year

top_publisher_year <- videogame %>% 
  group_by(Year, Publisher) %>% 
  summarize(Revenue = sum(Global_Sales)) %>%
  arrange(desc(Year))%>% top_n(1)

datatable(top_publisher_year)

ggplot(top_publisher_year, aes(Year, Revenue, fill = Publisher)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top Publisher by Revenue each Year") +
  theme1() +
  theme(legend.position = "top")

#Top Platform by Revenue each year

top_platforms <- videogame %>%
  group_by(Year, Platform) %>%
  summarize(Revenue = sum(Global_Sales)) %>%
  arrange(desc(Revenue)) %>%
  top_n(1)

datatable(top_platforms)

ggplot(top_platforms, aes(Year, Revenue, fill = Platform)) + 
  geom_bar(stat = "identity") +
  theme1() +
  ggtitle("Top Platform by Revenue each year") +
  theme(legend.position = "top") 


#Top Games by Rev each year

top_games <- videogame %>%
  group_by(Year, Name) %>%
  summarize(Revenue = sum(Global_Sales)) %>%
  arrange(desc(Revenue)) %>%
  top_n(1)

datatable(top_games)


ggplot(top_games, aes(Year, Revenue, fill = Name)) + 
  geom_bar(stat = "identity") +
  theme1() +
  ggtitle("Total Games by Revenue each year") +
  theme(legend.position = "top")

#top publishers by number of releases

length(unique(videogame$Publisher))

publishers <- videogame %>% group_by(Publisher) %>% 
  summarize(Total = n()) %>%  arrange(desc(Total)) %>% top_n(10)
publishers$Percentage <- round(publishers$Total/dim(videogame)[1] * 100,2)
publishers$Publisher <- as.factor(publishers$Publisher)

datatable(publishers)

ggplot(publishers, aes(reorder(Publisher, Total), Total, fill = Publisher)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top 10 Publishers by Number of Releases") + 
  theme(legend.position = "none") + 
  xlab("Publisher") +
  theme2() +
  coord_flip()

#top 10 publishers by Total Revenue

top_publishers <- videogame %>% group_by(Publisher) %>% 
  summarize(Revenue = sum(Global_Sales), 
            Percentage = Revenue/sum(videogame$Global_Sales) * 100) %>% 
  arrange(desc(Revenue)) %>% top_n(10)

top_publishers$Publisher <- factor(top_publishers$Publisher)

datatable(top_publishers)

ggplot(top_publishers, aes(reorder(Publisher, Revenue), Revenue, fill = Publisher)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top 5 Publishers by Revenue") + 
  theme(legend.position = "none") + 
  xlab("Publisher") +
  ylab("Revenue in millions") +
  theme2() +
  coord_flip()

# publisher avg rev by release

pub_avgrev <- videogame %>% group_by(Publisher) %>% 
  summarize(count=n(),
            Revenue = sum(Global_Sales), 
            avg_rev = round(Revenue/count,2)) %>% 
  arrange(desc(Revenue)) %>% head(10)

pub_avgrev$Publisher <- factor(pub_avgrev$Publisher)

datatable(pub_avgrev)

ggplot(pub_avgrev, aes(reorder(Publisher,avg_rev), avg_rev, fill = Publisher)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top 10 Publishers by avg_Revenue") + 
  theme(legend.position = "none") + 
  xlab("Publisher") +
  ylab("Revenue in millions") +
  theme2() +
  coord_flip()

#top 10 publishers have grown over the years by number of releases

top_publishers <- videogame[videogame$Publisher %in% publishers$Publisher,] %>% 
  group_by(Publisher, Year) %>% summarize(Total= n())

top_publishers$Publisher <- factor(top_publishers$Publisher)

ggplot(top_publishers, aes(Year, Total, group=Publisher,color=Publisher)) + 
  geom_line() + theme1() +  ggtitle("Top 10 Publishers Releases by Year") +
  xlab("Year") +  theme(legend.position = "top")

#top 10 publishers have grown over the years for revenue

top_publishers <- videogame[videogame$Publisher %in% publishers$Publisher,] %>% 
  group_by(Publisher, Year) %>%   summarize(Revenue = sum(Global_Sales))

top_publishers$Publisher <- factor(top_publishers$Publisher)

ggplot(top_publishers, aes(Year, Revenue, group=Publisher,color=Publisher)) + 
  geom_line(size=2) + theme1() +  ggtitle("Top 10 Publishers by Revenue") +
  xlab("Year") +  theme(legend.position = "top")

#top 10 publishers have grown over the years for avg_revenue release

avg_publishers <- videogame[videogame$Publisher %in% publishers$Publisher,] %>% 
  group_by( Year,Publisher) %>%  
  summarize(count = n(),Total=sum(Global_Sales),avg_rev = mean(Global_Sales))

avg_publishers$Publisher <- factor(avg_publishers$Publisher)

ggplot(avg_publishers, aes(Year, avg_rev, group=Publisher,color=Publisher)) + 
  geom_line(size=2) + theme1() +  ggtitle("Top 10 Publishers by avg_Revenue") +
  xlab("Year") +  theme(legend.position = "top") +scale_color_manual(values = mycolors)

#Genres by Number of Releases

by_genres <- videogame %>% 
  group_by(Genre) %>% 
  summarize(Total = n(), Percentage = Total/dim(videogame)[1] * 100) %>%  
  arrange(desc(Percentage))

datatable(by_genres)

ggplot(by_genres, aes(reorder(Genre, -Total), Total, fill = Genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Video Games Genres by Number of Releases") +
  ylab("Percentage") +
  xlab("Genres") +
  theme(legend.position = "none") + 
  theme1()

#Genres by Revenue

by_genres <- videogame %>% 
  group_by(Genre) %>% 
  summarize(Revenue = sum(Global_Sales), Percentage = Revenue/sum(videogame$Global_Sales) * 100) %>%  
  arrange(desc(Percentage))

datatable(by_genres)


ggplot(by_genres, aes(reorder(Genre, -Revenue), Revenue, fill = Genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Video Games Genres by Revenue") +
  ylab("Revenue in Millions") +
  xlab("Genres") +
  theme(legend.position = "none") + 
  theme1()

#genre generated most revenue per release

revenue_per_release <- videogame %>% 
  group_by(Genre) %>% 
  summarize(Total = n(), Revenue = sum(Global_Sales), RevenuePerRelease = Revenue/Total) %>%  
  arrange(desc(RevenuePerRelease))

datatable(revenue_per_release)

ggplot(revenue_per_release, aes(reorder(Genre, -RevenuePerRelease), RevenuePerRelease, fill = Genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Video Games Genre Ranking based on Revenue Per Release") +
  ylab("Revenue in Millions") +
  xlab("Genres") +
  theme(legend.position = "none") + 
  theme1()

#Top 10 Games by Revenue

top_games <- videogame %>%
  group_by(Name) %>%
  summarize(Revenue = sum(Global_Sales), Percentage = Revenue/sum(videogame$Global_Sales) * 100) %>%
  arrange(desc(Revenue)) %>%
  top_n(10)

datatable(top_games)

ggplot(top_games, aes(reorder(Name, Revenue), Revenue, fill = Name)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Games by Revenue") +
  ylab("Revenue in Millions") +
  xlab("Games") +
  theme2() +
  theme(legend.position = "none") +
  coord_flip()

#Sales Revenue by regions

by_regions <- videogame %>% 
  group_by(Region) %>%
  summarize(TotalRevenue = sum(Revenue), Percentage = TotalRevenue/sum(videogame$Revenue) * 100) %>%
  arrange(desc(TotalRevenue))

datatable(by_regions)

ggplot(by_regions, aes(reorder(Region,TotalRevenue),TotalRevenue, fill = Region)) + 
  geom_bar(stat = "identity") +
  theme2() +
  ggtitle("Total Revenue by Region") +
  theme(legend.position = "top")+coord_flip()

#Distribution of Sales Revenue by regions

ggplot(videogame, aes(Region, Revenue, fill = Region)) + 
  geom_boxplot() +
  scale_y_log10() +
  theme2() +
  ggtitle("Distribution of Sales Revenue") +
  theme(legend.position = "top") 
 

#Distribution of Sales Revenue by Year

ggplot(videogame, aes(Year, Revenue)) + 
  geom_boxplot(fill = "yellow") +
  scale_y_log10() +
  theme2() +
  ggtitle("Distribution of Sales Revenue - Year") +
  theme(legend.position = "top") +
  coord_flip()

#Sales Revenue by regions and year

by_region_year <- videogame %>% 
  group_by(Year, Region) %>%
  summarize(TotalRevenue = sum(Revenue)) %>%
  arrange(desc(TotalRevenue))
by_region_year$Region <- factor(by_region_year$Region)
by_region_year$Year <- factor(by_region_year$Year)

datatable(by_region_year)

ggplot(by_region_year, aes(Year, TotalRevenue, group=Region,color = Region)) +
  geom_line()+
  theme1() +
  ggtitle("Total Revenue by Region and Year") +
  theme(legend.position = "top")

#Top 3 Publishers by Revenue in each Region

top_publishers_region <- videogame %>%
  group_by(Region, Publisher) %>%
  summarize(Revenue = sum(Revenue)) %>%
  arrange(desc(Revenue)) %>%
  top_n(3)

datatable(top_publishers_region)

ggplot(top_publishers_region, aes(Region, Revenue, fill = Publisher)) + 
  geom_bar( stat = "identity")  +  #?i?[?Jposition = "dodge"?bgeom_bar?̭?
  ggtitle("Top 3 Publishers by Revenue in each Region") +
  ylab("Revenue in Millions") +
  xlab("Region") +
  theme2() +
  theme(legend.position = "top")

### Top 3 Games by Revenue in each Region

top_games_region <- videogame %>%
  group_by(Region, Name) %>%
  summarize(Revenue = sum(Revenue)) %>%
  arrange(desc(Revenue)) %>%
  top_n(3)

datatable(top_games_region)

ggplot(top_games_region, aes(Region, Revenue, fill = Name)) + 
  geom_bar(position = "dodge", stat = "identity")  +
  ggtitle("Top 3 Games by Revenue in each Region") +
  ylab("Revenue in Millions") +
  xlab("Region") +
  theme2() +
  theme(legend.position = "top")

#Top 3 Genres by Revenue in each Region

top_genres_region <- videogame %>%
  group_by(Region, Genre) %>%
  summarize(Revenue = sum(Revenue)) %>%
  arrange(desc(Revenue)) %>%
  top_n(3)

datatable(top_genres_region)

ggplot(top_genres_region, aes(reorder(Region, Revenue),Revenue, fill = Genre)) + 
  geom_bar(position = "dodge", stat = "identity")  +
  ggtitle("Top 3 Genres by Revenue in each Region") +
  ylab("Revenue in Millions") +
  xlab("Region") +
  theme2() +
  theme(legend.position = "top")+coord_flip()

### Top 3 Years by Revenue in each Region

top_year_region <- videogame %>%
  group_by(Region, Year) %>%
  summarize(Revenue = sum(Revenue)) %>%
  arrange(desc(Revenue)) %>%
  top_n(3)

datatable(top_year_region)

ggplot(top_year_region, aes(Region, Revenue, fill = Year)) + 
  geom_bar(position = "dodge", stat = "identity")  +
  ggtitle("Top 3 Years by Revenue in each Region") +
  ylab("Revenue in Millions") +
  xlab("Region") +
  theme2() +
  theme(legend.position = "top")+coord_flip() 


#Top 3 Platforms by Revenue in each Region

top_platform_region <- videogame %>%
  group_by(Region, Platform) %>%
  summarize(Revenue = sum(Revenue)) %>%
  arrange(desc(Revenue)) %>%
  top_n(3)

datatable(top_platform_region)

ggplot(top_platform_region, aes(Region, Revenue, fill = Platform)) + 
  geom_bar(position = "dodge", stat = "identity")  +
  ggtitle("Top 3 Platform by Revenue in each Region") +
  ylab("Revenue in Millions") +
  xlab("Region") +
  theme2() +
  theme(legend.position = "top") 

### line plot 

year_genre <- videogame %>% 
  group_by(Year, Genre) %>% 
  summarise(TotalRevenue = sum(Revenue)) 

ggplot(year_genre, aes(Year, TotalRevenue,group=Genre, color =Genre)) +
  geom_line(size=1.5) +
  ggtitle("Total Revenue by Year and Genre") + 
  theme1()

### treemap
genre <- videogame %>% 
  group_by(Genre) %>% 
  summarise(TotalRevenue = sum(Revenue)) 

p <- hchart(genre , x= Genre, value= TotalRevenue, color= TotalRevenue , type='treemap')%>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, tstyle = list(fontSize = "8px")) %>%
  hc_title(text = "Total Revenue by Genre")

export_hc(p,"treemap.png")
