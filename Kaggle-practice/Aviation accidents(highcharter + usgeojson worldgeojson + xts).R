library("viridisLite")
library(highcharter)
library(ggplot2)
library(dplyr)
library(tidyr)
library(countrycode)
library(PerformanceAnalytics)
library(readr)

data("usgeojson")
data(worldgeojson)

accidents <- read_csv("./input/new/AviationDataUP.csv")

accidents[is.na(accidents)] <- 0

number_of_accidents  <- as.data.frame(table(accidents$Country))
names(number_of_accidents) <- c("country.name", "total")
str(number_of_accidents)
number_of_accidents$country.name <- as.character(number_of_accidents$country.name)
number_of_accidents$country.name[number_of_accidents$country.name=='Russia'] <- 'Russian Federation'
number_of_accidents$country.name <- as.factor(number_of_accidents$country.name)
number_of_accidents$iso3 <- countrycode_data[match(number_of_accidents$country.name,
                                                   countrycode_data$country.name),'iso3c']
dshmstops <- data.frame(q=c(0,exp(1:5)/exp(5)), c=substring(viridis(5 + 1, 
                                option = "D"), 0, 7)) %>%   list_parse2()
highchart() %>% hc_add_series_map(worldgeojson,number_of_accidents,value='total',
                                  joinBy='iso3') %>%
  hc_colorAxis(stops = dshmstops) %>% hc_legend(enabled = TRUE) %>%
  hc_add_theme(hc_theme_google())%>% hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Total Number of Accidents (1948-2016) ") %>%
  hc_credits(enabled = TRUE, text = "Source :NTSB aviation accident database")
  
usa <- separate(accidents[accidents$Country=='United States',],col = Location, 
                into = c("City", "State"), sep = ", ")
by_states <- usa %>% group_by(State) %>% summarise(Total = n(),
        Fatal = sum(Total.Fatal.Injuries), Serious = sum(Total.Serious.Injuries)) 
by_states$statename <- state.name[match(by_states$State,state.abb)]
by_states <- na.omit(by_states)

highchart() %>%
  hc_title(text = "Number of Accidents in USA (1948-2016)") %>%
  hc_add_series_map(usgeojson, by_states, name = "Number of Accidents",
                    value = "Total", joinBy = c("woename", "statename"))  %>% 
  hc_colorAxis(stops = dshmstops) %>%
  hc_credits(enabled = TRUE, text = "Source :NTSB aviation accident database")
  

accidents$Make <- toupper(accidents$Make)
makers <- accidents %>% group_by(Make) %>% summarise(Total = n(), Fatal = sum(Total.Fatal.Injuries), Serious = sum(Total.Serious.Injuries)) 


top20_makers_accidents <- head(makers %>% arrange(desc(Total)), 20)

hchart(top20_makers_accidents, type = "treemap", x = Make, value = Total, 
       color = Total) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Top 20 Makers with Most Number of Accidents (1948-2016) ") %>%
  hc_credits(enabled = TRUE, text = "Source :NTSB aviation accident database")

acc <- as.data.frame(table(accidents$Event.Date))
accidents_ts <- xts(acc$Freq, as.Date(acc$Var1))
accidents_ts <- na.omit(accidents_ts)

hchart(accidents_ts) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Number of Accidents (1948-2016) ") %>%
  hc_credits(enabled = TRUE, text = "Source :NTSB aviation accident database")

fatal_ts <- xts(round(accidents$Total.Fatal.Injuries), as.Date(accidents$Event.Date))
fatal_ts <- na.omit(fatal_ts)

hchart(fatal_ts) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Number of Fatal Injuries (1948-2016) ") %>%
  hc_credits(enabled = TRUE, text = "Source :NTSB aviation accident database")