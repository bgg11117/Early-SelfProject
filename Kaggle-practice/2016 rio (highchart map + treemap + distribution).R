library(dplyr)
library(tidyr)
library(highcharter)
library(viridisLite)
library(countrycode)
library(readr)

olympic <- read_csv("./input/athletes.csv")
countries <- olympic %>% group_by(nationality) %>% summarise(Gold = sum(gold),
        Silver = sum(silver), Bronze = sum(bronze), Total = sum(Gold, Silver,Bronze), 
        WON = ifelse(Total >0 , 'Yes','No'), TotalAthletes = n())

goldmedals <- countries[1:2]
names(goldmedals) <- c('ISO','Gold')
goldmedals$iso3 <- countrycode_data[match(goldmedals$ISO,countrycode_data$ioc),"iso3c"]

data("worldgeojson")
dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)),
                        c = substring(viridis(5 + 1, option = "D"), 0, 7)) %>%
                        list_parse2()
highchart() %>% hc_add_series_map(worldgeojson, goldmedals,value='Gold',joinBy='iso3') %>%
  hc_colorAxis(stops =dshmstops)  %>% hc_legend(enabled= TRUE)%>%
  hc_mapNavigation(enabled = TRUE) %>% hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Source Data: Rio 2016 website", 
             style = list(fontSize = "10px")) %>%
  hc_title(text = "Gold Medals - Rio Olympics 2016")

athletes <- olympic %>% group_by(nationality, sex) %>% summarise( Total = n())
athletes <- spread(athletes, key = sex, value = Total)
athletes <- athletes %>% mutate( Total = female + male)

female_athletes <- athletes[1:2]
names(female_athletes) <- c("ISO", "Female")
female_athletes$iso3 <- countrycode_data[match(female_athletes$ISO, 
                                               countrycode_data$ioc), "iso3c"]

data(worldgeojson, package = "highcharter")
dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)),
                        c = substring(viridis(5 + 1, option = "A"), 0, 7)) %>% 
                        list_parse2()

highchart() %>% 
  hc_add_series_map(worldgeojson, female_athletes, value = "Female", joinBy = "iso3") %>% 
  hc_colorAxis(stops = dshmstops) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_db()) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Source Data: Rio 2016 website", style = list(fontSize = "10px")) %>%
  hc_title(text = "Female Athletes - Rio Olympics 2016")

hchart(olympic[olympic$sex=="male", ]$height, color = "darkgreen") %>%
                   hc_title(text = "Distribution of Height of the Male Athletes") %>%
                   hc_add_theme(hc_theme_google()) %>%
                   hc_credits(enabled = TRUE, text = "Source : Rio 2016 Website") 

hchart(olympic$height) %>%
                   hc_title(text = "Distribution of Height of the Athletes") %>%
                   hc_add_theme(hc_theme_google()) %>%
                   hc_credits(enabled = TRUE, text = "Source : Rio 2016 Website")