library(ggplot2)
library(maps)
library(RColorBrewer)
library(data.table)
library(reshape2)
library(ggthemes)
library(ggmap)

terr <- fread("./input/globalterrorismdb_0616dist.csv")
length(unique(terr[, country_txt]))
world <- map_data("world")
ggplot() + geom_polygon(data = world, aes(x =  long, y = lat, group = group))+  
  geom_point(data = terr, aes(x = longitude, y = latitude, colour = country_txt ), 
             show.legend = FALSE)

ggplot(terr[, .N, by = list(iyear, region_txt)], aes(x =  iyear,  y= N, 
                        fill = region_txt, colour = region_txt)) + 
  geom_bar(stat ="identity") + ggtitle("Frequency by year") + xlab("Year") +
  ylab("Frequency")

ggplot(terr[, .N, by = list(weaptype1_txt, iyear)], aes(x  = weaptype1_txt, y = N,
                                                        fill = weaptype1_txt))+
  geom_bar(stat= "identity") +  theme_few() + 
  theme(axis.text.x  = element_text(angle=90),legend.position = 'None') + 
  ggtitle("Frequency of Weapon Type")+  xlab("Weapon Type") + ylab("Frequency")+
  scale_y_log10()

ggplot(terr[, .N, by = list(region_txt, iyear)], aes(x=  iyear, y = N, 
                                  colour = region_txt, fill = region_txt)) + 
  geom_bar(stat = "identity") + facet_grid(~ region_txt) +  
  theme(axis.text.x  = element_text(angle=90))+
  ggtitle("Freq 1970-2016 by region_txt") + xlab("Year") + ylab("N")

ggplot() + geom_polygon(data = world, aes(x =  long,  y= lat, group = group)) + 
  geom_point(data = terr[, .N, by = list(country_txt, latitude, longitude)], 
             aes(x = longitude, y = latitude, size = N,colour = country_txt, 
                 alpha = N/ncol(terr)), show.legend = FALSE) +
  stat_density2d(show.legend = FALSE)

ggplot() + geom_polygon(data = world, aes(x =  long, y = lat, group = group))+  
  geom_point(data = terr, aes(x = longitude, y = latitude,
                              colour = weaptype1_txt ), show.legend = TRUE)

ggplot() + geom_polygon(data = world, aes(x =  long,  y= lat, group = group)) + 
  geom_point(data = terr[, .N, by = list(weaptype1_txt, country_txt, latitude, longitude)], 
             aes(x = longitude, y = latitude, size = N ,colour = weaptype1_txt, alpha = N/ncol(terr))) +
  stat_density2d(show.legend = FALSE) + xlab("Longitude") +ylab("Latitude") + ggtitle("Intensity by Weapon type")

eur <- get_map("Europe",source = "stamen", zoom = 4, maptype = "watercolor")
asia <- get_map("Asia", source = "stamen", zoom =3, maptype = "watercolor")
africa <- get_map("Africa", source = "stamen", zoom=3, maptype = "watercolor")
north_america <- get_map("North America", source = "stamen", zoom = 3, maptype = "watercolor")

ggmap(africa) + geom_density2d(data = terr[ region_txt == "Sub-Saharan Africa" | 
     region_txt == "Middle East & North Africa", .N, by = list(country_txt, latitude, longitude)], 
     aes(x = longitude, y = latitude), size = 0.3) + stat_density2d(data =terr[  region_txt == "Sub-Saharan Africa" | 
     region_txt == "Middle East & North Africa", .N, by = list(country_txt, latitude, longitude)], 
     aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.40, 
     bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0.3, 0.1), guide = FALSE)+
     xlab("Longitude") +ylab("Latitude") + ggtitle("Africa\n 1970 to 2017")

ggmap(eur)+
geom_tile(data = terr[region_txt=="Western Europe" | region_txt == "Eastern Europe", .N, by = list(country_txt, latitude, longitude)], 
 aes(x = longitude, y = latitude, size = N,colour = country_txt, alpha = N/ncol(terr)), show.legend = FALSE) +
 xlab("Longitude") + xlab("Latitude") + ggtitle("Western and Eastern Europe \n 1970-2016")

#--------------
library(dplyr)
library(DT)
library(highcharter)
library(viridisLite)
library(countrycode)
library(readr)

globalterror <- read_csv("./input/globalterrorismdb_0616dist.csv")

globalterror$iyear <- factor(globalterror$iyear)
globalterror$success <- factor(globalterror$success)

# Map of Global Terror Attacks 1970-2015
countries <- globalterror %>% group_by(country_txt) %>% summarise(Total = n())
names(countries) <- c("country.name", "total")
countries$iso3 <- countrycode_data[match(countries$country.name, 
                                  countrycode_data$country.name), "iso3c"]

data(worldgeojson, package = "highcharter")
dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)),
                        c = substring(viridis(5 + 1, option = "D"), 0, 7)) %>% 
  list_parse2()

highchart() %>% 
  hc_add_series_map(worldgeojson, countries, value = "total", joinBy = "iso3") %>% 
  hc_colorAxis(stops = dshmstops) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_db()) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Global Terror Attacks 1970-2015") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, 
      text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", 
       style = list(fontSize = "10px")) 

# Number of Attacks 1970-2015
by_year <- as.data.frame(table(globalterror$iyear))
names(by_year) <- c("Year", "Total")

hchart(globalterror$iyear, name = "Year", color = "darkblue") %>%
  hc_title(text = "Number of Terror Attacks 1970-2015") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 

globalterror2 <- count(globalterror, iyear, success)

hchart(globalterror2, "column", x = iyear, y = n, group = success, name = "Year") %>%
  hc_title(text = "Number of Terror Attacks 1970-2015 ") %>%
  hc_subtitle(text = "(Success or Failure)") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 


hchart(by_year, "treemap", x = Year, value = Total, color = Total) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Yearwise distribution of Attacks") %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 

# Terror Across Different Regions by Number of Attacks
hchart(globalterror$region_txt, colorByPoint = TRUE, name = "Region") %>%
  hc_title(text = "Number of Terror Attacks Across Different Regions") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 

by_regions <- globalterror %>% group_by(region_txt) %>% summarise(Total = n()) 

hchart(by_regions, x = region_txt, value = Total, color = Total, type = "treemap") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 

# Top 20 Countries by Number of Attacks
by_countries <- globalterror %>% group_by(country_txt) %>% summarise(Total = n()) %>% arrange(desc(Total)) %>% head(20)


hchart(by_countries, type = "column", x = country_txt, y = Total) %>%
  hc_title(text = "Top 20 Countries by Number of Attacks") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 

hchart(by_countries, type = "treemap", x = country_txt, value = Total, color = Total) %>%
  hc_title(text = "Top 20 Countries by Number of Attacks") %>%
  hc_subtitle(text = "Tree Map of the Number of Attacks") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 

# Top 20 Cities by Number of attacks
cities <- globalterror[globalterror$city !="Unknown",] %>% group_by(city) %>% summarise(Total = n()) %>% arrange(desc(Total)) %>% head(20)

hchart(cities, x = city, value = Total, color = Total, "treemap") %>%
  hc_title(text = "Top 20 Cities by Number of Attacks") %>%
  hc_subtitle("Majority of the attacks do not have the city recorded") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 

# Map of Global Terror Attacks 1970-2015 - Number of Killed
countries <- globalterror[!is.na(globalterror$nkill),] %>% group_by(country_txt) %>% summarise(Total = sum(nkill))
names(countries) <- c("country.name", "total")
countries$iso3 <- countrycode_data[match(countries$country.name, countrycode_data$country.name), "iso3c"]


highchart() %>% 
  hc_add_series_map(worldgeojson, countries, value = "total", joinBy = "iso3") %>% 
  hc_colorAxis(stops = dshmstops) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_db()) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Global Terror Attacks 1970-2015 - Number of Killed") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 

# Regions based on Number of People Killed
killed_regions <- globalterror[!is.na(globalterror$nkill),] %>% group_by(region_txt) %>% summarise(Total = sum(nkill)) %>% arrange(desc(Total))

hchart(killed_regions, x = region_txt, value = Total, color = Total, "treemap") %>%
  hc_title(text = "Regions based on Number of People Killed") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 

# Top 20 Countries based on Number of People Killed
countries_killed <- globalterror[!is.na(globalterror$nkill),] %>% group_by(country_txt) %>% summarise(Total = round(sum(nkill))) %>% arrange(desc(Total)) %>% head(20)
                                                                                                      
hchart(countries_killed, x = country_txt, value = Total, color = Total, "treemap") %>%
  hc_title(text = "Top 20 Countries based on Number of People Killed") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px"))

# Top 20 Cities based on Number of People Killed
cities_killed <- globalterror[globalterror$city !="Unknown" &
                                !is.na(globalterror$nkill),] %>% 
  group_by(city) %>% summarise(Total = round(sum(nkill))) %>%
  arrange(desc(Total)) %>% head(20)

hchart(cities_killed, x = city, value = Total, color = Total, "treemap") %>%
  hc_title(text = "Top 20 Cities based on Number of People Killed") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px"))

# Top 10 Countries based on Number of USA Citizens killed
countries_killed_US <- globalterror[!is.na(globalterror$nkillus),] %>% 
  group_by(country_txt) %>% summarise(Total = sum(nkillus)) %>% 
  arrange(desc(Total)) %>% head(10)

hchart(countries_killed_US, x = country_txt, value = Total, color = Total, "treemap") %>%
  hc_title(text = "Top 10 Countries based on Number of USA Citizens killed") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 

# Top 10 Cities based on Number of USA Citizens killed
cities_killed_US <- globalterror[globalterror$city !="Unknown" &
                                   !is.na(globalterror$nkillus),] %>%
  group_by(city) %>% summarise(Total = sum(nkillus)) %>% 
  arrange(desc(Total)) %>% head(10)

hchart(cities_killed_US, x = city, value = Total, color = Total, "treemap") %>%
  hc_title(text = "Top 10 Cities based on Number of USA Citizens killed") %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: National Consortium for the Study of Terrorism and Responses to Terrorism (START)", style = list(fontSize = "10px")) 