library(data.table)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(reshape2)
library(tidyr)
#setwd("./input")
Sys.setlocale("LC_TIME", "English")
world_temp = fread("./input/GlobalLandTemperaturesByCountry.csv")
str(world_temp)
world_temp$date = as.Date(world_temp$dt, "%Y-%m-%d")
world_temp$month = month(world_temp$date)
world_temp$year = year(world_temp$date)
world_temp = world_temp[!is.na(AverageTemperature),]

# extracting world's average temperature month-wise & year-wise
world_temp_2 = world_temp %>%
  group_by(year, month) %>%
  summarise(avg_Temp = mean(AverageTemperature))

world_temp_2$date = paste0("01-",as.character(world_temp_2$month),"-",as.character(world_temp_2$year))
world_temp_2$date = as.Date(world_temp_2$date, "%d-%m-%Y")

# month-on-month change in world's average temperature from 1743 to 2013
ggplot(world_temp_2, aes(x = date, y = avg_Temp)) + geom_line() + 
  geom_smooth(model = lm,size = 2) +
  xlab("Years") + ylab("Average Monthly Temperature") + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13)) +
  ggtitle("Average Monthly Temperature Trend") + 
  theme(plot.title = element_text(size = 13, lineheight=.8, face="bold"))

#--------
country_temp = world_temp %>%
  group_by(Country, year, month) %>%
  summarise(avg_Temp = mean(AverageTemperature)) %>%
  filter(month == 1)

country_temp = as.data.table(country_temp)
country_temp = country_temp[!is.na(avg_Temp),]

# country-wise comparison of average January month average temperature, for the years 1950 and 2013
ggplot(data=country_temp[country_temp$year == 1950 | country_temp$year == 2013,], 
       aes(x = reorder(Country, avg_Temp), y = avg_Temp, group = as.factor(year), 
           colour = as.factor(year))) +
  labs(colour = "Year") + 
  theme(legend.title = element_text(size = 13, face="bold")) +
  theme(legend.text = element_text(size = 13)) +
  geom_line(size = 1) + xlab("Coldest to Hottest Countries") + ylab("Average Temperature") + 
  theme_few()
  theme(axis.title = element_text(size = 15, face = "bold"), axis.text.y = element_text(size = 13),
        axis.text.x = element_blank()) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "gray25"), 
        panel.border = element_blank(),
        panel.background = element_rect(fill = "black")) +
  ggtitle("Country-wise Change in Temperature (for the years 1950 & 2013)") + 
  theme(plot.title = element_text(size = 13, lineheight=.8, face="bold"))

#--------------
# calculating annual median temperature for every country for the years 1950 & 2013
country_temp = world_temp %>%
  group_by(Country, year) %>%
  summarise(med_Temp = median(AverageTemperature)) %>%
  filter(year == 1950 | year == 2013)

# converting data to wide format
country_temp_wide = spread(country_temp, year,med_Temp)

# denoting a country with '1' if the temperature increased during 1950-2013 and '0' otherwise
country_temp_wide$temp_incrs_flag = ifelse(country_temp_wide$`2013` > country_temp_wide$`1950`,1,0)

### add a map showing countries with increase and decrease in temp ###

map = map_data("world") # map_data() is a ggplot function
unique_country_temp = data.frame(region = country_temp_wide$Country, temp_increased = country_temp_wide$temp_incrs_flag)

# fetching desired data to 'map' dataframe
map = left_join(map, unique_country_temp) 

missed_countries = c("Antigua","Bosnia and Herzegovina","Republic of Congo",
                     "Curacao", "Falkland Islands", "UK","Heard Island",
                     "Saint Kitts","Myanmar", "Bonaire", "South Sudan", "South Georgia",
                     "Sao Tome and Principe", "Timor-Leste", "Tobago", "Saint Vincent",
                     "French Southern and Antarctic Lands", "Barbuda", "Saint Barthelemy",
                     "Democratic Republic of the Congo","Guinea-Bissau",
                     "Palestine","Saint Pierre and Miquelon", "Turks and Caicos Islands",
                     "Trinidad","USA")

map$temp_increased = ifelse(map$region %in% missed_countries, 1, map$temp_increased)
map = map[!is.na(map$temp_increased),]

ggplot(map) + geom_polygon(aes(x = long, y = lat, group = group, fill = as.factor(temp_increased)), colour = "black") +
  labs(fill = "Temp increased") + scale_fill_manual(labels = c("0 (No)", "1 (Yes)"), values = c("cornflowerblue", "coral1")) +
  theme(legend.title = element_text(size = 13, face="bold")) +
  theme(legend.text = element_text(size = 13)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "black")) +
  ggtitle("Increase in median annual temperature between the years 1950 & 2013") + 
  theme(plot.title = element_text(size = 13, lineheight=.8, face="bold"))

#-------------------
# calculating percentage change in average annual temperature form 1950 to 2013, for all the countries
country_temp_wide$temp_change = round(((country_temp_wide$`2013`- country_temp_wide$`1950`)*100/country_temp_wide$`1950`),digits = 1)
country_temp_wide = country_temp_wide[country_temp_wide$temp_incrs_flag == 1,]
country_temp_wide$temp_change = abs(country_temp_wide$temp_change) 
country_temp_wide = country_temp_wide[order(country_temp_wide$temp_change),]

not_countries = c("Africa","North America","Asia","Europe","Western Sahara","Reunion")

# Removing invalid entries 
country_temp_wide = country_temp_wide %>%
  filter(!(Country %in% not_countries))

# top 20 countries with maximum change in median annual temperature

ggplot(data=country_temp_wide[194:213,]) + geom_line(aes(x = Country, y = `1950`, group = 1), size = 1, colour = 'cyan3') +
  geom_point(aes(x = Country, y = `1950`), size = 4, colour = 'cyan3') +
  geom_line(aes(x = Country, y = `2013`, group = 1), size = 1, colour = 'red') + geom_point(aes(x = Country, y = `2013`), size = 4, colour = 'red') + 
  coord_flip() + theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "gray25"), panel.grid.minor = element_blank()) +
  ylab("Median Annual Temperature: 1950 (blue) & 2013 (red)") + xlab("Country") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13)) +
  ggtitle("Top 20 Nations with Maximum Temperature Change (for the years 1950 & 2013)") + 
  theme(plot.title = element_text(size = 13, lineheight=.8, face="bold")) +
  scale_x_discrete(position = 'top')
