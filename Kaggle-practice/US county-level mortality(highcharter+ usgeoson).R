library(tidyr)
library(highcharter)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library("viridis")
data("usgeojson")

mortality <- read_csv("./input/new/mort.csv")
usa_mortality <- mortality[mortality$Location=='United States',]
usa_mortality_yearwise <- gather(usa_mortality, USA_Mortality, Rate, c(4,7,10,13,16,19,22,25))[, c(1,2,3,23,24)]
usa_mortality_yearwise <- separate(usa_mortality_yearwise, USA_Mortality, into = c("Mortalities", "Year"), sep = ',')
usa_mortality_yearwise <- usa_mortality_yearwise[, -4]
usa_mortality_yearwise$Year <- substr(usa_mortality_yearwise$Year,1,5)
#usa_mortality_yearwise$Year <- gsub('[*]','',usa_mortality_yearwise$Year)
usa_mortality_yearwise$Year <- str_trim(usa_mortality_yearwise$Year, "left")

hchart(usa_mortality_yearwise[usa_mortality_yearwise$Year == "1980",], type = "treemap", x = Category, value = Rate, color = Rate)

ggplot(usa_mortality_yearwise, aes(Year, Category, fill = Rate)) +
  geom_tile(color = "white") +
  ggtitle("What is Killing Americans(1980-2014)?") +
  scale_fill_gradient(low = "darkgreen",  high = "yellow") +
  geom_text(aes(label=Rate), color='white') +
  theme(legend.position = "top")

#----------
mortality <- na.omit(mortality)

mortality <- gather(mortality, Mortality, Rate, c(4,7,10,13,16,19,22,25))[, c(1,2,3,23,24)]

mortality <- separate(mortality, Mortality, into = c("Mortalities", "Year"), sep = ',')
mortality <- mortality[, -4]
mortality$Year <- substr(mortality$Year,1,5)
mortality$Year <- str_trim(mortality$Year, "left")
state_mortality <- mortality[mortality$FIPS < 57,]
state_mortality$Location <- factor(state_mortality$Location)
state_mortality$FIPS <- factor(state_mortality$FIPS)


county_mortality <- mortality[mortality$FIPS > 57,]
county_mortality$Location <- factor(county_mortality$Location)
county_mortality$FIPS <- factor(county_mortality$FIPS)
county_mortality <- na.omit(county_mortality)

state_neonatal <- state_mortality[state_mortality$Category == "Neonatal disorders", ]

state_neonatal_1980 <- state_neonatal[state_neonatal$Year == "1980",]
state_neonatal_1990 <- state_neonatal[state_neonatal$Year == "1990",]
state_neonatal_2000 <- state_neonatal[state_neonatal$Year == "2000",]
state_neonatal_2010 <- state_neonatal[state_neonatal$Year == "2010",]
state_neonatal_2014 <- state_neonatal[state_neonatal$Year == "2014",]

highchart() %>%
  hc_title(text = "Neonatal Disorders - 1980") %>%
  hc_add_series_map(usgeojson, state_neonatal_1980, name = "Neonatal Disorders",
                    value = "Rate", joinBy = c("woename", "Location"))  %>% 
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 8, by = 4), 13))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_credits(enabled = TRUE, text = "Source : Institute for Health Metrics and Evaluation - (IHME)")

highchart() %>%
  hc_title(text = "Neonatal Disorders - 2014") %>%
  hc_add_series_map(usgeojson, state_neonatal_2014, name = "Neonatal Disorders",
                    value = "Rate", joinBy = c("woename", "Location"))  %>% 
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 8, by = 4), 13))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_credits(enabled = TRUE, text = "Source : Institute for Health Metrics and Evaluation - (IHME)")

state_mental <- state_mortality[state_mortality$Category == "Mental and substance use disorders", ]
state_mental <- na.omit(state_mental)
state_mental_2014 <- state_mental[state_mental$Year == "2014",]

highchart() %>%
  hc_title(text = "Mental and substance use disorders - 2014") %>%
  hc_add_series_map(usgeojson, state_mental_2014, name = "Mental and substance use disorders",
                    value = "Rate", joinBy = c("woename", "Location"))  %>%   
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 18, by = 6), 24))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_credits(enabled = TRUE, text = "Source : Institute for Health Metrics and Evaluation - (IHME)")

state_unintentional <- state_mortality[state_mortality$Category == "Unintentional injuries", ]
state_unintentional <- na.omit(state_unintentional)
state_unintentional_2014 <- state_unintentional[state_unintentional$Year == "2014",]

highchart() %>%
  hc_title(text = "Unintentional injuries - 2014") %>%
  hc_add_series_map(usgeojson, state_unintentional_2014, name = "Unintentional injuries",
                    value = "Rate", joinBy = c("woename", "Location"))  %>% 
  hc_colorAxis(dataClasses = color_classes(c(seq(10, 42, by = 8), 50))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) %>%
  hc_credits(enabled = TRUE, text = "Source : Institute for Health Metrics and Evaluation - (IHME)")