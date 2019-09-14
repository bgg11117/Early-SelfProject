library(dplyr)
library(ggplot2)
library(highcharter)
library(maps)
library(scales)
library(readr)
library(DT)


indiancities <- read_csv("./input/cities_r2.csv")

by_state <- indiancities %>% group_by(state_name) %>% summarise(Total = n(), 
    Population = sum(population_total), Male_Population = sum(population_male), 
    Female_Population = sum(population_female), 
    Male_Percent = Male_Population/Population * 100, 
    Female_Percent = Female_Population/Population * 100, 
    Graduates = sum(total_graduates), Male_Grads = sum(male_graduates),
    Female_Grads = sum(female_graduates), 
    Grads_percent = Graduates/Population * 100,
    Male_Grads_Percent = Male_Grads/Male_Population * 100, 
    Female_Grads_Percent = Female_Grads/Female_Population * 100) %>% 
  arrange(desc(Total))

hchart(by_state, x = state_name, y = Total, type = "column", color = state_name) %>%
  hc_title(text = "States by number of cities in Top 500") %>%
  hc_add_theme(hc_theme_google())


hchart(by_state, x = state_name, value = Total, color = Total, type = "treemap")  %>% 
  hc_add_theme(hc_theme_google()) %>%
  hc_credits(enabled = TRUE, text = "Sources: Census India 2011", 
             style = list(fontSize = "10px")) %>%
  hc_title(text = "States by Number of Cities in Top 500")

hchart(by_state[order(-by_state$Population),], x = state_name, y = Population, 
       type = "column", color = state_name) %>%
  hc_title(text = "States by Total Population in Top 500") %>%
  hc_add_theme(hc_theme_google())

top10_states <- indiancities[indiancities$state_name %in% by_state[1:10,]$state_name,]

highchart() %>% 
  hc_add_series_boxplot(top10_states$population_total, by = top10_states$state_name,
                        name = "Total Population", color = "blue") %>%
  hc_credits(enabled = TRUE, text = "Sources: Census India 2011", 
             style = list(fontSize = "10px")) %>%
  hc_title(text = "Top 10 States Population Distribution") %>%
  hc_add_theme(hc_theme_google())

hchart(indiancities, "scatter", x = effective_literacy_rate_male,
       y = effective_literacy_rate_female, group = state_name) %>%
  hc_credits(enabled = TRUE, text = "Sources: Census India 2011",
             style = list(fontSize = "10px")) %>%
  hc_title(text = "Male Literacy rate versus Female Literacy Rate") %>%
  hc_add_theme(hc_theme_google())

hchart(by_state, "scatter", x = Male_Grads_Percent , y = Female_Grads_Percent, 
       group = state_name) %>%
  hc_credits(enabled = TRUE, text = "Sources: Census India 2011", 
             style = list(fontSize = "10px")) %>%
  hc_title(text = "Male Graduate rate versus Female Graduate Rate") %>%
  hc_add_theme(hc_theme_google())