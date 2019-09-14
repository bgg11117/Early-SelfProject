library(ggplot2)
library(maps)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(scales)
library(viridis)

baltimore <- read.csv("./input/BPD_Arrests.csv")

baltimore$Day <- factor(day(as.POSIXlt(baltimore$ArrestDate, format = "%m/%d/%Y")))
baltimore$Month <- factor(month(as.POSIXlt(baltimore$ArrestDate, format = "%m/%d/%Y")))
baltimore$Year <- factor(year(as.POSIXlt(baltimore$ArrestDate, format = "%m/%d/%Y")))
baltimore$Weekday <- wday(as.POSIXlt(baltimore$ArrestDate, format = "%m/%d/%Y"), label = TRUE)

baltimore$Hour <- factor(hour(hm(baltimore$ArrestTime)))

### Heat Maps

year_month <- baltimore %>%
  group_by(Year, Month) %>%
  dplyr::summarize(Total = n()) 


weekday_month <- baltimore %>%
  group_by(Weekday, Month) %>%
  dplyr::summarize(Total = n()) 


race_month <- baltimore %>%
  group_by(Race, Month) %>%
  dplyr::summarize(Total = n()) 



race_district <- baltimore[baltimore$District != "",] %>%
  group_by(Race, District) %>%
  dplyr::summarize(Total = n()) 


## Change the levels of the Month 
levels(year_month$Month) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

levels(weekday_month$Month) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

levels(race_month$Month) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")


## Building heatmap using ggplot2
ggplot(year_month, aes(Month, Year, fill = Total)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_fill_viridis(name = "Number of Arrests") + 
  theme(legend.position = "top") +
  labs( title = "Baltimore Arrests by Month and Year") +
  geom_text(aes(label=Total), color='white')


ggplot(weekday_month, aes(Month, Weekday, fill = Total)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_fill_viridis(name = "Number of Arrests") + 
  theme(legend.position = "top") +
  labs( title = "Baltimore Arrests by Month and Weekday") +
  geom_text(aes(label=Total), color='white')

ggplot(race_month, aes(Month, Race, fill = Total)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_fill_viridis(name = "Number of Arrests") + 
  theme(legend.position = "top") +
  labs( title = "Baltimore Arrests by Month and Race") +
  geom_text(aes(label=Total), color='white')

ggplot(race_district, aes(Race, District, fill = Total)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_fill_viridis(name = "Number of Arrests") + 
  theme(legend.position = "top") +
  labs( title = "Baltimore Arrests by District and Race") +
  geom_text(aes(label=Total), color='white')

### Number of Arrests by Age

ggplot(baltimore[baltimore$Age > 17 & baltimore$Age < 75,], aes(Age)) + 
  geom_bar(fill = "darkred") +
  ggtitle("Arrests by Age") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore[baltimore$Age > 17 & baltimore$Age < 75,], aes(Age, fill = Race)) + 
  geom_bar() +
  ggtitle("Arrests by Age and Race ") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore[baltimore$Age > 17 & baltimore$Age < 75,], aes(Age, fill = Sex)) + 
  geom_bar() +
  ggtitle("Arrests by Age and Sex ") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore[baltimore$District != "" & baltimore$Age > 17 & baltimore$Age < 75,], aes(Age, fill = District)) + 
  geom_bar() +
  ggtitle("Arrests by Age and District ") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


* Majority of the people arrested are between the age of 18-35
* Men are most in this group
* Overwhelming majority belong to black race
* Southeastern and Western districts have the most arrests

### Number of Arrests by Hour of the day

by_hour <- baltimore %>%
  group_by(Hour) %>%
  dplyr::summarize(Total = n()) 

datatable(by_hour)

ggplot(by_hour, aes(Hour, Total)) + 
  geom_bar( stat = "identity", fill = "darkgreen") +
  ggtitle("Arrests by Hour of the Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore, aes(Hour, fill = Race)) + 
  geom_bar() +
  ggtitle("Arrests by Hour of the Day and Race") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore, aes(Hour, fill = Sex)) + 
  geom_bar() +
  ggtitle("Arrests by Hour of the Day and Sex") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore[baltimore$District != "",], aes(Hour, fill = District)) + 
  geom_bar() +
  ggtitle("Arrests by Hour and District") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 

* Majority of the arrests are between 9am-9pm
* It has two peaks during 11-12am in the morning and 6-7pm in the evening
* Again Overwhelming majority is black race and men



### Number of Arrests by Month

by_month <- baltimore %>%
  group_by(Month) %>%
  dplyr::summarize(Total = n()) 

datatable(by_month)

ggplot(by_month, aes(Month, Total)) + 
  geom_bar( stat = "identity", fill = "darkred") +
  ggtitle("Arrests by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore, aes(Month, fill = Race)) + 
  geom_bar() +
  ggtitle("Arrests by Month and Race") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore, aes(Month, fill = Sex)) + 
  geom_bar() +
  ggtitle("Arrests by Month and Sex") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore[baltimore$District != "",], aes(Month, fill = District)) + 
  geom_bar() +
  ggtitle("Arrests by Month and District") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 

* Arrests are lowest during end of the year
* Arrests peak just before Spring season and summer season.
* Again majority arrests are among black race and men


### Number of Arrests by Weekday

by_weekday <- baltimore %>%
  group_by(Weekday) %>%
  dplyr::summarize(Total = n()) 

datatable(by_weekday)

ggplot(by_weekday, aes(Weekday, Total)) + 
  geom_bar( stat = "identity", fill = "darkblue") +
  ggtitle("Arrests by Weekday") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore, aes(Weekday, fill = Race)) + 
  geom_bar() +
  ggtitle("Arrests by Weekday and Race") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore, aes(Weekday, fill = Sex)) + 
  geom_bar() +
  ggtitle("Arrests by Weekday and Sex") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 

ggplot(baltimore[baltimore$District != "",], aes(Weekday, fill = District)) + 
  geom_bar(position = "dodge") +
  ggtitle("Arrests by Weekday and District") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 

* Surprisingly weekends are more silent
* Mid of the week has more arrests
* Again majority arrests are among black race and men
* Southern district always had more arrests along with eastern

### Number of Arrests by Year

by_year <- baltimore %>%
  group_by(Year) %>%
  dplyr::summarize(Total = n()) 

datatable(by_year)

ggplot(by_year, aes(Year, Total)) + 
  geom_bar( stat = "identity", fill = "darkgreen") +
  ggtitle("Arrests by Year") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore, aes(Year, fill = Race)) + 
  geom_bar(position = "dodge") +
  ggtitle("Arrests by Year and Race") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore, aes(Year, fill = Sex)) + 
  geom_bar(position = "dodge") +
  ggtitle("Arrests by Year and Sex") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


ggplot(baltimore[baltimore$District != "",], aes(Year, fill = District)) + 
  geom_bar(position = "dodge") +
  ggtitle("Arrests by Year and District") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 

* Arrests have decreased over the years

### Top 10 Charges and Charge Description by Number of Arrests

top10_charges <- baltimore[baltimore$Charge != "",] %>%
  group_by(Charge) %>%
  dplyr::summarize(Total = n()) %>%
  arrange(desc(Total)) %>%
  head(10)

datatable(top10_charges)

ggplot(top10_charges, aes(reorder(Charge, -Total), Total)) + 
  geom_bar(stat = "identity" , fill = "darkred") +
  ggtitle("Top 10 Charges for Arrest") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 

top10_chargedesc <- baltimore[baltimore$Charge != "",] %>%
  group_by(ChargeDescription) %>%
  dplyr::summarize(Total = n()) %>%
  arrange(desc(Total)) %>%
  head(10)

datatable(top10_chargedesc)

ggplot(top10_chargedesc, aes(reorder(ChargeDescription, Total), Total)) + 
  geom_bar(stat = "identity", fill = "darkblue") +
  ggtitle("Top 10 ChargeDescriptions for Arrest") +
  coord_flip() +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 

### Arrests by Top 5 Charges 

by_desc <- subset(baltimore, Charge %in% c("4 3550", "1 1415", "1 0077", "1 1635", "1 0573"))

dim(by_desc)


ggplot(by_desc, aes(Charge, fill = Sex)) + 
  geom_bar(position = "dodge") +
  ggtitle("Arrests by Top 5 Charges and Sex") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 


ggplot(by_desc, aes(Charge, fill = Race)) + 
  geom_bar(position = "dodge") +
  ggtitle("Arrests by Top 5 Charges and Race") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 

### Neighborhoods with Most Arrests

baltimore$Neighborhood <- toupper(baltimore$Neighborhood)
top10_neighborhoods <- baltimore[baltimore$Neighborhood !="",] %>%
  group_by(Neighborhood) %>%
  dplyr::summarize(Total = n()) %>%
  arrange(desc(Total)) %>%
  head(10)

datatable(top10_neighborhoods)

by_Neighborhood <- subset(baltimore, Neighborhood %in% top10_neighborhoods$Neighborhood)


ggplot(by_Neighborhood, aes(Neighborhood)) + 
  geom_bar(fill = "darkred") +
  ggtitle("Top 10 Neighborhood by Number of Arrests") +
  xlab("Neighborhoods") +
  theme(legend.position = "top") +
  coord_flip() +
  scale_y_continuous(labels = comma) 

ggplot(by_Neighborhood, aes(Neighborhood, fill = Sex)) + 
  geom_bar() +
  ggtitle("Top 10 Neighborhood by Number of Arrests and Sex") +
  xlab("Neighborhoods") +
  theme(legend.position = "top") +
  coord_flip() +
  scale_y_continuous(labels = comma) 


ggplot(by_Neighborhood, aes(Neighborhood, fill = Race)) + 
  geom_bar() +
  ggtitle("Top 10 Neighborhood by Number of Arrests and Race") +
  xlab("Neighborhoods") +
  theme(legend.position = "top") +
  coord_flip() +
  scale_y_continuous(labels = comma) 