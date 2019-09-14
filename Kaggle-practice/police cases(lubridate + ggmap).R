library(plyr)
library(dplyr)
library(lubridate)
library(highcharter)
library(DT)
library(plotly)
library(ggmap)
library(readr)
slc <- read_csv("./input/SLC_Police_Cases_2016_cleaned_geocoded.csv")
#head(slc)
slc$occurred <- mdy_hms(slc$occurred)
slc$month <- month(slc$occurred,label=T)
slc$day<- wday(slc$occurred,label = T)
slc$hour <- hour(slc$occurred)

slc <- na.omit(slc)
apply(slc,2,function(x)sum(is.na(x)))
slc$description <- factor(slc$description)
slc$city <- factor(slc$city)
crimes<- c("LARCENY","PUBLIC ORDER","DRUGS","ASSAULT","PUBLIC PEACE",
           "ESCAPE","DAMAGED PROP","NONREPTABL TA","REPORTABLE TA","STOLEN VEHICLE")

colors <- c("#487098","#484898","#34348d","#19198b","#208582","#942f59",
            "#dd125b","#000000","#c61051","#000FFF")

temp <- filter(slc,description %in% crimes)
testmap <- get_googlemap(c(lon=mean(slc$x_gps_coords),lat=mean(slc$y_gps_coords)),
            zoom=10, xlim=c(min(slc$x_gps_coords),max(slc$x_gps_coords)),
            ylim=c(min(slc$y_gps_coords),max(slc$y_gps_coords)))

ggmap(testmap) + geom_point(aes(x=temp$x_gps_coords, y=temp$y_gps_coords,
            colour=description),data=temp)+ggtitle("Frequency of top 10 Crimes")

slc %>% group_by(description) %>% summarise(n=n()) %>% na.omit() %>% 
  arrange(desc(n)) %>%  hchart("column",x=description,y=log(n))%>% 
  hc_add_theme(hc_theme_google())

slc %>% group_by(hour,description) %>% summarise(n=mean(n())) %>% 
  filter(description %in% crimes) %>% na.omit() %>%
  hchart("line",x=hour,y=n,group=description)%>% 
  hc_add_theme(hc_theme_google()) %>% hc_colors(colors)

slc %>% group_by(day,description) %>% summarise(n=mean(n())) %>%
  filter(description %in% crimes) %>% na.omit() %>%
  hchart("column",x=day,y=n,group=as.factor(description))%>%
  hc_add_theme(hc_theme_google()) %>% hc_colors(colors)
