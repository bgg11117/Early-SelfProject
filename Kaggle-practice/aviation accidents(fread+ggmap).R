library(data.table)
library(ggplot2)
library(lubridate)
library(ggmap)
library(scales)

aviation <- fread("./input/AviationDataUP.csv",stringsAsFactors = T)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

str(aviation)
# Removing columns that we are not going to use
aviation$Event.Id <- NULL
aviation$Accident.Number <- NULL
aviation$Location <- NULL

#Date formatting
aviation$date <- ymd(aviation$Event.Date)
aviation$year <- year(aviation$date)
aviation$month <- month(aviation$date,label = T)
aviation$day <- day(aviation$date)
aviation$weekday <- wday(aviation$date,label = T)

summary(aviation)

#accidents by year and month
ggplot(aviation[year>1981,.N,by=.(month,year)],aes(x = month,y=N,fill=N))+
  geom_bar(stat = "identity")+
  facet_wrap(~year)+
  labs(title="Accidents by year and month",caption="Donyoe")+ylab(NULL)+
  theme(axis.text.x = element_text(angle=90))

#accidents by year 
ggplot(aviation[year>1981,.N,by=.(year)],aes(x = year,y=N,fill=N))+
  geom_bar(stat = "identity")+
  labs(title="Accidents by year",caption="Donyoe")+ylab(NULL)

# Removing NA 
ggplot(aviation[!Aircraft.Category%in%c("")],aes(x = Aircraft.Category))+
  stat_count(aes(fill=Aircraft.Category))+
  scale_y_log10()+
  labs(title="Aircraft Categories",caption="Donyoe")+ylab(NULL)

aa <-aviation[!Aircraft.Category%in%c(""),.N,by=Aircraft.Category][order(-N)]

#Injuries by Broad Phase of Flight
p <- aviation[!Aircraft.Category%in%c(""),.("Mean"=mean(Total.Fatal.Injuries,na.rm = T)),
              by=Aircraft.Category][order(-Mean)]

ggplot(p, aes(x="", y=Mean, fill=Aircraft.Category))+ geom_bar(width = 1, 
      stat = "identity")+ coord_polar("y", start=0)+ blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = Mean/3 + c(0, cumsum(Mean)[-length(Mean)]), 
                label = percent(Mean/100)), size=5)+
  labs(title=" Mean Serious Injuries",subtitle="By Aircraft Category",caption="Donyoe")


#Injuries by Aircraft Category
b<- aviation[!Aircraft.Category%in%c(""),.("Mean"=mean(Total.Fatal.Injuries,
                                  na.rm = T)),by=Aircraft.Category][order(-Mean)]

#Injuries by Weather Condition
c <- aviation[!Weather.Condition%in%c(""),.("Mean"=mean(Total.Fatal.Injuries,
                  na.rm = T)),by=Weather.Condition][order(-Mean)]

ggplot(aviation[year>1981,.N,by=.(Engine.Type)][N>5],aes(x = Engine.Type,y=N,fill=N))+
  geom_bar(stat = "identity")+
  labs(title="Accidents by Engine Type",caption="Donyoe")+ylab(NULL)

#Accidents in Europe
map<-get_map(location="Europe",zoom=3)
ggmap(map)+
  geom_point(aes(x=Longitude,y=Latitude,colour=Aircraft.Category),
             data=aviation)+
  labs(title="Accidents by Aircraft Categorie",caption="Donyoe")+ylab(NULL)+xlab(NULL)

##Accidents in Europe
map<-get_map(location="Europe",zoom=3)
ggmap(map)+
  geom_point(data=aviation[Aircraft.Category%in%c("Airplane")],aes(x=Longitude,y=Latitude),alpha=0.5,col="red")+
  labs(title="Aircraft Accidents",caption="Donyoe")+ylab(NULL)+xlab(NULL)

#not airplane or helicopter accidents in europe
map<-get_map(location="Europe",zoom=3)
ggmap(map)+
  geom_point(data=aviation[!Aircraft.Category%in%c("","Helicopter","Airplane")],aes(x=Longitude,y=Latitude,colour=Aircraft.Category))+
  labs(title="Other Accidents",caption="Donyoe")+ylab(NULL)+xlab(NULL)