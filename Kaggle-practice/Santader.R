rm(list=ls())
library(dtplyr)
library(dplyr)
library(data.table)
library(tidyr)
library(lubridate)
library(ggplot2)

my_theme <- theme_bw() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))

my_theme_dark <- theme_dark() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))

set.seed(1)
df <- fread("./input/Santander.csv",nrows=-1)

unique.id <- unique(df$ncodpers)
limit.people <- 2.50e5
unique.id <- unique.id[sample(length(unique.id),limit.people)]
df <- df[df$ncodpers %in% unique.id,]
str(df)

df$fecha_dato <- as.POSIXct(strptime(df$fecha_dato,format="%Y-%m-%d"))
df$fecha_alta <- as.POSIXct(strptime(df$fecha_alta,format="%Y-%m-%d"))
unique(df$fecha_dato)

df$month <- month(df$fecha_dato)
sapply(df,function(x)any(is.na(x)))

ggplot(data=df,aes(x=age)) +  geom_bar(alpha=0.75,fill="tomato",color="black") +
  ggtitle("Age Distribution") + my_theme

summary(df$age)

df$age[(df$age < 18)]  <- mean(df$age[(df$age >= 18) & (df$age <=30)],na.rm=TRUE)
df$age[(df$age > 100)] <- mean(df$age[(df$age >= 30) & (df$age <=100)],na.rm=TRUE)
df$age[is.na(df$age)]  <- median(df$age,na.rm=TRUE)
df$age                 <- round(df$age)

ggplot(data=df,aes(x=age)) +  geom_bar(alpha=0.75,fill="tomato",color="black") +
  xlim(c(18,100)) + ggtitle("Age Distribution") +   my_theme

sum(is.na(df$ind_nuevo))

months.active <- df[is.na(df$ind_nuevo),] %>%
  group_by(ncodpers) %>%   summarise(months.active=n())  %>%
  select(months.active)

max(months.active)
df$ind_nuevo[is.na(df$ind_nuevo)] <- 1 

sum(is.na(df$antiguedad))
summary(df[is.na(df$antiguedad),]%>%select(ind_nuevo))

df$antiguedad[is.na(df$antiguedad)] <- min(df$antiguedad,na.rm=TRUE)
df$antiguedad[df$antiguedad<0]      <- 0

df$fecha_alta[is.na(df$fecha_alta)] <- median(df$fecha_alta,na.rm=TRUE)

table(df$indrel)
df$indrel[is.na(df$indrel)] <- 1

df <- df %>% select(-tipodom,-cod_prov)
sapply(df,function(x)any(is.na(x)))

sum(is.na(df$ind_actividad_cliente))
df$ind_actividad_cliente[is.na(df$ind_actividad_cliente)] <- median(df$ind_actividad_cliente,na.rm=TRUE)
unique(df$nomprov)

df$nomprov[df$nomprov==""] <- "UNKNOWN"
sum(is.na(df$renta))

df %>%
  filter(!is.na(renta)) %>%
  group_by(nomprov) %>%
  summarise(med.income = median(renta)) %>%
  arrange(med.income) %>%
  mutate(city=factor(nomprov,levels=nomprov)) %>% # the factor() call prevents reordering the names
  ggplot(aes(x=city,y=med.income)) + 
  geom_point(color="#c60b1e") + 
  guides(color=FALSE) + 
  xlab("City") +
  ylab("Median Income") +  
  my_theme + 
  theme(axis.text.x=element_blank(), axis.ticks = element_blank()) + 
  geom_text(aes(x=city,y=med.income,label=city),angle=90,hjust=-.25) +
  theme(plot.background=element_rect(fill="#c60b1e"),
        panel.background=element_rect(fill="#ffc400"),
        panel.grid =element_blank(),
        axis.title =element_text(color="#ffc400"),
        axis.text  =element_text(color="#ffc400"),
        plot.title =element_text(color="#ffc400",size=32)) +
  ylim(c(50000,200000)) +
  ggtitle("Income Distribution by City")

new.incomes <-df %>%
  select(nomprov) %>%
  merge(df %>%
          group_by(nomprov) %>%
          summarise(med.income=median(renta,na.rm=TRUE)),by="nomprov") %>%
  select(nomprov,med.income) %>%
  arrange(nomprov)
df <- arrange(df,nomprov)
df$renta[is.na(df$renta)] <- new.incomes$med.income[is.na(df$renta)]
rm(new.incomes)

#if still ant na in df$renta
table(sapply(df$renta, function(x)any(is.na(x))))

df$renta[is.na(df$renta)] <- median(df$renta,na.rm=TRUE)

sum(is.na(df$ind_nomina_ult1))
table(is.na(df$ind_nomina_ult1))

df[is.na(df)] <- 0
str(df)

char.cols <- names(df)[sapply(df,is.character)]
for (name in char.cols){
  print(sprintf("Unique values for %s:", name))
  cat(unique(df[[name]]),"\n\n")
}

#------------------
library(data.table)
library(ggplot2)
selectrow <- c("fecha_dato","fecha_alta","pais_residencia") # Rows that I want to analize
train <- fread("./input/Santander.csv",select = selectrow,showProgress =F)
train$fecha_dato <- as.POSIXct(strptime(x = train$fecha_dato,format = "%Y-%m-%d")) # Format date
train$fecha_alta <- as.POSIXct(strptime(x = train$fecha_alta,format = "%Y-%m-%d"))
train$year_dato <- year(train$fecha_dato) # Extract year
train$year_alta <- year(train$fecha_alta)
train$month_dato <- lubridate::month(train$fecha_dato,label=T) # Extract month
train$month_alta <- lubridate::month(train$fecha_alta,label=T)
train$weekday_alta <- lubridate::wday(train$fecha_alta,label=T)
train<-as.data.table(train)

ggplot(train[,.N,by=weekday_alta],
       aes(x = weekday_alta,y = N,fill=weekday_alta))+
  geom_bar(stat="identity")+
  ggtitle("Number of customers that became 'first holder' by day of week")

ggplot(train[year_alta>2009,.N,by=.(month_alta,year_alta)],
       aes(x = month_alta,y=N,fill=month_alta))+
  geom_bar(stat="identity")+
  ggtitle("Number of customers that became 'first holder' by month and year")+
  facet_wrap(~year_alta)