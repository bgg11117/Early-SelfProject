library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(lubridate)
library(dplyr)
library(ggthemes)

Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
death <- read.csv("./input/cpj.csv", na.strings = "")
str(death)
# converting date proper format
death$Date <- as.POSIXct(strptime(death$Date, format = "%B %d, %Y"))
death$Year <- format(death$Date, "%Y")

## data cleaning

death$Name <- as.character(death$Name)
death$Organization <- as.character(death$Organization)
death$Tortured <- as.character(death$Tortured)
death$Tortured[death$Tortured == "\n"] <- NA
death$Tortured[death$Tortured == "No\n"] <- "No"
death$Tortured[death$Tortured == "Yes\n"] <- "Yes"
death$Tortured <- factor(death$Tortured)

death$Threatened <- as.character(death$Threatened)
death$Threatened[death$Threatened == " Yes" |death$Threatened == "Yes" ] <- "Yes"
death$Threatened[death$Threatened == "No " |death$Threatened == "No" ] <- "No"
death$Threatened <- factor(death$Threatened)
death$Taken_captive <- as.character(death$Taken_captive)
death$Taken_captive[death$Taken_captive == "Yes" | death$Taken_captive == "Yes "] <- "Yes"
death$Taken_captive <- factor(death$Taken_captive)

death$Freelance <- as.character(death$Freelance)
death$Freelance[death$Freelance == "No"] <- "Non-Freelancer"
death$Freelance[death$Freelance == "Yes"] <- "Freelancer"
death$Freelance <- factor(death$Freelance)

death$Impunity_for_murder <- as.character(death$Impunity_for_murder)
death$Impunity_for_murder[death$Impunity_for_murder == "No"] <- "No-Impunity"
death$Impunity_for_murder[death$Impunity_for_murder == "Yes"] <- "Impunity"
death$Impunity_for_murder[death$Impunity_for_murder == "Partial"] <- "Partial-Impunity"
death$Impunity_for_murder <- factor(death$Impunity_for_murder)

death$Taken_captive <- as.character(death$Taken_captive)
death$Taken_captive[death$Taken_captive == "No"] <- "Not-Taken-Captive"
death$Taken_captive[death$Taken_captive == "Yes"] <- "Taken-Captive"
death$Taken_captive <- factor(death$Taken_captive)

death$Threatened <- as.character(death$Threatened)
death$Threatened[death$Threatened == "No"] <- "Not-Threatened"
death$Threatened[death$Threatened == "Yes"] <- "Threatened"
death$Threatened <- factor(death$Threatened)

death$Tortured <- as.character(death$Tortured)
death$Tortured[death$Tortured == "No"] <- "Not-Tortured"
death$Tortured[death$Tortured == "Yes"] <- "Tortured"
death$Tortured <- factor(death$Tortured)

#Count of death type by year

ggplot(data = subset(death, death$Year>1990), aes(x = Year,fill=Type_death)) + 
  geom_bar() +  scale_x_discrete(breaks = seq(1992,2016,1)) + 
  labs(title = "Year wise deaths", y = "Death Count")+theme_few()+
  theme(legend.position='top',axis.text.x=element_text(angle=90))

#Count of death type by type
ggplot(death, aes(x = Type, y = ..count.., fill = Type_death)) +
  geom_bar() + ylab("Death Count")+theme_few()

# by country killed

country_killed <- as.data.frame(table(death$Country_killed))
top_country_killed <- country_killed %>% arrange(desc(Freq)) %>% top_n(10)

ggplot(data = subset(death, death$Nationality %in% (top_country_killed$Var1)),
       aes(x = Country_killed, y = ..count.., fill = Sex)) + geom_bar() + theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + 
  scale_y_continuous(breaks = seq(0,300,20)) + 
  labs(title = "", y="Death Count", x = "Country Killed")

ggplot(data = subset(death, death$Nationality %in% (top_country_killed$Var1)),
       aes(x = Country_killed, y = ..count.., fill = Source_fire)) + geom_bar() + theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  scale_y_continuous(breaks = seq(0,300,20)) + ylab("Death Count") + xlab("Country Killed")

#levels top10卻仍顯示全部
ggplot(data = subset(death, death$Nationality %in% levels(top_country_killed$Var1)),
       aes(x = Country_killed, y = ..count.., fill = Medium)) + geom_bar() +theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + 
  scale_y_continuous(breaks = seq(0,300,20)) + ylab("Death Count") + xlab("Country Killed")

# by nationality

JR_nationality <- as.data.frame(table(death$Nationality))
top_JR_nationality <- JR_nationality %>% arrange(desc(Freq)) %>% top_n(10)

ggplot(data = subset(death, death$Nationality %in% (top_JR_nationality$Var1)) ,
       aes(x = Nationality, y = ..count.., fill = Type_death)) + geom_bar()+ theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + ylab("Death Count") +
  xlab("Journalist's Nationality")

ggplot(data = subset(death, death$Nationality %in% (top_JR_nationality$Var1)) ,
       aes(x = Nationality, y = ..count.., fill = Taken_captive)) + geom_bar()+ theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + ylab("Death Count") + 
  xlab("Journalist's Nationality")

ggplot(data = subset(death, death$Nationality %in% (top_JR_nationality$Var1)) ,
       aes(x = Nationality, y = ..count.., fill = Source_fire)) + geom_bar()+ theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + ylab("Death Count") + 
  xlab("Journalist's Nationality")

ggplot(data = subset(death, death$Nationality %in% (top_JR_nationality$Var1)) ,
       aes(x = Nationality, y = ..count.., fill = Freelance)) + geom_bar()+ theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + ylab("Death Count") + 
  xlab("Journalist's Nationality")

ggplot(data = subset(death, death$Nationality %in% (top_JR_nationality$Var1)) ,
       aes(x = Nationality, y = ..count.., fill = Local_Foreign)) + geom_bar()+theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + ylab("Death Count") + xlab("Journalist's Nationality")

ggplot(data = subset(death, death$Nationality %in% (top_JR_nationality$Var1)) ,
       aes(x = Nationality, y = ..count.., fill = Medium)) + geom_bar()+theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + ylab("Death Count") + xlab("Journalist's Nationality")

# by medium

ggplot(data = death[!is.na(death$Medium),], aes(x = Medium, y = ..count..,fill = Medium)) + geom_bar() + theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + xlab("Journalist's Work Domain") + 
  ylab("Death Count")

# by job type

JR_job <- as.data.frame(table(death$Job))
top_JR_job <- JR_job %>% arrange(desc(Freq)) %>% top_n(10)

ggplot(data = subset(death, death$Job %in% (top_JR_job$Var1)),
       aes(x = Job, y = ..count..)) + geom_bar(fill = "#465775") + theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + xlab("Job Position") +
  ylab("Death Count")

# by coverage

JR_coverage <- as.data.frame(table(death$Coverage))
top_JR_coverage <- JR_coverage  %>% arrange(desc(Freq)) %>% top_n(20)

ggplot(data = subset(death, death$Coverage %in% (top_JR_coverage $Var1)), 
       aes(x = Coverage, y = ..count..)) + geom_bar() + theme_few()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + ylab("Death Count")

