library(tidyr)
library(stringr)
library(ggplot2)
library(corrplot)
library(dplyr)
library(ggthemes)

reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}
data15 = read.csv("./input/new/2015.csv", header = TRUE)
data16 = read.csv("./input/new/2016.csv", header = TRUE)

pdata = rbind(data15 , data16)
str(pdata)
pdata$age <- as.numeric(as.character(pdata$age))
ggplot(pdata, aes(x=age))+geom_histogram(data =subset(pdata,year=='2015'),
                                         fill= 'red',alpha=0.2)+
  geom_histogram(data =subset(pdata,year=='2016'),fill= 'blue',alpha=0.2)+
  ggtitle("Age Distribution of Deaths") +
  theme_few()

summary(pdata$age)
pdata$age[is.na(pdata$age)==TRUE] <- mean(na.omit(pdata$age))

table(pdata$gender)
pdata$genderN = 1
pdata$genderN[pdata$gender=="Female"] = 0

ggplot(pdata, aes(reorder_size(raceethnicity), fill=factor(year))) +
  geom_bar() +  xlab("Race & Ethnicity") +
  guides(fill=guide_legend(title=NULL)) + #legend title不見
  scale_fill_manual(values=c("#CC3399", "#6699FF")) +  coord_flip()+
  theme_few()

pdata$month = factor(pdata$month,levels=month.name)
ggplot(pdata, aes(x=month, fill=factor(year))) +
  geom_bar() +  xlab("Counts by Month") +  theme_few() +
  scale_fill_manual(values=c("#CC3399", "#6699FF")) +  guides(fill=FALSE)

t= table(pdata$lawenforcementagency)
t = as.data.frame(t)
colnames(t) <- c("Department", "Counts")
head(arrange(t, desc(t[,2])),10)

pdata_num <- pdata[,sapply(pdata,is.numeric)]
M = cor(pdata_num, use="pairwise.complete.obs")
corrplot(M, cl.pos="b", tl.pos="d", tl.col = "black", tl.cex=0.5, type="upper")

