library(syuzhet)
library(DT)
library(sentimentr)
library(corrplot)
library(gtable)
library(grid)
library(extrafont)
library(ggplot2)
library(sjPlot)
library(readr)
dj <- read_csv("./input/new/DJIA_table.csv")
dj$change<-((dj$Close-dj$Open)/dj$Open)*100

news <- read_csv("./input/new/Combined_News_DJIA.csv")
#All the news per row in one column
news$tops<-do.call(paste, as.data.frame(news, stringsAsFactors=FALSE)) 
sentiment <- get_nrc_sentiment(as.character(news$tops))
sent <- cbind(news$Date, sentiment)
colnames(sent)<-c("Date","Anger", "Anticipation", "Disgust", "Fear",
                  "Joy", "Sadness", "Surprise", "Trust", "Negative", "Positive")
sent_index <- as.data.frame(sentiment_by(news$tops))
colnames(sent_index)<-c("ID", "Word_Count", "sd", "Sent_index")

ID<-seq.int(nrow(dj))


dfa<-merge(dj, sent, by.x="Date", by.y="Date")

dfa<-cbind(ID, dfa)


df<-merge(dfa, sent_index, by.x="ID", by.y="ID")

datatable(df)

cor_jnk=round(cor(df[sapply(df, is.numeric)], use="complete.obs"),2)

corrplot(cor_jnk,  method="circle", tl.pos="lt", type="full",        
         tl.col="black", tl.cex=0.7, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         p.mat = 1-abs(cor_jnk), sig.level=0.50, insig = "blank", 
         number.cex = 1, title=" ", mar=c(0,0,2,0))
