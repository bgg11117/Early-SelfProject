library(devtools)
library(Rfacebook)
library(httr)
library(ggplot2)
library(ggthemes)
library(scales)
library(tm)
library(wordcloud)
library(jiebaRD)
library(jiebaR)      
library(slam)        
library(RColorBrewer)
library(topicmodels) 
library(tidyr)
library(dplyr)


#FB developer，
#http://thinktostart.com/analyzing-facebook-with-r/
#https://developers.facebook.com/?ref=pf

fb_token <- fbOAuth(app_id="yours please", app_secret="yours please",extended_permissions=TRUE)
save(fb_token, file="fb_token")

load("fb_token")
me <- getUsers("me",token=fb_token)
my_likes <- getLikes(user="me", token=fb_token)
#me$name

#choose fanpage(convert to number)
#http://findmyfbid.com/
page <- getPage(334587866726921,fb_token,n=2500) #n為取得之貼文數
str(page)

## convert Facebook date format to R date format
format_date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
# aggregate metric counts over month
aggregate_metric <- function(metric) {
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}
# create data frame with average metric counts per month
page$datetime <- format_date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df_list <- lapply(c("likes", "comments", "shares"),aggregate_metric)
df <- do.call(rbind, df_list)
# visualize evolution in metric

ggplot(df, aes(x = month, y = x, group = metric,color = metric)) +  geom_line() +
  scale_y_log10("Average count per post",breaks = c(2, 10, 50, 100)) +
  theme_few() +  theme(axis.title.x = element_blank())
#ggsave('avg count per post.png')

#find the article with most likes
top_post <- page[which.max(page$likes_count), ]
top_post

#analyze who likes the article(e.g last name)
post <- getPost(top_post$id, fb_token, n = 1000, likes = TRUE, comments = FALSE)
users <- getUsers(post$likes$from_id, fb_token)
head(sort(table(users$last_name), decreasing = TRUE))

top_lastname <- head(sort(table(users$last_name), decreasing = TRUE), n = 10)
top_lastname <- data.frame(top_lastname)
colnames(top_lastname) <- c('last_name','count')
top_lastname$last_name <- reorder(top_lastname$last_name, top_lastname$count)
ggplot(data = top_lastname, aes(x = last_name, y = count,fill=last_name)) +
  geom_bar(stat="identity")+  theme_few()+theme(legend.position='None')
ggsave('Top post likes_count by name.png')


#analyze which type has most avg_likes
page_type <- page %>% group_by(type,month)%>%summarise(avg_like=mean(likes_count),
                                                       avg_share=mean(shares_count),
                                                       avg_comment=mean(comments_count))

ggplot(page_type,aes(month,avg_share,group=type,color=type))+geom_line()+
  scale_y_log10()+  theme_few()+facet_wrap(~type)+
  theme(axis.text.x = element_text(angle = 90))
#ggsave('avg share per post by type.png',width=8,height=4)

#以按讚數之大小顯示期間內之貼文
wordcloud(page$message , page$likes_count)

mescor  <- Corpus(VectorSource(page$message))
mescor  <- tm_map(mescor , removePunctuation)
mescor  <- tm_map(mescor , removeNumbers)
mescor  <- tm_map(mescor , function(word){ gsub("[A-Za-z0-9]", "", word) })
#mescor  <- tm_map(mescor , removeWords, stopwords("english"))
#mescor  <- tm_map(mescor , stemDocument)
#mescor  <- tm_map(mescor , PlainTextDocument)

mixseg = worker()
mat <- matrix( unlist(mescor), nrow=length(mescor) )
totalSegment = data.frame()
for( j in 1:length(mescor) )
{
  for( i in 1:length(mat[j,]) )
  {
    result = segment(as.character(mat[j,i]), jiebar=mixseg)
  }
  totalSegment = rbind(totalSegment, data.frame(result))
}

totaldiff = levels(totalSegment$result)
countMat = data.frame(totaldiff, c(rep(0, length(totaldiff))))
for( i in 1:length(totalSegment$result) )
{
  for( j in 1:length(totaldiff) )
  {
    if( nchar(totaldiff[j]) >= 2 &&
        totaldiff[j] == as.character(totalSegment$result[i]) )
    {
      countMat[j,2] = countMat[j,2] + 1
    }
  }
}

names(countMat) = c("totaldiff", "freq")
countMat[,2] = sort(countMat[,2] / sum(countMat[,2]),decreasing = TRUE)

png("MM.png", width=12, height=8,units='in', res=300)

#rot.per=0.35
# min.freq = -Inf
#colors=brewer.pal(8, "Dark2"))

wordcloud(countMat$totaldiff, countMat$freq, min.freq = 5,random.order = F, 
          ordered.colors = T,  colors = rainbow(length(totaldiff)),scale=c(4,.8) ,
          res=300)

dev.off()


#------method 2------
#oauth_endpoints("facebook")
#app <- oauth_app('facebook',"app_id","app_secret")
#fb_token <- oauth2.0_token(oauth_endpoints("facebook"), app,
                           #scope = c("ads_management", "read_insights"),
                           #type  = "application/x-www-form-urlencoded", 
                           #cache = FALSE)
#save(fb_token, file = "./input/fb_token")