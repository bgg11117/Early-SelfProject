rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(NLP)
library(httr)
library(jiebaRD)
library(jiebaR)       # ???_?????????Q??????
library(tm)           # ?????????r?????????J???x???}???B??????
library(slam)         # ???}?????????x???}???B??????
library(RColorBrewer)
library(wordcloud)    # ?????????r??????
library(topicmodels)  # ???D???D????????

Sys.setlocale("LC_ALL", "cht")


orgURL = 'https://www.ptt.cc/bbs/baseball/index'
#orgURL = 'https://www.ptt.cc/bbs/StupidClown/index.html'

startPage = 4000
endPage = 5060
alldata = data.frame()
for( i in startPage:endPage)
{
  pttURL <- paste(orgURL, i, '.html', sep='')
  urlExists = url.exists(pttURL)
  
  if(urlExists)
  {
    html = getURL(pttURL, ssl.verifypeer = FALSE)
    xml = htmlParse(html, encoding ='utf-8')
    path = xpathSApply(xml, "//div[@class='title']/a//@href")
    tempdata = data.frame(path)
  }
  alldata = rbind(alldata, tempdata)
}

write.csv(alldata, 'alldata.csv')


allDate = levels(alldata$date)
res = hist(as.numeric(alldata$date), nclass=length(allDate), axes=F) 
axis(1, at=1:length(allDate), labels=allDate)
axis(2, at=1:max(res$counts), labels=1:max(res$counts))

alldata = read.csv('alldata.csv')
orgURL = 'https://www.ptt.cc'
for( i in 1:length(alldata$X))
{
  pttURL <- paste(orgURL, alldata$path[i], sep='')
  urlExists = url.exists(pttURL)
  
  if(urlExists)
  {
    html = getURL(pttURL, ssl.verifypeer = FALSE, encoding='UTF-8')
    xml = htmlParse(html, encoding='UTF-8')
    text = xpathSApply(xml, "//div[@id='main-content']", xmlValue)
    name <- paste('./allText/c', i, '.txt', sep='') #??????subText
    write(text, name)
  }
}

orgPath = "./AllText"
text = Corpus(DirSource(orgPath), list(language = NA))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, function(word)
{ gsub("[A-Za-z0-9]", "", word) })

# ???i???????????????_??????
mixseg = worker()
mat <- matrix( unlist(text), nrow=length(text) )
totalSegment = data.frame()
for( j in 1:length(text) )
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
# save the image in png format
#png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
#wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
#dev.off()
