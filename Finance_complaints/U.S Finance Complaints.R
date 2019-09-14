#rm(list=ls())
# Connect to the database in R (after installing RSQLite):
Sys.setlocale("LC_TIME", "English")
library(RSQLite)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library(ggthemes)
#library(MASS)
library(lubridate)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(xgboost)
library(Matrix)
library(party)
library(dplyr)
library(pROC)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "./input/database.sqlite")
dbListTables(con)
sqlfin <- dbGetQuery(con, "select * from consumer_complaints")
str(sqlfin)
sqlfin[,c(1:17)] <- lapply(sqlfin[,c(1:17)], as.factor)
sqlfin$Month <- as.factor(month(as.POSIXct(strptime(sqlfin$date_received,'%m/%d/%Y'))))
sqlfin$Year <- as.factor(year(as.POSIXct(strptime(sqlfin$date_received,'%m/%d/%Y'))))
#-----------

#顧客對於不同公司產品的抱怨次數是否有差異?

pro_sub <- table(sqlfin$product, sqlfin$company)
chisq.test(pro_sub) 

#reject the null hypothesis that complaint of product is independent of the company. 

length(unique(sqlfin$company))

top10companies <- sqlfin %>% group_by(company) %>%
  summarise(Total = n()) %>% arrange(desc(Total)) %>% top_n(10)

top10companies <- subset(sqlfin, company %in% top10companies$company)

#top10 companies complaint propotions
dim(top10companies)[1]/dim(sqlfin)[1]

ggplot(top10companies, aes(reorder(company, table(top10companies$company)[company]),
                            fill = product)) + geom_bar() + 
  coord_flip() + ggtitle("Top10Companies with most complaints against products") +
  ylab("Number of Complaints") + xlab("Companies")
ggsave('companies.png',width=8,height = 4)
#-------------------
#find 某行屬於什麼的對應company
#summary(sqlfin$company_response_to_consumer)
#summary(sqlfin$tags[complete.cases(sqlfin$tags)])
#summary(sqlfin[which(sqlfin$company_response_to_consumer == 'Untimely response'),][8])

sqlfin1 <- sqlfin
#sqlfin1 <- sqlfin1[,c(2:5,8:9,15)]
sqlfin1 <- sqlfin1 %>% select(c(2:5,8:9,15:16))%>% select(-c(2,4))%>% select(c(3,1:2,4:6))

t10 <- sqlfin1 %>% group_by(company) %>%summarize(count=n()) %>% 
  arrange(desc(count))%>%top_n(10)
t10 <- as.vector(t10$company)
t10 <- as.factor(t10)
#假設與公司進行和解過程中的不會投訴
sqlfin1 <- sqlfin1[!sqlfin1$company_response_to_consumer=='In progress',]
#假設非top10(約投訴市占率2%)的列為其他間
sqlfin1[!sqlfin1$company %in% t10,]$company <- 'Delbert Services'
levels(sqlfin1$company)[levels(sqlfin1$company) =='Delbert Services']<- 'others'
sqlfin1$company <- droplevels(sqlfin1$company)
sqlfin1 <- na.omit(sqlfin1)

#假設variable相同的會被移除
sqlfin1 <- sqlfin1[!duplicated(sqlfin1[,2:6]),]

#假設照美國五時區區分地理區域
sqlfin1$stateabb <- 0
sqlfin1$stateabb[sqlfin1$state %in% c('CT','DC','DE','FL','GA','IN','MA','MD','ME','MI','NC','NH',
                                    'NJ','NY','OH','PA','RI','SC','VA','VT','WV')] <- 1

sqlfin1$stateabb[sqlfin1$state %in% c('AL','AR','IA','IL','KS','KY','LA','MN','MO','MS','NE','OK',
                                    'SD','TN','TX','WI')] <- 2
sqlfin1$stateabb[sqlfin1$state %in% c('AZ','CO','ID','MT','ND','NM','UT','WY')] <- 3
sqlfin1$stateabb[sqlfin1$state %in% c('CA','NV','OR','WA')] <- 4
sqlfin1$stateabb[sqlfin1$state %in% c('AA','AK','AE','AP','AS','FM','GU','HI','MH','MP','PR','PW',
                                    'VI')] <- 5
sqlfin1 <- sqlfin1[,-4]
sqlfin1$stateabb <- as.factor(sqlfin1$stateabb)

#------------------
#選定70%作為training，交叉驗證10組，重複試驗200次

trainsample <- sample(nrow(sqlfin1),nrow(sqlfin1)*0.7)
sqlfin1train <- sqlfin1[trainsample,]
sqlfin1test <- sqlfin1[-trainsample,]
outputvector <- as.numeric((sqlfin1train$company))-1 #xgboost(startfrom 0)

train_sparse_matrix <- sparse.model.matrix(company~.-1, data = sqlfin1train)
test_sparse_matrix <- sparse.model.matrix(company~.-1, data = sqlfin1test)
head(train_sparse_matrix)

#cross-validation to choose the parameters
m=nlevels(sqlfin1$company)
param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = m)

cv.nround <- 200
cv.nfold <- 10
bst.cv = xgb.cv(param=param, data=train_sparse_matrix, label=outputvector, 
                nfold = cv.nfold, nrounds = cv.nround)
nround <- which(bst.cv$test.mlogloss.mean==min(bst.cv$test.mlogloss.mean))
#select the number of trees with the smallest test mlogloss for model building

bst <- xgboost(data = train_sparse_matrix, label = outputvector,
               param=param, nrounds = nround )
pred <- predict(bst,test_sparse_matrix)

pred = t(matrix(pred,m,length(pred)/m))
pred = levels(sqlfin1test$company)[max.col(pred)]

# confusion matrix
xg_accuracy = table(sqlfin1test$company,pred)
xg_accuracy
xg_ac <-sum(pred==sqlfin1test$company)/length(sqlfin1test$company)
#accuracy = 60%

importance <- xgb.importance(train_sparse_matrix @Dimnames[[2]], model = bst)
head(importance)
xgb.plot.importance(head(importance))+ theme(legend.position="none")
#ggsave("feature importance.png", width = 6, height = 3)
#xgb.plot.tree(train_sparse_matrix@Dimnames[[2]], model = bst)
#----------svm-------
#tuned = tune.svm(company~.,data = sqlfin1train, gamma = 2^(-7:-5), cost = 2^(2:4))
#summary(tuned)
svm.model <- svm(company~.,data = sqlfin1train, kernel='radial', type = 'C-classification'
                 , cost = 8, gamma = 0.03125)

sqlfin1pred <- predict(svm.model,sqlfin1test)
table(sqlfin1pred,sqlfin1test$company)

correction <- sqlfin1pred  == sqlfin1test$company
prop.table(table(correction)) 
svm_ac <- round(prop.table(table(correction))[[2]],2)
#accuracy 59%

svmweight <- t(svm.model$coefs) %*% svm.model$SV   # weight vectors
svmweight <- apply(svmweight, 2, function(v){sqrt(sum(v^2))})  # weight
svmweight<- sort(svmweight, decreasing = T)
head(svmweight)

#svm for prob
svm.modelprob <- svm(company~.,data = sqlfin1train, kernel='radial', type = 'C-classification'
                 , cost = 8, gamma = 0.03125, probability=TRUE)

sqlfin1predprob <- predict(svm.modelprob,sqlfin1test, probability=TRUE)

#ROC curve for different classes
#png(filename="ROC.png", units="in", width=5,  height=5, 
    #pointsize=12, res=72)
#roc(sqlfin1test$company, attr(sqlfin1predpp,'prob')[,i])
plot.roc(sqlfin1test$company,  attr(sqlfin1predprob,'prob')[,5],
         main=" Area Under Curve ", percent=TRUE, col='blue')
text(30, 40, labels=paste("lowest AUC = 0.5033\n (others)"),col='red')
text(80, 90, labels=paste("highest AUC = 0.8008\n (Bank Of America)"),col='blue')

par(new=TRUE) 
#cl <- topo.colors(10)

for (i in 1:11){
  print(roc(sqlfin1test$company, attr(sqlfin1predprob,'prob')[,i]))
  plot.roc(sqlfin1test$company, attr(sqlfin1predprob,'prob')[,i],
           axes = FALSE,xlab = "",ylab = "", percent=TRUE)
  par(new=TRUE)
}

par(new=TRUE) 
plot.roc(sqlfin1test$company,  attr(sqlfin1predprob,'prob')[,7],
         axes = FALSE,xlab = "",ylab = "",  col='red')
par(new=TRUE) 
plot.roc(sqlfin1test$company,  attr(sqlfin1predprob,'prob')[,5],
         main=" Area Under Curve ", percent=TRUE, col='blue')
#dev.off()
#colnames( attr(sqlfin1predprob,'prob'))
#--------各州抱怨數量--------

data("state.regions")

statemap <- sqlfin %>% group_by(state) %>% summarise(Total = n()) 
statemap <- subset(statemap, state %in% state.abb)
statemap <- as.data.frame(statemap)
names(statemap) <- c("region", "value")

statemap$region <- state.regions[match(statemap$region, state.regions$abb), 1]
state_choropleth(statemap, title = "Complaints by State",legend = "Number of Complaints")
ggsave('choro.png',width=8,height=4)

#--------以產品mortgage為文字雲--------
narrative <- sqlfin[sqlfin$product=="Mortgage",]$consumer_complaint_narrative

fincor <- Corpus(VectorSource(narrative))
fincor <- tm_map(fincor, removePunctuation)
fincor <- tm_map(fincor, removeNumbers)
fincor <- tm_map(fincor, stripWhitespace)
fincor<- tm_map(fincor, content_transformer(tolower))
fincor <- tm_map(fincor, removeWords, stopwords("english"))
fincor <- tm_map(fincor, stemDocument)
fincor <- tm_map(fincor, removeWords, c("xxxx", "xxxxxxxx"))
#fincor <- tm_map(fincor, PlainTextDocument)

fintdm <- TermDocumentMatrix(fincor)
fintdm <- removeSparseTerms(fintdm,0.99)
fintdm <- as.matrix(fintdm)
fintdm <- sort(rowSums(fintdm),decreasing=TRUE)
fin <- data.frame(word = names(fintdm),freq=fintdm)
#table(pd.d$freq)
png("wordcloud_mortgages1.png")
wordcloud(fin $word,fin $freq, scale=c(4,.6),max.words=Inf, 
          random.order=FALSE,colors=rainbow(length(fin$freq)))

#dev.off()

#--------SVM,XgBoost 準確度比較--------
results <- t(as.data.frame(list(XGboost = xg_ac, SVM= svm_ac)))
results <- as.data.frame(results)
colnames(results) <- 'Accuracy'

ggplot(results,aes(reorder(rownames(results),-Accuracy),Accuracy))+
  geom_bar(stat='identity',fill="#009E73",width=0.25)+
  coord_cartesian(ylim = c(0,1))+
  geom_text(data = results,aes(x= rownames(results),y=Accuracy,label=Accuracy,vjust=-1,size=3))+
  ggtitle('ML method accuracy')+xlab('method')+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(color='black',size=0.25),
        axis.line.y = element_line(color='black',size=0.25),
        axis.ticks.x=element_blank(),
        legend.position = 'None')    
ggsave('meth.png',width=8,height = 4)

#--------Top10 公司如何處理抱怨--------
ggplot(top10companies, aes(reorder(product, table(top10companies$product)[product]), fill = company_response_to_consumer)) + 
  geom_bar() +  ylab("Number of Complaints") + xlab("Product") + 
  coord_flip()+ theme_few()
ggsave('product complaint.png',width=8,height=3)

#--------------------