library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(tidyr)
dataset <- read.csv("./input/primary_debates_cleaned.csv",
                    stringsAsFactors = F, header = T)
first <- dataset[1:243,]
clean_first <- first %>% filter(!Speaker %in% c("AUDIENCE", "OTHER", "CANDIDATES"))

d <- clean_first$Text
d <- data.frame(text = d)
d$text <- gsub("[[:punct:]]", "", d$text)
d$speaker <- clean_first$Speaker

#our speakers.
members <- levels(factor(clean_first$Speaker))
#the stop_words dataset that comes with tidytext package to
#remove stopwords from the our text variable
data("stop_words")

#the for loop for wordcloud and bar plot
for(i in members){
  text <- d[d$speaker == i,]$text
  text <- paste(text, collapse = " ")
  text <- data.frame(text = text, stringsAsFactors = F)
  text <- text %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% 
    filter(!word %in% c("im", "weve", "ive", "100", "15", "11", "50", "da", 
                        "1bit")) %>%  count(word, sort = T)
  set.seed(7)
  wordcloud(text$word, text$n, colors = brewer.pal(members, "Paired"))
}

bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(word, sentiment)

#sentiment-analysis
sent <- d %>% group_by(speaker) %>% mutate(linenumber = row_number()) %>% 
  ungroup() %>% unnest_tokens(word, text) %>% inner_join(bing) %>% 
  count(speaker,index = linenumber, sentiment) %>% spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% ungroup()

ggplot(sent, aes(index, sentiment, fill = speaker)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(~speaker, scales = "free_x") + 
  labs(title = "Sentiment analysis", 
       subtitle = "The difference between Sanders and Clinton looks less", 
       caption = " Presidential debates 2016")

library(reshape2)

#comparison_words

set.seed(7)
d %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(!word %in% c("10", "100", "102030", "11", "12", "im", "5", "54",
                      "15", "170", "76000", "13", "21", "2015", "ive", "10000",
                      "weve", "16", "61", "400", "1930s", "1998", "200", "2000",
                      "2007", "2008", "2012", "2016", "5000", "2017", "20th", 
                      "23", "25", "40", "750", "750000", "8", "90", "911",
                      "2002", "500"))  %>% 
  count(speaker, word) %>% ungroup() %>%
  acast(word ~ speaker, value.var = "n", fill = 0) %>% 
  comparison.cloud(max.words = 500)

#We look for words that contribute most to the sentiment.
freq_sent <- d %>% group_by(speaker) %>% mutate(linenumber = row_number()) %>% 
  ungroup() %>% unnest_tokens(word, text) %>% inner_join(bing) %>% 
  count(word, sentiment, sort = T) %>% ungroup()

freq_sent %>% filter(n > 3) %>% 
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% 
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) + 
  geom_bar(stat = "identity" , col = "white") + 
  labs(y = "Frequency of sentiment words", x = NULL) + coord_flip() +
  labs(title = "Most frequent sentiment words", 
       subtitle = "More positive and less negative",
       caption= "Presidential debates 2016")

#tf-idf

#count the frequency pf words.
count_words <- d %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% 
  filter(!word %in% c("10", "100", "102030", "11", "12", "5", "54", "15", "170", 
                      "76000","da", "13", "21", "2015", "ive", "10000", "weve", "16", 
                      "61", "400", "1930s", "1998", "200", "2000", "2007",
                      "2008", "2012","2016", "5000", "2017", "20th", "23", "25", 
                      "40", "750", "750000","8", "90", "911", "2002", "500"))  %>% 
  count(speaker, word,  sort = T) %>% ungroup()
#count the total words spoken by a particular speak.
total_words <- count_words %>% group_by(speaker) %>% summarise(total = sum(n))
#join both the dataset.
count_words <- count_words %>% left_join(total_words)
#bind tf-idf to the dataset.
tf_idf <- count_words %>% bind_tf_idf(word, speaker,n)
#arrange the words with highest tf-idf on top.
tf_idf <- tf_idf %>% select(-total) %>% arrange(desc(tf_idf))

ggplot(tf_idf[1:50,], aes(x = reorder(factor(word), tf_idf), 
                          y = tf_idf, fill = speaker)) +
  geom_bar(alpha = 0.5, stat = "identity") +
  labs(title = "highest tf-idf words by speaker", y = "tf-idf", x = NULL, 
       subtitle = "look at Ifill's words", caption= "Presidential debates 2016") +
  coord_flip()


#individual.
individual <- tf_idf %>% group_by(speaker) %>% top_n(15)
ggplot(individual, aes(x = reorder(factor(word), tf_idf), y = tf_idf, fill = speaker)) +
  geom_bar(alpha = 0.5, stat = "identity") + 
  labs(title = "highest tf-idf words of speaker", 
       caption = "Presidential debates 2016", y = "tf-idf", x = NULL) + coord_flip() + 
  facet_wrap(~speaker, ncol = 2, scales = "free") + theme(legend.position =  "none")