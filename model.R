library(tidyverse)
library(tidytext)
#Creating NGRAMS Model
## Loading the data and creating the test,dev,train corpus
twitter<- readLines("./final/en_US/en_US.twitter.txt")
twitter_df <- data.frame(corp="twitter",line=1:length(twitter),text=twitter)
corp <- twitter_df
set.seed(2017)
test_corp <- sample_n(corp,max(corp$line)*0.1)
dev_corp  <- sample_n(corp,200)
train_corp <- sample_n(corp,max(corp$line)*0.8)

#Today we are working on DEV
corp <- train_corp  

##Let's clean up our data now...
corp$text <- gsub("rt","",corp$text) #Remove rt, retweet
corp$text <- gsub("@\\w+", "", corp$text) #Remoeve any @
corp$text <- gsub("[[:digit:]]", "", corp$text) # remove numbers
#Add a regexp for punctuations and non ASCII characters here
##Get a list of swears and naughty words
swears <- readLines("./data/profanity.txt")
swears <- as_tibble(as.character(swears))
colnames(swears) <- "word1"

##Unigrams
unigrams <- corp %>%
  unnest_tokens(word1,text,token="words") %>%
  anti_join(swears, by="word1") %>%
  count(word1,sort=TRUE)
## bigrams
bigrams <- unnest_tokens(corp,bigram,text,token="ngrams",n=2) %>%
separate(bigram,c("word1","word2"),sep=" ",remove=FALSE) %>%
filter(!word1 %in% swears$word1) %>%
filter(!word2 %in% swears$word1) %>%
group_by(bigram,word1) %>%
count(bigram,word1,sort=TRUE) %>%
left_join(unigrams,bigrams,by="word1") %>%
mutate(mle = n.x/n.y) %>%
select(c("bigram","word1","n.x","mle")) %>%
rename(bigram_count=n.x)
## trigrams
trigrams <- unnest_tokens(corp,trigram,text,token="ngrams",n=3) %>%
  separate(trigram,c("word1","word2","word3"),sep=" ",remove=FALSE) %>%
  filter(!word1 %in% swears$word1) %>%
  filter(!word2 %in% swears$word1) %>%
  filter(!word3 %in% swears$word1) %>%
  unite(bigram,word1,word2,sep=" ") %>%
  group_by(trigram,bigram) %>%
  count(trigram,bigram,sort=TRUE) %>%
  left_join(bigrams,trigrams,by="bigram") %>%
  mutate(mle = n/bigram_count) %>%
  rename(trigram_count=n) %>%
  select(c("trigram","bigram","trigram_count","mle"))
  
## fourgrams
fourgrams <- unnest_tokens(corp,fourgram,text,token="ngrams",n=4) %>%
  separate(fourgram,c("word1","word2","word3","word4"),sep=" ",remove=FALSE) %>%
  filter(!word1 %in% swears$word1) %>%
  filter(!word2 %in% swears$word1) %>%
  filter(!word3 %in% swears$word1) %>%
  filter(!word4 %in% swears$word1) %>%
  unite(trigram,word1,word2,word3,sep=" ") %>%
  group_by(fourgram,trigram) %>%
  count(fourgram,trigram,sort=TRUE) %>%
  left_join(trigrams,fourgrams,by="trigram") %>%
  mutate(mle = n/trigram_count) %>%
  rename(fourgram_count = n) %>%
  select(c("fourgram","trigram","fourgram_count","mle"))

