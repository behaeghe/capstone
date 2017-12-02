#Creating NGRAMS Model
twitter<- readLines("./final/en_US/en_US.twitter.txt")
twitter_df <- data_frame(corp="twitter",line=1:length(twitter),text=twitter)
corp <- twitter_df
set.seed(2017)
test_corp <- sample_n(corp,length(corp)*0.1)
dev_corp  <- sample_n(corp,length(corp)*0.1)
train_corp <- sample_n(corp,length(corp)*0.8)
