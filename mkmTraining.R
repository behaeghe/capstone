require(cmscu)

# a text cleaning function
clean <- function(line) {
  # upper-case everything
  str <- tolower(line);
  # strip-out small html tags
  str <- gsub('<[^>]{1,2}>', '', str);
  # replace all terminal punctuation with a period
  str <- gsub('[[:space:]]*[.?!:;]+[[:space:]]*', '.', str);
  # get rid of anything not A-Z, ', ., or whitespace
  str <- gsub('[^a-z\'.[:space:]]', ' ', str);
  # crowd apostrophes
  # str <- gsub("[[:space:]]+([A-Z]*'[A-Z]*)", "\\1", str);
  # collapse whitespace
  str <- gsub('[[:space:]]+', ' ', str);
  # make sure contraction's are "tight"
  str <- gsub(" ?' ?", "'", str);
  # make sure terminal . are tight
  str <- gsub(' ?\\. ?', '.', str);
  ## add twitter specific cleanup here
  return(str);
}

# this function lets us create n-grams from a list
ngrams <- function(lst, n) {
  len <- length(lst);
  sapply(1:(len-n+1), function(i) do.call(paste, as.list(lst[i:(i+n-1)])))
}
#BuildingVocabulary
#library(tidyverse)
#library(tidytext)




# # connect to the file, but don't load the contents! 
twitterfile <- file('./final/en_US/data.train.80', 'r', FALSE); 
# ####
# #Creating NGRAMS Model
# ## Loading the data and creating the test,dev,train corpus
# twitter=readLines(twitterfile)
# twitter_df <- data.frame(corp="twitter",line=1:length(twitter),text=twitter,stringsAsFactors=FALSE)
# corp <- twitter_df
# ##Let's clean up our data now...
# corp$text <- clean(corp$text)
# #Add a regexp for punctuations and non ASCII characters here
# ##Get a list of swears and naughty words
# swears <- readLines("./data/profanity.txt")
# swears <- as_tibble(as.character(swears))
# colnames(swears) <- "word"
# 
# ##Unigrams
# unigrams <- corp %>%
#   unnest_tokens(word,text,token="words") %>%
#   anti_join(swears, by="word") %>%
#   count(word,sort=TRUE) %>%
#   mutate(total = sum(n)) %>%
#   mutate(tf = n/total) %>%
#   arrange(desc(tf)) %>%
#   mutate(cumsum=cumsum(tf)) %>%
#   filter((cumsum)<=0.8)
# ##bigrams
# bigrams <- unnest_tokens(corp,bigram,text,token="ngrams",n=2) %>%
#   separate(bigram,c("word1","word2"),sep=" ",remove=FALSE) %>%
#   filter(!word1 %in% swears$word) %>%
#   filter(!word2 %in% swears$word) %>%
#   count(bigram,sort=TRUE) %>%
#   mutate(tf = n/sum(n))
#   
# 
# ####
# set the order here
n <- 2
i <- 500
smoother <- mkn(n,4,2^26)
repeat {
  # select the number of reviews to read at a time. 500 = ~550kb. 
  tweets <- readLines(con=twitterfile, n=500);
  # Break loop when you reach the end of the file
  #if (i<(10**6/500) ){ #only loop through 1000 reviews for testing your loop
  if (length(tweets) == 0) { #comment out if you only want to test loop on first 1000 reviews
    # disconnect the file link
    close(twitterfile);
    # break the loop
    break;
  }
  
  # read a single review 
  for (tweet in tweets){
    # parse the current review
    curtweet <- tweet
    # clean the current review
    text <- clean(curtweet)
    # split reviews into sentences
    for (sentence in unlist(strsplit(text, '\\.'))) {
      train(smoother,sentence)
      i<-i+500
      # # split to unigrams    
      # unilist <- unlist(strsplit(sentence, ' ')) 
      # # store unigrams
      # twitter_1g$store(unilist)
      # # add beginning amymodnd end of sentence tags to unigrams, and subsequent n-grams 
      # # (crucial for smoothing ngrams in test phase)
      # bilist <- c("<BOS>",unilist,'<EOS>')
      # # store bigrams, use the "ngrams" function bind unigrams together
      # twitter_2g$store(ngrams(bilist,2))
      # # store trigrams    
      # trilist <- c("<BOS>","<BOS>",unilist,'<EOS>')
      # twitter_3g$store(ngrams(trilist,3))
      # # store quadgrams
      # qualist <- c("<BOS>","<BOS>","<BOS>",unilist,'<EOS>')
      # twitter_4g$store(ngrams(qualist,4))
      # i <- i + 500
    }
  }
  cat('\r', paste('Trained', i, 'lines from twitter.')); #this will track your progress through your dataset!
}  #else {break}
mymod <- finalize(smoother)
