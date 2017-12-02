library(tidyverse) ##Because the universe is a better place with tidyverse
library(tidytext)
#' load source as a corpus (a simple data framce)
#'
#'
#' @param source The path of the file containing the text to load 
#' @param name name of the corpus inside the dataframe
#'
#' @return a data frame containing the corpus
#' @export
#'
#' @examples
load_corpus <- function(source,name="twitter") {
  t<- readLines(source)
  t_df <- data_frame(corp=name,line=1:length(t),text=t)
  t_df
}

clean_corpus <- function(corp) {
  corp$text <- gsub("rt","",corp$text) #Remove rt, retweet
  corp$text <- gsub("@\\w+", "", corp$text) #Remoeve any @
  corp$text <- gsub("[[:digit:]]", "", corp$text) # remove numbers
  corp
}

train_models <- function(corp,save_to=".") {
  #Get the unigrams count
  #Get the bigrams count
  #Get the trigrams count
  unigram_ref <- unnest_tokens(corp,word,text,token="words") %>% 
    count(word,sort=TRUE) 
  unigram_ref<- unigram_ref[,c(2,3)]
  colnames(unigram_ref) <- c("word1","n1")
  # We build a list of bigrams and evalute their MLE using Markov Principle
  corp_bigram_model <-  unnest_tokens(corp,ngram,text,token="ngrams",n=2) %>%
    count(ngram,sort=TRUE) %>% 
    separate(ngram, c("word1","word2"),sep=" ") %>%
    mutate(bigram=paste(word1,word2," ")) %>%
    full_join(unigram_ref,by="word1") %>% 
    mutate(mle=n/n1)
  #We reduce our model size to the top 3 of each bigram by mle
  bigrams_markov <- corp_bigram_model %>%  
    arrange(desc(mle)) %>% 
    group_by(word1) %>% 
    top_n(3,mle)
  saveRDS()
}