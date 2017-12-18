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
  t_df <- data_frame(corp=name,line=1:length(t),text=t,sringasfactors=FALSE)
  retur t_df
}

clean_corpus <- function(corp) {
  corp$text <- gsub("rt","",corp$text) #Remove rt, retweet
  corp$text <- gsub("@\\w+", "", corp$text) #Remoeve any @
  corp$text <- gsub("[[:digit:]]", "", corp$text) # remove numbers
  corp
}

get_counts <- function(corp,save_to=".") {
  unigram_ref <- unnest_tokens(corp,word,text,token="words") %>% 
  count(word,sort=TRUE) 
 
  # We build a list of bigrams and evalute their MLE using Markov Principle
  bigram_ref <-  unnest_tokens(corp,ngram,text,token="ngrams",n=2) %>%
    count(ngram,sort=TRUE) %>% 
    separate(ngram, c("word1","word2"),sep=" ") %>%
    mutate(bigram=paste(word1,word2," ")) 
  
  trigram_ref <-  unnest_tokens(corp,ngram,text,token="ngrams",n=3) %>%
    count(ngram,sort=TRUE) %>% 
    separate(ngram, c("word1","word2","word3"),sep=" ") %>%
    mutate(bigram=paste(word1,word2,word3," ")) 
  
  fourgram_ref <- unnest_tokens(corp,ngram,text,token="ngrams",n=4) %>%
    count(ngram,sort=TRUE) %>% 
    separate(ngram, c("word1","word2","word3","word4"),sep=" ") %>%
    mutate(bigram=paste(word1,word2,word3,word4," ")) 
  
}
get_mle <- function(ref,ngram){
    #get the count from reference (c(wi-1))
    ref
}

mle <- function(cwi_1,cngram) {cngram/cwi_1}

