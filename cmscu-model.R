require(cmscu)
##Create dictionaries

twitter_1g <- new(FrequencyDictionary,4,2^26)
twitter_2g <- new(FrequencyDictionary,4,2^26)
twitter_3g <- new(FrequencyDictionary,4,2^26)
twitter_4g <- new(FrequencyDictionary,4,2^26)

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
  #Remove rt, retweet
  str <- gsub("rt","",str) 
  #Remoeve any @
  str <- gsub("@\\w+", "", str) 
  
  return(str);
}
dict_1g <- " "
dict_2g <- " "
dict_3g <- " "
dict_4g <- " "
# this function lets us create n-grams from a list
ngrams <- function(lst, n) {
  len <- length(lst);
  sapply(1:(len-n+1), function(i) do.call(paste, as.list(lst[i:(i+n-1)])))
}


# connect to the file, but don't load the contents! 
twitterfile <- file('./final/en_US/data.train.80', 'r', FALSE); 
i <- 500
repeat {
  # select the number of reviews to read at a time. 500 = ~550kb. 
  tweets <- readLines(con=twitterfile, n=1);
  # Break loop when you reach the end of the file
  #@if (i<(10**6/500) ){ #only loop through 1000 reviews for testing your loop
      if (length(tweets) == 0) { #comment out if you only want to test loop on first 1000 reviews
    # disconnect the file link
    close(twitterfile);
    # break the loop
    break;
      }
  j<-1

  # read a single review 
  for (tweet in tweets){
    # parse the current review
    curtweet <- tweet
    # clean the current review
    text <- clean(curtweet)
    # split reviews into sentences
    for (sentence in unlist(strsplit(text, '\\.'))) {
      # split to unigrams    
      unilist <- unlist(strsplit(sentence, ' '))
      dict_1g <- unique(unlist(list(dict_1g,unilist)))
      # store unigrams
      twitter_1g$store(unilist)
      # add beginning and end of sentence tags to unigrams, and subsequent n-grams 
      # (crucial for smoothing ngrams in test phase)
      bilist <- c("<BOS>",unilist,'<EOS>')
      # store bigrams, use the "ngrams" function bind unigrams together
      bigrams <- ngrams(bilist,2)
      twitter_2g$store(bigrams)
      dict_2g <- unique(unlist(list(dict_2g,bigrams)))
      # store trigrams    
      trilist <- c("<BOS>","<BOS>",unilist,'<EOS>')
      trigrams <- ngrams(trilist,3)
      twitter_3g$store(trigrams)
      dict_3g <- unique(unlist(list(dict_3g,trigrams)))
      # store quadgrams
      qualist <- c("<BOS>","<BOS>","<BOS>",unilist,'<EOS>')
      fourgrams <- ngrams(qualist,4)
      twitter_4g$store(fourgrams)
      dict_4g <- unique(unlist(list(dict_4g,fourgrams)))
      i <- i + 500
    }
  }
  cat('\r', paste('Trained', i, 'lines from twitter.')); #this will track your progress through your dataset!
  }  #else {break;}
  

##Finalizing vocabularies

dict_1g <-as.tibble(data.frame(dict_1g,twitter_1g$query(dict_1g))) #make a dataframe
colnames(dict_1g) <- c("unigram","count") #rename columns so we can manipulate it
dict_1g <- dict_1g[-1,] #eliminate the " " introduced when initializing object
dict_1g <- arrange(dict_1g,desc(count)) #sort descending by count

dict_2g <-as.tibble(data.frame(dict_2g,twitter_2g$query(dict_2g))) #make a dataframe
colnames(dict_2g) <- c("unigram","count") #rename columns so we can manipulate it
dict_2g <- dict_2g[-1,] #eliminate the " " introduced when initializing object
dict_2g <- arrange(dict_2g,desc(count)) #sort descending by count

dict_3g <-as.tibble(data.frame(dict_3g,twitter_3g$query(dict_3g))) #make a dataframe
colnames(dict_3g) <- c("unigram","count") #rename columns so we can manipulate it
dict_3g <- dict_3g[-1,] #eliminate the " " introduced when initializing object
dict_3g <- arrange(dict_3g,desc(count)) #sort descending by count

dict_4g <-as.tibble(data.frame(dict_4g,twitter_4g$query(dict_4g))) #make a dataframe
colnames(dict_4g) <- c("unigram","count") #rename columns so we can manipulate it
dict_4g <- dict_4g[-1,] #eliminate the " " introduced when initializing object
dict_4g <- arrange(dict_4g,desc(count)) #sort descending by count