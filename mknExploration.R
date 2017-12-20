require(cmscu)
##Create dictionaries
##set order here
n<- 3

frequencies = replicate(n, new(FrequencyDictionary, 4,2^26))
# twitter_1g <- new(FrequencyDictionary,4,2^26)
# twitter_2g <- new(FrequencyDictionary,4,2^26)
# twitter_3g <- new(FrequencyDictionary,4,2^26)
# twitter_4g <- new(FrequencyDictionary,4,2^26)
# helper function
ngrams_h <- function(lst, n) {
  lst2 <- c(rep('<BOS>', n), lst, '<EOS>'); # this gets the BOS and EOS tokens inserted
  len <- length(lst2);
  sapply(1:(len-n+1), function(i) do.call(paste, as.list(lst2[i:(i+n-1)])))
}
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

# this function lets us create n-grams from a list
ngrams <- function(lst, n) {
  len <- length(lst);
  sapply(1:(len-n+1), function(i) do.call(paste, as.list(lst[i:(i+n-1)])))
}


# connect to the file, but don't load the contents! 
twitterfile <- file('./final/en_US/data.train.80', 'r', FALSE); 
k <- 500
repeat {
  # select the number of reviews to read at a time. 500 = ~550kb. 
  tweets <- readLines(con=twitterfile, n=1);
  # Break loop when you reach the end of the file
  if (k<1500 ){ #only loop through 1000 reviews for testing your loop
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
      for (order in 1:n){
          grams <- lapply(1:order, function(i) {
          tokens <- ngrams_h(sentence, i);
          counts <- frequencies[[i]]$store(tokens);
          #return(list(tokens = tokens, indices = lapply(1:3, function(j) counts == j)));
        });
        
      } 
 
      k <- k + 500
    }
  }
  cat('\r', paste('Trained', k, 'lines from twitter.')); #this will track your progress through your dataset!
  }  else {break;}
}


