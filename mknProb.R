##MKN smoothing implementation
##
## Possible Trigrams

 disc <- function(c){
    if(c==0){0} 
    else if(c==1){1}
    else if(c==2){2} 
    else {3}
 }
 
my_trigrams <- str_trim(paste("case of",dict_1g$unigram," "),side="both")
trig_counts<- arrange(as.tibble(data.frame(trigram = my_trigrams,count=twitter_3g$query(my_trigrams))),desc(count))
mutate(trig_counts, d = disc(count))
