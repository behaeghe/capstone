library(quanteda)
library(readtext)
t <- readtext("./final/en_US/en_US.twitter.txt")
corp <- corpus(t)

