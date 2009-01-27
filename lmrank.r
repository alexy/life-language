#!/usr/bin/env r
library(plyr)

file <- argv[1]

s <- read.table(file)
names(s) <- c("pid","rank")

top <- function(df,n) {
  if (n == 1000) df
  else ddply(df,.(pid),function (.df) .df[1:n,])
}

exact <- function(s) { 
  sm <- ddply(s,.(pid),function(df) median(df$rank,na.rm=T))
  names(sm)[2] <- "medrank"
  num.exact <-  dim(sm[sm$medrank==0.0,])[1] #length(sm[sm$medrank==0.0,]$pid)
  num.exact
}

subsets <- c(1,2,3,5,10,20,50,100,1000)

sapply(subsets, function (n) cat(n,"\t",exact(top(s,n)),"\n"))
