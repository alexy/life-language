#!/usr/bin/env r
a <- list()
ia <- 0
for (i in 1:97) { 
  fname <- paste("/Users/alexyk/cells/",i,"/",i,".fert",sep="")
  if (file.exists(fname)) {
    cat("reading file: ",fname,"\n")
    t <- read.table(fname)
    names(t) <- c("x","y")
    # a <- append(a,t)
    ia <- ia + 1
    a[[ia]] <- t
  } else {
    cat("doesn't exist: ",fname,"\n")
  }
}

xs <- sapply(a,function (df) { len <- dim(df)[1]; df$x[len]})
xs.max <- max(xs)
xs.max.i <- which(xs==xs.max)[1]

a.max <- a[[xs.max.i]]
a.rest <- a[-xs.max.i]

postscript("sensor-fertility.eps",horizontal=F,onefile=F,height=3.5,width=3.2)
plot(a.max,type="l",ann=F)
#devnull <- sapply(a.rest,lines)
devnull <- Map(lines,a.rest)
title(main="Fertility of MIT Reality")
title(xlab="total words", ylab="total new words so far")
dev.off()