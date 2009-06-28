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

par(mar=c(4, 4, 2, 1)+0.1)
# default margins
# par(mar=c(5, 4, 4, 2) + 0.1)

# positive tcl means the ticks are facing inwards; 0.5 is the default for tcl
plot(a.max,type="l",ann=F,cex.axis=0.5,tcl=0.5)
#devnull <- sapply(a.rest,lines)
devnull <- Map(lines,a.rest)
box()
title(main="Fertility of MIT Reality",cex.main=0.8)
title(xlab="total words", ylab="total new words so far",cex.lab=0.5)

dev.off()