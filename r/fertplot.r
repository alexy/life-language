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
graph.color <- "dark red"
title.color <- "dark blue"
par(col.main=title.color,col.lab=title.color)
# text label magnification and tick inversion
# positive tcl faces inwards, the default is -0.5 outwards
par(cex.main= 0.8,cex.lab=0.8,cex.axis=0.7,tcl=0.3) # 
# shrink margins; defaults are: par(mar=c(5, 4, 4, 2) + 0.1,mgp=c(3,1,0))
par(mar=c(2, 2, 2, 1)+0.1,mgp=c(0,1,1))

plot(a.max,type="l",ann=F,col="dark red")
#devnull <- sapply(a.rest,lines)
devnull <- Map(function(a) lines(a,col=graph.color),a.rest)
box()
title(main="Fertility of MIT Reality")
title(xlab="total words", ylab="total new words so far")

dev.off()