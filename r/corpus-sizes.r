#!/usr/bin/env r

file <- argv[1]

v <- scan(file)

postscript("corpus-sizes.eps",horizontal=F,onefile=F,height=3.5,width=3.2)
graph.color <- "dark red"
title.color <- "dark blue"
par(col.main=title.color,col.lab=title.color)
par(cex.main= 0.8,cex.lab=0.8,cex.axis=0.7,tcl=0.3) # 
par(mar=c(2, 2, 2, 1)+0.1,mgp=c(0,1,1))

hist(v,breaks=20,labels=F,
  main="Track Lengths Distribution",
  xlab="track length",ylab="number of people for such length")

dev.off()