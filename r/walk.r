#!/usr/bin/env r
# graph a fertility run
# used to run from shell:
# tail --lines=+4 "$0" | R --vanilla --slave --args $*; exit
# argv <- commandArgs()[-(1:4)]
# now running with littler

# the krivostroi library
source("/w/ct/r/kriv/krivostroi.r")

# NB: vector assignment in R?

defaults <- c(NA, 1, 1000000, file.basename(argv[1]), "maxgap.txt")
for (i in 1:length(defaults)) if (is.na(argv[i])) argv[i] <- defaults[i]

file        <- argv[1]
maxruns     <- as.integer(argv[2]) 
granularity <- as.integer(argv[3])
prefix      <- argv[4]
maxgap.file <- argv[5]

# NB make it another command-line switch or option
eps <- T

# the local, concrete story
source("story.r")

cex.text <- 0.6 # used in text()
# set par globally -- do oldpar <- par to preserve anything
set.par <- function() {
  par(col.main=story$color,col.lab=story$label.color)
  # text label magnification and tick inversion
  par(cex.main= 0.8,cex.lab=0.8,cex.axis=0.7,tcl=0.3)
  # shrink margins; defaults are: par(mar=c(5, 4, 4, 2) + 0.1,mgp=c(3,1,0))
  par(mar=c(2, 2, 2, 1)+0.1,mgp=c(0,1,1))
}

if (eps) {
  ext <- "eps" 
  draw <- function(file,height,width) postscript(file,height=height,width=width,horizontal=F,onefile=F)
  H <- 3.5
  W <- 3.2
} else {
  ext <- "png"
  draw <- png
}

# NB another cmdline param/option:
title.append <- F

#print(paste("walk file:",file,":: runs =>",maxruns,":: prefix =>",prefix))
cat(":: file =>",file,":: runs =>",maxruns,":: prefix =>",prefix,"\n")

if (maxruns > 1) {
	story$title <- paste(story$title,"--",maxruns,"runs")
	# if sep is not givem, lines will come tokenized!
	maxgap.lines <- scan(maxgap.file,what=character(0),sep="\n")
	# TODO FPize:
	for (i in 1:length(maxgap.lines))
		story$text <- paste(story$text,"\n",maxgap.lines[i])
	# TODO: input tokens, just numbers, with sep="", and do abline for where the gap is
}

t <- load.or.read(file)

#print(t)

# gaps for text palcement in the plot
g.dx <- 0.001
g.dy <- 0.01

# derivative is just scaled natural incremental U
if (F) {
	graphname <- paste(prefix,"-drv.",ext,sep="")
	draw(graphname,height=H,width=W)
	for (run in 1:maxruns) {
		xy   <- t.xy(t,run=run,angle=T)
		plotlines(xy,col=colors[run],new=(run==1),tck=T)
	}

  if (title.append)
  	story.title <- paste(story$title,"-- derivative (angle)")
  else
    story.title <- story$title

	story.text  <- story$text

	title(main=story.title)
	title(xlab=story$x, ylab="tg(alpha)")

	g <- par.xy(dx=g.dx, dy=g.dy)
	# for log graph, y => 0 goes who knows where!
	text(g$x.right, g$y.top, story.text, adj=c(1,1), col=story$text.color, cex=cex.text)
	dev.off()
}

graphname <- paste(prefix,"-raw.",ext,sep="")
draw(graphname,height=H,width=W)
set.par()
for (run in 1:maxruns) {
	xy <- t.xy(t,run=run)	
	plotlines(xy,col=colors[run],new=(run==1),tck=T)
}

grid()

if (title.append) { 
  story.title <- paste(story$title,"-- raw")
} else {
  story.title <- story$title
}
  
title(main=story.title) # font.main=4
title(xlab=story$x)
title(ylab="new words per 1M unit")

g <- par.xy(dx=g.dx, dy=g.dy)
text(g$x.right, g$y.top, story$text, adj=c(1,1), col=story$text.color, cex=cex.text)
dev.off()

graphname <- paste(prefix,"-acc.",ext,sep="")
draw(graphname,height=H,width=W)
set.par()
story.text <- story$text
for (run in 1:maxruns) {	
	xy <- t.xy(t,run=run)		
	xy$y <- sumr(xy$y) # cumsum would do, too
	plotlines(xy,col=colors[run],new=(run==1),tck=T)

	x.last  <- length(xy$x)
	y.last  <- xy$y[x.last]
	x.alpha <- floor(length(xy$x)*0.99)
	y.alpha <- xy$y[x.alpha]

	cat("x1",x.alpha,"x2",x.last,"y1",y.alpha,"y2",y.last,"\n")

	tg.alpha <- (y.last-y.alpha)/(x.last-x.alpha)/granularity

	story.text <- paste(story.text,"\n")
	if (maxruns > 1) story.text <- paste(story.text,run)
	story.text <- paste(story.text,"tg.alpha",sprintf("%.5f",tg.alpha))
}

grid() # add gridlines at ticks

if (title.append) {
  story.title <- paste(story$title,"-- cumulative")
} else {
  story.title <- story$title
}

title(main=story.title)
title(xlab=story$x)
title(ylab="total new words seen so far")

g <- par.xy(dx=g.dx, dy=g.dy)
text(g$x.right, g$y.bottom, story.text, adj=c(1,0), col=story$text.color, cex=cex.text)
dev.off()
# quit()

graphname <- paste(prefix,"-log.",ext,sep="")
draw(graphname,height=H,width=W)
set.par()
story.text <- story$text
for (run in 1:maxruns) {
	xy <- t.xy(t,run=run)
	xy$x <- log10(xy$x)
	xy$y <- log10(sumr(xy$y))
	# xy$log <- "xy" # would do natural logs, we want decimal
	# TODO add run color list to the story
	plotlines(xy,col=colors[run],new=(run==1))

	# do lm here!
	# fit linear regression and plot as overlay
	# полезная, между прочим, штука
	model <- lm(xy$y ~ xy$x)
	# TODO add regression color list to the story
	abline(model, col=story$fit.color, lwd=2)

	# TODO a shorthand for this?
	coefs     <- coef(model)
	intercept <- coefs[1]
	slope     <- coefs[2] # coef(model)[2]

	# TODO can factor out format(x,digits=2) to krivostroi
	story.text <- paste(story.text,
	                     "\n",run,"intercept",format(intercept, digits=2),
	                     "slope",format(slope,digits=2))
}

grid()

if (title.append) {
  story.title <- paste(story$title,"-- cumulative, log10")
} else {
  story.title <- story$title
}

title(main=story.title)
title(xlab="total words, log10")
title(ylab="cumulative new words, log10")

g <- par.xy(dx=g.dx, dy=g.dy)
text(g$x.right, g$y.bottom, story.text, adj=c(1,0), col=story$text.color, cex=cex.text)
dev.off()
