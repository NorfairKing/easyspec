args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 3) {
  stop("Usage: single_evaluator_bar.r common.r input.csv output.png")
}

common <- args[1]
inFile <- args[2]
outPng <- args[3]

source(common)

res = read.csv(inFile, header=TRUE)

dat = as.numeric(as.character(res$nrDifferentFunctions))
head(dat)

png(outPng, height=900, width=1200, bg="white")
h = hist(dat,
     main="Histogram of the number of different functions in an equation",
     xlab="Different functions",
     ylab="# of cases",
     breaks=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5))

h$density = h$counts/sum(h$counts)
plot(h,
     main="Histogram of the number of different functions in an equation",
     xlab="Different functions",
     ylab="relative # of cases",
     freq=FALSE,
     breaks=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5))
