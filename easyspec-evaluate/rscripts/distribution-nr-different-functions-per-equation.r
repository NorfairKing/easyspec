args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 4) {
  stop("Usage: distribution-nr-different-functions-per-equation.r common.r input.csv output.png (individual) [example] [focus] [strategy]")
}

common <- args[1]
inFile <- args[2]
outPng <- args[3]
kind <- args[4]

source(common)

res = read.csv(inFile, header=TRUE)

dat = as.numeric(as.character(res$nrDifferentFunctions))

if (length(dat) != 0) {
  png(outPng, height=900, width=1200, bg="white")
  title = "Histogram of the number of different functions in an equation"
  if (kind == "individual") { title = paste(title, args[5], args[6], args[7]) }
  if (kind == "per-strategy") { title = paste(title, args[5]) }
  h = hist(dat,
       main=title,
       xlab="Different functions",
       ylab="# of cases",
       breaks=seq(min(dat)-0.5, max(dat)+0.5, by=1))

  h$density = h$counts/sum(h$counts)
  plot(h,
       main=title,
       xlab="Different functions",
       ylab="relative # of cases",
       freq=FALSE)
} else {
  invalidDataPng(outPng)
}
