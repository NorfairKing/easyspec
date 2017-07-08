args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 4) {
  stop("Usage: distribution-nr-different-functions-per-equation.r common.r input.csv output.pdf granularity")
}

common <- args[1]
inFile <- args[2]
outPdf <- args[3]
granularity <- args[4]

source(common)

res = read.csv(inFile, header=TRUE)

dat = as.numeric(as.character(res$nrDifferentFunctions))

if (length(dat) != 0) {
  startPdf(outPdf)
  title = "Histogram of the number of different functions in an equation"
  if (granularity == "group-example-name-strategy") { title = paste(title, args[5], args[6], args[7]) }
  if (granularity == "group-strategy") { title = paste(title, args[5]) }
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
  invalidDataPdf(outPdf)
}
