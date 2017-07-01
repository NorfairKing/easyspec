args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 4) {
  stop("Usage: occurrences-in-all-equations.r common.r input.csv output.png (individual) [example] [focus] [strategy]")
}

common <- args[1]
inFile <- args[2]
outPng <- args[3]
kind <- args[4]

source(common)

res = read.csv(inFile, header=TRUE)

dat = as.numeric(as.character(res$occurrences))

if (length(dat) != 0) {
  png(outPng, height=900, width=1200, bg="white")
  title = "Histogram of the number of occurrences of the same function in all equations"
  if (kind == "individual") { title = paste(title, args[5], args[6], args[7]) }
  if (kind == "per-strategy") { title = paste(title, args[5]) }
  h = hist(dat,
       main=title,
       xlab="Occurrences",
       ylab="# of cases",
       breaks=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5))

  h$density = h$counts/sum(h$counts)
  plot(h,
       main=title,
       xlab="Occurrences",
       ylab="relative # of cases",
       freq=FALSE)
} else {
  invalidDataPng(outPng)
}
