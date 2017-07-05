args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 4) {
  stop("Usage: size-of-property.r common.r input.csv output.png granularity")
}

common <- args[1]
inFile <- args[2]
outPng <- args[3]
granularity <- args[4]

source(common)

res = read.csv(inFile, header=TRUE)

dat = as.numeric(as.character(res$size))

if (length(dat) != 0) {
  png(outPng, height=900, width=1200, bg="white")
  title = "Histogram of the size of a property"
  if (granularity == "group-example-name-strategy") { title = paste(title, args[5], args[6], args[7]) }
  if (granularity == "group-strategy") { title = paste(title, args[5]) }
  h = hist(dat,
       main=title,
       xlab="Size",
       ylab="# of cases",
       breaks=seq(min(dat)-0.5, max(dat)+0.5, by=1))

  h$density = h$counts/sum(h$counts)
  plot(h,
       main=title,
       xlab="Size",
       ylab="relative # of cases",
       freq=FALSE)
} else {
  invalidDataPng(outPng)
}
