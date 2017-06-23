args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 8) {
  stop("Usage: single_evaluator_bar.r common.r input.csv output.png basedir sourcefile funcname evaluator indication")
}

common <- args[1]
inFile <- args[2]
outPng <- args[3]
basedir <- args[4]
sourcefile <- args[5]
funcname <- args[6]
evaluator <- args[7]
indication <- args[8]

source(common)

res = read.csv(inFile, header=TRUE)
res <- res[res$focus == funcname,]
res <- res[res$evaluator == evaluator,]

# Make the output numeric
res$output <- suppressWarnings(as.numeric(as.character(res$output)))

# Replace NaN with '0'
res$output <- replace(res$output, is.na(res$output), 0)


if(length(res$output) != 0) {
  png(outPng, height=900, width=1200, bg="white")
  par(mar=c(35,4.1,4.1,2.1))

  # Extra large bottom margin
  barplot(
      res$output
    , names.arg=res$strategy
    , main = paste("Source:", sourcefile, ", ", "Focus:", funcname, ", ", "Evaluator:", evaluator, paste("(", indication, ")", sep=""))
    , las = 2
    )
} else {
  invalidDataPng(outPng)
}
