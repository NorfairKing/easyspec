args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 6) {
  stop("Usage: single_evaluator_bar.r input.csv output.png basedir sourcefile funcname evaluator")
}

inFile <- args[1]
outPng <- args[2]
basedir <- args[3]
sourcefile <- args[4]
funcname <- args[5]
evaluator <- args[6]


res = read.csv(inFile, header=TRUE)
res <- res[res$focus == funcname,]
res <- res[res$evaluator == evaluator,]

# Make the output numeric
res$output <- suppressWarnings(as.numeric(as.character(res$output)))

# Replace NaN with '0'
res$output <- replace(res$output, is.na(res$output), 0)

png(outPng, height=400, width=1200, bg="white")

# Extra large bottom margin
barplot(
    res$output
  , names.arg=res$strategy
  , main=paste("Source:", sourcefile, ", ", "Focus:", funcname, ", ", "Evaluator:", evaluator)
  )
