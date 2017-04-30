args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 5) {
  stop("Usage: single_evaluator_bar.r input.csv output.png sourcename funcname evaluator")
}

inFile <- args[1]
outPng <- args[2]
sourcename <- args[3]
funcname <- args[4]
evaluator <- args[5]


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
  , main=paste("Source:", sourcename, ", ", "Focus:", funcname, ", ", "Evaluator:", evaluator)
  )
