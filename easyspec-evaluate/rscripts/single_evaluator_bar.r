args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 4) {
  stop("Usage: single_evaluator_bar.r input.csv output.png funcname evaluator")
}

inFile <- args[1]
outPng <- args[2]
funcname <- args[3]
evaluator <- args[4]


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
  , main=paste(funcname, evaluator)
  )
