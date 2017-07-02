args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 6) {
  stop("Usage: single_evaluator_boxplot_average.r common.r input.csv output.png granularity evaluator indication")
}

common <- args[1]
inFile <- args[2]
outPng <- args[3]
# granularity <- args[4]
evaluator <- args[5]
indication <- args[6]

source(common)

res = read.csv(inFile, header=TRUE)
res <- res[res$evaluator == evaluator,]
res <- res[!is.na(res$output),]


if (length(res$output) != 0) {
  png(outPng, height=1200, width=1200, bg="white")
  aggregate(output ~ strategy, res, mean)
  par(mar=c(35,4.1,4.1,2.1))
  boxplot(output ~ strategy, res, main=paste("Global averages for", "Evaluator:", evaluator, paste("(", indication, ")", sep="")), las = 2)
} else {
  invalidDataPng(outPng)
}
