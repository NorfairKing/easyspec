args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 7) {
  stop("Usage: single_evaluator_boxplot_average.r common.r input.csv output.pdf granularity group evaluator indication")
}

common <- args[1]
inFile <- args[2]
outPdf <- args[3]
# granularity <- args[4]
evaluator <- args[6]
indication <- args[7]

source(common)

res = read.csv(inFile, header=TRUE)
res <- res[res$evaluator == evaluator,]

res <- res[!is.na(res$output),]

if (length(res$output) != 0) {
  startPdf(outPdf)
  aggregate(output ~ strategy, res, mean)
  par(mar=c(4,10,4,3))
  boxplot(output ~ strategy, res, main=paste("Boxplot for", evaluator, paste("(", indication, ")", sep="")), horizontal=TRUE, las=2)
} else {
  invalidDataPdf(outPdf)
}

