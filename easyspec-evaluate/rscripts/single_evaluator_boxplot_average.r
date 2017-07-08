args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 8) {
  print(args)
  stop("Usage: single_evaluator_boxplot_average.r common.r input.csv output.pdf granularity group example evaluator indication")
}

common <- args[1]
inFile <- args[2]
outPdf <- args[3]
# granularity <- args[4]
group <- args[5]
sourcefile <- args[6]
evaluator <- args[7]
indication <- args[8]

source(common)

res = read.csv(inFile, header=TRUE)
res <- res[res$file == sourcefile,]
res <- res[res$evaluator == evaluator,]
res <- res[!is.na(res$output),]


if (length(res$output) != 0) {
  startPdf(outPdf)
  aggregate(output ~ strategy, res, mean)
  par(mar=c(3.5,0.41,0.41,0.21))
  boxplot(output ~ strategy, res, main=paste("Averages for", "Source:", sourcefile, ", ", "Evaluator:", evaluator, paste("(", indication, ")", sep="")), las = 2)
} else {
  invalidDataPdf(outPdf)
}
