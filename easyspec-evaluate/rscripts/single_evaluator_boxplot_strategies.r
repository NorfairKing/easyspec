args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 11) {
  print(args)
  stop("Usage: single_evaluator_boxplot_strategies.r common.r input.csv output.pdf granularity group evaluator unit quantity indication strategy1 strategy2")
}

common <- args[1]
inFile <- args[2]
outPdf <- args[3]
# granularity <- args[4]
group <- args[5]
evaluator <- args[6]
unit <- args[7]
quantity <- args[8]
indication <- args[9]
s1 <- args[10]
s2 <- args[11]

source(common)

res = read.csv(inFile, header=TRUE)
res <- res[res$evaluator == evaluator,]
res <- res[(res$strategy == s1 | res$strategy == s2),]
res$strategy <- droplevels(res$strategy) # Necessary for some reason, otherwise unused strategies show up

res <- res[!is.na(res$output),]

if (length(res$output) != 0) {
  startPdf(outPdf)
  aggregate(output ~ strategy, res, mean)
  par(mar=c(4,10,4,3))
  boxplot(output ~ strategy, res, main=paste("Boxplot for", evaluator, paste("(", indication, ")", sep="")), horizontal=TRUE, las=2)
} else {
  invalidDataPdf(outPdf)
}
