args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 5) {
  stop("Usage: single_evaluator_boxplot_average.r input.csv output.png basedir sourcefile evaluator")
}

inFile <- args[1]
outPng <- args[2]
basedir <- args[3]
sourcefile <- args[4]
evaluator <- args[5]


res = read.csv(inFile, header=TRUE)
res <- res[res$file == sourcefile,]
res <- res[res$evaluator == evaluator,]

png(outPng, height=400, width=1200, bg="white")
aggregate(output ~ strategy, res, mean)
boxplot(output ~ strategy, res, main=paste("Averages for", "Source:", sourcefile, ", ", "Evaluator:", evaluator))
