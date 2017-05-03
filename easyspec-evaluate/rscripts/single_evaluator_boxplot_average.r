args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 5) {
  stop("Usage: single_evaluator_boxplot_average input.csv output.png sourcename sourcepath evaluator")
}

inFile <- args[1]
outPng <- args[2]
sourcepath <- args[3]
sourcename <- args[4]
evaluator <- args[5]


res = read.csv(inFile, header=TRUE)
res <- res[res$path == sourcepath,]
res <- res[res$evaluator == evaluator,]
print(res)

png(outPng, height=400, width=1200, bg="white")
aggregate(output ~ strategy, res, mean)
boxplot(output ~ strategy, res, main=paste("Averages for", "Source:", sourcename, ", ", "Evaluator:", evaluator))
