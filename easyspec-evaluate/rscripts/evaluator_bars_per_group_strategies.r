library(ggplot2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 15) {
  print(args)
  stop("Usage: evaluator_bars_per_group_strategies.r common.r input.csv output.pdf granularity groupName evaluator1 unit1 quantity1 indication1 evaluator2 unit2 quantity2 indication2 strategy1 strategy2")
}

common <- args[1]
inFile <- args[2]
outPdf <- args[3]
# granularity <- args[4]
groupName <- args[5]
e1 <- args[6]
u1 <- args[7]
q1 <- args[8]
i1 <- args[9]
e2 <- args[10]
u2 <- args[11]
q2 <- args[11]
i2 <- args[12]
s1 <- args[13]
s2 <- args[14]

source(common)

res = read.csv(inFile, header=TRUE)

res$origin <- paste(res$file, res$focus, res$strategy)

res <- res[(res$strategy == s1 | res$strategy == s2),]

# Select the right data
res1 <- res[res$evaluator == e1,]
res2 <- res[res$evaluator == e2,]

# Make the output numeric
res$output <- suppressWarnings(as.numeric(as.character(res$output)))

# Replace NaN with '0'
res$output <- replace(res$output, is.na(res$output), 0)

dat <- merge(res1, res2, by = "origin")
dat <- dat[!is.na(dat$output.x),]
dat <- dat[!is.na(dat$output.y),]

dat$strategy <- dat$strategy.x

if (length(dat$origin) != 0) {
  startPdf(outPdf)
    

  ggplot(dat, aes(output.x, output.y, fill = strategy)) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    ggtitle(paste(e2, "in terms of", e1, paste("(", i2, ")", sep=""))) +
    labs(x = e1) + labs(y = e2)

} else {
  invalidDataPdf(outPdf)
}

