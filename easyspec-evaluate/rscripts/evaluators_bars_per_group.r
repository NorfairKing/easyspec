library(ggplot2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 6) {
  stop("Usage: evaluators_bars_per_group.r common.r input.csv output.png groupName evaluator1 evaluator2")
}

common <- args[1]
inFile <- args[2]
outPng <- args[3]
groupName <- args[4]
e1 <- args[5]
e2 <- args[6]

source(common)

res = read.csv(inFile, header=TRUE)

res <- res[res$strategy == "full-background" | res$strategy == "full-breakthrough-1",]

res$origin <- paste(res$file, res$focus, res$strategy)

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

if (length(dat$origin) != 0) {
    png( outPng
    , height=600
    , width=1200
    , bg="white"
    )

  ggplot(dat, aes(output.x, output.y, fill = strategy.x)) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    ggtitle(paste(e2, "in terms of", e1)) +
    labs(x = e1) + labs(y = e2)

} else {
  invalidDataPng(outPng)
}

