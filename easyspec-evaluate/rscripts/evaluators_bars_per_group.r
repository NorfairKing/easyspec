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

res$origin <- paste(res$path, res$focus)

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

head(dat)
if (length(dat$origin) != 0) {
    png( outPng
    , height=600
    , width=1200
    , bg="white"
    )

  par(xpd=TRUE, mar=c(4,4,4,35))

  plot(dat$output.x
    , dat$output.y
    , type = "n" # Don't plot yet.
    , main = "title"
    )

  strats = unique(res$strategy)
  for (i in seq_along(strats)) {
    strat = strats[i]
    sdat <- dat[dat$strategy.x == strat,]
    lines(sdat$output.x
      , sdat$output.y
      , col = i
      )
  }


  # To draw legend outside of graph
  legend("right"
    , inset=c(-0.8, 0)
    , legend=levels(factor(strats))
    , col=as.numeric(unique(factor(strats)))
    , lty = 1
    )

} else {
  invalidDataPng(outPng)
}

