args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 8) {
  stop("Usage: points.r common.r input.csv output.pdf evaluator1 evaluator2 name indication1 indication2")
}

common <- args[1]
inFile <- args[2]
outPdf <- args[3]
e1 <- args[4]
e2 <- args[5]
name <- args[6]
i1 <- args[7]
i2 <- args[8]

source(common)

res = read.csv(inFile, header=TRUE)

if (length(res$output) != 0) {
  res$origin <- paste(res$path, res$focus)

  res1 <- res[res$evaluator == e1,]
  res2 <- res[res$evaluator == e2,]


  dat <- merge(res1, res2, by = "origin")
  dat <- dat[!is.na(dat$output.x),]
  dat <- dat[!is.na(dat$output.y),]

  startPdf(outPdf)

  par(xpd=TRUE, mar=c(0.4,0.4,0.4,3.5))

  plot(dat$output.x
    , dat$output.y
    , xlab = paste(e1, paste("(", i1, ")", sep=""))
    , ylab = paste(e2, paste("(", i2, ")", sep=""))
    , type = "n" # Don't plot yet.
    , main = paste(name, "Correlation:", format(round(cor(dat$output.x, dat$output.y), 2), nsmall = 2))
    )

  strats = unique(res$strategy)
  for (i in seq_along(strats)) { 
    strat = strats[i]
    sdat <- dat[dat$strategy.x == strat,]
    points(sdat$output.x
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
  invalidDataPdf(outPdf)
}
