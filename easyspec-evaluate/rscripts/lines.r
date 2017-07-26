args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 5) {
  stop("Usage: lines.r common.r input.csv output.pdf evaluator indication")
}

common <- args[1]
inFile <- args[2]
outPdf <- args[3]
evaluator <- args[4]
indication <- args[5]

source(common)

res = read.csv(inFile, header=TRUE)

baseline = "full-background"


# res <- res[res$path == sourcepath,]
res <- res[res$evaluator == evaluator,]

# Merge the 'source' and 'focus' collumns to 'origin'.
res$origin <- paste(res$path, res$focus)

if (length(res$output) != 0) { 
  startPdf(outPdf)

  # To draw legend outside of graph
  par(xpd = TRUE, mar=c(0.4,0.4,0.4,0.15))

  basevals <- res[res$strategy == baseline,]


  plot( basevals$output / basevals$output
    , type = "n" # Lines
    , main = paste("Evaluator:", evaluator, paste("(", indication, ")", sep="")) # Title
    , ylim = c(0, 2)
    , axes = FALSE
    , xlab = "source, focus"
    , ylab = "relative comparison with full-background"
    )

  axis( 1
    , at = 1:length(basevals$origin)
    , labels = basevals$origin
    , las = 2
    )

  strats = unique(res$strategy)

  cols = 1:length(strats)

  for (i in seq_along(strats)) {
    strat = strats[i]
    dat <- res[res$strategy == strat,]
    lines(dat$output / basevals$output, col = cols[i])
  }

  legend("right"
    , inset=c(-0.5, 0)
    , legend=levels(factor(strats))
    , col=as.numeric(unique(factor(strats)))
    , lty = 1
    )
} else {
  invalidDataPdf(outPdf)
}
