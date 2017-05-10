args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 4) {
  stop("Usage: lines.r common.r input.csv output.png evaluator")
}

common <- args[1]
inFile <- args[2]
outPng <- args[3]
evaluator <- args[4]

source(common)

res = read.csv(inFile, header=TRUE)

baseline = "full-background"


# res <- res[res$path == sourcepath,]
res <- res[res$evaluator == evaluator,]

# Merge the 'source' and 'focus' collumns to 'origin'.
res$origin <- paste(res$path, res$focus)

if (length(res$output) != 0) { 
  png( outPng
    , height=400
    , width=1200
    , bg="white"
    )
  # To draw legend outside of graph
  par(xpd = TRUE, mar=c(4,4,4,15))

  basevals <- res[res$strategy == baseline,]


  plot( basevals$output / basevals$output
    , type = "n" # Lines
    , main = paste("Evaluator:", evaluator) # Title
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
  invalidDataPng(outPng)
}
