args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 3) {
  stop("Usage: lines.r input.csv output.png evaluator")
}

inFile <- args[1]
outPng <- args[2]
evaluator <- args[3]

res = read.csv(inFile, header=TRUE)

baseline = "full-background"


# res <- res[res$path == sourcepath,]
res <- res[res$evaluator == evaluator,]

# Merge the 'source' and 'focus' collumns to 'origin'.
res$origin <- paste(res$path, res$focus)

png( outPng
  , height=400
  , width=1200
  , bg="white"
  )

if (length(res$output) != 0) { 
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
  plot(1, 1, main="No image could be generated: No non-NA data.")
}
