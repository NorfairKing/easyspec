args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 5) {
  stop("Usage: points.r common.r input.csv output.png evaluator1 evaluator2")
}

common <- args[1]
inFile <- args[2]
outPng <- args[3]
e1 <- args[4]
e2 <- args[5]

source(common)

res = read.csv(inFile, header=TRUE)

res$origin <- paste(res$path, res$focus)

res1 <- res[res$evaluator == e1,]
res2 <- res[res$evaluator == e2,]


dat <- merge(res1, res2, by = "origin")

png( outPng
  , height=600
  , width=1200
  , bg="white"
  )

par(xpd=TRUE, mar=c(4,4,4,35))

plot(dat$output.x
  , dat$output.y
  , xlab = e1
  , ylab = e2
  , type = "n" # Don't plot yet.
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
