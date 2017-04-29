args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 3) {
  stop("Usage: runtime.r input.csv output.png funcname")
}

inFile <- args[1]
outPng <- args[2]
funcname <- args[3]


res = read.csv(inFile, header=TRUE)
res <- res[res$focus == funcname,]
res <- res[res$evaluator == "runtime",]


# Convert to string
res$output <- as.character(res$output)
# Remove last char:  0.12s -> 0.12
res$output <- sapply(res$output, function(x) substr(x, 1, nchar(x) - 1))
# Convert to number
res$output <- as.numeric(res$output)

png(outPng, height=400, width=1200, bg="white")

# Extra large bottom margin
barplot(res$output, names.arg=res$strategy)
