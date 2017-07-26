invalidDataPdf <- function(outPdf) {
  pdf( outPdf
    , height=400
    , width=1200
    , bg="white"
    )

  plot(1, 1, main="No image could be generated: No non-NA data.")
}

startPdf <- function(outPdf) {
  pdf(
      outPdf
    , height=4
    , width=8
    , bg="white"
    )
}
