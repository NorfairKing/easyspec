invalidDataPng <- function(outPng) {
  png( outPng
    , height=400
    , width=1200
    , bg="white"
    )

  plot(1, 1, main="No image could be generated: No non-NA data.")
}
