
#' create a confitional inference tree, minify it and cross-check the responses.
#' analysis taken from https://datawookie.netlify.com/blog/2013/05/package-party-conditional-inference-trees/
#' @importFrom utils data
#' @importFrom stats predict
#' @export
validateBinaryTreeMap <- function(){

  airQuality <- subset(datasets::airquality, !is.na(datasets::airquality$Ozone))

  airTree <- party::ctree(Ozone ~ ., data = airQuality, controls = party::ctree_control(maxsurrogate = 3))

  airQualityNew <- airQuality

  sapply(airQuality[,-1], class)
  sapply(airQualityNew, class)

  ozonePrediction <- predict(airTree, newdata = airQualityNew)

  treeMap               <- createBinaryTreeMap(airTree@tree)
  ozonePredictionMapped <- getBinaryTreeResponse(treeMap, airQuality)

  identical(as.numeric(ozonePrediction), ozonePredictionMapped)
}
