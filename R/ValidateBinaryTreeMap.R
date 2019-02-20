
#' create a conditional inference tree, minify it and cross-check the responses.
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

  # now auto-generate code containing function definitiion for the conditional logic and implement...
  logic <- paste("evalAutoCTree <- function(Temp, Wind){\n", writeTreeMapCondition(treeMap), "}")
  outScript <- paste0(tempfile(), ".R")
  write(logic, file = outScript)

  source(outScript)
  autoPredict <- apply(airQuality, 1, function(x) {evalAutoCTree(x["Temp"], x["Wind"])})

  # precision...
  identical(round(as.numeric(ozonePrediction), 13), round(as.numeric(autoPredict), 13))
}
