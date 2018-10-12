
#' Gets the response for a single set of inputs by traversing the tree map
#' @param treeMap a minified BinaryTree exported as a data.frame
#' @param inputData a data.frame row containing the inputs to be evaluated
#' @param node an integer representing the nodeID currently under evaluation
#' @return the repsonse for the given input
#' @export
evaluateBinaryTreeMap <- function(treeMap, inputData, node = 1){

  treeRow <- treeMap[treeMap$Node == node,]

  if(treeRow$IsTerminal){
    return(treeRow$Prediction)
  }
  expr   <- treeRow$Expression
  result <- eval(parse(text = expr), inputData) # slow :(

  if(result) {
    evaluateBinaryTreeMap(treeMap, inputData, treeRow$Left)
  } else {
    evaluateBinaryTreeMap(treeMap, inputData, treeRow$Right)
  }
}

#' Given a binary tree map, and a data frame of inputs determine the responses
#' @param treeMap a minified BinaryTree exported as a data.frame
#' @param inputData a data.frame containing the inputs to be evaluated
#' @return the repsonses for the given inputs
#' @export
getBinaryTreeResponse <- function(treeMap, inputData){

  varNames <- unique(unlist(lapply(treeMap$Expression, function (x) unlist(strsplit(x, " "))[1])))
  varNames <- varNames[!is.na(varNames)]

  if(!all(varNames %in% names(inputData))){
    stop(paste("You must provide the correct inputs!:", paste(varNames, collapse = ", ")))
  }

  responses <- vector("list", length = nrow(inputData))
  for(i in 1:nrow(inputData)){
    responses[[i]] <- evaluateBinaryTreeMap(treeMap, inputData[i,])
  }
  return(unlist(responses))

}
