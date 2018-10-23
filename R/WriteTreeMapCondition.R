#' Auto generate the conditional logic from BinaryTree exported as a data.frame by \code{\link{createBinaryTreeMap}}
#'
#' @param treeMap the exported tree map (data.frame)
#' @param node id for recursive evaluation
#'
#' @return string containing the response function
#' @export
writeTreeMapCondition <- function(treeMap, node = 1){

  treeRow <- treeMap[treeMap$Node == node,]

  if(treeRow$IsTerminal){
    expr <- paste("# node", node, "\nreturn(", treeRow$Prediction,")")
    return(expr)
  }
  expr <- paste("# node ",node, "\nif(", treeRow$Expression, "){\n", writeTreeMapCondition(treeMap, treeRow$Left), "} else {\n", writeTreeMapCondition(treeMap, treeRow$Right), "}")
  return(expr)
}
