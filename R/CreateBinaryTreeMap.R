
#' Given a BinaryTree, traverse the nodes and map out the conditional logic
#' @importFrom utils capture.output
#' @param t an object of class BinaryTree
#' @param df a data.frame
#' @param path a string representing the nodes traversed
#' @return a data.frame representation of a BinaryTree
#' @export
createBinaryTreeMap <- function(t, df = NULL, path = NULL){

  node  <- t$node
  term  <- t$terminal

  if(term){
    expr  <- ""
    pred  <- t$prediction
    left  <- NA
    right <- NA
  } else {
    expr  <- as.character(capture.output(print(t$psplit)))
    pred  <- NA
    left  <- t$left$node
    right <- t$right$node
  }

  path  <- ifelse(is.null(path), node, paste(path, node, sep = ":"))

  df <- rbind(df, data.frame(Node = node, Expression = expr, Prediction = pred, IsTerminal = term, Left = left, Right = right, Path = path, stringsAsFactors = FALSE))
  if(term) { return(df) }

  df <- createBinaryTreeMap(t$left,  df, path)
  df <- createBinaryTreeMap(t$right, df, path)
}
