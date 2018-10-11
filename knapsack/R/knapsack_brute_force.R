#' @title Knapsack brute force algorithm implementation
#'
#' @param x Data.frame with variables 'v' and 'w'
#' @param W Maximum weight that knapsack can be packed with
#'
#' @return list with elements 
#'   \item 'elements' denoting which objects in x that were used
#'   \item 'value' denoting the sum of value for the used objects
#' @export
#'
#' @examples
knapsack_brute_force <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(c("v", "w") %in% names(x))
  stopifnot(is.numeric(x$w)&&is.numeric(x$v))
  stopifnot(any(x$w) <= W || any(x$v) > 0)
  
  n <- dim(x)[1]
  
  combn_mat <- matrix(NA, nrow = 2^n, ncol = n)
  for (rownum in  0:(nrow(combn_mat)-1)){
    combn_mat[rownum+1, ] <- as.integer(intToBits(rownum))[1:n]
  }
  
  stuff_mat <- as.matrix(stuff)
  
  result_mat <- combn_mat %*% stuff_mat
  result_mat[result_mat[,"w"] > W,2] <- NA
  max_element <- which.max(result_mat[,"v"])
  
  list(element = seq(1,n,1)[combn_mat[max_element,] == 1L],
       value = result_mat[max_element,"v"])
}