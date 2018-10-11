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
  # error checks
  stopifnot(is.data.frame(x))
  stopifnot(c("v", "w") %in% names(x))
  stopifnot(is.numeric(x$w)&&is.numeric(x$v))
  stopifnot(any(x$w) <= W || any(x$v) > 0)
  
  x <- as.matrix(x)
  n <- dim(x)[1]
  
  # generating all combinations of elements as binary matrix
  combn_mat <- matrix(NA, nrow = 2^n, ncol = n)
  for (rownum in  0:(nrow(combn_mat)-1)){
    combn_mat[rownum+1, ] <- as.integer(intToBits(rownum))[1:n]
  }
  
  # result is matrix product of combinations and data
  result_mat <- combn_mat %*% x
  
  # removing combinations with weight above maxweight
  result_mat[result_mat[,"w"] > W,2] <- NA
  
  max_element <- which.max(result_mat[,"v"])
  
  list(element = seq(1,n,1)[combn_mat[max_element,] == 1L],
       value = unname(result_mat[max_element,"v"]))
}
