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
brute_force_knapsack <- function(x, W, fast = FALSE, parallell = FALSE){
  if(fast){message("not yet implemented")}
  # error checks
  stopifnot(is.data.frame(x))
  stopifnot(c("v", "w") %in% names(x))
  stopifnot(is.numeric(x$w)&&is.numeric(x$v))
  stopifnot(any(x$w <= W) || any(x$v > 0))
  stopifnot(W > 0)
  
  x <- as.matrix(x)
  n <- dim(x)[1]
  
  # generating all combinations of elements as binary matrix
  combn_mat <- matrix(NA, nrow = 2^n, ncol = n)
  for (rownum in  0:(nrow(combn_mat)-1)){
    combn_mat[rownum+1, ] <- as.integer(intToBits(rownum))[1:n]
  }
  
  # result is matrix product of combinations and data
  if(parallell){
    result_mat <- parallel::mclapply(X=1:2^n, mc.cores = 1, FUN=function(row){
      `%*%`(combn_mat[row,],x)})
  } else {
    result_mat <- combn_mat %*% x
  }
  
  # removing combinations with weight above maxweight
  result_mat[result_mat[,"w"] > W,2] <- NA
  
  max_element <- which.max(result_mat[,"v"])
  
  list(value = unname(result_mat[max_element,"v"]),
       elements = seq(1,n,1)[combn_mat[max_element,] == 1L])
}
