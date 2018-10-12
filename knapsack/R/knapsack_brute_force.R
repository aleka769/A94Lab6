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
brute_force_knapsack <- function(x, W, fast = FALSE, parallel = FALSE){
  if(fast){message("not yet implemented")}
  # error checks
  stopifnot(is.data.frame(x))
  stopifnot(c("v", "w") %in% names(x))
  stopifnot(is.numeric(x$w)&&is.numeric(x$v))
  stopifnot(any(x$w <= W) || any(x$v > 0))
  stopifnot(W > 0)
  
  x <- as.matrix(x[,c("v","w")])
  n <- dim(x)[1]
  combo_count <- (2^n)-1
  
  # checking if parallelization is requested and can be used
  if (parallel & .Platform$OS.type == "unix"){ 
    core_count <- parallel::detectCores()-1 
  }
  else if (parallel & .Platform$OS.type != "unix") {
    core_count <- 1L 
    message("Can't parallelize on non-unix OS, using one core!")
  }
  else {
    core_count <- 1L
  }

  # generating all combinations of elements as binary matrix
  combn_mat <- matrix(NA, nrow = combo_count, ncol = n)
  for (rownum in  1:combo_count){
    combn_mat[rownum+1, ] <- as.integer(intToBits(rownum))[1:n]
  }

  # result is matrix product of combinations and data
  result_mat <- combn_mat %*% x

  # removing combinations with weight above maxweight
  result_mat[result_mat[,"w"] > W, ] <- NA
  max_element <- which.max(result_mat[,"v"])
  return(list(value = unname(result_mat[max_element,"v"]),
              elements = seq(1,n,1)[combn_mat[max_element,] == 1L]))
}