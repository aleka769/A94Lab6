#' @title Knapsack greedy algorithm implementation
#'
#' @param x Data.frame with variables 'v' and 'w'
#' @param W Maximum weight that knapsack can be packed with
#' @param fast not implemented
#'
#' @return list with elements 
#'   \item 'elements' denoting which objects in x that were used
#'   \item 'value' denoting the sum of value for the used objects
#' @export
#'
#' @examples

greedy_knapsack <- function(x, W, fast = FALSE){
  
  if(fast){message("Sorry, not yet implemented")}
  
  # error checks
  stopifnot(is.data.frame(x))
  stopifnot(c("v", "w") %in% names(x))
  stopifnot(is.numeric(x$w)&&is.numeric(x$v))
  stopifnot(any(x$w <= W) || any(x$v > 0) )
  stopifnot(W > 0)
  
  x$quota <- x$v / x$w
  row_order <- order(x$quota, decreasing = TRUE)
  x <- x[row_order,]
  
  value <- 0
  elements <- vector("integer")
  cum_weight <- 0
  curr_object <- 1
  
  
  while (curr_object <= dim(x)[1]){
    if (cum_weight + x[curr_object,"w"] > W) { 
      curr_object <- curr_object + 1 
    } 
    else {
      value <- value + x[curr_object,"v"]
      cum_weight <- cum_weight + x[curr_object,"w"]
      elements <- c(elements, curr_object)
      curr_object <- curr_object + 1 
    }
    
  }
  
  list(value = value,
       elements = sort(row_order[elements]),
       cum_weight)
}
