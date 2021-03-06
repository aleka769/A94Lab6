#' @title Knapsack greedy algorithm implementation
#'
#' @param x Data.frame with variables 'v' and 'w'
#' @param W Maximum weight that knapsack can be packed with
#' @param fast not implemented
#'
#' @return list with elements 
#' \itemize{
#'   \item {'elements' denoting which objects in x that were used}
#'   \item {'value' denoting the sum of value for the used objects}
#'   }
#' @export
#'
#' @examples \dontrun{
#' set.seed(666)
#' n <- 9000
#' stuff <- data.frame(w = sample(1:10, n, replace = TRUE),
#'                     v = sample(4:12, n, replace = TRUE))
#' # returns value = 46 made up from elements 2,4,5,7,8,9
#' greedy_knapsack(stuff,25)
#' }
greedy_knapsack <- function(x, W, fast = FALSE){
  
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
       elements = sort(row_order[elements]))
}
