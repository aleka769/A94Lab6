<<<<<<< HEAD
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(w=sample(x = 1:4000, 
                                        size = n, 
                                        replace = TRUE), 
                               v=runif(n = n, 
                                       min = 0, 
                                       max = 10000)
                               )

brute_force_knapsack <- function(x, W){
  stopifnot(is.data.frame(x) == TRUE)
  stopifnot(names(x) %in% c("v", "w"))
  stopifnot(is.numeric(x$v), is.numeric(x$w))
  stopifnot(x$v > 0, x$w > 0)
  
  n <- nrow(x)
  
  lapply(X = 1:n, FUN = function(i){
    c_ <- combn(1:n, i)
    print(c_[i])
    # apply(X = t(c_), MARGIN = 1, function(s){
    #   rowSums(x[s,w])
    # })
  })
  lapply(items, FUN = function(i){
    x$w[i,]
  })
  
  d <- lapply(X = 1:n, FUN = function(element){
    data.frame(
      item   = combn(1:n, element) %>% 
        apply(MARGIN = 2, paste0, collapse = " ") %>%
        as.character(),
      weight = combn(x$w, element) %>% 
        apply(MARGIN = 2, sum),
      cost   = combn(x$v, element) %>%
        apply(MARGIN = 2, sum)
    )}) %>% 
    do.call(rbind, .) %>% 
    filter(weight <= W) %>%
    slice(which.max(cost))
  
  return(list("value"    = d$cost,
              "elements" = d$item %>% as.character() %>% cat()))
}
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

nt
=======
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
>>>>>>> fa49feb285bb7dd12c79574965492a1bcfefaab0
