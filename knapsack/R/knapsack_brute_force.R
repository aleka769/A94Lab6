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
brute_force_knapsack <- function(x, W, parallel = FALSE){
  # error checks
  stopifnot(is.data.frame(x))
  stopifnot(c("v", "w") %in% names(x))
  stopifnot(is.numeric(x$w)&&is.numeric(x$v))
  stopifnot(any(x$w <= W) || any(x$v > 0))
  stopifnot(W > 0)
  
  n <- dim(x)[1]
  
  max_objects <- 30L
  min_parallel <- 20L
  force_parallel <- 25L
  # stopping if data will be too large
  stopifnot(n <= max_objects)
  if(.Platform$OS.type != "unix" & n > force_parallel){
    stop("Can only run calculations on large object sets in parallel which requires Linux")
  }
  
  # define function to do actual computaion and generate candidates for max value
  max_core_combo <- function(index, combo_vec=core_combos[[index]], object_matrix){
    n <- dim(object_matrix)[1]
    
    combn_mat <- matrix(NA, nrow = length(combo_vec), ncol = n)
    for (rownum in  1:length(combo_vec)){
      combn_mat[rownum, ] <- as.integer(intToBits(combo_vec[rownum]))[1:n]
    }
    
    result_mat <- combn_mat %*% object_matrix
    
    # removing combinations with weight above maxweight
    result_mat[result_mat[,"w"] > W, ] <- NA
    
    max_element <- which.max(result_mat[,"v"])
    
    c(unname(result_mat[max_element, ]), combn_mat[max_element, ])
  }
  
  # checking if parallelization is requested and can be used
  if (n >= min_parallel & parallel & .Platform$OS.type == "unix"){ 
    core_count <- parallel::detectCores()-1 
  }
  else if (parallel & .Platform$OS.type != "unix") {
    core_count <- 1L 
    message("Can't parallelize on non-unix OS, using one core!")
  }
  else {
    core_count <- 1L
  }
  
  # experimental way of handling memory issues in large object sets
  if (n %in% seq(force_parallel,max_objects)) {
    core_count <- core_count * 2^(n-force_parallel)
  }
  
  # generate list of numbers for each core to copmute on
  combo_count <- (2^n)-1
  combos_per_core <- combo_count%/%core_count
  core_combos <- lapply(1:core_count, function(x){
    seq((x-1)*(combos_per_core),(x)*(combos_per_core),1)
  })
  if (combo_count%%core_count != 0){
    remainder_seq <- seq(combos_per_core*core_count+1,combo_count,1)
    core_combos[[core_count]]<-c(core_combos[[core_count]],remainder_seq)
  }
  
  # runs computation in the specified no of segments and cores
  x <- as.matrix(x[,c("v","w")])
  max_per_core <-  mclapply(X = 1:core_count, FUN = max_core_combo, 
                            object_matrix = x, mc.cores = core_count)
  max_element <- which.max(do.call(rbind, max_per_core)[,1])
  
  list(value = max_per_core[[max_element]][1],
       elements = seq(1,n,1)[max_per_core[[max_element]][-c(1,2)] == 1L])
}
