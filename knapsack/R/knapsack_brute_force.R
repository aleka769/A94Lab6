#' @title Knapsack brute force algorithm implementation
#'
#' @param x Data.frame with variables 'v' and 'w'
#' @param W Maximum weight that knapsack can be packed with
#' @param parallel set to TRUE to use all but one cores of CPU for algorithm solving, only used in unx environments
#'
#' @return list with elements 
#' \itemize{
#'   \item {'elements' denoting which objects in x that were used}
#'   \item {'value' denoting the sum of value for the used objects}
#'   }
#' @export
#' @import parallel
#' @examples \dontrun{
#' set.seed(666)
#' n <- 9
#' stuff <- data.frame(w = sample(1:10, n, replace = TRUE),
#'                     v = sample(4:12, n, replace = TRUE))
#' # returns value = 46 made up from elements 2,4,5,7,8,9
#' brute_force_knapsack(stuff,25)
#' }
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
  # workaround to get parallelisation through check according to
  # https://cran.r-project.org/web/packages/policies.html
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    core_count <- 2L
  }
  else if (n >= min_parallel & parallel & .Platform$OS.type == "unix"){ 
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
  max_per_core <-  parallel::mclapply(X = 1:core_count, FUN = max_core_combo, 
                                      object_matrix = x, mc.cores = core_count)
  max_element <- which.max(do.call(rbind, max_per_core)[,1])
  
  list(value = max_per_core[[max_element]][1],
       elements = seq(1,n,1)[max_per_core[[max_element]][-c(1,2)] == 1L])
}
