## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")

## ---- echo = FALSE, message = FALSE, warning=FALSE-----------------------
library(knapsack)
library(profvis)
library(parallel)
library(testthat)

## ---- echo = TRUE, eval = FALSE, message = FALSE-------------------------
#  brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

## ---- echo = TRUE, eval = FALSE, message = FALSE-------------------------
#  dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)

## ---- echo = TRUE, eval = FALSE, message = FALSE-------------------------
#  ### greedy heuristic here

## ---- echo = TRUE, eval = TRUE, message = FALSE--------------------------
# Sampled data for profiling
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

# Assign x and W to match function...
x <- knapsack_objects[1:1000,]
W <- 3500

## ---- echo = FALSE, eval = TRUE------------------------------------------
# Profiling of heuristic function:
p <- profvis::profvis({
  # pausing because function is too fast!
  profvis::pause(.01)
  
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
})

p

## ---- echo = TRUE, eval = TRUE-------------------------------------------
# Assign x, W and parallel to match function...
x        <- knapsack_objects[1:18,] 
W        <- 3500
parallel <- FALSE

## ---- echo = FALSE, eval = TRUE------------------------------------------
# Profiling of brute force function:
p <- profvis::profvis({
  # pausing because function is too fast!
  profvis::pause(.001)
  
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
  
  # runs computation in the specified no of segments and cores
  x <- as.matrix(x[,c("v","w")])
  max_per_core <-  mclapply(X = 1:core_count, FUN = max_core_combo, 
                            object_matrix = x, mc.cores = core_count)
  max_element <- which.max(do.call(rbind, max_per_core)[,1])
  
  list(value = max_per_core[[max_element]][1],
       elements = seq(1,n,1)[max_per_core[[max_element]][-c(1,2)] == 1L])
})

p

## ---- eval=FALSE, echo = TRUE--------------------------------------------
#  test_that("Correct object is returned", {
#    expect_silent(bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
#    expect_named(bfk, c("value", "elements"))
#  })

## ---- eval=FALSE, echo = TRUE--------------------------------------------
#  test_that("functions rejects errounous input.", {
#    expect_error(greedy_knapsack("hej", 3500))
#    expect_error(greedy_knapsack(x = knapsack_objects[1:8,], W = -3500))
#  })

