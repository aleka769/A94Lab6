
dynamic_knapsack <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(c("v", "w") %in% names(x))
  stopifnot(is.numeric(x$w)&&is.numeric(x$v))
  stopifnot(any(x$w <= W) || any(x$v > 0) )
  stopifnot(W > 0)
  
  value_mat <- matrix(NA, nrow = nrow(x) + 1, ncol = W + 1)
  value_mat[1,] <- 0
  rownames(value_mat) <- paste("i =", 0:(nrow(x)))
  colnames(value_mat) <- paste("j =", 0:(W))
  
  x <- x[order(x$w),]
  w <- x$w
  v <- x$v
  n <- nrow(x)
  
  for (i in 2:(n + 1)){
    for (j in 2:(W + 2)){
      if (w[i - 1] > j - 2){
        value_mat[i, j - 1] = value_mat[i - 1, j - 1]
      } else {
        value_mat[i, j - 1] = max( ( value_mat[i - 1, j - 1] ),
                                   ( value_mat[i - 1, j - 1 - w[i-1]] + v[i-1] ) )
        
      }
    }
  }
  return(value_mat)
}

# y <- data.frame(w = c(4,1,3,2,5,2),
#                 v = c(7,3,6,9,2,4))
# dynamic_knapsack(x = y, W = 6)


# index <- seq(3000, 10000, by = 20)
# 
# a <- lapply(index, FUN = function(i){
#   microbenchmark::microbenchmark(dynamic_knapsack(x = knapsack_objects[1:8,], W = i), unit = "s", times = 1L)$time / 100000000
# })
# 
# d <- data.frame(time = do.call(rbind, a), i = index)
# 
# ggplot(d, aes(i, time)) + geom_point()
