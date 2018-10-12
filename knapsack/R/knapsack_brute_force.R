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