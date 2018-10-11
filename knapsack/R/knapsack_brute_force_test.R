W <- 25

set.seed(831117)
n <- 6
stuff <- data.frame(w = sample(1:10, n, replace = TRUE),
                    v = sample(4:12, n, replace = TRUE))

knapsack_brute_force(stuff,25)
