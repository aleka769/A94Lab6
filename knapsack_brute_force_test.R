W <- 25

set.seed(831117)
n <- 20
stuff <- data.frame(w = sample(1:10, n, replace = TRUE),
                    v = sample(4:12, n, replace = TRUE))

system.time(brute_force_knapsack(stuff,25))
system.time(brute_force_knapsack(stuff,25,parallel = TRUE))
