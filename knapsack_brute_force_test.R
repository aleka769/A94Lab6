W <- 25

set.seed(831117)
n <- 1000000
stuff <- data.frame(w = sample(1:10, n, replace = TRUE),
                    v = sample(4:12, n, replace = TRUE))

system.time(brute_force_knapsack(stuff[1:16,],25))
system.time(dynamic_knapsack(stuff[1:500,],25))
system.time(greedy_knapsack(stuff,25))
system.time(brute_force_knapsack(stuff,25,parallel = TRUE))
