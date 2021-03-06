% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knapsack_dynamic.R
\name{dynamic_knapsack}
\alias{dynamic_knapsack}
\title{Knapsack dynamic algorithm implementation}
\usage{
dynamic_knapsack(x, W)
}
\arguments{
\item{x}{Data.frame with variables 'v' and 'w'}

\item{W}{Maximum weight that knapsack can be packed with}
}
\value{
list with elements 
\itemize{
  \item {'elements' denoting which objects in x that were used}
  \item {'value' denoting the sum of value for the used objects}
}
}
\description{
Knapsack dynamic algorithm implementation
}
\examples{
\dontrun{
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
dynamic_knapsack(x = knapsack_objects[1:8,], W = 2000)
}
}
\references{
\url{https://en.wikipedia.org/wiki/Knapsack_problem#0/1_knapsack_problem}
}
