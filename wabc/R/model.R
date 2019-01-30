## Robin Eriksson 2019

##' Model0
##' The first model, a toy example.
##' y_{i+1} = y_{i} + N(theta, 1)
##' @export
##' @param N the length of the output data
##' @param M the number of realisations
##' @param theta the model parameter
model0_R <- function(N = 50, M = 1, theta) {
    eps <- matrix(rnorm(N*M, mean = theta[k], sd = 1), nrow = N, ncol = M)
    y <- apply(eps, 2, cumsum)
    return(y)
}
