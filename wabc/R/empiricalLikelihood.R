## Robin Eriksson 2019

##' Compute the empirical likelihood for data: data
##' with lag: lag
##' @export
##' @param data the data to construct from
##' @param lag the lag in data to use
empiricalLikelihood <- function(data, lag = 1) {
    t <- (1+lag):length(data)
    el <- matrix(c(data[t], data[t-1]), ncol = 2)
    return(el)
}
