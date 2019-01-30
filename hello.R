## Robin Eriksson 2019
library(wabc)
library(emdist)

##' abc rejection
##' @param Nprop total number of samples
##' @param Pacc percentage of accepted params
abc <- function(Nprop = 1000, Pacc = 1, SS = mean, thetaTrue = 0.5,  dataLen = 50, obs) {
    set.seed(0)
    ## construct observation
    dataLen <- 50
    obs.SS <- SS(obs)

    ## propose samples
    yprop <- matrix(0, dataLen, Nprop)
    proposals <- runif(Nprop, 0, 1)
    for(n in seq_len(Nprop)) {
        yprop[,n] <- model0(dataLen, 1, proposals[n])
    }

    yprop.SS <- apply(yprop, 2, SS)

    ## compute distance
    dist <- abs(yprop.SS - obs.SS)

    ## order distances
    dist.order <- order(dist, decreasing = FALSE)
    nAcc <- Nprop/100*Pacc

    ## pick the top nAcc proposals
    posterior <- proposals[dist.order[1:nAcc]]

    return(posterior)
}

##' abc rejection
##' @param Nprop total number of samples
##' @param Pacc percentage of accepted params
wabc <- function(Nprop = 1000, Pacc = 1, thetaTrue = 0.5, dataLen = 50, obs) {
    set.seed(0)

    ## construct observation
    obs.el <- empiricalLikelihood(obs)

    ## propose samples
    proposals <- runif(Nprop, 0, 1)
    dist <- numeric(Nprop)
    for(n in seq_len(Nprop)) {
        yprop <- model0(dataLen, 1, proposals[n])
        yprop.el <- empiricalLikelihood(yprop)
        ## compute distance (earth moving, Wassestein?)
        dist[n] <- emdist::emd2d(yprop.el, obs.el)
    }


    ## order distances
    dist.order <- order(dist, decreasing = FALSE)

    browser()
    nAcc <- Nprop/100*Pacc

    ## pick the top nAcc proposals
    posterior <- proposals[dist.order[1:nAcc]]

    return(posterior)
}

main <- function(Nprop = 1e3, Pacc = 1, thetaTrue = 0.5) {
    dataLen <- 50
    obs <- wabc::model0(dataLen, 1, thetaTrue)

    a <- abc(Nprop, Pacc, mean, thetaTrue, dataLen, obs)
    b <- wabc(Nprop, Pacc, thetaTrue, dataLen, obs)


    boxplot(a,b, names = c("abc", "wabc"))
    abline(h=thetaTrue, col = "red")
}
