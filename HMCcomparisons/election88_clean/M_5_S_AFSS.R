## Model: original
## Samplers: default

case <- "M_5_S_AFSS"
useNimble2 <- 5
source("BUGSsetup.R")
modelInfo <- structure(list(election88_BUGS),
                       names = case)

## Included b.v.prev here:  ABOUT FIVE TIMES IMPROVEMENT.
## Try the sigma joint samplers.
## Make sure we do sigma ESS on log scales.
## Try centering the variable.
updateConf_AFSS <- function(conf) {
    coefNodes <- c("b.0","b.female", "b.black", "b.female.black", "b.v.prev")
    conf$removeSamplers(coefNodes)
    conf$addSampler(target = coefNodes, type = "AF_slice")
    conf
}

library(nimble)
result <-
    compareMCMCs(modelInfo = modelInfo,
                 MCMCs=c('nimble_AFSS'),
                 MCMCdefs = list(
                     nimble_AFSS = quote({
                         conf <- configureMCMC(Rmodel)
                         conf <- updateConf_AFSS(conf)
                         conf
                     })),
                 niter=10000,
                 burnin = 1000,
                 thin=1,
                 summary=FALSE)

result.dir <- paste0(case,".cMCMC")
make_MCMC_comparison_pages(result,
                           dir = result.dir)
browseURL(file.path(result.dir,
                    paste0(names(result)[1], ".html")))

## save(electionResults1nimble,
##      file = 'election88_full_results1nimble.Rdata')

##electionResults1nimble <- updateMCMCcomparisonWithHighOrderESS(electionResults1nimble, logVars = c('sigma.age','sigma.edu','sigma.age.edu','sigma.state','sigma.region'), includeBurninTime = FALSE)

## save(electionResults1nimble, file = 'election88_full_results1nimble_updated.Rdata')

q('no')
