## Model: original
## Samplers: default

case <- "M_5_center_S_AFSS_shift"
useNimble2 <- 5
source("BUGSsetup.R")
library(nimble)
source("shiftSampler.R")

## election88_BUGS$data$v.prev <- as.numeric(
##     scale(election88_BUGS$data$v.prev, center = TRUE, scale = FALSE)
## )

modelInfo <- structure(list(election88_BUGS),
                       names = case)

## Included b.v.prev here:  ABOUT FIVE TIMES IMPROVEMENT.
## Try the sigma joint samplers.
## Make sure we do sigma ESS on log scales.
## Try centering the variable.
updateConf_AFSS <- function(conf) {
    coefNodes <- c("b.0","b.female", "b.black", "b.female.black", "b.v.prev")
    conf$removeSamplers(coefNodes)
    conf$addSampler(target = coefNodes, type = "RW_block")
    conf$addSampler(target = coefNodes, type = "RW_block")
    conf$removeSamplers('sigma.age')
    conf$removeSamplers('sigma.edu')
    conf$removeSamplers('sigma.region')
    conf$removeSamplers('sigma.state')
    conf$removeSamplers('sigma.age.edu')
##    instead sample them on a log scale
    conf$addSampler(target = 'sigma.age', type = 'RW', control = list(log = TRUE))
    conf$addSampler(target = 'sigma.edu', type = 'RW', control = list(log = TRUE))
    conf$addSampler(target = 'sigma.region', type = 'RW', control = list(log = TRUE))
    conf$addSampler(target = 'sigma.state', type = 'RW', control = list(log = TRUE))
    conf$addSampler(target = 'sigma.age.edu', type = 'RW', control = list(log = TRUE))

    ## conf$addSampler(target = 'sigma.age.edu', type = 'RW_log_shift', control = list(shiftNodes = 'b.age.edu'))
    ## conf$addSampler(target = 'sigma.edu', type = 'RW_log_shift', control = list(shiftNodes = 'b.edu')) 
    ## conf$addSampler(target = 'sigma.state', type = 'RW_log_shift', control = list(shiftNodes = 'b.state')) 
    ## conf$addSampler(target = 'sigma.age', type = 'RW_log_shift', control = list(shiftNodes = 'b.age')) 
    ## conf$addSampler(target = 'sigma.region', type = 'RW_log_shift', control = list(shiftNodes = 'b.region'))
    
    conf
}

result <-
    compareMCMCs(modelInfo = modelInfo,
                 MCMCs=c('nimble_AFSS_shift'),
                 MCMCdefs = list(
                     nimble_AFSS_shift= quote({
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

result[[1]]$summary
## save(electionResults1nimble,
##      file = 'election88_full_results1nimble.Rdata')

result <- updateMCMCcomparisonWithHighOrderESS(result, logVars = c('sigma.age','sigma.edu','sigma.age.edu','sigma.state','sigma.region'), includeBurninTime = FALSE)

## save(electionResults1nimble, file = 'election88_full_results1nimble_updated.Rdata')

q('no')
