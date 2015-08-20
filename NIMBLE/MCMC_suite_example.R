
#### Example of NIMBLE's MCMC Suite

## The MCMCsuite() function in NIMBLE can be used to easuly compare MCMC algorithms of NIMBLE, WinBUGS, OpenBUGS, and JAGS.  In this example I'll only include NIMBLE and JAGS, since I'm running on OSX.

###### load NIMBLE library

##```{r, message = FALSE}
library(nimble)
##```

###### Define model, constants, data, and initial values

##```{r}
code <- nimbleCode({
    beta0 ~ dnorm(0, 0.001)
    beta1 ~ dnorm(0, 0.001)
    tau_RE ~ dgamma(1, 0.001)
    for(i in 1:N) {
        beta2[i] ~ dnorm(0, tau_RE)
        logit(p[i]) <- beta0 + beta1 * x[i] + beta2[i]
        r[i] ~ dbin(p[i], n[i])
    }
})

constants <- list(
    N = 10,
    x = c(0,  0,  0,  0,  0,  1, 1,  1,  1,  1),
    n = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79)
)

data <- list(
    r = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46)
)

inits <- list(beta0 = 0, beta1 = 0, tau_RE = 1, beta2 = rep(0, 10))
##```


###### Use NIMBLE's MCMC Suite

## ?MCMCsuite for more details of the MCMC suite.

## For now, we'll just run NIMBLE and JAGS MCMCs.

##```{r}
out <- MCMCsuite(
    code = code,
    constants = constants,
    data = data,
    inits = inits,
    MCMCs = c('nimble', 'jags'),   ## could also include 'WinBUGS' or 'OpenBUGS'
    niter = 10000,
    burnin = 2000,
    makePlot = TRUE,   ## creates density and trace plots for each parameter and MCMC algorithm
    savePlot = FALSE
)
##```

###### Examine summary statistics and timing

## These are the default summary statistics calculated.  They can be changed, or custom statistics can be added using the summaryStats argument to MCMCsuite().

out$summary

## This is the runtime of each algorithm, in seconds, and also the time required for NIMBLE compilation to C++.

out$timing

## All samples are also avaialble in out$samples, which is a 3-D array indexed by (1) MCMC algorithm, (2) model paramter, and (3) MCMC iteration.  So in this case, it's a 2 x 3 x 8000 array

dim(out$samples)

dimnames(out$samples)

apply(out$samples, c(1,2), mean)


###### Closing

## Yes, you can easily compare MCMC algorithms using NIMBLE.  And do a whole lot more, too!

## I encourage you to check-out our [User Manual](http://r-nimble.org/manuals/NimbleUserManual.pdf), or join our Google User Group (nimble-users) for more help or information.

## Cheers!

## Daniel

    
