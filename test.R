library(nimble)

a <- 1 + 1
print(a)
a

warning('this is the warning message')

b <- 1 + 2
print(b)

c <- 1 + 3
print(c)

d <- 1 + 4
print(d)

code <- nimbleCode({
    a ~ dnorm(0, 1)
    b ~ dnorm(a, 1)
})

constants <- list()
data <- list(b = 1)
inits <- list(a = 0)

Rmodel <- nimbleModel(code, constants, data, inits)

spec <- configureMCMC(Rmodel)
spec$getSamplers()
Rmcmc <- buildMCMC(spec)

Cmodel <- compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

set.seed(0)
Cmcmc$run(10000)
samples <- as.matrix(Cmcmc$mvSamples)
apply(samples, 2, mean)

