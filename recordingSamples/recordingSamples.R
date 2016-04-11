##---
##author: Daniel Turek
##title: 'Recording samples from NIMBLE MCMC'
##publish: true
##---

##This demonstrates how to modify NIMBLE samplers, so that they record the node values before and after updating (on each iteration).  This quick implementation will only work for sampling univariate nodes, not multivariate.

##### Load NIMBLE

##First load NIMBLE, and set a NIMBLE option to allow access to the samples from compiled functions.

##```{r, message = FALSE}
library(nimble)
nimbleOptions(buildInterfacesForCompiledNestedNimbleFunctions = TRUE)
##```

##### Modify sampler functions

##Now you'll have to modify the source code of any sampler functions that you want to record samples from.  The samplers are all implemented in MCMC_samplers.R.  The modification will be the same for any sampler, here I've only done the 'end' sampler.  The steps will be:

##- Change the name of the sampler.  Here, I've changed "sampler_end" to "sampler_end_record".

##- Add three lines to the setup function

##- Add four lines to the very beginning of the run function, and one line at the very end.

##The comments in the sampler definition below also show the changes.

##```{r}
#### new sampler name:
sampler_end_record <- nimbleFunction(
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        calcNodes  <- model$getDependencies(target)
        ## these lines are new:
        numSamples <- 0
        before <- c(0, 0)
        after <- c(0, 0)
    },
    run = function() {
        ## these lines are new:
        numSamples <<- numSamples + 1
        setSize(before, numSamples)
        setSize(after, numSamples)
        before[numSamples] <<- model[[target]]
        ## back to the original sampler function code:
        simulate(model, target)
        calculate(model, calcNodes)
        nimCopy(from = model, to = mvSaved, row = 1, nodes = calcNodes, logProb = TRUE)
        ## this line new:
        after[numSamples] <<- model[[target]]
    },
    methods = list(
        reset = function() { }
    ), where = getLoadingNamespace()
)
##```

##### Create model and MCMC

##```{r}
#### create a NIMBLE model object
code <- nimbleCode({
    a ~ dnorm(0, 1)
    b ~ dnorm(a, 1)
})

Rmodel <- nimbleModel(code, inits = list(a=0, b=0))

#### create an MCMC configuration object for the model
spec <- configureMCMC(Rmodel)
spec$printSamplers()

#### remove samplers that you don't want
spec$removeSamplers('b', print = TRUE)

#### add your new samplers
#### we'll sample node 'b' twice on each MCMC iteration
spec$addSampler('b', 'end_record', print = FALSE)
spec$printSamplers()
spec$addSampler('b', 'end_record', print = FALSE)
spec$printSamplers()

spec$addMonitors(c('a', 'b'))

#### build MCMC
Rmcmc <- buildMCMC(spec)

#### compile model and MCMC
Cmodel <- compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)
##```

##### Run MCMC and extract samples

## Now you can run both the un-compiled (R) and compiled (C) MCMC algorithms, and also extract the 'before' and 'after' samples from each sampler.

## Below shows how to extract the samples from each MCMC iteration (the mvSamples object), and also the samples 'before' and 'after' from both sampler functions acting on node 'b'.

##```{r}
set.seed(0)
Rmcmc$run(5)

#### this is the 'before' and 'after' for the *first* sampler on 'b'
cbind(Rmcmc$samplerFunctions$contentsList[[2]]$before,
      Rmcmc$samplerFunctions$contentsList[[2]]$after)

#### this is the 'before' and 'after' for the *second* sampler on 'b'
cbind(Rmcmc$samplerFunctions$contentsList[[3]]$before,
      Rmcmc$samplerFunctions$contentsList[[3]]$after)

#### these are the samples recorded after each full MCMC iteration
as.matrix(Rmcmc$mvSamples)[, 'b', drop=FALSE]
##```

##The same things all work (the same) for the compiled MCMC
 
##```{r}
set.seed(0)
Cmcmc$run(5)

#### this is the 'before' and 'after' for the *first* sampler on 'b'
cbind(Cmcmc$samplerFunctions$contentsList[[2]]$before,
      Cmcmc$samplerFunctions$contentsList[[2]]$after)

#### this is the 'before' and 'after' for the *second* sampler on 'b'
cbind(Cmcmc$samplerFunctions$contentsList[[3]]$before,
      Cmcmc$samplerFunctions$contentsList[[3]]$after)

#### these are the samples recorded after each full MCMC iteration
as.matrix(Cmcmc$mvSamples)[, 'b', drop=FALSE]
##```

##Let me know if you have any questions.

##-Daniel
