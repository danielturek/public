##---
##publish: true
##---


##```{r, echo=FALSE, message=FALSE}
library(ggplot2)
library(dplyr)
options(digits = 2)
load('~/GitHub/userDistMCMC/results.RData')
df <- results$df
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    ## Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    ## If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        ## Make the panel
        ## ncol: Number of columns of plots
        ## nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    if (numPlots==1) {
        print(plots[[1]])
    } else {
        ## Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        ## Make each plot, in the correct location
        for (i in 1:numPlots) {
            ## Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                  layout.pos.col = matchidx$col))
        }
    }
}
makePlots <- function(df, mcmcs, metric) {
    tempdf <- if(missing(mcmcs)) df else filter(df, mcmc %in% mcmcs)
    ymax <- max(tempdf[, metric])
    codeBlock <- substitute({
        p1 <- ggplot(tempdf, aes(mcmc, METRIC, fill=mcmc)) + stat_summary(fun.y='mean', geom='bar') + ggtitle(paste0(name, '\nMean')) + theme(legend.position='none', axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank())
        p2 <- ggplot(tempdf, aes(mcmc, METRIC, fill=mcmc)) + stat_summary(fun.y='min', geom='bar') + ggtitle(paste0(name, '\nMin'))   + theme(legend.position='none', axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank())
        p3 <- ggplot(tempdf, aes(mcmc, METRIC, colour=mcmc)) + geom_point(size=3) + ggtitle(paste0(name, '\npoints')) + theme(legend.position='none', axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank()) + ylim(c(0,ymax))
        p4 <- ggplot(tempdf, aes(mcmc, METRIC, fill=mcmc)) + stat_summary(fun.y='mean', geom='bar') + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
    },
                            list(METRIC = as.name(metric)))
    eval(codeBlock)
    multiplot(p1, p2, p3, p4, cols=4)
}
##```


#### Introduction

## This provides a summary of the capture-recapture (CR) MCMC analyses done to-date, making use of user-defined distribution functions.

## The analyses include three (each fundamentally different) CR datasets & models, presented in order of increasing complexity.  The first (Dipper) is **not** a multistate CR model, and thus is vastly simpler than the others.  The second (Orchid) is a multistate CR model.  The third and final (Goose) is a multistate model, where each CR history also includes a "multiplicity", which represents the number of times that particular CR history is represented in the complete dataset being represented.  Thus, the total number of individuals represented by the Goose dataset is equal to the sum of these "multiplicities".




#### Explanation of plots

## In the plots that follow:

##- The y-axes represent one of two values, as indicated by the y-axis label.  This is either: **ESS**, the effective sample size resulting from 100,000 MCMC samples, or, **Efficiency**, the effective sample size per unit of algorithm run-time.

##- The left pane displays the **mean** value of y, averaged over all top-level model parameters.

##- The middle pane displays the **minimum** value of y, among all top-level model parameters.

##- The right pane displays points giving the **actual values** of y, for each top-level model parameter.



#### Dipper model

##```{r, echo = FALSE}
name <- 'dipper'
thisdf <- filter(df, model == name)
mcmcs <- c('nimble','nimble_slice','jags','autoBlock')
##```

###### Model Explanation

## This is the simplest model, which is **not** a multistate model.  Therefore, the model can be specified **without** using stochastic indexing, and we can sample directly using NIMBLE's MCMC (without any custom distributions).  There are only two top-level model parameters (survival and probability of detection), and roughly 300 (discrete, binary valued) latent states.

###### Effective Sample Size (ESS)

##```{r, echo = FALSE, fig.with=10, fig.height=3.5}
makePlots(thisdf, mcmcs=mcmcs, metric='ESS')
##```

## For this model, auto-blocking results in scalar sampling of the top-level parameters (since it only operates over continuous-valued parameters, and all latent states are discrete-valued).  Thus, the auto-blocking results are more-or-less identical to the default NIMBLE MCMC, the minor differences are a stochastic result.

## The general result is that JAGS produces much higher ESS, nearly 3X higher than NIMBLE's default MCMC.  I looked into the samplers being assigned by JAGS.  These are slice samplers on the two top-level parameters, and direct sampling by CDF inversion for the 300 (binary-valued) discrete latent states.  This motivated me to try the nimble_slice MCMC, which assigns slice samplers to the top-level parameters (background: NIMBLE's default behaviour is to assign RWM to these continuous parameters; and NIMBLE **always**, out of necessity, assigns slice samplers to the discrete latent states, since currently that's our only means of sampling these).

## Interestingly, we see that using slice sampling, NIMBLE's ESS is nearly identical to that of JAGS (see rightmost 'points' plot).  That's good.

###### Timing

##```{r, echo = FALSE}
results$out[[name]]$timing[mcmcs]
##```

## In terms of timing, JAGS is slower than NIMBLE's default MCMC, but the nimble_slice sampling algorithm is slower yet.

###### Efficiency (ESS/time)

##```{r, echo = FALSE, fig.with=10, fig.height=3.5}
makePlots(thisdf, mcmcs=mcmcs, metric='Efficiency')
##```

## End result is that nimble_slice is an improvement, but JAGS wins overall.  JAGS is roughly 2X more efficient than NIMBLE's default algorithm.


###### Dipper model with custom distributions

## We now approach the Dipper model using a trick in JAGS, and a custom distribution in NIMBLE.  Even though the algorithm names below are different, the NIMBLE and JAGS algorithms are designed to be identical -- that is, to perform the same calculations.  The naming differences (CJS, Poisson) result from implementation details.

## jagsPoisson uses a trick using the Poisson distribution to perform a direct likelihood calculation.

## The 'CJS' flavours of NIMBLE use a custom-defined distribution (dCJS) to do an identical calculation.  There are now exactly two unknwown model parameters, which both follow this custom distribution.  Again, this custom distribution is written to perform the same calculation taking place in JAGS.  Using this custom-distribution, nimbleCJS is NIMBLE's default MCMC (RWM sampling), and sliceCJS uses slice sampling.

## Inteestingly, in this case, the autoBlocking algorithm now converges on block sampling these two parameters; consistently so.

##```{r, echo = FALSE}
mcmcs <- c('nimbleCJS', 'sliceCJS', 'autoBlockCJS', 'jagsPoisson')
##```

###### Effective Sample Size (ESS)

##```{r, echo = FALSE, fig.with=10, fig.height=3.5}
makePlots(thisdf, mcmcs=mcmcs, metric='ESS')
##```

## The blocked sampling produces the lowest ESS, but we'll see it has a quick runtime which compensates for this.

## NIMBLE's slice sampling produces a very high ESS, but this will be offset by a long runtime -- this will be a recurring phenomenon.

###### Timing

##```{r, echo = FALSE}
results$out[[name]]$timing[mcmcs]
##```

###### Efficiency (ESS/time)

##```{r, echo = FALSE, fig.with=10, fig.height=3.5}
makePlots(thisdf, mcmcs=mcmcs, metric='Efficiency')
##```

## We have fundamentally different results this time.  NIMBLE (autoBlocking) is now right on par with JAGS, and NIMBLE's slice sampling is no longer advantageous.

## I don't have exact explanations for these results.  Probably could infer something, if pressed to do so.  But the take-home message would be that all algorithms, using the tricks and custom distributions available, are more-or-less on par with one another.



#### Orchid model

###### Model Explanation

## The orchid model is a multistate model, representing the observations of flowering orchids.  Orchids are observed as flowering, non-flowering, or un-observed.  There are a total of 19 top-level parameters which govern state transitions and observations probabilities, 236 flowers being tracked, and 11 observation periods.

## The orhid model permits use of a very generally written NIMBLE custom distribution, dDHMM (Discrete Hidden Markov Model), which allows for an arbitrary number of latent (hidden) and observed states, either constant or time-dependent state transition and observation matricies, and a multiplicity for each CR history (see Goose model)

## More so, the NIMBLE implementation of the Orchid model (and generally, any multistate CR model) mandates the use of this custom distribution function.  Since we cannot handle stochastic indexing, the only (realistic, practical) way to express this model in the NIMBLE DSL is using the dDHMM distribution function.  In contrast, in JAGS this model can be expressed using discrete latent states (which then are used to stochastically index the transition and observation matricies).

##```{r, echo = FALSE}
name <- 'orchid'
thisdf <- filter(df, model == name)
mcmcs <- as.character(unique(thisdf$mcmc))
##```

###### Effective Sample Size (ESS)

##```{r, echo = FALSE, fig.with=10, fig.height=3.5}
makePlots(thisdf, mcmcs=mcmcs, metric='ESS')
##```

###### Timing

##```{r, echo = FALSE}
results$out[[name]]$timing[mcmcs]
##```

###### Efficiency (ESS/time)

##```{r, echo = FALSE, fig.with=10, fig.height=3.5}
makePlots(thisdf, mcmcs=mcmcs, metric='Efficiency')
##```

## We notice a recurring theme, that NIMBLE's slice sampling produces an excellent ESS, but takes a very long time to do so; and the resulting efficiency isn't very good.  We could probably find the root cause for this, in the fact that **each iteration** of slice sampling involves **possibly many** evaluations of the custom distribution function, which, being written in such a very general manner, certainly makes some sacrifices in (algorithmic runtime) efficiency.

## JAGS and NIMBLE default algorithm (RWM sampling of top-level parameters) are very comparable.  JAGS is slightly better, but overall quite similar.  Once again, JAGS is using slice sampling of top-level parameters, while the nimbleDHMM algorithm uses RWM.

## The pleasing surprise result comes from autoBlocking, which converges on two distinct blocks of two parameters each (and univariate sampling of the remaining 15 parameters).  This has the effect of increasing the efficiency of the slowest-mixing parameters (since that's the principle underlying the autoBlocking algorithm), which produces a nice result for the minimum efficiency across all 19 parameters (middle plot).



#### Goose model

###### Model Explanation

## The Goose model is another multistate model, with the addition of a multiplicity for each unique CR history.  The multiplicities represent the number of times that each CR history was observed (distinct, individual Canadian geese).  The dataset includes 153 **unique** CR histories, but these represent a total of 11,200 unique Canadian geese.  That is, the sum of the multiplicities is 11,200.

## In this model, there are four possible observed states (juvenile, non-breeding, breeding, or unobserved), four observation periods, and 21 top-level parameters which govern state transitions and observations.

## Once again, since NIMBLE doesn't support stochastic indexing, our only practical approach is using the custom dDHMM distribution.  This distribution function is generalized to allow for a multiplicity, so it natually handles models like this (an example of inefficiency introduced by the very general implementation).

## In JAGS, being restricted to the BUGS syntax, there's only one approach for handling a CR model with multiplicities.  The dataset was expanded (that's what the "Exp" in jagsExp means -- "expanded" -- not exponential!) to have dimension 11,200 x 4.  That is, the need for multiplicities is removed, since we completely enumerate all CR histories.  I think you get what I mean.  So, JAGS has a comparatively massive model structure to deal with.

##```{r, echo = FALSE}
name <- 'goose'
thisdf <- filter(df, model == name)
mcmcs <- as.character(unique(thisdf$mcmc))
##```

###### Efficiency (ESS/time)

##```{r, echo = FALSE, fig.with=10, fig.height=3.5}
makePlots(thisdf, mcmcs=mcmcs, metric='ESS')
##```

## Both JAGS and NIMBLE have comparable ESS.  Even with a huge model structure, JAGS does a fine job sampling...

###### Timing

##```{r, echo = FALSE}
results$out[[name]]$timing[mcmcs]
##```

## .. it just takes forever to do so; that 1702 is **minutes**, which comes out to about 28 hours(!)

###### Efficiency (ESS/time)

##```{r, echo = FALSE, fig.with=10, fig.height=3.5}
makePlots(thisdf, mcmcs=mcmcs, metric='Efficiency')
##```

## So, on the Efficiency plots, JAGS doesn't even show up, really.

## NIMBLE's default algorithm does a good job.  Runs in 22 minutes, and looks great compared to JAGS.

## Yet another pleasing surprise from autoBlocking.  It converges on the groups that I'll copy below.

##Auto-Blocking converged on the node groupings:

##- alpha[1, 1, 1], alpha[2, 1, 1]

##- alpha[1, 1, 2], alpha[2, 1, 2]

##- alpha[1, 2, 1], alpha[2, 2, 1]

##- alpha[1, 2, 2], alpha[1, 3, 2], alpha[2, 2, 2], alpha[2, 3, 2], p[6]

##- alpha[2, 3, 1], p[3], phi[3]

##- p[1], p[4], phi[1]

##- p[2], p[5], phi[2]

## Perhaps interesting to note, of the 21 model parameters, autoBlocking has blocked 20 of them.  There's only one parameter left out, alpha[1, 3, 1], which is assigned a univariate RWM sampler.

## As a result of this blocking, we realize an increase in ESS (both mean and minimum), a **reduction** in runtime down to 8 minutes, approximately a 4X increase in mean Efficiency, and close to a 15X increase in minimum Efficiency.  And that's compared to NIMBLE with a custom distribution.  If you compare this to JAGS, it's order of 100X or more improvement.



