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
    tempdf <- filter(df, mcmc %in% mcmcs)
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


##### Basic explanation of plots

## In the plots that follow:

##- The left pane displays the *mean* value of X, averaged over all top-level model parameters.

##- The middle pane displays the *minimum* value of X among all top-level model paramters.

##- The right pane displays points giving the actual *value* of X for each top-level model paramter.

##- The value of X (indicated by the y-axis label) is either: *ESS*, the effective sample size (per 10,000 MCMC samples), or, *Efficiency*, the effective sample size per second of run-time


##### Dipper model

##```{r, echo = FALSE}
name <- 'dipper'
thisdf <- filter(df, model == name)
mcmcs <- c('nimble','jags','autoBlock')
##```

## This is the simplest model, which is *not* a multistate model.  Therefore the model structure does not require stochastic indexing, and we can sample directly using NIMBLE's MCMC (without any custom distributions).  There are only two top-level model paramters (p, and phi), and roughly 300 latent states.

## For this model, auto-blocking results in scalar sampling of the top-level parameters (since it only operates over continuous-valued parameters, and all latent states are discrete-valued).  Thus, the auto-blocking results are more-or-less identical to the default NIMBLE MCMC.

## The general result is that JAGS produces much higher ESS:

##```{r, echo = FALSE, fig.with=10, fig.height=3.5}
makePlots(thisdf, mcmcs=mcmcs, metric='ESS')
##```

## but takes longer than NIMBLE to do so:

##```{r, echo = FALSE}
message('algorithm run times:')
results$out[[name]]$timing[mcmcs]
##```

## still resulting in roughly 2X higher Efficiency in JAGS:

##```{r, echo = FALSE, fig.with=10, fig.height=3.5}
makePlots(thisdf, mcmcs=mcmcs, metric='Efficiency')
##```




