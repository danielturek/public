##---
##publish: true
##---


##```{r, echo=FALSE, message=FALSE}
library(ggplot2)
library(dplyr)
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
##```


##```{r, echo = FALSE}
name <- 'dipper'
thisdf <- filter(df, model == name)
##```




    

##```{r, echo = FALSE, fig.with=10, fig.height=5}
tempdf <- filter(thisdf, mcmc %in% c('nimble','jags','autoBlock'))
p1 <- ggplot(tempdf, aes(mcmc, Efficiency, fill=mcmc)) + stat_summary(fun.y='mean', geom='bar') + ggtitle(paste0(name, '\nMean')) + theme(legend.position='none')
p2 <- ggplot(tempdf, aes(mcmc, Efficiency, fill=mcmc)) + stat_summary(fun.y='min', geom='bar') + ggtitle(paste0(name, '\nMin'))   + theme(legend.position='none')
p3 <- ggplot(tempdf, aes(mcmc, Efficiency, colour=mcmc)) + geom_point(size=3) + ggtitle(paste0(name, '\npoints')) + theme(legend.position='none')
p4 <- ggplot(tempdf, aes(mcmc, Efficiency, fill=mcmc)) + stat_summary(fun.y='mean', geom='bar')
multiplot(p1, p2, p3, p4, cols=4)
##```


##```{r, echo = FALSE}
results$out$timing
##```

