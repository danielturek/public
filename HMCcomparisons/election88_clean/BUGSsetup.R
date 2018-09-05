library(nimble)
## ARMdir <- "/Users/perry/Google Drive/AppliedRegressionAnalysis_Gelman_and_Hill"

## See code in directory data_setup_from_stan_github
load(file.path("data_setup_from_stan_github", "data_setup_from_stan_github.RData"))
## From lines 41-48 of (ARMdir)/Book_Codes/Ch.17/17.4_Multilevel logistic regression.R"
## Can be loaded directly via
## ARM_ch17_code <- readLines(file.path(ARMdir, "Book_Codes", "Ch.17", "17.4_Multilevel logistic regression.R"))
## initsFunction <- eval( parse(text = ARM_ch17_code[41:47]) )

election.inits <- function (){
 list(b.0=rnorm(1), b.female=rnorm(1), b.black=rnorm(1), b.female.black=rnorm(1),
  b.age=rnorm(n.age), b.edu=rnorm(n.edu), b.age.edu=array(rnorm(n.age*n.edu), 
  c(n.age,n.edu)), b.state=rnorm(n.state), b.v.prev=rnorm(1), 
  b.region=rnorm(n.region), sigma.age=runif(1), sigma.edu=runif(1), 
  sigma.age.edu=runif(1), sigma.state=runif(1), sigma.region=runif(1))
}


if(!exists('useNimble2')) useNimble2 <- FALSE
if(!useNimble2) {
    election88_BUGS <- readBUGSmodel("17.4_Bugs_codes.bug", dir = 'models', returnComponents = TRUE) 
} else {
    election88_BUGS_orig <- readBUGSmodel("17.4_Bugs_codes.bug", dir = 'models', returnComponents = TRUE)
    if(useNimble2 == 1) ## there is no 2 in the system
        election88_BUGS <- readBUGSmodel("election88_full_nimble.bug", dir = 'models', returnComponents = TRUE) 
    else if(useNimble2 == 3)
        election88_BUGS <- readBUGSmodel("election88_full_nimble3.bug", dir = 'models', returnComponents = TRUE)
    else if(useNimble2 == 4)
        election88_BUGS <- readBUGSmodel("election88_full_nimble4.bug", dir = 'models', returnComponents = TRUE)
    else if(useNimble2 == 5)
        election88_BUGS <- readBUGSmodel("election88_full_nimble5.bug", dir = 'models', returnComponents = TRUE)

    election88_BUGS$data <- election88_BUGS_orig$data
}

BUGSdataList.1 <- dataList.1
## To match the BUGS code as provided, make variable names use "." instead of "_" and lower case
names(BUGSdataList.1) <- tolower( gsub("_",".",names(BUGSdataList.1)))
## set the data in the election88_BUGS list
election88_BUGS$data <- BUGSdataList.1
## Oops, we used initial values as a list, not a function, so we'll call the function now to create one set of values
attach(BUGSdataList.1)
initsList.1 <- election.inits()
detach(BUGSdataList.1)
election88_BUGS$inits <- initsList.1

code <- election88_BUGS$code
yInd <- which(names(election88_BUGS$data) == 'y')
constants <- election88_BUGS$data[-yInd]
data <- election88_BUGS$data[yInd]
inits <- election88_BUGS$inits

Rmodel <- nimbleModel(code, constants, data, inits)
Rmodel$calculate()

conf <- configureMCMC(Rmodel)
conf$printSamplers()
Rmcmc <- buildMCMC(conf)

Cmodel <- compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)#, showCompilerOutput = TRUE)
##compiledList <- compileNimble(list(model=Rmodel, mcmc=Rmcmc))
##Cmodel <- compiledList$model; Cmcmc <- compiledList$mcmc

set.seed(0)
samples <- runMCMC(Cmcmc, 10000)

samplesSummary(samples)
samplesPlot(samples)

library(coda)
apply(samples, 2, effectiveSize)







