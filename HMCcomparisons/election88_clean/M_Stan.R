library(methods)
library(nimble)
source("BUGSsetup.R")

if(file.exists("setNimReplicate.R"))
    source("setNimReplicate.R")

case <- "M_Stan"
if(exists("iNimReplicate"))
    case <- paste0(case, "_oneRep_R", iNimReplicate)

stanDataList.1 <- dataList.1

stanInitsList.1 <- initsList.1
BUGSparamNames <- names(initsList.1)
stanParamNames <- gsub("\\.","_", BUGSparamNames)
## Stan code uses one arbitrarily different parameter name:
stanParamNames[ which(BUGSparamNames=="b.state") ] <- "b_hat" ## b_state would have seemed more logical
names(stanInitsList.1) <- stanParamNames

stanParameterRules <- lapply(stanParamNames, function(x) list(StanSourceName = x))
names(stanParameterRules) <- BUGSparamNames

stanInfo <- list(election88_full = list(
                     stanParameterRules = stanParameterRules,
                     codeFile = '17.4_multilevel_logistic.stan',
                     modelName = '', ## intentionally blank
                     data = stanDataList.1,
                     inits = stanInitsList.1))


result <-compareMCMCs(list(election88_full = election88_BUGS), MCMCs=c('stan'),
                      niter=10000, burnin = 1000, thin=1, stanInfo = stanInfo,
                      stanDir = getwd(),summary=FALSE)

saveFile <- paste0("election88_", case, "_result.Rdata")
saveRDS(result,
        file = saveFile)

result <- updateMCMCcomparisonWithHighOrderESS(result, logVars = c('sigma.age','sigma.edu','sigma.age.edu','sigma.state','sigma.region'), includeBurninTime = FALSE)


saveFile <- paste0("election88_", case, "_result_updatedESS.Rdata")
saveRDS(result,
        file = saveFile)

q('no')
