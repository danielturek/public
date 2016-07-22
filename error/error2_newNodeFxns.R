## build off current 'devel' branch
library(nimble)

## PART 1
## -------------------------------
## this shows the result (the logProb value) that we expect to see later.
## essentially:
## dgamma(6e-100, 0.001, 1, log = TRUE)
## this example shows that both R and C versions
## of NFs are capable of doing this calculation correctly

Rnf <- nimbleFunction(
    run = function(x = double(), a = double(), b = double()) {
        lp <- dgamma(x, a, b, log = 1)
        returnType(double())
        return(lp)
    }
)

Cnf <- compileNimble(Rnf)

x <- 6e-100
a <- 0.001
b <- 1.0

Rnf(x, a, b)
## [1] 221.3311

Cnf(x, a, b)
## [1] 221.3311

## great, they agree


## PART 2
## -------------------------------
## now we'll see the model where this calculation should take place, but doesn't work.

library(VGAM)     ## needed for rdiric()

dmultiSum <- nimbleFunction(
    run = function(x = double(), prob = double(1), x1 = double(1), x2 = double(1), log.p = double()) {
        len <- dim(x1)[1]
        data <- x1 + x2
        size <- sum(data)
        logL <- dmulti(data[1:len], prob = prob[1:len], size = size, log = 1)
        returnType(double())
        return(logL)
    }
)
rmultiSum <- nimbleFunction(
    run = function(n = integer(), prob = double(1), x1 = double(1), x2 = double(1)) {
        print('not implemented')
        returnType(double())
        return(1)
    }
)
registerDistributions(list(
    dmultiSum = list(
        BUGSdist = 'dmultiSum(prob, x1, x2)',
        types = c('prob = double(1)', 'x1 = double(1)', 'x2 = double(1)'),
        discrete = TRUE
    )
))

mdata <- read.csv('mortalities.simulated.April.pDet.const.csv')
regions <- c('ATL', 'USJ', 'NW', 'SW')
nRegions <- length(regions)
habitats <- c('Low', 'Medium', 'High')
nHabitats <- length(habitats)
causes <- c('watercraft', 'wcs', 'debris', 'cold', 'redtide', 'other')
nCauses <- length(causes)
full.causes <- c(causes, 'undetermined')
causes2 <- c('watercraft', 'wcs', 'debris', 'cold', 'other', 'redtide')
full.causes2 <- c(causes2, 'undetermined')
classes = c('Calves', 'Subadults', 'Adults')
nClasses <- length(classes)
severities <- c('Normal', 'Cold', 'Severe')
nSeverities <- length(severities)
base.mort <- array(c(0.123000, 0.093000, 0.100000, 0.103000, 
                     0.027417, 0.020739, 0.022355, 0.023073,
                     0.027417, 0.020739, 0.022355, 0.023073),
                   dim = c(nRegions, nClasses),
                   dimnames = list(regions, classes))
data <- transform(mdata,
                  area    = factor(area,    levels = regions),
                  age     = factor(age,     levels = classes),
                  habitat = factor(habitat, levels = habitats))
STARTYEAR <- 1996
ENDYEAR <- 2013
nYears <- ENDYEAR - STARTYEAR + 1
mod_tide_years <- c(2002, 2003, 2005, 2006, 2012)
int_tide_years <- c(1996, 2013)
modTideYears <- intTideYears <- rep(0, nYears)
tideYears <- rep(1, nYears)
names(modTideYears) <- names(intTideYears) <- names(tideYears) <- STARTYEAR:ENDYEAR
modTideYears[as.character(mod_tide_years)] <- 1
intTideYears[as.character(int_tide_years)] <- 1
tideYears[as.character(mod_tide_years)] <- 2
tideYears[as.character(int_tide_years)] <- 3
coldDesignations <- matrix(c(
    'Cold', rep('Normal', 4), 'Cold', 'Normal', 'Cold', rep('Normal', 6), 'Severe', rep('Normal', 3),
    'Cold', rep('Normal', 4), 'Cold', rep('Normal', 7), 'Cold', 'Severe', 'Cold', rep('Normal', 2),
    'Cold', 'Normal', 'Cold', 'Normal', 'Normal', 'Cold', 'Normal', 'Severe', rep('Normal', 6), 'Severe', 'Cold', rep('Normal', 2),
    'Cold', rep('Normal', 4), 'Cold', rep('Normal', 8), 'Severe', rep('Normal', 3)),
                           nYears, nRegions,
                           dimnames = list(STARTYEAR:ENDYEAR, regions))
coldYears <- matrix(as.integer(factor(coldDesignations, levels = severities)), 
                    nYears, nRegions,
                    dimnames = list(STARTYEAR:ENDYEAR, regions))
prior1 <- array(1, c(nClasses, nRegions, nCauses))
dimnames(prior1) <- list(classes, regions, causes2)
prior1[,'USJ','redtide'] <- 0.001
data.array <- array(NA, c(nYears, nClasses, nRegions, nHabitats, nCauses + 1))
dimnames(data.array) <- list(STARTYEAR:ENDYEAR, classes, regions, habitats, full.causes2)
for (class in classes) {
    for (region in regions) {
        for (qual in habitats) 
            data.array[ , class, region, qual, ] <- as.matrix(subset(data, age==class & area==region & habitat == qual)[,full.causes2])
    }
}

fraction.code5 <- nimbleCode({
    tide_mort[1] <- 0
    tide_mort[2] ~ dunif(0, 1-baseMort[SW])
    tide_mort[3] ~ dunif(0, 1-baseMort[SW])
    cold_mort[1, 1] ~ dunif(0, 1-baseMort[1]) # Low normal
    cold_mort[2, 1] ~ dunif(0, 1-baseMort[1]) # Medium normal
    cold_mort[3, 1] <- 0                      # High normal
    cold_mort[1, 2] ~ dunif(0, 1-baseMort[1]) # Low cold
    cold_mort[2, 2] ~ dunif(0, 1-baseMort[1]) # Medium cold
    cold_mort[3, 2] <- 0                      # High cold
    cold_mort[1, 3] ~ dunif(0, 1-baseMort[1]) # Low severe
    cold_mort[2, 3] ~ dunif(0, 1-baseMort[1]) # Medium severe
    cold_mort[3, 3] ~ dunif(0, 1-baseMort[1]) # High severe
    for (area in 1:nRegions) {
        for (cause in 1:nCauses) {
            pi0[cause, area] ~ dgamma(prior1[area,cause], 1.0)
            p0[cause, area] ~ dgamma(prior1[area,cause], 1.0)
        }
        pi[1:nCauses,area] <- pi0[1:nCauses,area] / sum(pi0[1:nCauses,area])
        p[1:nCauses, area] <- p0[1:nCauses,area] / sum(p0[1:nCauses,area])
        for (habitat in 1:nHabitats) {
            for (year in 1:nYears) {
                theta[year,area,habitat,1:nCauses] <- (pi[1:nCauses, area] + (area==SW) * (tideYears[year] > 1) * tideVector[1:nCauses] * tide_mort[tideYears[year]] / baseMort[SW] +
                                                           (1 - (habitat == 3) * (coldYears[year, area] < 3)) * coldVector[1:nCauses] * cold_mort[habitat, coldYears[year, area]] / baseMort[area]) /
                                                               (1 + (area==SW) * (tideYears[year] > 1) * tide_mort[tideYears[year]] / baseMort[SW] + 
                                                                    (1 - (habitat == 3) * (coldYears[year, area] < 3)) * cold_mort[habitat, coldYears[year, area]] / baseMort[area])
                eta[year,area,habitat,1:nCauses] <- (p[1:nCauses, area] + (1 - (habitat == 3) * (coldYears[year, area] < 3)) * coldVector[1:nCauses] * cold_mort[habitat, coldYears[year, area]] / baseMort[area]) /
                    (1 + (1 - (habitat == 3) * (coldYears[year, area] < 3)) * cold_mort[habitat, coldYears[year, area]] / baseMort[area])
                U[year,area,habitat,1:nCauses] ~ dmulti(prob = eta[year,area,habitat,1:nCauses],
                                                        size = undet[year,area,habitat])
                zeros[year,area,habitat] ~ dmultiSum(prob = theta[year,area,habitat,1:nCauses],
                                                     x1 = data1[year,area,habitat,1:nCauses],
                                                     x2 = U[year,area,habitat,1:nCauses])
            }
        }
    }
})

constants.fraction.calf5 <- list(
    data1 = data.array[ , 'Calves', , , 1:nCauses],
    nCauses = nCauses,
    nRegions = nRegions, 
    nYears = nYears,
    nSeverities = nSeverities,
    nHabitats = nHabitats,
    SW = 4,
    tideVector = c(0, 0, 0, 0, 0, 1),
    coldVector = c(0, 0, 0, 1, 0, 0),
    prior1 = prior1['Calves', , ], 
    tideYears = tideYears,
    coldYears = coldYears,
    baseMort = as.vector(base.mort[,'Calves']),
    undet = data.array[ , 'Calves', , , 1 + nCauses]
)
data5 <- list(
    zeros = array(0, c(nYears,nRegions,nHabitats))
)
inits.calf5 <- function() {
    p <- pi <- matrix(0, nCauses, nRegions)
    dimnames(p) <- dimnames(pi) <- list(causes2, regions)
    U <- array(NA, c(nYears, nRegions, nHabitats, nCauses))
    dimnames(U) <- list(STARTYEAR:ENDYEAR, regions, habitats, causes2)
    baseMort <- base.mort[,'Calves']
    for (region in regions) {
        if (region=='SW') {
            p[ , region] <- rdiric(1, rep(1, nCauses))
            for (habitat in habitats) {
                for (yr in STARTYEAR:ENDYEAR) {
                    year <- as.character(yr)
                    U[year, region, habitat, ] <- rmultinom(1, data.array[year,'Calves',
                                                                          region, habitat, nCauses+1], p[ , region])
                }
            }
            pi[,region] <- rdiric(1, apply(data.array[(1 - modTideYears) * 
                                                          (1 - intTideYears) * coldYears[,region]==1,
                                                      'Calves',region, , 1:nCauses], 3, sum)+1)
        }
        else if (region=='USJ') {
            p[1:(nCauses-1), region] <- rdiric(1, rep(1, nCauses - 1))
            p[nCauses, region] <- 1e-100
            p[nCauses - 1, region] <- p[nCauses - 1, region] - 1e-100
            for (habitat in habitats) {
                for (yr in STARTYEAR:ENDYEAR) {
                    year <- as.character(yr)
                    U[year, region, habitat, 1:(nCauses-1)] <- rmultinom(1, data.array[year,'Calves',
                                                                                       region, habitat, nCauses+1], p[1:(nCauses-1), region])
                    U[year, region, habitat, nCauses] <- 0
                }
            }
            pi[1:(nCauses-1),region] <- rdiric(1, apply(data.array[coldYears[,region]==1,'Calves',
                                                                   region, , 1:(nCauses-1)], 3, sum)+1)
            pi[nCauses, region] <- 1e-100
            pi[nCauses - 1, region] <- pi[nCauses - 1, region] - 1e-100
        } else {
            p[,region] <- rdiric(1, rep(1, nCauses))
            for (habitat in habitats) {
                for (yr in STARTYEAR:ENDYEAR) {
                    year <- as.character(yr)
                    U[year, region, habitat, ] <- rmultinom(1, data.array[year, 'Calves',
                                                                          region, habitat, nCauses+1], p[ , region])
                }
            }
            pi[,region] <- rdiric(1, apply(data.array[coldYears[,region]==1,'Calves',region, , 1:nCauses], 3, sum)+1) 
        }
    }
    cold_mort <- matrix(0, nHabitats, nSeverities, dimnames = list(habitats, severities))
    cold_mort[1:2, 'Normal'] <- runif(2, 0, 0.1)
    cold_mort[1:2, 'Cold'] <- runif(2, 0, 0.2)
    cold_mort[, 'Severe'] <- runif(3, 0, 1 - max(baseMort))
    tide_mort <- c(0, runif(2, 0, c(0.1, 0.3)))
    list(pi0 = pi * nCauses, p0 = p * nCauses, U = U, 
         tide_mort = tide_mort, cold_mort = cold_mort) 
}

set.seed(0)
inits5 <- inits.calf5()

fraction.model5 <- nimbleModel(fraction.code5, constants = constants.fraction.calf5, data = data5, inits = inits5)
fraction.comp5 <- compileNimble(fraction.model5)

## something strange
fraction.model5$calculate()
## [1] -1312.902
fraction.comp5$calculate()
## [1] -Inf

## here's the problem
node <- 'pi0[6, 2]'

## in case it helps, node = 'p0[6, 2]' is the same, also

fraction.model5$calculate(node)
fraction.comp5$calculate(node)     ## discrepancy

fraction.model5[[node]]
fraction.comp5[[node]]

## if you sort through the constants and model defintion, the
## declartion for this node is:
## pi0[6, 2] ~ dgamma(0.001, 1.0)

## again, same goes for node: p0[6, 2]

fraction.model5$getParam(node, 'shape')
fraction.model5$getParam(node, 'scale')

## that's as far as I got

## what's the deal??

