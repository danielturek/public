
## NEW
## using v0.5-1 of nimble, for consistancy
remove.packages('nimble')
install.packages('nimble', repos = 'http://r-nimble.org', type = 'source')

library(nimble)

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


fraction.code4 <- nimbleCode({
    ## Red tide effect factors and additional mortality
    tide_mort[1] <- 0
    tide_mort[2] ~ dunif(0, 1-baseMort[SW])
    tide_mort[3] ~ dunif(0, 1-baseMort[SW])
    ## Cold effect additional mortality
    cold_mort[1, 1] ~ dunif(0, 1-baseMort[1]) # Low normal
    cold_mort[2, 1] ~ dunif(0, 1-baseMort[1]) # Medium normal
    cold_mort[3, 1] <- 0                      # High normal
    cold_mort[1, 2] ~ dunif(0, 1-baseMort[1]) # Low cold
    cold_mort[2, 2] ~ dunif(0, 1-baseMort[1]) # Medium cold
    cold_mort[3, 2] <- 0                      # High cold
    cold_mort[1, 3] ~ dunif(0, 1-baseMort[1]) # Low severe
    cold_mort[2, 3] ~ dunif(0, 1-baseMort[1]) # Medium severe
    cold_mort[3, 3] ~ dunif(0, 1-baseMort[1]) # High severe
    ##
    ## Loop over regions
    for (area in 1:nRegions) {
        ##
        ## Proportions of mortality for region area
        for (cause in 1:nCauses) {
            pi0[cause, area] ~ dgamma(prior1[area,cause], 1.0)
            p0[cause, area] ~ dgamma(prior1[area,cause], 1.0)
        }
        ## Determined
        pi[1:nCauses,area] <- pi0[1:nCauses,area] / sum(pi0[1:nCauses,area])
        ## Undetermined
        p[1:nCauses, area] <- p0[1:nCauses,area] / sum(p0[1:nCauses,area])
        ##
        for (habitat in 1:nHabitats) {
            ## Loop over years
            for (year in 1:nYears) {
                theta[year,area,habitat,1:nCauses] <- (pi[1:nCauses, area] + (area==SW) * (tideYears[year] > 1) * tideVector[1:nCauses] * tide_mort[tideYears[year]] / baseMort[SW] +
                                                           (1 - (habitat == 3) * (coldYears[year, area] < 3)) * coldVector[1:nCauses] * cold_mort[habitat, coldYears[year, area]] / baseMort[area]) /
                                                               (1 + (area==SW) * (tideYears[year] > 1) * tide_mort[tideYears[year]] / baseMort[SW] + 
                                                                    (1 - (habitat == 3) * (coldYears[year, area] < 3)) * cold_mort[habitat, coldYears[year, area]] / baseMort[area])
                data1[year,area,habitat,1:nCauses] ~ dmulti(prob = theta[year,area,habitat,1:nCauses],
                                                            size = totals[year,area,habitat])
                U[year,area,habitat,1:nCauses] ~ dmulti(prob = p[1:nCauses, area],
                                                        size = undet[year,area,habitat])
            }
        }
    }
})


data.fraction.calf4 <- list(
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
    totals = apply(data.array[ , 'Calves', , , 1:nCauses], 1:3, sum),
    baseMort = as.vector(base.mort[,'Calves']),
    undet = data.array[ , 'Calves', , , 1 + nCauses]
)

fraction.model4 <- nimbleModel(fraction.code4, constants = data.fraction.calf4)

fraction.comp4 <- compileNimble(fraction.model4)  ## ERROR

