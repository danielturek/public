

GetDetectorIndex <- function(habitat.mx = habitat.mx,
                             detectors.xy = detectors.xy,
                             maxDist = maxDist,
                             ResizeFactor = 0) {
    ##
    habitatID <- habCoordsx <- habCoordsy <- habitat.mx 
    ##
    if(ResizeFactor == 0) {
        from <- 1
        ResizeFactor <- 1 
    } else {
        from <- (ResizeFactor/2) + 0.5
    }
    ##
    dimCoords <- c(ceiling(dim(habCoordsx)[1]/ResizeFactor),
                   ceiling(dim(habCoordsx)[2]/ResizeFactor))
    CoordsMax <- dimCoords*ResizeFactor
    habCoordsx <- matrix(rep(seq(from, CoordsMax[2], by=ResizeFactor ), dimCoords[1]),
                         nrow = dimCoords[1], ncol= dimCoords[2], byrow = T )
    habCoordsy <- matrix(rep(seq(from, CoordsMax[1], by=ResizeFactor ), dimCoords[2]),
                         nrow = dimCoords[1], ncol=dimCoords[2], byrow = F )
    habCoordsy <- as.vector(habCoordsy)
    habCoordsx <- as.vector(habCoordsx)
    habCoordsxy <- cbind(habCoordsx, habCoordsy)
    ##
    ## rescale habitat matrix
    r <- raster(habitat.mx)
    r <- aggregate(r, fact = ResizeFactor)
    r[r>0] <- 1
    habitat.mx1 <- as.matrix(r)
    ##
    ## create habitat id matrix
    habitatID <- habitat.mx1
    habitatID[] <- as.character(habitat.mx1)
    habitatID[habitat.mx1 == "1"] <- 1:sum(habitat.mx1=="1")
    m <- sapply(habitatID, FUN=as.numeric)
    habitatID <- matrix(m, nrow=dim(habitatID)[1], ncol=dim(habitatID)[2], byrow = F)
    habCoordsxy  <- habCoordsxy[as.character(as.vector(habitat.mx1))=="1",]
    ##
    ## find detectors within radius of center of each cell
    detector.index <- apply(habCoordsxy, 1, function(x){
        D <- sqrt((x[1] - detectors.xy[,1])^2 + (x[2] - detectors.xy[,2])^2) 
        which(D < maxDist)
    })
    ##
    nDetectors <- unlist(lapply(detector.index, function(x) length(x)))
    maxNBDets <- max(nDetectors)
    ##
    detectorIndex <- matrix(0, nrow=length(detector.index), ncol = maxNBDets)
    for(j in 1:length(detector.index)) {
        if(length(detector.index[[j]])!=0){
            detectorIndex[j, 1:nDetectors[j]] <- detector.index[[j]]
        }
    }
    ##
    ## plot
    SXY <- habCoordsxy[sample(1:dim(habCoordsxy)[1],size=1),]
    sxyID <- habitatID[trunc(SXY[2]/ResizeFactor)+1, trunc(SXY[1]/ResizeFactor)+1]
    index <- detectorIndex[sxyID,1:nDetectors[sxyID]]
    plot(habCoordsxy[,2]~habCoordsxy[,1], pch=16, cex=0.1,
         xlab = '', ylab = '')
    points(habCoordsxy[sxyID,2]~habCoordsxy[sxyID,1], pch=16, cex=0.4, col="orange")
    points(detectors.xy[,2]~detectors.xy[,1], pch=16, cex=0.2, col="red")
    points(detectors.xy[index,2]~detectors.xy[index,1], pch=16, cex=0.4, col="blue")
    points(SXY[2]~SXY[1], bg="red", pch=21, cex=1.2)
    ##
    return(list(habitatID = habitatID,
                detectorIndex = detectorIndex,
                nDetectors = nDetectors,
                maxNBDets = maxNBDets,
                ResizeFactor = ResizeFactor))
}






