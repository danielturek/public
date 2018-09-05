case <- "M_Stan"
for(i in 1:10) {
    cat(paste0("iNimReplicate <- ",i,"\n"), file = "setNimReplicate.R")
    outfile <- paste0(case, "_oneRep_R",i,".Rout")
    system(paste0("caffeinate -i R CMD BATCH ", case, ".R ",outfile))
}
q('no')
