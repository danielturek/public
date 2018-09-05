NIMBLE <- readRDS("election88_M_orig_S_default_result_updatedESS.Rdata")
NIMBLEefficiency <- NIMBLE[[1]]$summary[1,'efficiency',]
JAGS <- readRDS("election88_M_orig_JAGS_result_updatedESS.Rdata")
JAGSefficiency <- JAGS[[1]]$summary[1,'efficiency',]
JAGSglm <- readRDS("election88_M_orig_JAGS_glm_result_updatedESS.Rdata")
JAGSglmEfficiency <- JAGS[[1]]$summary[1,'efficiency',]
NIMBLE2 <- readRDS("election88_M_5_S_default_result_updatedESS.Rdata")
NIMBLE2efficiency <- NIMBLE2[[1]]$summary[1,'efficiency',]
NIMBLEcustomList <- list()
for(i in 1:10) {
    filename <- paste0("election88_M_5_S_block_logsd_oneRep_R",i,"_result_updatedESS.Rdata")
    if(file.exists(filename)) {
        NIMBLEcustomList[[i]] <- readRDS(filename)
        NIMBLEcustomList[[i]][[1]]$samples <- NULL
    }
}
NIMBLEcustomListEfficiencies <- lapply(NIMBLEcustomList,
                                    function(x) x[[1]]$summary[1,'efficiency',])
NIMBLEcustomEfficiency <- do.call("rbind", NIMBLEcustomListEfficiencies)
NIMBLEcustomEfficiency <- colMeans(NIMBLEcustomEfficiency)

NIMBLEautoBlockList <- list()
for(i in 1:10) {
    filename <- paste0("election88_M_5_S_autoBlock_oneRep_R",i,"_result_updatedESS.Rdata")
    if(file.exists(filename)) {
        NIMBLEautoBlockList[[i]] <- readRDS(filename)
        NIMBLEautoBlockList[[i]][[1]]$samples <- NULL
    }
}
NIMBLEautoBlockListEfficiencies <- lapply(NIMBLEautoBlockList,
                                    function(x) x[[1]]$summary[1,'efficiency',])
NIMBLEautoBlockEfficiency <- do.call("rbind", NIMBLEautoBlockListEfficiencies)
NIMBLEautoBlockEfficiency <- colMeans(NIMBLEautoBlockEfficiency)


StanList <- list()
for(i in 1:10) {
    filename <- paste0("election88_M_Stan_oneRep_R",i,"_result_updatedESS.Rdata")
    if(file.exists(filename)) {
        StanList[[i]] <- readRDS(filename)
        StanList[[i]][[1]]$samples <- NULL
    }
}
StanListEfficiencies <- lapply(StanList,
                                    function(x) x[[1]]$summary[1,'efficiency',])
StanEfficiency <- do.call("rbind", StanListEfficiencies)
StanEfficiency <- colMeans(StanEfficiency)

library(reshape2)
efficiencies <- data.frame(do.call("rbind",
                                   list(JAGSefficiency,
                                        JAGSglmEfficiency,
                                        NIMBLEefficiency,
                                        NIMBLE2efficiency,
                                        NIMBLEcustomEfficiency,
                                        NIMBLEautoBlockEfficiency,
                                        StanEfficiency)))
efficienciesAll <- efficiencies
efficienciesAll$Method <- factor(c("JAGS", "JAGSglm", "NIMBLE","NIMBLE2", "NIMBLE\ncustom", "NIMBLE\nautoBlock", "Stan"),
                                 levels = c("JAGS", "JAGSglm", "NIMBLE","NIMBLE2", "NIMBLE\ncustom", "NIMBLE\nautoBlock", "Stan"))
efficienciesAll <- melt(efficienciesAll)
colnames(efficienciesAll) <- c("Method", "Param", "MCMC_efficiency")
efficienciesAll
library(ggplot2)
paramPlot <- ggplot(efficienciesAll, aes(x = Method, y = MCMC_efficiency, color = Param, group = Param)) + geom_point() + geom_line() + scale_y_log10() + labs(title="MCMC performance on election88 model & data", x ="MCMC", y = "MCMC efficiency = ESS/computation time")
paramPlot
ggsave("election88_paramPlot.pdf", height = 4, width = 6)

efficienciesMin <- apply(efficiencies, 1, min)
efficienciesMin <- data.frame(min_MCMC_efficiency = efficienciesMin,
                              Method = factor(c("JAGS", "JAGSglm", "NIMBLE","NIMBLE2", "NIMBLE\ncustom", "NIMBLE\nautoBlock", "Stan"),
                                              levels = c("JAGS", "JAGSglm", "NIMBLE","NIMBLE2", "NIMBLE\ncustom", "NIMBLE\nautoBlock", "Stan")))

efficiencyPlot <- ggplot(efficienciesMin, aes(x = Method, y = min_MCMC_efficiency, color = Method, fill = Method)) + geom_bar(stat = "identity") + labs(title = "min(MCMC efficiency) on election88 model & data", x = "MCMC", y = "net MCMC efficiency = min(ESS)/computation time")

efficiencyPlot
ggsave("election88_minEfficiencyPlot.pdf", height = 4, width = 6)
