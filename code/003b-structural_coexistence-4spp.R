# COMPUTE STRUCTURAL COEXISTENCE

#sound alarms
library(beepr)

### LOAD DATA
load("results/scenarios.RData")

### LOAD STRUCTURAL COEXISTENCE FUNCTIONS
source("code/tools/str_coex_functions.R")


#module size
richness <- 4


#define start_time
start_time <- Sys.time()


## Scenario 1
scenario <- 1
S1 <- structural_coex(alpha = as.matrix(ALPHA[[scenario]]), intrinsic = as.matrix(IGR[[scenario]]), n = richness)


## Scenario 2
scenario <- 2
S2 <- structural_coex(alpha = as.matrix(ALPHA[[scenario]]), intrinsic = as.matrix(IGR[[scenario]]), n = richness)


## Scenario 3
scenario <- 3
S3 <- structural_coex(alpha = as.matrix(ALPHA[[scenario]]), intrinsic = as.matrix(IGR[[scenario]]), n = richness)


## Scenario 4
scenario <- 4
S4 <- structural_coex(alpha = as.matrix(ALPHA[[scenario]]), intrinsic = as.matrix(IGR[[scenario]]), n = richness)


## Scenario 5
scenario <- 5
S5 <- structural_coex(alpha = as.matrix(ALPHA[[scenario]]), intrinsic = as.matrix(IGR[[scenario]]), n = richness)


#end time
end_time <- Sys.time()
print(end_time - start_time)


#alarm!
beep(8)


#bind them
res <- rbind(S1, S2, S3, S4, S5)


#create one df only for structural outputs
str_coex <- data.frame("scenario" = rep(1:length(ALPHA), each = nrow(res) / length(ALPHA)),
                       "n" = rep(richness, nrow(res)),
                       "combos" = res$combos,
                       "SND" = res$SND,
                       "SFD" = res$SFD,
                       "feasibility" = res$feasibility)
rownames(str_coex) <- NULL


#create one df with structural coex. and module metrics
metrics <- cbind(str_coex, res[, 4:9])
rownames(metrics) <- NULL


#save both
write.table(str_coex, "results/str_coex/FG-structural_coexistence-complete-4spp.txt", sep = "\t", row.names = FALSE)
write.table(metrics, "results/str_coex/FG-metrics_coexistence-complete-4spp.txt", sep = "\t", row.names = FALSE)


#remove NAs
str_coex <- str_coex[complete.cases(str_coex),]
metrics <- metrics[complete.cases(metrics),]


#save both without NAs
write.table(str_coex, "results/str_coex/FG-structural_coexistence-4spp.txt", sep = "\t", row.names = FALSE)
write.table(metrics, "results/str_coex/FG-metrics_coexistence-4spp.txt", sep = "\t", row.names = FALSE)


rm(list = ls())
