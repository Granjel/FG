#load plant-grasshopper data
data <- read.table("data/data_grasshoppers.txt", sep = "\t", header = TRUE)
species <- unique(data$Focal) #species involved
grasshoppers <- c("Cb", "Cd", "Ci", "Ee", "Pg", "Pp")

#load the glinternet package
library(glinternet)

#glinternet loop --- TAKES SOME TIME (avoid)
gli_models <- list()
for (i in 1:length(species)){
  gli_models[[i]] <- glinternet.cv(X = data[data$Focal == species[i],][, 8:52],
                                   Y = data[data$Focal == species[i],]$Cover,
                                   numLevels = rep(1, 45))
  cat(round(i / length(species) * 100, 1), "%\n")
}