# creating the matrices for the five different scenarios

load("results/matrices.RData")


# Lists to write the matrices for each scenario
IGR <- list()
ALPHA <- list()


# SCENARIO 1
## only direct plant-plant effects
IGR[[1]] <- igr
ALPHA[[1]] <- alpha


# SCENARIO 2
## only plants: direct effects and HOIs
IGR[[2]] <- igr
ALPHA[[2]] <- alpha + (Reduce("+", beta_pp))


# SCENARIO 3
## only direct plant-plant and grasshopper-plant effects
IGR[[3]] <- igr + igr * apply(gamma, 1, sum)
ALPHA[[3]] <- alpha


# SCENARIO 4
## grasshoppers + HOIs plant-plants
IGR[[4]] <- igr + igr * apply(gamma, 1, sum)
ALPHA[[4]] <- alpha + (Reduce("+", beta_pp))


# SCENARIO 5
## full community, all direct effects and HOIs
IGR[[5]] <- igr + igr * apply(gamma, 1, sum)
ALPHA[[5]] <- alpha + (Reduce("+", beta_pp)) + (Reduce("+", beta_gp))


#save
save(IGR, ALPHA, species, grasshoppers, file = "results/scenarios.RData")


rm(list = ls())