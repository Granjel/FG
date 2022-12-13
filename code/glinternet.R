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
save(gli_models, file = "results/glinternet/glinternet_models.RData") #save glinternet results







##load glinternet results
load("results/glinternet/gli_models.RData")

#select the lambdas for each glinternet model to always obtain "intras" - TAKES SOME TIME
lambdas <- NULL
for (i in 1:length(gli_models)){
  clause <- FALSE
  n_i <- which(gli_models[[i]]$lambdaHat == gli_models[[i]]$lambda) #lambda selected
  while ((isFALSE(clause)) && (n_i <= 50)){
    coefs <- coef(gli_models[[i]]$glinternetFit)[[n_i]]
    clause <- isTRUE(i %in% coefs$mainEffects$cont)
    n_i <- n_i + 1
  }
  lambdas <- c(lambdas, n_i)
}

#species number 21
lambdas[which(lambdas > 50)] <- which(gli_models[[which(lambdas > 50)]]$lambdaHat == gli_models[[which(lambdas > 50)]]$lambda)

#save IGR, fixed effects and interactions
igr <- NULL
fixed <- list()
inter <- list()

for (i in 1:length(gli_models)){
  #IGR
  igr <- c(igr, gli_models[[i]]$betahat[[1]][1])
  
  #fixed effects
  n_i <- lambdas[i]
  coefs <- coef(gli_models[[i]]$glinternetFit)[[n_i]] #save the coefficients
  fixed[[i]] <- data.frame("pos" = coefs$mainEffects$cont,
                           "coef" = unlist(coefs$mainEffectsCoef$cont))
  if (nrow(fixed[[i]]) > 0){ #order
    fixed[[i]] <- fixed[[i]][order(fixed[[i]]$pos),]
  }
  inter[[i]] <- data.frame("spp1" = coefs$interactions$contcont[,1],
                           "spp2" = coefs$interactions$contcont[,2],
                           "coef" = unlist(coefs$interactionsCoef$contcont))
}

##removing these species
#igr <- igr[-which(zero == 0)]
#fixed <- fixed[-which(zero == 0)]
#inter <- inter[-which(zero == 0)]
#species <- species[-which(zero == 0)]

##remove these species from the fixed and interaction list (and also the non-focal spp.)
#for (i in 1:length(species)){
#  #fixed
#  fixed[[i]] <- fixed[[i]][-unique(c(which(fixed[[i]]$pos %in% c(which(zero == 0), 37:39)))),]
#  #inter
#  inter[[i]] <- inter[[i]][-unique(c(which(inter[[i]]$spp1 %in% c(which(zero == 0), 37:39)),
#                                     which(inter[[i]]$spp2 %in% c(which(zero == 0), 37:39)))),]
#}

#save these elements
save(species, grasshoppers, igr, fixed, inter, file = "results/glinternet/fixed_igr.RData")

#clean house
rm(list = ls())


##matrices
load("results/glinternet/fixed_igr.RData") #load coefficients

#create a matrix for plant-plant + grasshopper-plant together
alpha_gamma <- matrix(0, 36, 45)
for (i in 1:length(species)){
  for (j in 1:nrow(fixed[[i]])){
    alpha_gamma[i, fixed[[i]]$pos[j]] <- fixed[[i]]$coef[j]
  }
}

#species to be removed
zero <- which(apply(alpha_gamma, 1, sum) == 0)

#alpha_gamma without rows of "zero" species
alpha_gamma <- alpha_gamma[-zero,]

#separate alpha
alpha <- alpha_gamma[, 1:length(species)]
alpha <- alpha[, -zero] #remove columns with "zero" species

#separate gamma
gamma <- alpha_gamma[, (ncol(alpha_gamma)-(length(grasshoppers)-1)):ncol(alpha_gamma)]

#remove from the species list
species <- species[-zero]

#name the columns and rows
colnames(alpha) <- species
rownames(alpha) <- species
colnames(gamma) <- grasshoppers
rownames(gamma) <- species



#separate the different information for later matrices
inter_pp <- list()
inter_gp <- list()
inter_gg <- list()

for (i in 1:length(inter)){
  #for beta_pp
  inter_pp[[i]] <- inter[[i]][-unique(c(which(inter[[i]]$spp1 %in% c(37:45)),
                                        which(inter[[i]]$spp2 %in% c(37:45)))),]
  #for beta_gp
  inter_gp[[i]] <- inter[[i]][which(inter[[i]]$spp2 %in% c(40:45)),]
  inter_gp[[i]] <- inter_gp[[i]][which(inter_gp[[i]]$spp1 %in% c(1:36)),]

  #for beta_gg
  inter_gg[[i]] <- inter[[i]][which(inter[[i]]$spp1 %in% c(40:45)),]
}



#building the matrices!
beta_pp <- lapply(1:length(inter), matrix, data = 0, nrow = length(igr), ncol = length(igr)) #plants on plant-plant
beta_gp <- lapply(1:length(grasshoppers), matrix, data = 0, nrow = length(igr), ncol = length(igr)) #grasshoppers on plant-plant
beta_gg <- lapply(1:length(grasshoppers), matrix, data = 0, nrow = length(igr), ncol = length(grasshoppers)) #grasshoppers on grasshopper-plant
for (i in 1:length(inter)){
  #beta_pp
  for (j in 1:nrow(inter_pp[[i]])){
    if(nrow(inter_pp[[i]]) > 0){
      beta_pp[[inter_pp[[i]]$spp1[j]]][i, inter_pp[[i]]$spp2[j]] <- inter_pp[[i]]$coef[j] / 2
      beta_pp[[inter_pp[[i]]$spp2[j]]][i, inter_pp[[i]]$spp1[j]] <- inter_pp[[i]]$coef[j] / 2
    }
  }
  #beta_gp
  for (j in 1:nrow(inter_gp[[i]])){
    if(nrow(inter_gp[[i]]) > 0){
      beta_gp[[(inter_gp[[i]]$spp2[j] - 39)]][i, inter_gp[[i]]$spp1[j]] <- inter_gp[[i]]$coef[j]
    }
  }
  #beta_gg
  for (j in 1:nrow(inter_gg[[i]])){
    if(nrow(inter_gg[[i]]) > 0){
      beta_gg[[(inter_gg[[i]]$spp1[j] - 39)]][i, (inter_gg[[i]]$spp2[j] - 39)] <- inter_gg[[i]]$coef[j] / 2
      beta_gg[[(inter_gg[[i]]$spp2[j] - 39)]][i, (inter_gg[[i]]$spp1[j] - 39)] <- inter_gg[[i]]$coef[j] / 2
    }
  }
}

#remove undesired species from igr and all beta_pp matrices
igr <- igr[-zero]
names(igr) <- species
beta_pp <- beta_pp[-zero]

#remove undesired species from all beta_pp matrices
for (i in 1:length(beta_pp)){
  beta_pp[[i]] <- beta_pp[[i]][-zero, -zero]
  rownames(beta_pp[[i]]) <- species
  colnames(beta_pp[[i]]) <- species
}

#remove undesired species from all beta_gp and beta_gg matrices
for (i in 1:length(beta_gg)){
  beta_gp[[i]] <- beta_gp[[i]][-zero, -zero]
  rownames(beta_gp[[i]]) <- species
  colnames(beta_gp[[i]]) <- species
  beta_gg[[i]] <- beta_gg[[i]][-zero, -zero]
  rownames(beta_gg[[i]]) <- species
  colnames(beta_gg[[i]]) <- grasshoppers
}

#save all matrices
save(igr, alpha, gamma, beta_pp, beta_gp, beta_gg, species, grasshoppers, file = "results/matrices.RData")

#clean house
rm(list = ls())
