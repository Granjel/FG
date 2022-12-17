library(mvtnorm)
library(foreach)
library(doParallel)
library(parallel)
library(MASS)
library(EnvStats)
library(bipartite)

#SND
Omega <- function(alpha){
  n <- nrow(alpha)
  Sigma <- solve(t(alpha) %*% alpha, tol = 1e-20) #tolerance modified due to singularities
  d <- pmvnorm(lower = rep(0,n), upper = rep(Inf,n), mean = rep(0,n), sigma = Sigma)
  out <- log10(d[1]) + n * log10(2)
  return(out)
}

r_centroid <- function(alpha){
  n <- nrow(alpha)
  D <- diag(1/sqrt(diag(t(alpha)%*%alpha)))
  alpha_n <- alpha %*% D
  r_c <- rowSums(alpha_n) /n
  r_c <- t(t(r_c))
  return(r_c)
}

#SFD
theta <- function(alpha,r){
  r_c <- r_centroid(alpha)
  out <- acos(sum(r_c*r)/(sqrt(sum(r^2))*sqrt(sum(r_c^2)))) * 180/pi
  return(out)
}

#Feasibility
test_feasibility <- function(alpha,r){
  out <- prod(solve(alpha, r) > 0)
  return(out)
}

#differential
test_feasibility_pairs <- function(alpha,r){
  n <- length(r)
  c <- combn(n,2)
  nc <- dim(c)[2]
  f <- rep(NA,nc)
  for (i in 1:nc){
    f[i] <- prod(solve(alpha[c[,i],c[,i]],r[c[,i]])>0)
  }
  out <- list(pairs = c, feasibility = f)
  return(out)
}

#overlap
compute_overlap <- function(alpha,Nrand){
  
  n <- dim(alpha)[1]
  
  counter_f <- 0
  counter_overlap <- 0
  counter_all <- 0
  
  for (i in 1:Nrand){
    
    r_rand <- abs(rnorm(n))  
    r_rand <- r_rand/sqrt(sum(r_rand^2))
    
    f1 <- test_feasibility(alpha,r_rand)  
    f2 <- test_feasibility_pairs(alpha,r_rand)$feasibility  
    
    counter_f <- counter_f + f1
    counter_all <- counter_all + prod(f2)
    counter_overlap <- counter_overlap + f1*prod(f2)
    
  }
  
  Omega <- counter_f/Nrand
  Omega_all <- counter_all/Nrand
  overlap <- counter_overlap/Nrand
  
  out <- list(Omega = Omega, Omega_all = Omega_all, overlap = overlap)
  return(out)
  
}

##function to calculate the dominance of the diagonal (intra- over inter-)
#dominance <- function(alpha){
#  dom <- sum(abs(alpha[row(alpha) == col(alpha)]), na.rm = TRUE) /
#    sum(abs(alpha[row(alpha) != col(alpha)]), na.rm = TRUE)
#  return(dom)
#}

#function to calculate the dominance of the diagonal (intra- over inter-)
dominance <- function(alpha){
  dom <- abs(sum(alpha[row(alpha) == col(alpha)], na.rm = TRUE) /
    sum(alpha[row(alpha) != col(alpha)], na.rm = TRUE))
  return(dom)
}

#running, final function
structural_coex <- function(alpha, intrinsic, n){
  
  pair_names <- apply(combn(n, 2), 2, paste, collapse = "_") #name all the possible pairs
  
  combos <- t(combn(rownames(alpha), n)) #matrix with all possible combinations of n species
  
  results_combos <- as.data.frame(matrix(nrow = dim(combos)[1], ncol = 8))
  row.names(results_combos) <- apply(combos, 1, paste, collapse = "_") #change the 'collapse' feature if needed
  colnames(results_combos) <- c("SND", "SFD", "feasibility", "cv", "dominance", "SKR", "skewness", "kurtosis")
  
  for(i in 1:nrow(combos)){
    zeroes <- matrix(data = 0, nrow = n, ncol = n)
    intrinsic2 <- as.matrix(subset(intrinsic, rownames(intrinsic) %in% combos[i,]))
    alpha2 <- as.matrix(alpha[combos[i,], combos[i,]])
    
    #loop
    if(isFALSE(FALSE %in% (alpha2 == zeroes))){
      results_combos[i, ] <- NA
    } else {
      #catch error Omega
      possibleError1 <- tryCatch(10^Omega(alpha2), error=function(e) e)
      if(inherits(possibleError1, "error")){
        results_combos$SND[i] <- NA
      } else {
        #omega
        results_combos$SND[i] <- 10^Omega(alpha2)
        
        #catch error theta
        possibleError2 <- tryCatch(theta(alpha2, intrinsic2), error=function(e) e)
        if(inherits(possibleError2, "error")){
          results_combos$SFD[i] <- NA
        } else {
          #theta
          results_combos$SFD[i] <- theta(alpha2, intrinsic2)
          
          #catch error feasibility
          possibleError3 <- tryCatch(test_feasibility(alpha2, intrinsic2), error=function(e) e)
          if(inherits(possibleError3, "error")){
            results_combos$feasibility[i] <- NA
          } else {
            #feasibility (all)
            results_combos$feasibility[i] <- test_feasibility(alpha2, intrinsic2)
            
            #catch error cv_inter
            possibleError4 <- tryCatch(cv(as.numeric(alpha2[row(alpha2) != col(alpha2)])), error=function(e) e)
            if(inherits(possibleError4, "error")){
              results_combos$cv[i] <- NA
            } else {
              #cv_inter (off diagonal)
              results_combos$cv[i] <- cv(as.numeric(alpha2[row(alpha2) != col(alpha2)]))
              
              #catch error dominance
              possibleError5 <- tryCatch(dominance(alpha2), error=function(e) e)
              if(inherits(possibleError5, "error")){
                results_combos$dominance[i] <- NA
              } else {
                #dominance
                results_combos$dominance[i] <- dominance(alpha2)
                
                #catch error SKR
                possibleError5 <- tryCatch((skewness(as.numeric(alpha2)))^2 / kurtosis(as.numeric(alpha2)), error=function(e) e)
                if(inherits(possibleError5, "error")){
                  results_combos$SKR[i] <- NA
                  results_combos$skewness[i] <- NA
                  results_combos$kurtosis[i] <- NA
                } else {
                  #skewness/kurtosis
                  results_combos$SKR[i] <- (skewness(as.numeric(alpha2)))^2 / kurtosis(as.numeric(alpha2))
                  #skewness
                  results_combos$skewness[i] <- skewness(as.numeric(alpha2))
                  #kurtosis
                  results_combos$kurtosis[i] <- kurtosis(as.numeric(alpha2))
                }
              }
            }
          }
        }
      }
    }
  }
  results_combos <- as.data.frame(results_combos)
  
  results_combos$combos <- rownames(results_combos)
  
  return(results_combos)
}