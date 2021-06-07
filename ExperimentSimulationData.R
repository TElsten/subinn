rm(list=ls())
setwd("/home/taelsten") # Shark WD

source("DataSets.R")
source("ModelSUBiNN.R")
source("ModelOthers.R")

# --------------------------------------------------------------------
# This script performs the simulation data study
# 
# It is meant to be run on the shark cluster, and takes arguments from cli:
# - Argument 1: Replication. 
# - Argument 2: nrOfNoninformatives = how many noise variables to add
# - Argument 3: w = the multiplication factor for covariance matrix
# - Argument 4: type = cov/cor: do we change the covariance (experiments 1/2) or the correlation (experiments 3/4)
#
# Otherwise, just set them yourself

args <- commandArgs(TRUE)

Replication <- as.numeric(args[1])
nrOfNoninformatives <- as.numeric(args[2])
w <- as.numeric(args[3])
type <- args[4]

# Make sure the seed is different for every combination of replication, nrOfNoninformatives, and w
set.seed(Replication + ((w * 15) * (nrOfNoninformatives + 1050)))

if (type == 'cov'){
  data <- GetCovarianceData(nrOfNoninformatives, 1000, w)
} else {
  data <- GetCorrelatedData(nrOfNoninformatives, 1000, w)
}

X <- data[, -ncol(data)]
y <- data[, ncol(data)] - 1

idx <- sample(rep(1:10, length.out=nrow(X)))
acc <- base::matrix(NA, ncol=8, nrow=10)
times <- base::matrix(NA, ncol=8, nrow=10)
featuresSelected <- base::matrix(NA, ncol=ncol(X) + ncol(combn(ncol(X), 2)), nrow=10)


modelFunctions <- c('GetkNNPredictions', 'GetBkNNPredictions',
                    'GetRkNNPredictions', 'GetMFSPredictions',
                    'GetRFPredictions', 'GetSVMPredictions',
                    'GetESkNNPredictions', 'GetSUBiNNPredictions')

for(i in 1:10) {
  Xtrain <- X[idx != i, ]
  ytrain <- y[idx != i]
  Xtest  <- X[idx == i, ]
  ytest  <- y[idx == i]
  
  counter <- 1
  for(modelFunction in modelFunctions){
    cat(modelFunction)
    start <- proc.time()
    # Obtain the predictions from the model function either ModelSUBiNN.R or ModelOthers.R
    result <-  tryCatch({
      do.call(modelFunction, list(Xtrain, Xtest, ytrain))
    }, error = function(error) {
      cat("We encountered an error, but we continue anyway:", message(error))
      rep(NA, length(ytest))
    })
    end <- proc.time()
    total <- end - start
    
    # Get accuracy, raw predictions, and for SUBiNN the base-learner selections
    if (modelFunction == 'GetSUBiNNPredictions'){
      acc[i, counter] <- mean(result[[1]] != ytest)
      featuresSelected[i, ] <- result[[2]][,1]
    } else {
      acc[i, counter] <- mean(result != ytest)
    }
    
    GetSUBiNNPredictions(Xtrain, Xtest, ytrain)
    times[i, counter] <- total[3]
    counter <- counter + 1;
  }
}

results <- list(acc, times, featuresSelected)

if (type == 'cor'){
  save(results, file=paste("./Results/Sim1/Sim1CORRUninf", nrOfNoninformatives, "w", w, "Rep", Replication, ".Rdata", sep=""))
} else {
  save(results, file=paste("./Results/Sim1/Sim1Uninf", nrOfNoninformatives, "w", w, "Rep", Replication, ".Rdata", sep=""))
}
