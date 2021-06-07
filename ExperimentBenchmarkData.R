rm(list=ls())
setwd("/home/taelsten") # Shark WD

source("DataSets.R")
source("ModelSUBiNN.R")
source("ModelOthers.R")

# --------------------------------------------------------------------
# This script performs the benchmark dataset experiment
# 
# It is meant to be run on the shark cluster, and takes arguments from cli:
# - Argument 1: Replication. 
# - Argument 2: dataset name
#
# Otherwise, just set them yourself

args <- commandArgs(TRUE)

Replication <- as.numeric(args[1])
dataName <- args[2]

# Convert the dataName to numbers to obtain unique seed
letterAsNumbers <- sapply(unlist(strsplit(dataName, '')), function(letter) {
  which(letter == letters)
})

set.seed(Replication + as.numeric(paste(letterAsNumbers, collapse=""))/5)
data <- GetBenchmarkData(dataName)

X <- data[[1]]
y <- data[[2]]


GetError <- function(pred, real, dataName){
  return(mean(pred != real))
}

# Create random folds and init all matrices for results
idx <- sample(rep(1:10, length.out=nrow(X)))
acc <- base::matrix(NA, ncol=8, nrow=10)
predictions <- base::matrix(0, ncol=9, nrow=nrow(X))
times <- base::matrix(NA, ncol=8, nrow=10)
featuresSelected <- base::matrix(NA, ncol=ncol(X) + ncol(combn(ncol(X), 2)), nrow=10)

modelFunctions <- c('GetkNNPredictions', 'GetBkNNPredictions',
                    'GetRkNNPredictions', 'GetMFSPredictions',
                    'GetRFPredictions', 'GetSVMPredictions',
                    'GetESkNNPredictions', 'GetSUBiNNPredictions')


lastPredictionRow <- 1
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
    # Keep track of elapsed time
    result <-  tryCatch({
      do.call(modelFunction, list(Xtrain, Xtest, ytrain, TRUE))
    }, error = function(error) {
      cat('error', message(error))
      rep(NA, length(ytest))
    })
    end <- proc.time()
    total <- end - start
  
    # Get accuracy, raw predictions, and for SUBiNN the base-learner selections
    acc[i, counter] <- GetError(as.numeric(as.character(result[[1]])), as.numeric(as.character(ytest)), dataName)
    predictions[lastPredictionRow:(lastPredictionRow + nrow(Xtest) - 1), counter] <- result[[2]]
    if (modelFunction == 'GetSUBiNNPredictions'){
      featuresSelected[i, ] <- result[[3]]
    } 

    times[i, counter] <- total[3]
    counter <- counter + 1;
  }
  predictions[lastPredictionRow:(lastPredictionRow + nrow(Xtest) - 1), 9] <- as.numeric(as.character(ytest))
  lastPredictionRow <- lastPredictionRow + nrow(Xtest)
}

results <- list(acc, times, featuresSelected, predictions)
save(results, file=paste("Results/Bench/raw/Bench", dataName, "Rep", Replication, ".Rdata", sep=""))


