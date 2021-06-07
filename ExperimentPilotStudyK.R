rm(list=ls())
setwd("/home/taelsten") # Shark WD

source("DataSets.R")
source("ModelSUBiNN.R")
source("ModelOthers.R")

# --------------------------------------------------------------------
# This script performs a study to the effect of the parameter k in kNN
# on the performance of subinn
# 
# It is meant to be run on the shark cluster, and takes arguments from cli:
# - Argument 1: Replication. 
# - Argument 2: k
# - Argument 3: dataset name
#
# Otherwise, just set them yourself

args <- commandArgs(TRUE)
Replication <- as.numeric(args[1])
k <- args[2]
dataName <- args[3]

cat(dataName)

# Make sure the seed is different for every combination of replication and k
set.seed(switch(k, 
                opt = Replication + 2000, 
                sqrt = Replication + 4000, 
                Replication  + (1000 * as.numeric(k))))
# Get the data
data <- GetBenchmarkData(dataName)
X <- data[[1]]
y <- data[[2]]

# CV indices
idx <- sample(rep(1:10, length.out=nrow(X)))

SUBiNNAccuracy <- rep(0, 10)
kNNAccuracy <- rep(0, 10)
nrOfCoefs <- rep(0, 10)
SUBiNNBaseLearnerSelection <- matrix(NA, nrow=10, ncol=ncol(X) + choose(ncol(X), 2))

for (i in 1:10){
  Xtrain  = X[idx != i,]
  ytrain  = as.matrix(y[idx != i], ncol = 1)
  Xtest   = X[idx == i,] 
  ytest   = as.matrix(y[idx == i], ncol = 1)
  
  SUBiNNResults <- GetSUBiNNPredictions(Xtrain, Xtest, ytrain, TRUE, k)
  kNNResults <- GetkNNPredictions(Xtrain, Xtest, ytrain, FALSE, k)
  
  SUBiNNAccuracy[i] <- mean(ytest == SUBiNNResults[[1]])
  kNNAccuracy[i] <- mean(ytest == kNNResults)
  
  coefs <-  SUBiNNResults[[3]]
  SUBiNNBaseLearnerSelection[i,] <- coefs != 0
  nrOfCoefs[i] <- sum(coefs != 0)
}

timeElapsed <- proc.time()
results <- list(
  'results' = c(mean(kNNAccuracy), mean(SUBiNNAccuracy), mean(nrOfCoefs)),
  'varSelection' = SUBiNNBaseLearnerSelection,
  'time' = timeElapsed
)


save(results, file=paste("./Results/StudyK/StudyKRep", Replication, "K", k, "Data", dataName, ".Rdata", sep=""))
