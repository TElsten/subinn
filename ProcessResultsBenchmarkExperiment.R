#------------------------------------------
# Loads in all the raw results for the benchmark dataset study
# saves one file of results for each dataset

rm(list=ls())
setwd("D:/Documents/Jaar6 2019-2020/Thesis/Results")
source("../../Code/DataSets.R")
library(xtable)

computeVar <- function(mat) {
  apply(mat, 2, var)
}

ReadAndSaveBenchResults <- function() {
  models <- c('knn', 'bknn', 'rknn', 'mfs', 'rf', 'svm', 'esknn', 'snn')
  dataSets <- c('haber', 'mammo', 'trans', 'phone', 'bupa', 'appen', 'dystr',
                'diabe', 'biops', 'heart', 'india', 'solar', 'credi', 'house',
                'bands', 'hepat', 'twono', 'germa', 'wpbc', 'sonar',
                'glauc', 'musk')
  reps <- 100
  countAllDatasets <- 1
  allAccuracies <- matrix(NA, nrow=length(dataSets), ncol=9)
  allExecutionTimes <- matrix(NA, nrow=length(dataSets), ncol=8)
  
  
  for(dataSet in dataSets){
    
    cat(dataSet, '\n')
    allAcc <- matrix(NA, nrow=reps, ncol=8)
    allTimes <- matrix(NA, nrow=reps, ncol=8)
    allPredictions <- matrix(NA, nrow=nrow(GetBenchmarkData(dataSet)[[1]]), ncol=9)
    
    nrOfFeatures <- ncol(GetBenchmarkData(dataSet)[[1]])
    nrOfBaseLearners <- nrOfFeatures + choose(nrOfFeatures, 2)
    allFeatureSelections <- matrix(NA, nrow=1000, ncol=nrOfBaseLearners)
    
    for(rep in 1:reps){
      result <- tryCatch({
        load( paste("Bench/raw/Bench", dataSet, "Rep", rep, ".RData", sep=""))
        TRUE
      }, error = function(error) {
        cat('ERROR!', dataSet, ' ', rep, '\n')
        FALSE
      }, warning = function(warning) {
        cat('Warning no file for ', dataSet, ' ', rep, '\n')
        FALSE
      })
      
      if (result){
        allAcc[rep, ] <- colMeans(results[[1]])
        allTimes[rep, ] <- colSums(results[[2]])
        allPredictions <- results[[4]]
        from <- (rep - 1) * 10 + 1
        to <- (rep - 1) * 10 + 10
        allFeatureSelections[from:to, ] <- results[[3]]
      }
      
    }
    
    results <- list(allAcc, allTimes, allFeatureSelections)
    save(results, file=paste("Bench/Bench", dataSet, ".RData", sep=""))
    colMeans(allAcc, na.rm=TRUE)
    colMeans(allTimes, na.rm=TRUE)
    
    allAccuracies[countAllDatasets, 1:8] <- colMeans(allAcc, na.rm=TRUE)
    chosenCoefs <- colSums(results[[3]] != 0)
    howManyCoefsPerRun <- rowSums(results[[3]] != 0)
    
    
    allAccuracies[countAllDatasets, 9] <- mean(howManyCoefsPerRun)
    allExecutionTimes[countAllDatasets, ] <- colMeans(allTimes, na.rm=TRUE)
    countAllDatasets <- countAllDatasets + 1
  }
  
  
  allAccuracies <- data.frame(allAccuracies, row.names = dataSets)
  allExecutionTimes <- data.frame(allExecutionTimes, row.names = dataSets)
  save(allAccuracies, file="Bench/AllBenchAccuracies.RData");
  save(allExecutionTimes, file="Bench/AllBenchTimes.RData")
}


# Use this to re-process the raw results
# ReadAndSaveBenchResults()

load("Bench/AllBenchTimes.RData")
load("Bench/AllBenchAccuracies.RData")

# Latex tables -----------------------------------
xtable::xtable(allAccuracies, digits=3)
xtable::xtable(allExecutionTimes, digits=3)


