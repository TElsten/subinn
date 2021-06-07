
#------------------------------------------
# Loads in all the raw results for the simulation experiments
# and saves one file of results for each value of w and nr of uninformative features. 
rm(list=ls())
setwd("D:/Documents/Jaar6 2019-2020/Thesis/Results/Sim1")
library(xtable)


computeVar <- function(mat) {
  apply(mat, 2, var)
}

models <- c('knn', 'bknn', 'rknn', 'mfs', 'rf', 'svm', 'esknn', 'snn')

ReadAndSaveSimulationResults <- function(Ws, features, correlation = FALSE){
  reps <- 100
  for(w in Ws){
    for(feature in features){
      allAcc <- matrix(NA, nrow=reps, ncol=8)
      allTimes <- matrix(NA, nrow=reps, ncol=8)
      
      nrOfFeatures <- 20 + as.numeric(feature) + ncol(combn(20 + as.numeric(feature), 2))
      allFeatureSelections <- matrix(NA, nrow=1000, ncol=nrOfFeatures)
      
      for (rep in 1:reps) {
        # Attempt to load the raw result
        result <- tryCatch({
          if (correlation){
            load(paste("raw/Sim1CORRUninf", feature, "w", w, "rep", rep, ".RData", sep=""))
          } else {
            load(paste("raw/Sim1Uninf", feature, "w", w, "rep", rep, ".RData", sep=""))
          }
          TRUE
        }, error = function(error) {
          cat('error for ', rep, w, feature, '\n')
          FALSE
        }, warning = function(warning) {
          cat('error: no file for results with rep ', rep, ' feature ', feature, ' w ', w, '\n')
          FALSE
        })
        
        if (result){
          allAcc[rep, ] <- colMeans(results[[1]])
          allTimes[rep, ] <- colSums(results[[2]])
          from <- (rep - 1) * 10 + 1
          to <- (rep - 1) * 10 + 10
          allFeatureSelections[from:to, ] <- results[[3]]
        }
      }
      
      allResults <- list(allAcc, allTimes, allFeatureSelections)
      if (correlation){
        save(allResults, file=paste("Sim1CORR", "Uninf", feature, "w", w, ".RData", sep=""))
      } else {
        save(allResults, file=paste("Sim1", "Uninf", feature, "w", w, ".RData", sep=""))
      }
    }
  }
}


# Use this to process the raw results again
# ReadAndSaveSimulationResults(Ws = c('1'), features = c('0', '50', '100', '200', '500'), FALSE)
# ReadAndSaveSimulationResults(Ws = c('3', '5', '10', '15', '20'), features = c('50'), FALSE)
# ReadAndSaveSimulationResults(Ws = c('1'), features = c('0', '50', '100', '200', '500'), TRUE)
# ReadAndSaveSimulationResults(Ws = c('3', '5', '10', '15', '20'), features = c('50'), TRUE)

# -------------------------------------------
# Put it all in latex tables
Table1 <- matrix(NA, ncol=8, nrow=5)
Table1Time <- matrix(NA, ncol=8, nrow=5)
load("Sim1Uninf0w1.RData")
mean(rowSums(allResults[[3]] != 0))
Table1[1, ] <- colMeans(allResults[[1]])
Table1Time[1, ] <- colMeans(allResults[[2]])
load("Sim1Uninf50w1.RData")
mean(rowSums(allResults[[3]] != 0))
Table1[2, ] <- colMeans(allResults[[1]])
Table1Time[2, ] <- colMeans(allResults[[2]])
load("Sim1Uninf100w1.RData")
mean(rowSums(allResults[[3]] != 0))
Table1[3, ] <- colMeans(allResults[[1]])
Table1Time[3, ] <- colMeans(allResults[[2]])
load("Sim1Uninf200w1.RData")
mean(rowSums(allResults[[3]] != 0))
Table1[4, ] <- colMeans(allResults[[1]])
Table1Time[4, ] <- colMeans(allResults[[2]])
load("Sim1Uninf500w1.RData")
mean(rowSums(allResults[[3]] != 0))
Table1[5, ] <- colMeans(allResults[[1]])
Table1Time[5, ] <- colMeans(allResults[[2]])


Table2 <- matrix(NA, ncol=8, nrow=5)
Table2Time <- matrix(NA, ncol=8, nrow=5)
load("Sim1Uninf50w3.RData")
Table2[1, ] <- colMeans(allResults[[1]])
Table2Time[1, ] <- colMeans(allResults[[2]])
load("Sim1Uninf50w5.RData")
Table2[2, ] <- colMeans(allResults[[1]])
Table2Time[2, ] <- colMeans(allResults[[2]])
load("Sim1Uninf50w10.RData")
Table2[3, ] <- colMeans(allResults[[1]])
Table2Time[3, ] <- colMeans(allResults[[2]])
load("Sim1Uninf50w15.RData")
Table2[4, ] <- colMeans(allResults[[1]])
Table2Time[4, ] <- colMeans(allResults[[2]])
load("Sim1Uninf50w20.RData")
Table2[5, ] <- colMeans(allResults[[1]])
Table2Time[5, ] <- colMeans(allResults[[2]])

Table1; Table1Time;
Table2; Table2Time;
xtable::xtable(Table1, digits=3)
xtable::xtable(Table1Time, digits=1)

xtable::xtable(Table2, digits=3)
xtable::xtable(Table2Time, digits=1)


## Correlation tables
Table3 <- matrix(NA, nrow=5, ncol=8)
load("Sim1CORRUninf0w1.RData")
Table3[1, ] <- colMeans(allResults[[1]])
load("Sim1CORRUninf50w1.RData")
Table3[2, ] <- colMeans(allResults[[1]])
load("Sim1CORRUninf100w1.RData")
Table3[3, ] <- colMeans(allResults[[1]])
load("Sim1CORRUninf200w1.RData")
Table3[4, ] <- colMeans(allResults[[1]])
load("Sim1CORRUninf500w1.RData")
Table3[5, ] <- colMeans(allResults[[1]])

## Correlation tables
Table4 <- matrix(NA, nrow=5, ncol=8)
load("Sim1CORRUninf50w3.RData")
Table4[1, ] <- colMeans(allResults[[1]])
load("Sim1CORRUninf50w5.RData")
Table4[2, ] <- colMeans(allResults[[1]])
load("Sim1CORRUninf50w10.RData")
Table4[3, ] <- colMeans(allResults[[1]])
load("Sim1CORRUninf50w15.RData")
Table4[4, ] <- colMeans(allResults[[1]])
load("Sim1CORRUninf50w20.RData")
Table4[5, ] <- colMeans(allResults[[1]])

xtable::xtable(Table3, digits=3)
xtable::xtable(Table4, digits=3)

#### VARIANCE TABLE
TableVAR <- matrix(NA, ncol=8, nrow=5)
load("Sim1Uninf0w1.RData")
TableVAR[1, ] <- computeVar(allResults[[1]])
load("Sim1Uninf50w1.RData")
TableVAR[2, ] <- computeVar(allResults[[1]])
load("Sim1Uninf100w1.RData")
TableVAR[3, ] <- computeVar(allResults[[1]])
load("Sim1Uninf200w1.RData")
TableVAR[4, ] <- computeVar(allResults[[1]])
load("Sim1Uninf500w1.RData")
TableVAR[5, ] <- computeVar(allResults[[1]])


#### W vs nr of variables selected
## Uncorrelated tables
Table5 <- matrix(NA, nrow=6, ncol=3)
load("Sim1Uninf50w1.RData")
Table5[1, ] <- c(1, sd(rowSums(allResults[[3]] != 0)), mean(rowSums(allResults[[3]] != 0)))
load("Sim1Uninf50w3.RData")
Table5[2, ] <- c(3, sd(rowSums(allResults[[3]] != 0)), mean(rowSums(allResults[[3]] != 0)))
load("Sim1Uninf50w5.RData")
Table5[3, ] <- c(5, sd(rowSums(allResults[[3]] != 0)), mean(rowSums(allResults[[3]] != 0)))
load("Sim1Uninf50w10.RData")
Table5[4, ] <- c(10, sd(rowSums(allResults[[3]] != 0)), mean(rowSums(allResults[[3]] != 0)))
load("Sim1Uninf50w15.RData")
Table5[5, ] <- c(15, sd(rowSums(allResults[[3]] != 0)), mean(rowSums(allResults[[3]] != 0)))
load("Sim1Uninf50w20.RData")
Table5[6, ] <- c(20, sd(rowSums(allResults[[3]] != 0)), mean(rowSums(allResults[[3]] != 0)))


## Correlation tables
Table5 <- matrix(NA, nrow=6, ncol=2)
load("Sim1CORRUninf50w1.RData")
Table5[1, ] <- c(1, mean(rowSums(allResults[[3]] != 0)))
load("Sim1CORRUninf50w3.RData")
Table5[2, ] <- c(3, mean(rowSums(allResults[[3]] != 0)))
load("Sim1CORRUninf50w5.RData")
Table5[3, ] <- c(5, mean(rowSums(allResults[[3]] != 0)))
load("Sim1CORRUninf50w10.RData")
Table5[4, ] <- c(10, mean(rowSums(allResults[[3]] != 0)))
load("Sim1CORRUninf50w15.RData")
Table5[5, ] <- c(15, mean(rowSums(allResults[[3]] != 0)))
load("Sim1CORRUninf50w20.RData")
Table5[6, ] <- c(20, mean(rowSums(allResults[[3]] != 0)))
