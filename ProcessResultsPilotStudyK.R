#------------------------------------------
# Loads in all the raw results for the pilot study to the effect of k
# saves one file of results for each value of k, for datasets dystrophy and diabetes 

rm(list=ls())
setwd("D:/Documents/Jaar6 2019-2020/Thesis/Results/StudyK")
library(xtable)

computeVar <- function(mat) {
  apply(mat, 2, var)
}

ReadAndSaveStudyKResults <- function(){
  Ks <- c('5', '10', 'sqrt', 'opt')
  reps <- 100
  dataSets <- c('diabetes', 'dystrophy')
  for(name in dataSets){
    featuresLength <- ifelse(name == 'diabetes', 36, 15)
    datasetResults <- list()
    for (k in Ks){
      varNameResult <- paste("K", k, name, sep="")
      varNameSelection <- paste("K", k, name, "selection", sep="")
      varNameTime <- paste("K", k, name, "time", sep="")
      allResults <- matrix(NA, nrow=reps, ncol=3)
      allSelections <- matrix(NA, nrow=reps*10, ncol=featuresLength)
      allTimes <- matrix(NA, nrow=reps, ncol=1)
      
      for (rep in 1:reps) {
        load(paste("raw/StudyKRep", rep, "K", k, "Data", name, ".RData", sep=""))
        allResults[rep, ] <- results$results
        allTimes[rep, ] <- results$time[1] + results$time[2]
        from <- (rep - 1) * 10 + 1
        to <- rep * 10
        allSelections[from:to, ] <- results$varSelection
      }
      
      datasetResults[[varNameResult]] <- allResults
      datasetResults[[varNameSelection]] <- allSelections
      datasetResults[[varNameTime]] <- allTimes
    }
    
    save(datasetResults, file=paste("StudyK", name, "Results.RData", sep=""))
  }
}

# Use this to process the raw results into 2 files
# ReadAndSaveStudyKResults()


load("StudyKdiabetesResults.RData")
StudyKdiabetesResults <- datasetResults
load("StudyKdystrophyResults.RData")
StudyKdystrophyResults <- datasetResults


# Make latex tables ------------------------------------
############
# DIABETES
############
# Accuracy + time
diabetesAccuracy <- data.frame(
  rbind(colMeans(StudyKdiabetesResults$K5diabetes),
        colMeans(StudyKdiabetesResults$K10diabetes),
        colMeans(StudyKdiabetesResults$Ksqrtdiabetes),
        colMeans(StudyKdiabetesResults$Koptdiabetes)),
  rbind(colMeans(StudyKdiabetesResults$K5diabetestime),
        colMeans(StudyKdiabetesResults$K10diabetestime),
        colMeans(StudyKdiabetesResults$Ksqrtdiabetestime),
        colMeans(StudyKdiabetesResults$Koptdiabetestime)),
  row.names = c('K=5', 'K=10', 'K=sqrt(N)', 'K=opt'))

colnames(diabetesAccuracy) <- c('kNN', 'sNN', 'NrVars', 'sNN time')
diabetesAccuracy
xtable(diabetesAccuracy, digits=3)
# Variance
diabetesVariance <- data.frame(
  rbind(computeVar(StudyKdiabetesResults$K5diabetes),
        computeVar(StudyKdiabetesResults$K10diabetes),
        computeVar(StudyKdiabetesResults$Ksqrtdiabetes),
        computeVar(StudyKdiabetesResults$Koptdiabetes)),
  row.names = c('K=5', 'K=10', 'K=sqrt(N)', 'K=opt'))

colnames(diabetesVariance) <- c('kNN', 'sNN', 'NrVars')
diabetesVariance

############
# DYSTROPHY
############
# Accuracy + time
dystrophyAccuracy <- data.frame(
  rbind(colMeans(StudyKdystrophyResults$K5dystrophy),
        colMeans(StudyKdystrophyResults$K10dystrophy),
        colMeans(StudyKdystrophyResults$Ksqrtdystrophy),
        colMeans(StudyKdystrophyResults$Koptdystrophy)),
  rbind(colMeans(StudyKdystrophyResults$K5dystrophytime),
        colMeans(StudyKdystrophyResults$K10dystrophytime),
        colMeans(StudyKdystrophyResults$Ksqrtdystrophytime),
        colMeans(StudyKdystrophyResults$Koptdystrophytime)),
  row.names = c('K=5', 'K=10', 'K=sqrt(N)', 'K=opt'))

colnames(dystrophyAccuracy) <- c('kNN', 'sNN', 'NrVars', 'sNN time')
dystrophyAccuracy
xtable(dystrophyAccuracy, digits=3)

# Variance
dystrophyVariance <- data.frame(
  rbind(computeVar(StudyKdystrophyResults$K5dystrophy),
        computeVar(StudyKdystrophyResults$K10dystrophy),
        computeVar(StudyKdystrophyResults$Ksqrtdystrophy),
        computeVar(StudyKdystrophyResults$Koptdystrophy)),
  row.names = c('K=5', 'K=10', 'K=sqrt(N)', 'K=opt'))

colnames(dystrophyVariance) <- c('kNN', 'sNN', 'NrVars')
dystrophyVariance
