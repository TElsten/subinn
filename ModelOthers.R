# Install missing packages if any
# list.of.packages <- c("class", "e1071", "kernlab", "ESKNN", "rknn", "randomForest")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(class) # for kNN
library(e1071) # for tuning kNN, RF
library(kernlab) # for SVM
library(ESKNN) # for ESKNN
library(rknn) # for rknn
library(randomForest) # needed for e1071 tune.randomForest

# ------------------------------------
# This script contains the implementation of the other 7 models:
# - kNN
# - Bagged kNN 
# - Random kNN
# - Multiple feature subsets
# - Random Forest
# - Support Vector Machines
# - ESkNN


# Perform simple kNN
#
# - Xtrain: training input
# - Xtest: test input
# - ytrain: training output
# - withPredictions (optional): also return the probabilities
GetkNNPredictions <- function(Xtrain, Xtest, ytrain, withPredictions = FALSE, k = 'sqrt') {
  if (k == 'opt'){
    tuneK <- e1071::tune.knn(Xtrain, factor(ytrain), k=1:10, tune.control = tune.control(sampling = 'cross', cross=10))
    k <- tuneK$best.parameters
  } else if (k == 'sqrt') {
    k <- round(sqrt(nrow(Xtrain)))
  } else {
    k <- as.numeric(k)
  }
  
  if (withPredictions){
    out <- class::knn(Xtrain, Xtest, ytrain, k = k, prob = TRUE)
    return(list(out,  ifelse(out == 1, attr(out,"prob"), 1 - attr(out,"prob"))))
  } else {
    return(class::knn(Xtrain, Xtest, ytrain, k = k))
  }
}

# Perform Bagged kNN
# Performs simple kNN 1001 times on the sampled(with replacement) input, and takes a majority vote
#
# - Xtrain: training input
# - Xtest: test input
# - ytrain: training output
# - withPredictions (optional): also return the probabilities
GetBkNNPredictions <- function(Xtrain, Xtest, ytrain, withPredictions = FALSE) {
  B <- 1001
  kNNPredictions <- matrix(NA, nrow=B, ncol=nrow(Xtest))
  # Perform 1001x knn on sample of input
  for (b in 1:B){
    indices <- sample(1:nrow(Xtrain), replace = TRUE)
    kNNPredictions[b, ] <- GetkNNPredictions(Xtrain[indices,], Xtest, ytrain[indices])
  }
  
  # take the majority vote for final prediction
  threshold <- 500
  if (withPredictions){
    return(list(
      as.numeric(colSums(kNNPredictions - 1) > threshold),
      colSums(kNNPredictions - 1) / 1001
    ))
  } else {
    return(as.numeric(colSums(kNNPredictions - 1) > threshold))
  }
}

# Determines the size of feature subsets
# A helper function for rknn and esknn
GetNrOfFeatures <- function(P){
  return(ifelse(P < 6, 2, round(P/3)))
}

# Get a random subset of features of specific size
# A helper function for rknn and esknn
GetRandomFeatures <- function(P) {
  return(sample(1:P, GetNrOfFeatures(P)))
}

# Perform rknn and allow for return of raw probabilities
# This is a copy of rknn::rknn, but with the addition of preds.all as return 
# The original rknn function returns only classifications, an adaptation was necessary
# to return the probabilities
#
# - data: training input
# - newdata: test input
# - y: training input
# - k: nearest neighbors
# - r: number of replications to take the majority vote of
# - mtry: feature subset size
rknn <- function (data, newdata, y, k = 1, r = 500, mtry = trunc(sqrt(ncol(data))), 
                  cluster = NULL, seed = NULL) 
{
  knns <- function(data, newdata, y, k, r, mtry) {
    p <- ncol(newdata)
    n <- nrow(newdata)
    selected <- matrix(integer(), nrow = r, ncol = mtry)
    pred.all <- matrix(nrow = n, ncol = r)
    for (j in 1:r) {
      fset <- sample(p, mtry)
      aknn <- knn(train = data[, fset], test = newdata[, 
                                                       fset], cl = y, k = k)
      selected[j, ] <- fset
      pred.all[, j] <- as.integer(aknn)
    }
    return(list(selected = selected, pred.all = pred.all))
  }
  res <- list(call = match.call())
  p <- ncol(newdata)
  n <- nrow(newdata)
  res$k <- k
  res$r <- r
  res$mtry <- mtry
  res$n <- n
  res$p <- p
  if (!is.factor(y)) 
    y <- as.factor(y)
  if (!is.null(cluster)) {
    getLoadedDLLs()
    clusterSetRNGStream(cluster, seed)
    clusterExport(cluster, c("knns", "data", 
                             "newdata", "y", "k", "mtry"), 
                  envir = environment())
    cluster.result <- clusterApply(cluster, splitR(r, length(cluster)), 
                                   function(r) knns(data = data, newdata = newdata, 
                                                    y = y, k = k, r = r, mtry = mtry))
    selected <- matrix(unlist(lapply(cluster.result, function(x) t(x$selected))), 
                       ncol = mtry, byrow = TRUE)
    pred.all <- matrix(unlist(lapply(cluster.result, function(x) x$pred.all)), 
                       nrow = n, byrow = FALSE)
  }
  else {
    set.seed(seed)
    result <- knns(data = data, newdata = newdata, y = y, 
                   k = k, r = r, mtry = mtry)
    selected <- result$selected
    pred.all <- result$pred.all
  }
  pred <- character(n)
  for (i in 1:n) pred[i] <- names(which.max(table(pred.all[i, 
                                                           ])))
  res$pred <- factor(pred, levels = seq_along(levels(y)), labels = levels(y))
  res$features <- if (is.null(colnames(data))) {
    1:p
  }
  else colnames(data)
  res$features.used <- selected
  res$preds.all <- rowSums(pred.all - 1) / r
  class(res) <- "rknn"
  return(res)
}

# Obtain predictions for rknn
#
# - Xtrain: training input
# - Xtest: test input
# - ytrain: training outcome
# - withPredictions (optional): should the raw probabilities be returned as well
GetRkNNPredictions <- function(Xtrain, Xtest, ytrain, withPredictions = FALSE) {
  # Was omitted later, but for calculating the optimal K:
  # tuneK <- e1071::tune.knn(Xtrain, ytrain, k=1:10, tunecontrol = tune.control(sampling = 'cross', cross=10))
  # optimalK <- tuneK$best.parameters
  optimalK <- round(sqrt(nrow(Xtrain)))
  if (withPredictions){
    out <- rknn(Xtrain, Xtest, factor(ytrain), optimalK, 1001, mtry=GetNrOfFeatures(ncol(Xtrain)))
    return(list(
      out$pred,
      out$preds.all
    ))
  } else {
    return(rknn::rknn(Xtrain, Xtest, factor(ytrain), optimalK, 1001, mtry=GetNrOfFeatures(ncol(Xtrain)))$pred)
  }
}

# Obtain the MFS predictions
# Takes a majority vote of 1001 knn models that are fit on a random subset of features
# 
# - Xtrain: training input
# - Xtest: test input
# - ytrain: training outcome
# - withPredictions (optional): should the raw probabilities be returned as well
GetMFSPredictions <- function(Xtrain, Xtest, ytrain, withPredictions = FALSE) {
  B <- 1001
  
  kNNPredictions <- matrix(NA, nrow=B, ncol=nrow(Xtest))
  for (b in 1:B){
    features <- GetRandomFeatures(ncol(Xtrain))
    kNNPredictions[b, ] <- GetkNNPredictions(Xtrain[, features], Xtest[, features], ytrain)
  }
  
  threshold <- 500
  if (withPredictions){
    return(list(
      as.numeric(colSums(kNNPredictions - 1) > threshold),
      colSums(kNNPredictions - 1) / 1001
    ))
  } else {
    return(as.numeric(colSums(kNNPredictions - 1) > threshold))
  }
}

# Get predictions using random forest
#
# - Xtrain: training input
# - Xtest: test input
# - ytrain: training outcome
# - withPredictions (optional): should the raw probabilities be returned as well
GetRFPredictions <- function(Xtrain, Xtest, ytrain, withPredictions = FALSE) {
  # Nodesize: minimum node size. The larger, the more training samples in the smallest node left over.
  # Mtry: nr of random variables to select for split
  # Ntree: number of trees. Default is 500
  # This wrapper function finds the best parameter values
  bestRf <- e1071::best.randomForest(Xtrain, tunecontrol=tune.control(sampling = 'cross', cross=10), y=factor(ytrain))
  
  outProb <- predict(bestRf, Xtest, 'prob')
  out <- predict(bestRf, Xtest)
  
  if(withPredictions) {
    return(list(
      out,
      outProb[,2]
    ))
  } else {
    out
  }
}

# Get predictions using support vector machine
#
# - Xtrain: training input
# - Xtest: test input
# - ytrain: training outcome
# - withPredictions (optional): should the raw probabilities be returned as well
GetSVMPredictions <- function(Xtrain, Xtest, ytrain, withPredictions = FALSE) {
  # does svm classification with some obtained optimal value for kpar
  result <- kernlab::ksvm(x=Xtrain, y=factor(ytrain), kernel='rbfdot', kpar='automatic', cross=10, prob.model=TRUE)
  
  outProb <- predict(result, Xtest, 'prob')
  out <- predict(result, Xtest)
  
  if(withPredictions) {
    return(list(
      out,
      outProb[,2]
    ))
  } else {
    out
  }
}

# Get predictions using esknn
#
# - Xtrain: training input
# - Xtest: test input
# - ytrain: training outcome
# - withPredictions (optional): should the raw probabilities be returned as well
GetESkNNPredictions <- function(Xtrain, Xtest, ytrain, withPredictions = FALSE) {
  # Was omitted later, but for calculating the optimal K:
  # tuneK <- e1071::tune.knn(Xtrain, ytrain, k=1:10, tune.control = tune.control(sampling = 'cross', cross=10))
  # optimalK <- tuneK$best.parameters
  optimalK <- sqrt(nrow(Xtrain))
  
  optModels <- ESKNN::esknnClass(Xtrain, ytrain, k=as.numeric(optimalK), q=40, m=1001, ss=GetNrOfFeatures(ncol(Xtrain)))

  # There is a mistake in the code of ESKNN::Predict.esknnClass at xtest <- xtest. This should be test=xtest, and it breaks the function
  # It is necessary to use an adapted version here:
  # ------ begin of adaptation
  zpred <- list()
  k <- sqrt(nrow(Xtrain))
  mod <- function(x) {
    vt <- table(x)
    as.numeric(names(vt[vt == max(vt)]))
  }
  xtest <- Xtest
  len <- length(optModels$fsfinal)
  if (len%%2 == 0) 
    len <- len - 1
  for (z in 1:len) {
    fit <- caret::knn3Train(optModels$trainfinal[[z]][, names(optModels$trainfinal[[z]]) != 
                                                 "Class"], test = xtest[, optModels$fsfinal[[z]]], 
                     optModels$trainfinal[[z]]$Class, k = k)
    zpred[[z]] <- as.factor(fit[1:length(fit)])
  }
  mclass <- do.call("cbind", zpred)
  # ------ end of adaptation
  
  if (withPredictions) {
    return(list(
      apply(mclass, 1, mod) - 1,
      rowMeans(mclass - 1)
    ))
  } else {
    return(apply(mclass, 1, mod) - 1)
  }
}




