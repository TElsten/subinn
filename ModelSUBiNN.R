# Install missing packages if any:
# list.of.packages <- c("class", "glmnet")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(class) # for knn
library(glmnet) # for Lasso

# The SUBiNN model implementation -----------------------------------

# Base-learning -----------------------------------------------------

# Get all single and pairwise combinations for features
# Which form the definition of the base-learners
#
# P: the number of original dataset input features
GetBaseLearnerMatrix <- function(P) {
  learners <- cbind(rbind(combn(1:P, 1), 0), combn(1:P, 2))
  learners[2, 1:P] <- 1:P
  return(learners)
}

# Get the base-learner cross-validated kNN outputs (predictions)
# 
# y: training output
# X: training input
# baseLearners: base-learner definitions
GenBaseLearnerTrainOutput <-  function(y, X, baseLearners, k){
  N <- nrow(X)
  P <- ncol(X)
  
  # Create random folds for cross-val
  C <-  10
  folds <- sample(rep(1:C, length.out = nrow(X)))
  
  # Init the matrix for the base-learner outputs
  Z <-  matrix(NA, nrow = nrow(X), ncol = choose(P, 2) + P)
  
  # 10-fold xvalidation
  for(c in 1:C){
    idx <- which(folds == c)
    # Fit a kNN for each base-learner and fill Ztrain
    for(p in 1:ncol(baseLearners)){
      # If set, compute optimal k
      if (k == 'opt'){
        tuneK <- e1071::tune.knn(X[-idx, as.character(baseLearners[, p]), drop=FALSE], factor(y[-idx]), k=1:10, tune.control = tune.control(sampling = 'cross', cross=10))
        k <- tuneK$best.parameters
      } else if (k == 'sqrt'){
        k <- round(sqrt(nrow(Xtrain))) # k is pre-set
      } else {
        k <- as.numeric(k)
      }
      out <- class::knn(X[-idx, as.character(baseLearners[, p]), drop=FALSE], X[idx, as.character(baseLearners[, p]), drop=FALSE], y[-idx], k, prob = TRUE)
      Z[idx,p] <- ifelse(out==1, attr(out,"prob"), 1 - attr(out,"prob"))
    }
  }
  return(Z)
}

# Get the base-learner kNN outputs (predictions) for the test set
# Based on the nearest neighbors of Xtest in Xtrain
# 
# Xtrain: training input
# Xtest: test input
# ytrain: training output
# k: nr of nearest neighbors for kNN
# baseLearners: base-learner definitions
# computeFor: for which base-learners to compute the output. This saves computation time as some base-learners have a 0-coefficient
GenBaseLearnerTestOutput <- function(Xtrain, Xtest, ytrain, k, baseLearners, computeFor = c()) {
  P <- ncol(Xtrain)
  
  # Init the matrix for base-learner test outputs
  Z <- matrix(0, nrow(Xtest), (P + P*(P-1)/2))
  
  # Fit a knn for each base-learner and fill Ztest
  for (p in 1:ncol(baseLearners)){
    if (computeFor[p]){
      # If set, compute optimal k
      if (k == 'opt'){
        tuneK <- e1071::tune.knn(Xtrain[, as.character(baseLearners[, p]), drop=FALSE], factor(ytrain), k=1:10, tune.control = tune.control(sampling = 'cross', cross=10))
        k <- tuneK$best.parameters
      } else {
        k <- as.numeric(k)
      }
      out <- class::knn(Xtrain[, as.character(baseLearners[, p]), drop=FALSE], Xtest[, as.character(baseLearners[, p]), drop=FALSE], ytrain, k, prob = TRUE)
      Z[,p] = ifelse(out==1, attr(out,"prob"), 1 - attr(out,"prob"))
    }
  }
  return (Z)
}


# Meta-learner ----------------------------------------------------

# Obtain the Lasso fit based on the optimal (min error) lambda
# Returns the fit
#
# - ytrain: the training set outcome
# - Ztrain: the xvalidated training predictions
GetLassoMetaLearnerFit <- function(ytrain, Ztrain){
  cv.fit <- cv.glmnet(Ztrain, as.numeric(ytrain), alpha=1, standardize=FALSE, family="gaussian", nfolds=10)
  fit <- glmnet(Ztrain, as.numeric(ytrain), alpha=1, standardize=FALSE, family="gaussian",
                lower.limits=0, intercept=FALSE, lambda=cv.fit$lambda.min)
  return(fit)
}

# SUBiNN fitting ----------------------------------------------

# Obtain the test-set predictions (classifications) from SUBiNN
#
# - Xtrain: training set input
# - Xtest: test set input
# - ytrain: training set output
# - withPrediction (optional): also return the original predictions, not just the classifications
GetSUBiNNPredictions <- function(Xtrain, Xtest, ytrain, withPredictions = FALSE, k = 'sqrt') {
  baseLearners <- GetBaseLearnerMatrix(ncol(Xtrain)) # base-learner definitions
  
  # Obtain base-learner output on training set
  Ztrain <- GenBaseLearnerTrainOutput(ytrain, Xtrain, baseLearners, k)
  
  # Get the Lasso fit
  lassoFit <- GetLassoMetaLearnerFit(ytrain, Ztrain)
  coefs <-  matrix(lassoFit$beta, ncol=1)
  
  if (k == 'sqrt'){
    k <- round(sqrt(nrow(Xtrain)))
  }
  # Obtain base-learner output on test set
  # Do not compute all base-learners, but only those that have a non-zero coefficient.
  Ztest <- GenBaseLearnerTestOutput(Xtrain, Xtest, ytrain, k, baseLearners, coefs != 0)
  
  # Obtain test-set predictions
  pred <- predict(lassoFit, newx = Ztest, type="class", s="lambda.min")
  predictions <- as.numeric(pred >= 0.5)
  ifelse(withPredictions,
         return(list(predictions, pred, coefs
         )),
         return(list(predictions, coefs)))
}



