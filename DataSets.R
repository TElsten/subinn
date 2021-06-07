# Install missing packages if any
# list.of.packages <- c("MASS", "ipred", "mlbench", "ElemStatLearn")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(MASS) # for mvrnorm
library(ipred) # for Dystrophy and Glaucoma
library(mlbench) # for Diabetes and Sonar
library(ElemStatLearn) # for SAHeart

# Obtain the simulation data as defined for the first two experiments 
# This function replicates the data generation from Gul et al. (2016)
# Simulated data is of differing covariance structure
# 
# - nrOFRandomFeatures: how many noise features to add to input
# - N: total sample size
# - w: multiplication factor for covar matrix, increase means higher covariance
#
GetCovarianceData <- function(nrOfRandomFeatures, N = 100, w = 1) {
  d <- 20 # Number of informatives
  
  # Generate the covar matrix
  psi <- matrix(NA, nrow=d, ncol=d) # Covar matrix
  for (i in 1:d){
    for (j in 1:d){
      psi[i, j] <- w * ifelse(i == j, 1, (0.5)^(abs(i - j)))
    }
  }
  
  # Class 1 = N(2, w * psi)
  dataClass1 <- mvrnorm(N/2, rep(2, 20), psi)
  # Class 2 = N(1, 1)
  dataClass2 <- mvrnorm(N/2, rep(1, 20), diag(rep(1, 20)))
  
  y <- c(rep(1, N/2), rep(2, N/2))
  
  # Add non-informative features to input
  nonInformatives <- matrix(rnorm(N * nrOfRandomFeatures, 0, 1), ncol=nrOfRandomFeatures, nrow=N)
  informatives <- rbind(dataClass1, dataClass2)
  X <- cbind(informatives, nonInformatives)
  
  # Shuffle the columns
  featureNames <- c(1:ncol(X))
  shuffle <- sample(1:ncol(X))
  data <- cbind(X[, shuffle], y)
  
  # Re-add the featurenames to be able to identify them after shuffling
  colnames(data) <- c(featureNames[shuffle], 'y')
  return(data)
}


# Obtain the simulation data as defined for the second two experiments 
# An adaptation of the simulation from Gul et al. (2016)
# With the difference that this data has a differing correlation structure
# 
# - nrOFRandomFeatures: how many noise features to add to input
# - N: total sample size
# - w: multiplication factor for covar matrix, increase means lower correlation
#
GetCorrelatedData <- function(nrRandomFeatures, N = 100, w = 1) {
  d <- 20
  psi <- matrix(NA, nrow=d, ncol=d)
  
  # Generate the covar matrix
  for (i in 1:d){
    for (j in 1:d){ 
      psi[i, j] <- (0.99^w)^(abs(i - j))
    }
  }
  
  # Class 1 = N(2, w * psi)
  dataClass1 <- mvrnorm(N/2, rep(2, 20), psi)
  # Class 2 = N(1, 1)
  dataClass2 <- mvrnorm(N/2, rep(1, 20), diag(rep(1, 20)))
  
  y <- c(rep(1, N/2), rep(2, N/2))
  
  # Add non-informative features
  nonInformatives <- matrix(rnorm(N * nrRandomFeatures, 0, 1), ncol=nrRandomFeatures, nrow=N)
  informatives <- rbind(dataClass1, dataClass2)
  X <- cbind(informatives, nonInformatives)
  
  #shuffle the columns
  featureNames <- c(1:ncol(X))
  shuffle <- sample(1:ncol(X))

  # Re-add the featurenames to be able to identify them after shuffling
  data <- cbind(X[, shuffle], y)
  colnames(data) <- c(featureNames[shuffle], 'y')
  return(data)
}

# Obtain one of the 22 benchmark datasets, either standardized or original
# Some originate from packages, others from .data files in the 'data' folder one level up
# Only complete cases are used. 
# 
# - name: abbreviated name op dataset
# - standardized: z-scaled or not
#
GetBenchmarkData <- function(name, standardized = TRUE) {
  X <- NULL
  y <- NULL
  switch(name,
         haber = {
           # https://archive.ics.uci.edu/ml/datasets/Haberman%27s+Survival
           haberman <- read.csv('./data/haberman.data', header=FALSE, stringsAsFactors = FALSE, na.strings="?")
           X <- haberman[, -4]
           y <- factor(haberman[, 4])
         }, 
         dystr = {
           data("dystrophy")
           completes <- complete.cases(dystrophy)
           # Features 1 and 2 are IDs
           X <- dystrophy[completes, c(3:9)]
           y <- factor(dystrophy$Class[completes])
         },
         mammo = {
           # https://archive.ics.uci.edu/ml/datasets/Mammographic+Mass
           mammography <- read.csv('./data/mammographic_masses.data', header=FALSE, stringsAsFactors = FALSE, na.strings="?")
           completes <- complete.cases(mammography)
           # Feature 1 is non-predictive according to source
           X <- mammography[completes, -c(1,6)]
           
           # Features 3 and 4 are nominal, make indicator variables
           X <- cbind(X, V3.2 = as.numeric(X$V3 == 2))
           X <- cbind(X, V3.3 = as.numeric(X$V3 == 3))
           X <- cbind(X, V3.4 = as.numeric(X$V3 == 4))
           X$V3 <- as.numeric(X$V3 == 1)
           X <- cbind(X, V4.2 = as.numeric(X$V4 == 2))
           X <- cbind(X, V4.3 = as.numeric(X$V4 == 3))
           X <- cbind(X, V4.4 = as.numeric(X$V4 == 4))
           X <- cbind(X, V4.5 = as.numeric(X$V4 == 5))
           X$V4 <- as.numeric(X$V4 == 1)
           
           y <- factor(mammography[completes, 6])
         },
         trans = {
           # https://archive.ics.uci.edu/ml/datasets/Blood+Transfusion+Service+Center
           transfusion <- read.csv('./data/transfusion.data', header=TRUE, stringsAsFactors = FALSE, na.strings="?")
           X <- transfusion[, -5]
           y <- factor(transfusion[, 5])
         },
         phone = {
           # https://web.stanford.edu/~hastie/ElemStatLearn/data.html
           # NOTE: Use max of 1000
           phoneme <- read.table("./data/phoneme-sampled.data", header=FALSE)
           # We use a pre-generated sample of the original dataset.
           # If no sampled dataset is present, use this to generate a new one
           # phoneme <- read.csv('../Data/phoneme.csv', header=TRUE, stringsAsFactors = FALSE) 
           # completes <- complete.cases(phoneme)
           # which(completes)
           # set.seed(1)
           # idx <- sample(1:sum(completes))[1:1000]
           # phoneme <- phoneme[idx, ]
           # write.table(phoneme, file="../Data/phoneme-sampled.data", col.names=FALSE, row.names=FALSE)
           X <- phoneme[, -6]
           y <- factor(phoneme[, 6])
         },
         bupa = {
           # https://archive.ics.uci.edu/ml/datasets/Liver+Disorders
           bupa <- read.csv('./data/bupa.data', header=FALSE, stringsAsFactors = FALSE, na.strings="?")
           # See the source about the use of dependent variables. Features 7 should not be used as dependent or predictive.
           X <- bupa[, -c(6,7)]
           y <- factor(bupa[, 6] > 5)
         },
         appen = {
           # https://sci2s.ugr.es/keel/dataset.php?cod=183
           appendicitis <- read.csv('./data/appendicitis.dat', header=FALSE, stringsAsFactors = FALSE, skip = 12)
           X <- appendicitis[, -8]
           y <- factor(appendicitis[, 8])
         },
         diabe = {
           library(mlbench)
           data("PimaIndiansDiabetes", package="mlbench")
           X <- PimaIndiansDiabetes[,-9]
           y <- factor(PimaIndiansDiabetes$diabetes)
         },
         biops = {
           # https://archive.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+(original)
           biopsy <- read.csv('./data/breast-cancer-wisconsin.data', header=FALSE, stringsAsFactors = FALSE, na.strings="?")
           completes <- complete.cases(biopsy)
           # Feature 1 is an ID
           X <- biopsy[completes, -c(1, 11)]
           y <- factor(biopsy[completes, 11])
         },
         heart = {
           # https://rdrr.io/cran/ElemStatLearn/man/SAheart.html
           SAheart <- ElemStatLearn::SAheart
           X <- SAheart[, -10]
           X$famhist <- as.numeric(X$famhist)
           y <- factor(SAheart[, 10])
         },
         india = {
           # https://archive.ics.uci.edu/ml/datasets/ILPD+%28Indian+Liver+Patient+Dataset%29
           indianLiver <- read.csv('./data/Indian Liver Patient Dataset (ILPD).csv', header=FALSE, na.strings = "?")
           completes <- complete.cases(indianLiver)
           X <- indianLiver[completes, -11]
           X$V2 <- as.numeric(X$V2)
           y <- factor(indianLiver[completes, 11])
         },
         solar = {
           # http://archive.ics.uci.edu/ml/datasets/solar+flare
           # Note: we predict the last class, severe flare. It is unclear what Gul et al. did.
           solar <- read.table('./data/flare.data2', sep=" ", header=FALSE, skip=1, na.strings="?")
           # Features 10,12,13 are also possible dependent variables
           X <- solar[, -c(10, 11, 12, 13)]
           X$V1 <- as.numeric(X$V1)
           X$V2 <- as.numeric(X$V2)
           X$V3 <- as.numeric(X$V3)
           
           # Make indicator variables for the nominal features
           X <- cbind(X, V1.2 = as.numeric(X$V1 == 2))
           X <- cbind(X, V1.3 = as.numeric(X$V1 == 3))
           X <- cbind(X, V1.4 = as.numeric(X$V1 == 4))
           X <- cbind(X, V1.5 = as.numeric(X$V1 == 5))
           X$V1 <- as.numeric(X$V1 == 1)
           X <- cbind(X, V2.2 = as.numeric(X$V2 == 2))
           X <- cbind(X, V2.3 = as.numeric(X$V2 == 3))
           X <- cbind(X, V2.4 = as.numeric(X$V2 == 4))
           X <- cbind(X, V2.5 = as.numeric(X$V2 == 5))
           X$V2 <- as.numeric(X$V2 == 1)
           X <- cbind(X, V3.2 = as.numeric(X$V3 == 2))
           X <- cbind(X, V3.3 = as.numeric(X$V3 == 3))
           X <- cbind(X, V3.4 = as.numeric(X$V3 == 4))
           X$V3 <- as.numeric(X$V3 == 1)
           
           y <- factor(solar[, 11] > 0)
           levels(y) <- c(0, 1)
         },
         credi = {
           # https://archive.ics.uci.edu/ml/datasets/Credit+Approval
           credit <- read.csv('./data/crx.data', header = FALSE, na.strings = "?")
           completes <- complete.cases(credit)
           X <- credit[completes, -16]
           y <- factor(credit[completes, 16])
           
           # Features 1,4,5,6,7,9,10,11,12,13 are categorical. turn them numeric.
           X$V1 <- as.numeric(X$V1)
           X$V4 <- as.numeric(X$V4)
           X$V5 <- as.numeric(X$V5)
           X$V6 <- as.numeric(X$V6)
           X$V7 <- as.numeric(X$V7)
           X$V9 <- as.numeric(X$V9)
           X$V10 <- as.numeric(X$V10)
           X$V11 <- as.numeric(X$V11)
           X$V12 <- as.numeric(X$V12)
           X$V13 <- as.numeric(X$V13)
           
           # Make indicator variables for the nominal features
           X <- cbind(X, V4.2 = as.numeric(X$V4 == 2))
           X <- cbind(X, V4.3 = as.numeric(X$V4  == 3))
           X$V4 <- as.numeric(X$V4 == 1)
           X <- cbind(X, V5.2 = as.numeric(X$V5 == 2))
           X <- cbind(X, V5.3 = as.numeric(X$V5 == 3))
           X$V5 <- as.numeric(X$V5 == 1)
           X <- cbind(X, V6.2 = as.numeric(X$V6 == 2))
           X <- cbind(X, V6.3 = as.numeric(X$V6 == 3))
           X <- cbind(X, V6.4 = as.numeric(X$V6 == 4))
           X <- cbind(X, V6.5 = as.numeric(X$V6 == 5))           
           X <- cbind(X, V6.6 = as.numeric(X$V6 == 6))
           X <- cbind(X, V6.7 = as.numeric(X$V6 == 7))           
           X <- cbind(X, V6.8 = as.numeric(X$V6 == 8))
           X <- cbind(X, V6.9 = as.numeric(X$V6 == 9))          
           X <- cbind(X, V6.10 = as.numeric(X$V6 == 10))
           X <- cbind(X, V6.11 = as.numeric(X$V6 == 11))           
           X <- cbind(X, V6.12 = as.numeric(X$V6 == 12))
           X <- cbind(X, V6.13 = as.numeric(X$V6 == 13))
           X$V6 <- as.numeric(X$V6 == 1)
           X <- cbind(X, V7.2 = as.numeric(X$V7 == 2))
           X <- cbind(X, V7.3 = as.numeric(X$V7 == 3))
           X <- cbind(X, V7.4 = as.numeric(X$V7 == 4))
           X <- cbind(X, V7.5 = as.numeric(X$V7 == 5))           
           X <- cbind(X, V7.6 = as.numeric(X$V7 == 6))
           X <- cbind(X, V7.7 = as.numeric(X$V7 == 7))           
           X <- cbind(X, V7.8 = as.numeric(X$V7 == 8))
           X$V7 <- as.numeric(X$V7 == 1)
           X <- cbind(X, V13.2 = as.numeric(X$V13 == 2))
           X <- cbind(X, V13.3 = as.numeric(X$V13 == 3))
           X$V13 <- as.numeric(X$V13 == 1)
         },
         house = {
           # https://archive.ics.uci.edu/ml/datasets/congressional+voting+records
           house <- read.csv('./data/house-votes-84.data', header=FALSE, na.strings = "?")
           completes <- complete.cases(house)
           X <- house[completes, -17]
           y <- factor(house[completes, 17])
           
           # This dataset has many two-level features. These are turned numeric.
           X <- apply(X, 2, function(col) {as.numeric(factor(col))})
         },
         bands = {
           # https://archive.ics.uci.edu/ml/datasets/Cylinder+Bands
           # The number of nominal features is too large for this dataset. 
           # We remove all nominal features of more than 2 levels.
           bands <- read.csv('./data/bands.data', header=FALSE, na.strings="?")
           removeThese <- c(1,2,3,4,6,8,9,11,13,15,16,17,18,19,20)
           selectedFeatures <- bands[,-removeThese]
           completes <- complete.cases(selectedFeatures)
           y <- factor(bands[completes, 40])
           X <- bands[completes, -c(removeThese, 40)]
           X <- apply(X, 2, function(col) {as.numeric(as.factor(col))})
         },
         hepat = {
           # https://archive.ics.uci.edu/ml/datasets/Hepatitis
           hepatitis <- read.csv('./data/hepatitis.data', header=FALSE, na.strings = "?")
           completes <- complete.cases(hepatitis)
           X <- hepatitis[completes, -1]
           y <- factor(hepatitis[completes, 1])
         },
         twono = {
           # https://www.cs.toronto.edu/~delve/data/twonorm/desc.html
           #NOTE: Use max of 1000
           twonorm <- read.table('./data/twonorm-sampled.data', header=FALSE)
           # The original saving, if no sampled dataset is present already
           # Use the above sampled data afterwards
           # twonorm <- read.table('../Data/twonorm.data', header=FALSE)
           # set.seed(1)
           # idx <- sample(1:nrow(twonorm))[1:1000]
           # twonorm <- twonorm[idx, ]
           # write.table(twonorm, file=".,/data/twonorm-sampled.data", col.names=FALSE, row.names=FALSE)
           X <- twonorm[, -21]
           y <- factor(twonorm[, 21])
         },
         germa = {
           # https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)
           # NOTE: instead of using the numeric version, they (Gul et al) numerify the original. We use the numeric version.
           german <- read.table('./data/german.data-numeric', header=FALSE)
           X <- german[, -25]
           y <- factor(german[, 25])
         },
         wpbc = {
           # https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Prognostic)
           wpbc <- read.csv('./data/wpbc.data', header=FALSE, na.strings = "?")
           selectedFeatures <- wpbc[, -1]
           completes <- complete.cases(selectedFeatures)
           X <- wpbc[completes, -c(1, 2)]
           y <- factor(wpbc[completes, 2])
         },
         sonar = {
           data("Sonar")
           X <- Sonar[,-61]
           y <- factor(Sonar$Class)
         },
         glauc = {
           data("GlaucomaMVF")
           completes <- complete.cases(GlaucomaMVF)
           y <- factor(GlaucomaMVF$Class[completes])
           X <- GlaucomaMVF[completes, -ncol(GlaucomaMVF)]
         },
         musk = {
           # https://archive.ics.uci.edu/ml/datasets/Musk+%28Version+1%29
           musk <- read.csv('./data/clean1.data', header=FALSE, na.strings = "?")
           X <- musk[, -c(1, 2, 169)]
           y <- factor(musk[, 169])
         }, {
           # Name is not a correct abbreviation of dataset
           return(warning("Dataset not found, try a different name"))
         })
  if (standardized){
    X <- scale(X)
  }
  X <- as.matrix(X)
  colnames(X) <- 1:ncol(X)
  levels(y) <- c(0, 1)
  y <- as.numeric(y) - 1
  return(list(X, y))
}