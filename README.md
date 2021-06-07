# SUBiNN: A Stacked Uni- and Bivariate Nearest Neighbors classifier

This R code contains the SUBiNN model (`ModelSUBiNN.R`) and experiments to compare its performance to 7 other classifiers (`ModelOthers.R`). 
The experiments were run on both simulated and benchmark datasets. All experiments are written to run on a cluster computer and therefore produce many small files of result output. 
Scripts to process and combine the results are also included.

## Data
`DataSets.R` is used to retrieve datasets from packages and simulate data for experiments

## Experiments

### Pilot Study K
Was used to determine SUBiNN's optimal parameter K.

### Simulation studies
Simulated data with a varying number of non-informative features and a varying covariance/correlation matrix is used to measure SUBiNN's robustness in feature selection.

### Benchmark
22 benchmark datasets are used to measure the prediction performance on life-like data.

## Results 
All `ProcessResults<experiment>.R` scripts were used to process the raw experiment output.
