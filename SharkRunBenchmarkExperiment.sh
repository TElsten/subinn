#!/bin/bash
#$ -S /bin/bash
#$ -q all.q
#$ -N sim_bands
#$ -l h_vmem=1G
#$ -cwd
#$ -j Y
#$ -V
#$ -t 1
#$ -o sim_1.out

i=$SGE_TASK_ID
j='haber'

module load R/3.6.1

R CMD BATCH "--args $i $j" ExperimentBenchmarkData.R