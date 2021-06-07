#!/bin/bash
#$ -S /bin/bash
#$ -q all.q
#$ -N sim_1
#$ -l h_vmem=1G
#$ -cwd
#$ -j Y
#$ -V
#$ -t 1-100
#$ -o ExperimentPilotStudyK.out

i=$SGE_TASK_ID
j=5
k='diabe' 

module load R/3.6.1

R CMD BATCH "--args $i $j $k" ExperimentPilotStudyK.R