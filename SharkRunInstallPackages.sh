#!/bin/bash
#$ -S /bin/bash
#$ -q all.q
#$ -N installpackages
#$ -cwd
#$ -j Y
#$ -V

module load R/3.6.1

R CMD BATCH SharkInstallPackages.R