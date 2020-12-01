#!/bin/bash -l
#SBATCH --array=1-1143
#SBATCH --job-name=ball
#SBATCH --partition=long

Rscript fit.R
