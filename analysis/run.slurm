#!/bin/bash -l
#SBATCH --array=1-1143
#SBATCH --job-name=ball

Rscript fit.R
