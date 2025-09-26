#!/bin/bash

#SBATCH --job-name=mv51_highIter
#SBATCH --nodes=1
#SBATCH --mem=128G
#SBATCH --ntasks=4
#SBATCH --partition=cpu
#SBATCH --account=swbsc

#load conda environment where streamMetabolizer is installed
source activate /home/ibishop/miniforge3/envs/sM

Rscript mv58_onechunk.r
