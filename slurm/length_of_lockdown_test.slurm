#!/bin/bash

#SBATCH --mail-type=END
#SBATCH --time=0-20:00
#SBATCH --job-name=wave2_lol_test
#SBATCH --mem-per-cpu=8G
#SBATCH --array=1-9
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

export http_proxy=http://proxy.arc-ts.umich.edu:3128/
export FTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export ftp_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTPS_PROXY=http://proxy.arc-ts.umich.edu:3128/
export https_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export production=FALSE

module load R/4.1.0
module load jags

Rscript --vanilla ~/projects/science_revision/r/models/lol/length_of_lockdown.R
