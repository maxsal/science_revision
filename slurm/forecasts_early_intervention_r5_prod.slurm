#!/bin/bash

#SBATCH --mail-type=END
#SBATCH --time=0-20:00:00
#SBATCH --job-name=wave2_early_mh_prod
#SBATCH --mem-per-cpu=40G
#SBATCH --array=1-10
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

export http_proxy=http://proxy.arc-ts.umich.edu:3128/
export FTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export ftp_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTPS_PROXY=http://proxy.arc-ts.umich.edu:3128/
export https_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export production=TRUE
export R_0=5

module load Rtidyverse/4.1.0
module load jags

Rscript --vanilla ~/projects/science_revision/r/models/early_intervention/forecasts_smooth_early_mh.R
