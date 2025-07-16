#!/bin/bash
#SBATCH -p gt_compute
#SBATCH --cpus-per-task=1
#SBATCH -t 00:02:00
#SBATCH --mem=1G
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=raman.lawal@jax.org
#SBATCH --job-name=crawlerQCmetricsScript
#SBATCH --begin=now+10minutes
#SBATCH --output=/gt/data/seqdma/GTwebMetricsTables/.slurmlog/%x.%N.o%j.log

scriptDir="/gt/research_development/qifa/elion/software/qifa-ops/0.1.0/dashboardCodes"
slurmLogDir="/gt/data/seqdma/GTwebMetricsTables/.slurmlog"

# Submit duckDB if missing
if ! squeue --format="%.j" | grep -qw "duckDBgatherwebQCmetrics"; then
  sbatch "$scriptDir/duckDBgatherwebQCmetrics.sh"
fi

#submit plasmid
#if ! squeue --format="%.j" | grep -qw "plasmidQC"; then
#  sbatch "/gt/data/seqdma/plasmid_epi2me/plasmidQC.sh"
#fi

# Remove SLURM logs older than 24 hours
if [[ -d "$slurmLogDir" ]]; then
  find "$slurmLogDir" -type f -mtime +1 -delete
else
  mkdir -p "$slurmLogDir"
fi

# Resubmit this wrapper to run again in 10 minutes
sbatch "$0"

