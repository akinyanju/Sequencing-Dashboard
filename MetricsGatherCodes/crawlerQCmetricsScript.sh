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

#####################################
EMAILS=("Raman.Lawal@jax.org" "Harianto.Tjong@jax.org" "Gabriel.Rech@jax.org" "dave.john.harrison@jax.org")
scriptDir="/gt/research_development/qifa/elion/software/qifa-ops/0.1.0/dashboardCodes"
slurmLogDir="/gt/data/seqdma/GTwebMetricsTables/.slurmlog"
#####################################
# Submit duckDB if missing
if ! squeue --format="%.j" | grep -qw "duckDBgatherwebQCmetrics"; then
  sbatch "$scriptDir/duckDBgatherwebQCmetrics.sh"
fi
#####################################
#submit plasmid
#if ! squeue --format="%.j" | grep -qw "plasmidQC"; then
#  sbatch "/gt/data/seqdma/plasmid_epi2me/plasmidQC.sh"
#fi
#####################################
# Remove SLURM logs older than 24 hours
if [[ -d "$slurmLogDir" ]]; then
  find "$slurmLogDir" -type f -mtime +1 -delete
else
  mkdir -p "$slurmLogDir"
fi
#####################################
# Check if crawlerSeqMetrics is running or queued
if ! squeue --format="%.j" | grep -qw "crawlerSeqMetrics"; then
mailx -r "GTdrylab@jax.org" \
  -s "Missing SLURM job: crawlerSeqMetrics" \
  "${EMAILS[@]}" <<EOF
ALERT: 'crawlerSeqMetrics' job is NOT currently running or queued (as of $(date)).

This may have occurred due to an unexpected failure, cancellation, or system issue.

To manually restart this job, follow these steps **exactly**:

  1. Switch to the service user account:
     **sudo su - svc-gt-delivery**

  2. Navigate to the metrics script directory:
     cd /gt/research_development/qifa/elion/software/qifa-ops/0.1.0/dashboardCodes

  3. Submit the crawler script:
     sbatch crawlerSeqMetrics.sh

**IMPORTANT**: If 'crawlerSeqMetrics' is not resubmitted, sequencing run-level metrics will NOT be collected before being purged from the delivery folder, potentially leading to incomplete or outdated dashboard reports.

Please take action promptly.

- The Automation System
EOF
fi

#####################################

# Resubmit this wrapper to run again in 10 minutes
sbatch "$0"

