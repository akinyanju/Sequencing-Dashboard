#!/bin/bash

#SBATCH -p gt_compute
#SBATCH --cpus-per-task=1
#SBATCH -t 00:2:00
#SBATCH --mem=1G
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=raman.lawal@jax.org
#SBATCH --job-name=crawlerSeqMetrics
#SBATCH --begin=now+1minutes
#SBATCH --output=/gt/data/seqdma/GTwebMetricsTables/SeqMetrics/.slurmlogSeqMet/%x.%N.o%j.log

#####################################
EMAILS=("Raman.Lawal@jax.org" "Harianto.Tjong@jax.org" "Gabriel.Rech@jax.org" "dave.john.harrison@jax.org")
scriptDir="/gt/research_development/qifa/elion/software/qifa-ops/0.1.0/dashboardCodes"
slurmfileDir="/gt/data/seqdma/GTwebMetricsTables/SeqMetrics"
#####################################
if ! squeue --format="%.j" | grep -qw "gatherSequencingMetrics"; then
  sbatch "$scriptDir/gatherSequencingMetrics.sh"
fi
#####################################
##sudo chown svc-gt-delivery .slurmlogSeqMet
##remove slurm error/ouput file older than 24 hours
if [[ -d "$slurmfileDir/.slurmlogSeqMet" ]]; then
  	find $slurmfileDir/.slurmlogSeqMet -type f -mtime +1 -delete 
else
	mkdir $slurmfileDir/.slurmlogSeqMet
fi
#####################################
# Check if crawlerQCmetricsScript is running or queued
if ! squeue --format="%.j" | grep -qw "crawlerQCmetricsScript"; then
  mailx -r "GTdrylab@jax.org" \
    -s "Missing SLURM job: crawlerQCmetricsScript" \
    "${EMAILS[@]}" <<EOF
ALERT: 'crawlerQCmetricsScript' job is NOT currently running or queued (as of $(date)).

This may have occurred due to an unexpected failure, cancellation, or other system interruption.

To resume operation, please manually resubmit the job using the following commands:
To manually restart this job, follow these steps **exactly**:

  1. Switch to the service user account:
     **sudo su - svc-gt-delivery**

  2. Navigate to the metrics script directory:
     cd /gt/research_development/qifa/elion/software/qifa-ops/0.1.0/dashboardCodes

  3. Submit the crawler QC script:
     sbatch crawlerQCmetricsScript.sh

**NOTE:** If 'crawlerQCmetricsScript' is not resubmitted promptly, the associated duckDB web QC metrics will NOT be collected and updated.

Please take action as soon as possible to restore metrics processing.

- The Automation System
EOF
fi
#####################################
sbatch $0
