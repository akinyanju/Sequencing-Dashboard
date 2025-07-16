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

scriptDir="/gt/research_development/qifa/elion/software/qifa-ops/0.1.0/dashboardCodes"
slurmfileDir="/gt/data/seqdma/GTwebMetricsTables/SeqMetrics"

if ! squeue --format="%.j" | grep -qw "gatherSequencingMetrics"; then
  sbatch "$scriptDir/gatherSequencingMetrics.sh"
fi


##sudo chown svc-gt-delivery .slurmlogSeqMet
##remove slurm error/ouput file older than 24 hours
if [[ -d "$slurmfileDir/.slurmlogSeqMet" ]]; then
  	find $slurmfileDir/.slurmlogSeqMet -type f -mtime +1 -delete 
else
	mkdir $slurmfileDir/.slurmlogSeqMet
fi
sbatch $0
