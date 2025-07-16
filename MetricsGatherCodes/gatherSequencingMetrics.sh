#!/usr/bin/env bash

#SBATCH -p gt_compute
#SBATCH --cpus-per-task=1
#SBATCH -t 48:00:00
#SBATCH --mem=20G
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=raman.lawal@jax.org
#SBATCH --job-name=gatherSequencingMetrics
#SBATCH --output=/gt/data/seqdma/GTwebMetricsTables/SeqMetrics/.slurmlogSeqMet/%x.%N.o%j.log

username="lawalr" #change this to be able to push to ctgenometech03 from svc-gt-delivery account. Make sure you are added to ctgenomtech03 server
Email="akinyanju.lawal@jax.org" #Will issue email incase critical error is found. Not SBATCH --mail-type=FAIL related 
OUT="/gt/data/seqdma/GTwebMetricsTables/SeqMetrics"
export SETJSONFILE=".settings.json"
export SequencingMetrics="SequencingMetrics.csv"

set -E
set -o functrace
function failed_command {
    local retval=$?
    local line=${last_lineno:-$1}
    echo "Offending command @ line $line: $BASH_COMMAND" > $OUT/.logfile
    #echo "Trace: " "$@"
    #exit $retval
}
if (( ${BASH_VERSION%%.*} <= 3 )) || [[ ${BASH_VERSION%.*} = 4.0 ]]; then
        trap '[[ $FUNCNAME = handle_error ]] || { last_lineno=$real_lineno; real_lineno=$LINENO; }' DEBUG
fi

exec_script() {
	funcSeqMet
}
#######################################
#collect metrics one at a time on each platform
#######################################
function funcSeqMet {
    for Platform in Illumina ONT PacBio; do
        if [[ "$Platform" == "Illumina" ]]; then
            qcdir="/gt/data/seqdma/qifa"
			export RunInfo="RunInfo.xml"
            collect_qcdir
            if ! isNewProject; then
                echo "[INFO] No new Illumina projects to process. Skipping..."
                continue
            fi
			backupSeqMetrics
            collect_metrics
			fillEmptyColnRmDup
            header
			#pushToServer
        elif [[ "$Platform" == "ONT" ]]; then
            qcdir="/gt/data/seqdma/qifa-ont"
            collect_qcdir
            if ! isNewProject; then
            	echo "[INFO] No new ONT projects to process. Skipping..."
                continue
            fi
			backupSeqMetrics
            collect_metrics
			fillEmptyColnRmDup
            header
			#pushToServer
		elif [[ "$Platform" == "PacBio" ]]; then
            qcdir="/gt/data/seqdma/qifa-pb"
            collect_qcdir
            if ! isNewProject; then
                echo "[INFO] No new PacBio projects to process. Skipping..."
                continue
            fi
			backupSeqMetrics
            collect_metrics
			fillEmptyColnRmDup
            header
			#pushToServer
        fi
    done
}
#####################################
#error out and issue the error email
####################################
throw_error() {
   exit_code=$?
   if [[ ${exit_code} -ne 0 ]]; then
      	ErrMsg1=$(cat $OUT/.logfile)
		echo -e "Failure to Sequencing Metrics collection encountered.\n\nReview error file @ $OUT/.logfile or faulty command line below: 
		\n $ErrMsg1" | mail -s "Failure: Sequence Metrics Collection" $Email 
		> $OUT/.logfile #clear error file
	fi
}

#######################################
#Back up prior metrics, remove after 10 days
#######################################

function backupSeqMetrics {
    stampTime=$(date +"%Y-%m-%d")
    backup_file="$OUT/.SeqMetricsbackup/$SequencingMetrics.$stampTime.csv"
    current_file="$OUT/$SequencingMetrics"

    # Ensure backup dir exists
    [[ ! -d "$OUT/.SeqMetricsbackup" ]] && mkdir -p "$OUT/.SeqMetricsbackup"

    # Only copy if current file is different from last backup
    if [[ -f "$current_file" ]]; then
        if [[ ! -f "$backup_file" ]] || ! cmp -s "$current_file" "$backup_file"; then
            cp "$current_file" "$backup_file"
        else
            echo "[INFO] No changes to SequencingMetrics.csv — skipping backup for $stampTime"
        fi
    fi

    # Clean up old backups (10+ days)
    find "$OUT/.SeqMetricsbackup" -type f -mtime +10 -delete

    # Also clean up old slurm logs
    if [[ -d "$OUT/.slurmlogSeqMet" ]]; then
        find "$OUT/.slurmlogSeqMet" -type f -mtime +1 -delete 
    else
        mkdir -p "$OUT/.slurmlogSeqMet"
    fi
}

#######################################
#Save New Metrics to file
#######################################
function SaveMetricsFile {
	if [[ -f "$OUT/$SequencingMetrics" ]] ; then
		if grep -w $releaseMonth $OUT/$SequencingMetrics | grep $releaseYear | grep $RunFolder | grep $Platform | grep $Reads | grep $Bases >/dev/null 2>&1; then
			echo "Metrics previously added. Skipping duplicate"
		else
			echo $releaseMonth,$releaseYear,$Project,$Site,$InstrumentID,$RunFolder,$groupFolder,$Application,$releasePath,$Platform,$Reads,$Bases,$Bytes,$DeliveryDirectory,$SampleSize,$PolymeraseRLbp >> $OUT/$SequencingMetrics
		fi
	else
		echo $releaseMonth,$releaseYear,$Project,$Site,$InstrumentID,$RunFolder,$groupFolder,$Application,$releasePath,$Platform,$Reads,$Bases,$Bytes,$DeliveryDirectory,$SampleSize,$PolymeraseRLbp >> $OUT/$SequencingMetrics
	fi	
}

#######################################
#QC diretory information
#######################################
function collect_qcdir {
	if [[ ! -f "$OUT/.SequencingMetricsQCdir.$Platform.txt" ]]; then
		find  $qcdir -mindepth 2 -maxdepth 2 -type d >> $OUT/.SequencingMetricsQCdir.$Platform.txt
		awk '!seen[$0]++' $OUT/.SequencingMetricsQCdir.$Platform.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt
		mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt $OUT/.SequencingMetricsQCdir.$Platform.txt
		ProjDirs=`cat $OUT/.SequencingMetricsQCdir.$Platform.txt`
		ProjTotal=`echo -en "$ProjDirs\n" | wc -l`
	else
		#This loop perform multiple checks to ensure metrics previoulsy not collected is collected
		if [[ -f "$OUT/.SequencingMetricsQCdir.$Platform.txt" ]] ; then
			paste $OUT/.SequencingMetricsQCdir.$Platform.txt | while read Prior_ProjDir ; do
				#if path exist but QC not yet released, discard that path from being updated to file
				if ! grep -wE "releaseTimestamp|releasePath|releaseDate" "$Prior_ProjDir"/$SETJSONFILE >/dev/null 2>&1; then
					grep -v "$Prior_ProjDir" $OUT/.SequencingMetricsQCdir.$Platform.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt
					mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt $OUT/.SequencingMetricsQCdir.$Platform.txt
				fi
				#check if the path is archived, discard from being updated to file
				if [[ ! -e "$Prior_ProjDir" ]] ; then
        			grep -v "$Prior_ProjDir" $OUT/.SequencingMetricsQCdir.$Platform.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt
					mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt $OUT/.SequencingMetricsQCdir.$Platform.txt
    			else
					if [[ -f "$OUT/$SequencingMetrics" ]] ; then
						#Recheck if metric was not previously collected so that it can be collected in next round of gather
						Project=$(grep -w project $Prior_ProjDir/$SETJSONFILE | awk '{print $2}' | grep -o '".*"' | sed 's/"//g')
						RunFolderDir="${Prior_ProjDir%/*}"
						RunFolder=${RunFolderDir##*/}
    					if ! cat $OUT/$SequencingMetrics | grep  $Project | grep $RunFolder >/dev/null 2>&1; then
        					grep -v "$Prior_ProjDir" $OUT/.SequencingMetricsQCdir.$Platform.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt
							mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt $OUT/.SequencingMetricsQCdir.$Platform.txt
    					fi
					fi
				fi
			done
		fi
		cat $OUT/.SequencingMetricsQCdir.$Platform.txt >> $OUT/.SequencingMetricsQCdir.$Platform.update.txt
        awk '!seen[$0]++' $OUT/.SequencingMetricsQCdir.$Platform.update.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.update.txt
        mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.update.txt $OUT/.SequencingMetricsQCdir.$Platform.update.txt
        find  $qcdir -mindepth 2 -maxdepth 2 -type d >> $OUT/.SequencingMetricsQCdir.$Platform.txt
        #extract only the new QC folder
        grep -vf $OUT/.SequencingMetricsQCdir.$Platform.update.txt $OUT/.SequencingMetricsQCdir.$Platform.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt
        mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt $OUT/.SequencingMetricsQCdir.$Platform.txt
        DirList=`cat $OUT/.SequencingMetricsQCdir.$Platform.txt`
	    ProjDirs=`cat $OUT/.SequencingMetricsQCdir.$Platform.txt`
	    ProjTotal=`echo -en "$ProjDirs\n" | wc -l`
	fi
}

function isNewProject {
  ProjTotal=$(printf "%s\n" "$ProjDirs" | sed '/^\s*$/d' | wc -l)
  if [[ "$ProjTotal" -eq 0 ]]; then
      return 1
  else
      return 0
  fi
}


function collect_metrics {
	for n in $(seq 1 $ProjTotal); do
	ProjDir=`echo -en "$ProjDirs\n" | head -n $n | tail -n1`
	#Keep only directory of delivered projects and skip collecting metrics of such project. 
	#In the future, program will rescan that directory if project is now delivered so that metrics is collected
		if ! grep -wE "releaseTimestamp|releasePath|releaseDate" $ProjDir/$SETJSONFILE >/dev/null 2>&1; then
			grep -v $ProjDir $OUT/.SequencingMetricsQCdir.$Platform.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt
			mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt $OUT/.SequencingMetricsQCdir.$Platform.txt
			if [[ -f "$OUT/.SequencingMetricsQCdir.$Platform.update.txt" ]] ; then
				#Also ensure that the project whose metrics is not collected at this time is remved from udpated
				grep -v $ProjDir $OUT/.SequencingMetricsQCdir.$Platform.update.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.update.txt
				mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.update.txt $OUT/.SequencingMetricsQCdir.$Platform.update.txt
			fi
			continue;
		fi
		if [[ -f "$ProjDir/$SETJSONFILE" ]]; then
			releaseMonth=$(grep -w packageTimestamp $ProjDir/$SETJSONFILE | awk '{print $2}' | grep -o '".*"' | sed 's/"//g' | cut -d"-" -f2)
			releaseYear=$(grep -w packageTimestamp $ProjDir/$SETJSONFILE | awk '{print $2}' | grep -o '".*"' | sed 's/"//g' | cut -d"-" -f1)
			Project=$(grep -w project $ProjDir/$SETJSONFILE | awk '{print $2}' | grep -o '".*"' | sed 's/"//g')
			groupFolder=$(grep deliveryFolder $ProjDir/$SETJSONFILE | awk '{print $2}' | grep -o '".*"' | sed 's/"//g' | awk -F/ '{print $4}')
			if echo $Project | grep 'BH' >/dev/null 2>&1; then
                Site="BH"
            else
                Site="CT"
            fi
			RunFolderDir="${ProjDir%/*}"
			RunFolder=${RunFolderDir##*/}
			Application=$(grep '"application":' $ProjDir/$SETJSONFILE | awk '{print $2}' | grep -o '".*"' | sed 's/"//g')
			releasePath=$(grep releasePath $ProjDir/$SETJSONFILE | awk '{print $2}' | grep -oP '(?<=").*(?=")' | sed -e 's+^+/gt+g')
			DeliveryDirectory=$(echo "${releasePath##*/}")
			projectFinal=$(grep projectFinal $ProjDir/$SETJSONFILE | awk '{print $2}' | grep -o '".*"' | sed 's/"//g')
		else
			echo -e ".settings missing at location: $ProjDir. Not all field will be populated.\n
			Review $OUT/$SequencingMetrics. \n You may consider removing the incomplete project in the file and repush to ctgenometech03:/srv/shiny-server/.InputDatabase. \n
			You may need $RunFolder" | mail -s "Sequencing Metrics Failure: .settings missing" $Email
		fi
        #collect the instrument ID
        if [[ "$Platform" == "Illumina" ]]; then
		    if [[ "$(ls $ProjDir 2> /dev/null | grep "Run_Metric_Summary" | grep "draft")" ]] ; then
			    export RunMetricsSummary=$ProjDir/$(ls $ProjDir | grep "Run_Metric_Summary" | grep "draft")
			    InstrumentName=$(grep -A1 "FlowCellID" $RunMetricsSummary | tr ',' '\t' | sed 's+"++g' | \
		  		    awk 'NR == 1{ for(i=1; i<=NF; i++) if ($i == "MachineID") { pos = i; break } } NR == 2{ print $pos; exit }')
		    elif [[ "$(ls $ProjDir 2> /dev/null | grep "Run_Metric_Summary.csv")" ]] ; then
			    export RunMetricsSummary=$ProjDir/$(ls $ProjDir | grep "Run_Metric_Summary.csv")
			    InstrumentName=$(grep -A1 "FlowCellID" $RunMetricsSummary | tr ',' '\t' | sed 's+"++g' | \
		  		    awk 'NR == 1{ for(i=1; i<=NF; i++) if ($i == "MachineID") { pos = i; break } } NR == 2{ print $pos; exit }')
		    elif [[ "$(ls $ProjDir/package 2> /dev/null | grep "Run_Metric_Summary" | grep "draft")" ]] ; then
			    export RunMetricsSummary=$ProjDir/package/$(ls $ProjDir/package | grep "Run_Metric_Summary" | grep "draft")
			    InstrumentName=$(grep -A1 "FlowCellID" $RunMetricsSummary | tr ',' '\t' | sed 's+"++g' | \
		  		    awk 'NR == 1{ for(i=1; i<=NF; i++) if ($i == "MachineID") { pos = i; break } } NR == 2{ print $pos; exit }')
		    elif [[ "$(ls $ProjDir/package 2> /dev/null | grep "Run_Metric_Summary.csv")" ]] ; then
			    export RunMetricsSummary=$ProjDir/package/$(ls $ProjDir | grep "Run_Metric_Summary.csv")
			    InstrumentName=$(grep -A1 "FlowCellID" $$RunMetricsSummary | tr ',' '\t' | sed 's+"++g' | \
		  		    awk 'NR == 1{ for(i=1; i<=NF; i++) if ($i == "MachineID") { pos = i; break } } NR == 2{ print $pos; exit }')
		    elif [[ "$(ls $ProjDir 2> /dev/null | grep "$RunInfo")" ]]; then
			    InstrumentName=$(grep "Instrument" $ProjDir/$RunInfo | sed -r 's/\s+//g' | tr '>' '\t' | tr '<' '\t' | sed -r 's/^\s+//g' | cut -f2)
		    else
			    InstrumentName=$RunFolder
		    fi
			if [[ "$(echo $InstrumentName | grep "M02838" 2> /dev/null)" ]] ; then
            	InstrumentID="CT_MiSeq_M02838"
			elif [[ "$(echo $InstrumentName | grep "M03204" 2> /dev/null)" ]]; then
				InstrumentID="CT_MiSeq_M03204"
			elif [[ "$(echo $InstrumentName | grep "VH01929" 2> /dev/null)" ]]; then
				InstrumentID="CT_NextSeq2000_VH01929"
			elif [[ "$(echo $InstrumentName | grep "A00724" 2> /dev/null)" ]]; then
				InstrumentID="CT_NovaSeq6000_A00724"
			elif [[ "$(echo $InstrumentName | grep "A00739" 2> /dev/null)" ]]; then
				InstrumentID="CT_NovaSeq6000_A00739"
			elif [[ "$(echo $InstrumentName | grep "LH00341" 2> /dev/null)" ]]; then
				InstrumentID="CT_NovaSeqX_Plus_LH00341"
			elif [[ "$(echo $InstrumentName | grep "M00263" 2> /dev/null)" ]] ; then
				InstrumentID="BH_MiSeq_M00263"
			elif [[ "$(echo $InstrumentName | grep "VH01930" 2> /dev/null)" ]] ; then
				InstrumentID="BH_NextSeq2000_VH01930"
			elif [[ "$(echo $InstrumentName | grep "NB501381" 2> /dev/null)" ]] ; then
				InstrumentID="BH_NextSeq500_NB501381"
			elif [[ "$(echo $InstrumentName | grep "D00138" 2> /dev/null)" ]] ; then
				InstrumentID="CT_HiSeq2500_D00138"
			elif [[ "$(echo $InstrumentName | grep "K00384" 2> /dev/null)" ]] ; then
				InstrumentID="CT_Illumina_HiSeq4000_K00384"               
			elif [[ "$(echo $InstrumentName | grep "M03341" 2> /dev/null)" ]] ; then
				InstrumentID="CT_MiSeq_M03341"               
			elif [[ "$(echo $InstrumentName | grep "NB501370" 2> /dev/null)" ]] ; then
				InstrumentID="CT_NextSeq500_NB501370"               
			elif [[ "$(echo $InstrumentName | grep "NS500440" 2> /dev/null)" ]] ; then
				InstrumentID="CT_NextSeq500_NS500440"               
			elif [[ "$(echo $InstrumentName | grep "NS500460" 2> /dev/null)" ]] ; then
				InstrumentID="CT_NextSeq500_NS500460"               
			elif [[ "$(echo $InstrumentName | grep "NS500440" 2> /dev/null)" ]] ; then
				InstrumentID="CT_NextSeq500_NS500440"               
			elif [[ "$(echo $InstrumentName | grep "245735200492" 2> /dev/null)" ]] ; then
				InstrumentID="CT_ThermoFisher_IonTorrentS5XL_245735200492"   
			elif [[ "$(echo $InstrumentName | grep "IonTorrent" 2> /dev/null)" ]] ; then
				InstrumentID="CT_ThermoFisher_IonTorrentS5XL_245735200492"             
			else
				InstrumentID=NULL
        	fi
		    ###Calculate the total reads, bases and folder size
			if [[ -d "$ProjDir/basic" ]] ; then
				ls $ProjDir/basic/*.json > $OUT/.$Platform.$DeliveryDirectory.txt 
		    	Reads=`printf '%s\n' \
			    	$(paste $OUT/.$Platform.$DeliveryDirectory.txt  | while read SAMPLE ; do
				    	grep -A2 '"before_filtering":' "$SAMPLE" | grep '"total_reads":' | awk -F':' '{print $2}' | sed 's+,++g'
			    	done) | paste -sd+ - | bc` #numfmt --grouping
		    	Bases=`printf '%s\n' \
			    	$(paste $OUT/.$Platform.$DeliveryDirectory.txt  | while read SAMPLE ; do
				    	grep -A2 '"before_filtering":' "$SAMPLE" | grep '"total_bases":' | awk -F':' '{print $2}' | sed 's+,++g'
			    	done) | paste -sd+ - | bc`
		    	SampleSize=$(ls $ProjDir/basic/*.json | grep -v "Undetermined"  | wc -l)
				PolymeraseRLbp="0"
		    	rm $OUT/.$Platform.$DeliveryDirectory.txt 
			elif stat --format '%a' $releasePath | grep "750" >/dev/null 2>&1 ; then
				Bytes=$(du -scb $releasePath | head -1 | cut -f1)
                SampleSize=$(ls $releasePath/*.fastq.gz | grep -v "Undetermined" \
					| sed 's/_R1_/_/g;s/_R2_/_/g;s/_R3_/_/g;s/_I1_/_/g;s/_I2_/_/g;/^$/d;' | awk '!seen[$0]++' | wc -l)
                ls $releasePath/*.fastq.gz | grep -v "Undetermined" > $OUT/.$Platform.$DeliveryDirectory.txt
                Reads=`printf '%s\n' \
			        $(paste $OUT/.$Platform.$DeliveryDirectory.txt | while read SAMPLE ; do
				        zcat "$SAMPLE" | wc -l | awk '{print$1/4}' 
			        done) | paste -sd+ - | bc`
                Bases=`printf '%s\n' \
			        $(paste $OUT/.$Platform.$DeliveryDirectory.txt | while read SAMPLE ; do
				        zcat "$SAMPLE" | awk 'NR%4==2 {sum += length($0)} END {print sum}'
			        done) | paste -sd+ - | bc`
				PolymeraseRLbp="0"
                rm $OUT/.$Platform.$DeliveryDirectory.txt
			else
				echo ""
				echo ""
				echo "Metrics from $releasePath will not be collected this time" 
				echo ""
				echo "Reason:"
				echo ""
				echo "1) Release folder may have been purged from customer's directory but not yet archived"
				echo "2) If not, then it might mean that fastq data is actively been copied into the folder"
				echo "3) Or analyst forgot to add 750 permission."
				echo ""
				echo "ACTION: Metrics from this project, if not already collected based on (1), will be skipped at this time"
				echo ""
				echo ""
			    grep -v $ProjDir $OUT/.SequencingMetricsQCdir.$Platform.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt
			    mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt $OUT/.SequencingMetricsQCdir.$Platform.txt
				continue;
			fi
		    #Skip the project if certain permission is not set
		    if [[ "$(stat --format '%a' $releasePath)" == "750" ]] ; then
			    Bytes=$(du -scb $releasePath | head -1 | cut -f1)
		    else
				echo ""
				echo ""
				echo "Metrics from $releasePath will not be collected this time" 
				echo ""
				echo "Reason:"
				echo ""
				echo "1) Release folder may have been purged from customer's directory but not yet archived"
				echo "2) If not, then it might mean that fastq data is actively been copied into the folder"
				echo "3) Or analyst forgot to add 750 permission."
				echo ""
				echo "ACTION: Metrics from this project, if not already collected based on (1), will be skipped at this time"
				echo ""
				echo ""
			    grep -v $ProjDir $OUT/.SequencingMetricsQCdir.$Platform.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt
			    mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt $OUT/.SequencingMetricsQCdir.$Platform.txt
			    continue;
		    fi
        elif [[ "$Platform" == "ONT" ]]; then
            if [[ -f "$ProjDir/.qifa.ont.json" ]] ; then
                if [[ "$(grep '"hostname":' $ProjDir/.qifa.ont.json  | awk '{print $2}' | grep -o '".*"' | sed 's/"//g')" ]] ; then
                    InstrumentName=$(grep '"hostname":' $ProjDir/.qifa.ont.json  | awk '{print $2}' | grep -o '".*"' | sed 's/"//g')
                elif [[ "$(grep '"host_product_serial_number":' $ProjDir/.qifa.ont.json  | awk '{print $2}' | grep -o '".*"' | sed 's/"//g')" ]] ; then
                    InstrumentName=$(grep '"hostname":' $ProjDir/.qifa.ont.json  | awk '{print $2}' | grep -o '".*"' | sed 's/"//g')
                fi
            else 
                InstrumentName="NULL"
            fi
			if [[ "$(echo $InstrumentName | grep "GXB02036" 2> /dev/null)" ]] ; then
            	InstrumentID="CT_GridIONX5_GXB02036"
			elif [[ "$(echo $InstrumentName | grep "PCA100115" 2> /dev/null)" ]] ; then
				InstrumentID="CT_PromethION_PCA100115"
			elif [[ "$(echo $InstrumentName | grep "GXB03074" 2> /dev/null)" ]] ; then
				InstrumentID="BH_GridIONX5_GXB03074"
			elif [[ "$(echo $InstrumentName | grep "P2S-01535" 2> /dev/null)" ]] ; then
				InstrumentID="BH_P2_Solo_P2S-01535"
			elif [[ "$(echo $InstrumentName | grep "GXB01025" 2> /dev/null)" ]] ; then
				InstrumentID="CT_GridIONX5_GXB01025"
			elif [[ "$(echo $InstrumentName | grep "GXB01102" 2> /dev/null)" ]] ; then
				InstrumentID="CT_GridIONX5_GXB01102"
			elif [[ "$(echo $InstrumentName | grep "GXB01186" 2> /dev/null)" ]] ; then
				InstrumentID="CT_GridIONX5_GXB01186"
			elif [[ "$(echo $InstrumentName | grep "PC24B149" 2> /dev/null)" ]] ; then
				InstrumentID="CT_PromethION_PC24B149"
			elif [[ "$(echo $InstrumentName | grep "PCT0053" 2> /dev/null)" ]] ; then
				InstrumentID="CT_PromethION_PCT0053"
			else
				InstrumentID=$InstrumentName
        	fi
            ###collect metrics from ${projectFinal}_QCreport.csv, else, calculate that from the fastq
            if stat --format '%a' $releasePath | grep "750" >/dev/null 2>&1 ; then
                Bytes=$(du -scb $releasePath | head -1 | cut -f1)
                SampleSize=$(grep "Sample Size:" $releasePath/${projectFinal}_QCreport.csv | tr ':' '\t' | cut -f2)
                if [[ -f "$releasePath/${projectFinal}_QCreport.csv" ]] ; then
                #First collect information from combine metrics, if it exist. Otherwise, add all necessary columns
                    if grep "combined" "$releasePath/${projectFinal}_QCreport.csv" >/dev/null 2>&1 ; then
                        Reads=`sed -n '/Reads_Total/,$p' ${releasePath}/${projectFinal}_QCreport.csv | \
                        awk 'NR==1 {print; next} /combined/' | awk -F'"' -v column_val="Reads_Total" '{ if (NR==1) {val=-1; for(i=1;i<=NF;i++)
                        { if ($i == column_val) {val=i;}}} if(val != -1) print $val}' | sed '1d; s+,++g' | paste -sd+ - | bc`
                        Bases=`sed -n '/Reads_Total/,$p' ${releasePath}/${projectFinal}_QCreport.csv | \
                        awk 'NR==1 {print; next} /combined/' | awk -F'"' -v column_val="Reads_Base_Total" '{ if (NR==1) {val=-1; for(i=1;i<=NF;i++) 
                            { if ($i == column_val) {val=i;}}} if(val != -1) print $val}' | sed '1d; s+,++g' | paste -sd+ - | bc`
                        PolymeraseRLbp="0"
                    else
                        Reads=`sed -n '/Reads_Total/,$p' ${releasePath}/${projectFinal}_QCreport.csv | grep -v "combined" | \
                        awk -F'"' -v column_val="Reads_Total" '{ if (NR==1) {val=-1; for(i=1;i<=NF;i++) 
                            { if ($i == column_val) {val=i;}}} if(val != -1) print $val}' | sed '1d; s+,++g' | paste -sd+ - | bc`
                        Bases=`sed -n '/Reads_Base_Total/,$p' ${releasePath}/${projectFinal}_QCreport.csv | grep -v "combined" | \
                        awk -F'"' -v column_val="Reads_Base_Total" '{ if (NR==1) {val=-1; for(i=1;i<=NF;i++) 
                            { if ($i == column_val) {val=i;}}} if(val != -1) print $val}' | sed '1d; s+,++g' | paste -sd+ - | bc`
                        PolymeraseRLbp="0"
                    fi 
                else
                    #if ${projectFinal}_QCreport.csv is not available, rely on fastq files
                    ls $releasePath/*.fastq.gz | grep -v "barcodes_*.*fastq.gz" | grep -v "combined" > $OUT/.$Platform.$DeliveryDirectory.txt
                    Reads=`printf '%s\n' \
			            $(paste $OUT/.$Platform.$DeliveryDirectory.txt | while read SAMPLE ; do
				            zcat "$SAMPLE" | wc -l | awk '{print$1/4}' 
			            done) | paste -sd+ - | bc`
                    Bases=`printf '%s\n' \
			            $(paste $OUT/.$Platform.$DeliveryDirectory.txt | while read SAMPLE ; do
				            zcat "$SAMPLE" | awk 'NR%4==2 {sum += length($0)} END {print sum}'
			            done) | paste -sd+ - | bc`
					PolymeraseRLbp="0"
                    rm $OUT/.$Platform.$DeliveryDirectory.txt
                fi
            else
				echo ""
				echo ""
 				echo "Metrics from $releasePath will not be collected this time" 
				echo ""
				echo "Reason:"
				echo ""
				echo "1) Release folder may have been purged from customer's directory but not yet archived"
				echo "2) If not, then it might mean that fastq data is actively been copied into the folder"
				echo "3) Or analyst forgot to add 750 permission."
				echo ""
				echo "ACTION: Metrics from this project, if not already collected based on (1), will be skipped at this time"
				echo ""
				echo ""
			    grep -v $ProjDir $OUT/.SequencingMetricsQCdir.$Platform.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt
			    mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt $OUT/.SequencingMetricsQCdir.$Platform.txt
			    continue;
            fi
		elif [[ "$Platform" == "PacBio" ]]; then
			#Position of the delivery directory is different thatn with the ont
			DeliveryDirectory=$(echo "${releasePath#*`echo $groupFolder/`}" | sed 's+/+\t+g' | cut -f1)
			InstrumentName=$(echo $RunFolder | sed 's|_[^/]*$||')
			if [[ -z "$InstrumentName" ]] ; then
				if [[ -f "$ProjDir/CCS_Report_${projectFinal}*.html" ]] ; then
         			InstrumentName=$(grep "Instrument Name" $ProjDir/CCS_Report_${projectFinal}*.html | \
            		sed 's+<tr><th>++g; s+</th><td>+\t+g; s+</td></tr>++g; s+Instrument Name++g') 
    			fi
			fi
			if echo $InstrumentName | grep "84148" >/dev/null 2>&1; then
            	InstrumentID="CT_Revio_84148"
			elif echo $InstrumentName | grep "SQ65119" >/dev/null 2>&1 ; then
            	InstrumentID="CT_Sequel_SQ65119"
            elif echo $InstrumentName | grep "64119" >/dev/null 2>&1 ; then
            	InstrumentID="CT_Sequel_SQ65119"
            elif echo $InstrumentName | grep "SQ65039" >/dev/null 2>&1 ; then
            	InstrumentID="CT_Sequel_SQ65039"
            elif echo $InstrumentName | grep "r64039" >/dev/null 2>&1 ; then
            	InstrumentID="CT_Sequel_SQ65039"			
			else
				InstrumentID=$InstrumentName
        	fi
		#
    		if [[ "$(stat --format '%a' $releasePath)" == "750" ]] ; then
        		#calculate reads total and bases from fastq instead
        		ls $releasePath/*.fastq.gz > $OUT/.$Platform.$DeliveryDirectory.txt
        		Reads=`printf '%s\n' $(paste $OUT/.$Platform.$DeliveryDirectory.txt | while read SAMPLE ; do
            			zcat "$SAMPLE" | grep -c '^@'
		    		done) | paste -sd+ - | bc`
        		Bases=`printf '%s\n' \
					$(paste $OUT/.$Platform.$DeliveryDirectory.txt | while read SAMPLE ; do
						zcat "$SAMPLE" | sed -n '2~4p' | tr -d '\n' | wc -c
					done) | paste -sd+ - | bc`
        		rm $OUT/.$Platform.$DeliveryDirectory.txt
    		else
				echo ""
				echo ""
				echo "Metrics from $releasePath will not be collected this time" 
				echo ""
				echo "Reason:"
				echo ""
				echo "1) Release folder may have been purged from customer's directory but not yet archived"
				echo "2) If not, then it might mean that fastq data is actively been copied into the folder"
				echo "3) Or analyst forgot to add 750 permission."
				echo ""
				echo "ACTION: Metrics from this project, if not already collected based on (1), will be skipped at this time"
				echo ""
				echo ""
				grep -v $ProjDir $OUT/.SequencingMetricsQCdir.$Platform.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt
				mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt $OUT/.SequencingMetricsQCdir.$Platform.txt
				continue;
    		fi
			#Extract polymerase from the 
			if grep -w "path" $ProjDir/$SETJSONFILE >/dev/null 2>&1; then
                rawReportPacBio=$(grep -w "path" $ProjDir/$SETJSONFILE | head -n1 | awk '{print $2}' | grep -o '".*"' | sed 's/"//g')
                PolymeraseRLbp=$(grep -A3 -w "raw_data_report.read_length" $rawReportPacBio/outputs/raw_data.report.json | grep -w "value" \
                    | awk '{print $2}' | sed 's/"//g')
				#For future consideration, the below code alread collect Polymerage reads and polymerase read bases from the raw smrtcell but result not yet included in the final file
				PolymeraseReads=$(grep -A3 -w "raw_data_report.nreads" $rawReportPacBio/outputs/raw_data.report.json | grep -w "value" \
                    | awk '{print $2}' | sed 's/"//g')
				PolymeraseReadBases=$(grep -A3 -w "raw_data_report.nbases" $rawReportPacBio/outputs/raw_data.report.json | grep -w "value" \
                    | awk '{print $2}' | sed 's/"//g')
            elif [[ -f "$ProjDir/package/Run_Report_${projectFinal}.csv" ]] ; then
				PolymeraseRLbp=`sed -n '/Sample Name/,$p' $ProjDir/package/Run_Report_${projectFinal}.csv |  \
        			awk -F'"' -v OFS='' '{ for (i=2; i<=NF; i+=2) gsub(",", "", $i) } 1' | \
            		tr ',' '\t' | awk -v ColNum="$WellColNum" -v cell="$Well" 'NR==1; $ColNum ~ cell { print }' | \
            		cut -f $(sed -n '/Sample Name/,$p' $ProjDir/package/Run_Report_${projectFinal}.csv | \
            		head -n1 | tr ',' '\n' | nl | awk '$2 == "Polymerase"' | cut -f1) | sed '1d' | sed '1d'`
            else
               	PolymeraseRLbp="0"
            fi
    		#
			#Check both the parent and subfolder for the permission
			shoutWell=`echo "${releasePath##*/}"`
			DeliveryDirectoryPath=$(echo "$releasePath" | sed "s+$shoutWell.*++")
			#check if subfolder is 750 or main folder. Subfolder permission is changed from 755 to 750
    		if $(stat --format '%a' $releasePath | grep "750" >/dev/null 2>&1) || \
				$(stat --format '%a' $DeliveryDirectoryPath | grep "750" >/dev/null 2>&1) ; then
        			Bytes=$(du -scb $releasePath | head -1 | cut -f1)
        			SampleSize=$(ls $releasePath/*.fastq.gz | grep "hifi_reads" | grep -v "unassigned" | wc -l)
    		else
				echo ""
				echo ""
				echo "Metrics from $releasePath will not be collected this time" 
				echo ""
				echo "Reason:"
				echo ""
				echo "1) Release folder may have been purged from customer's directory but not yet archived"
				echo "2) If not, then it might mean that fastq data is actively been copied into the folder"
				echo "3) Or analyst forgot to add 750 permission."
				echo ""
				echo "ACTION: Metrics from this project, if not already collected based on (1), will be skipped at this time"
				echo ""
				echo ""
				grep -v $ProjDir $OUT/.SequencingMetricsQCdir.$Platform.txt > $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt
				mv $OUT/.tmp.SequencingMetricsQCdir.$Platform.txt $OUT/.SequencingMetricsQCdir.$Platform.txt
				continue;
    		fi 
		fi   
    SaveMetricsFile
    done
}
function header {
    if [[ -f "$OUT/$SequencingMetrics" ]] ; then
	    if ! grep -E "Month|Year|Project|Site|InstrumentID|RunFolder" $OUT/$SequencingMetrics >/dev/null 2>&1; then
			sed -i -e '1iMonth,Year,Project,Site,InstrumentID,RunFolder,groupFolder,Application,FullPath,Platform,Reads,Bases,Bytes,DeliveryDirectory,SampleSize,PolymeraseRLbp' $OUT/$SequencingMetrics
		fi
	fi	
}
function fillEmptyColnRmDup {
	#Introduce NULL to empty column. If duplicate metrics is also present plus empty lines, remove | sed "/^\s*$/d"
	cat $OUT/$SequencingMetrics | sed -e 's/,,/,NULL,/g' >> $OUT/.tmpMetrics.csv
	#Remove rows if metrics is same for Month, Year, Project, Reads, Bases. This is becasue until the inception of this script,
	#the inherited .csv metric file has some column with no information. If those columns where later updated, they might be added as new metrics
	#whereas, they are not a new metrics. Checking these must constant column will help filtering out.
	#Lastly, pass awk '!seen[$0]++ { print $0 }' to double check for rows that are strickly the same.
	#Also remove any space before and after comma
	cat $OUT/.tmpMetrics.csv | awk -v RS="\n" -v FS="," 'BEGIN { FS="," } !seen[$1 "," $2"," $3"," $11"," $12]++ { print }' | \
		sed 's/[[:space:]]*,[[:space:]]*/,/g' > $OUT/$SequencingMetrics
	rm $OUT/.tmpMetrics.csv
}
function pushToServer {
	rsync -vahP $OUT/SequencingMetrics.csv $username@ctgenometech03.jax.org:/srv/shiny-server/.InputDatabase
}
trap 'failed_command $LINENO ${BASH_LINENO[@]}' ERR
trap 'throw_error' EXIT
#
#
exec_script 2>/dev/null