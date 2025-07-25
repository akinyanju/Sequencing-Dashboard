#!/usr/bin/env bash

set -euo pipefail

##############################
# Load required modules
module use --append /gt/research_development/qifa/elion/modulefiles
module load node/8.6.0
module load duckdb/1.2.2
##############################
OUT="/gt/data/seqdma/GTwebMetricsTables"
duckDB_PATH="$OUT/GTdashboardMetrics.duckdb"
duckDB_lastpush="$OUT/.last_import_push/GTdashboardMetrics.lastpush.duckdb"
push_server="ctgenometech03:/srv/shiny-server/.InputDatabase/duckDB"
duckDB_lockfile="$OUT/.duckdb.lock"
##############################
# Color-coded log functions
log_info()    { echo -e "\033[1;34m[INFO]\033[0m $1"; }
log_warn()    { echo -e "\033[1;33m[WARN]\033[0m $1"; }
log_error()   { echo -e "\033[1;31m[ERROR]\033[0m $1"; }
##############################
# Help text
  #--apply option removed excluded in the help page so user can view proposed changes first. the option will automatically be supplied 
print_help() {
  cat <<EOF
Usage: $0 [OPTIONS]

Options:
  -s, --samples ID1,ID2,...       Comma-separated GT_QC_Sample_ID (.e.g S53_GT25-03619) or Sample_Name (e.g. Islet229_0h_GT25-03619_TCCTACCTNNNNNNNNN-ACTCTCCA_S53_L002_R1_001.fastq.gz)
  -p, --project_run_type TYPE     Exact match for Project_run_type (e.g. GT25-StitzelM-123-run2 or GT25-StitzelM-123 if single run)
  -a, --application APPNAME       Application name (e.g., rnaseq, wgs)
  --to Delivered|Undelivered      Target ProjStatus value .e.g. Delivered or Undelivered
  -d, --delete                    Delete rows instead of updating (requires --project_run_type, --table, and --application)
  -t, --table TABLENAME           Target table: qc_illumina_metrics, qc_pacbio_metrics, or qc_ont_metrics
  -v, --view                      Preview changes only
  -h, --help                      Show this help message
EOF
}
##############################
# Parse arguments
samples=""
proj_type=""
new_status=""
target_table=""
app=""
do_view=false
do_apply=false
delete_mode=false
##############################
while [[ $# -gt 0 ]]; do
  case "$1" in
    -s|--samples) samples="$2"; shift 2 ;;
    -p|--project_run_type) proj_type="$2"; shift 2 ;;
    -a|--application) app="$2"; shift 2 ;;
    --to) new_status="$2"; shift 2 ;;
    -d|--delete) delete_mode=true; shift ;;
    -t|--table) target_table="$2"; shift 2 ;;
    -v|--view) do_view=true; shift ;;
    -h|--help) print_help; exit 0 ;;
    *) echo "Unknown option: $1" >&2; print_help; exit 1 ;;
  esac
done
##############################
# Require --project_run_type in all cases except --help
if [[ -z "$proj_type" ]]; then
  if [[ "$delete_mode" == true || -n "$samples" || -n "$new_status" || -n "$app" ]]; then
    log_error "--project_run_type is required when using --samples, --application, --to, or --delete"
    exit 1
  fi
fi
##############################
# Validate minimum required arguments
if [[ "$delete_mode" == true ]]; then
  if [[ -z "$target_table" || -z "$app" || -z "$proj_type" ]]; then
    log_error "--delete requires --project_run_type, --table, and --application"
    exit 1
  fi
elif [[ "$do_view" == true ]]; then
  if [[ -z "$target_table" || -z "$app" || -z "$proj_type" ]]; then
    log_error "--view requires --project_run_type, --table, and --application"
    exit 1
  fi
else
  if [[ -z "$target_table" || -z "$new_status" || -z "$app" || -z "$proj_type" ]]; then
    log_error "Updating ProjStatus requires --project_run_type, --table, --application, and --to"
    exit 1
  fi
fi
##############################
# Prepare WHERE clause
samples=$(echo "$samples" | xargs)
proj_type=$(echo "$proj_type" | xargs)

IFS=',' read -r -a sample_items <<< "$samples"
IFS=',' read -r -a proj_items <<< "$proj_type"

quoted_samples=$(printf "'%s'," "${sample_items[@]}")
quoted_samples="${quoted_samples%,}"

quoted_proj_types=$(printf "'%s'," "${proj_items[@]}")
quoted_proj_types="${quoted_proj_types%,}"

columns=$(duckdb "$duckDB_PATH" -csv -header -c "PRAGMA table_info($target_table);" | cut -d',' -f2)

if echo "$columns" | grep -q "^Sample_Name$"; then
  sample_column="Sample_Name"
elif echo "$columns" | grep -q "^Sample_ID$"; then
  sample_column="Sample_ID"
else
  log_error "Neither Sample_Name nor Sample_ID found in $target_table. Cannot proceed."
  exit 1
fi

if [[ -n "$samples" ]]; then
  where_clause="Application = '$app' AND Project_run_type IN ($quoted_proj_types) AND (GT_QC_Sample_ID IN ($quoted_samples) OR $sample_column IN ($quoted_samples))"
else
  where_clause="Application = '$app' AND Project_run_type IN ($quoted_proj_types)"
fi
##############################
if [[ "$do_view" == true ]]; then
  echo -e "\033[1;36m-- Preview Mode: Changes will NOT be written --\033[0m"
  if [[ -z "$new_status" ]]; then
    echo -e "\033[1;33mNo '--to' provided. Showing original table contents...\033[0m"
    duckdb "$duckDB_PATH" -c "
      SELECT * FROM $target_table
      WHERE $where_clause;
    " | less -S
  else
    echo -e "\033[1;33mShowing preview of ProjStatus changes to '$new_status'...\033[0m"
    duckdb "$duckDB_PATH" -c "
      SELECT 
        q.* EXCLUDE ProjStatus,
        CASE 
          WHEN q.ProjStatus != '$new_status' THEN '$new_status'
          ELSE q.ProjStatus
        END AS ProjStatus
      FROM (
        SELECT * FROM $target_table
        WHERE $where_clause
      ) AS q;
    " | less -S
  fi
  exit 0
fi
##############################
#Delete Mode
if [[ "$delete_mode" == true ]]; then
  delete_count=$(duckdb "$duckDB_PATH" -csv -noheader -c "SELECT COUNT(*) FROM $target_table WHERE $where_clause;")

  if [[ "$do_apply" == false ]]; then
    echo ""
    log_warn "You are about to delete \033[1;36m$delete_count\033[0m row(s) from $target_table where Application = '$app' and Project_run_type = $quoted_proj_types."
    log_warn "This action is \033[1;31mIRREVERSIBLE\033[0;33m. Proceed with caution."
    echo ""
    log_info "To proceed with deletion, rerun the command with --apply."
    echo ""
    echo "  $0 --delete --table $target_table --application $app ${samples:+--samples \"$samples\"} ${proj_type:+--project_run_type \"$proj_type\"} --apply"
    echo ""
    exit 0
  fi
##############################
  if [[ -f "$duckDB_lockfile" ]]; then
    log_warn "The database is currently locked. Another process is writing to it."
    exit 1
  fi
##############################
  touch "$duckDB_lockfile"
  trap 'rm -f "$duckDB_lockfile"' EXIT

  log_info "Deleting $delete_count row(s) from $target_table..."
  duckdb "$duckDB_PATH" -c "DELETE FROM $target_table WHERE $where_clause;"
  log_info "Deletion complete."

  cp "$duckDB_PATH" "$duckDB_lastpush"
  rsync -av "$duckDB_PATH" "$push_server"

  log_info "All done."
  exit 0
fi
##############################
# APPLY mode (for updating ProjStatus)
if [[ "$do_apply" == true ]]; then
  echo ""
  log_info "Preparing to apply ProjStatus update to $target_table for Application = '$app'..."

  change_count=$(duckdb "$duckDB_PATH" -csv -noheader -c "
    SELECT COUNT(*) FROM $target_table
    WHERE $where_clause AND ProjStatus != '$new_status';
  ")
##############################
  if [[ "$change_count" == "0" ]]; then
    log_info "No changes needed."
    log_info "All matching rows already have ProjStatus = '$new_status'."
    log_info "To view these rows, run:"
    echo ""
    echo "  $0 --table $target_table --application $app --to $new_status ${samples:+--samples \"$samples\"} ${proj_type:+--project_run_type \"$proj_type\"} --view"
    echo ""
    exit 0
  fi
##############################
  if [[ -f "$duckDB_lockfile" ]]; then
    log_warn "The database is currently locked. Another process is writing to it."
    exit 1
  fi
##############################
  touch "$duckDB_lockfile"
  trap 'rm -f "$duckDB_lockfile"' EXIT

  log_info "Updating $change_count row(s) in $target_table..."
  duckdb "$duckDB_PATH" -c "
    UPDATE $target_table
    SET ProjStatus = '$new_status'
    WHERE $where_clause AND ProjStatus != '$new_status';
  "
  log_info "Update complete."

  cp "$duckDB_PATH" "$duckDB_lastpush"
  rsync -av "$duckDB_PATH" "$push_server"

  log_info "All done."
  exit 0
fi
##############################
# If no --view or --apply were given, suggest next steps
echo ""
log_warn "No --view or --apply flag provided. Therefore, follow the below instruction:"
log_info "$(echo -e "\033[38;5;208m[RECOMMENDED]\033[0m preview the proposed changes before applying them. Initiate the below command:")"
echo "  $0 --table $target_table --application $app --to $new_status ${samples:+--samples \"$samples\"} ${proj_type:+--project_run_type \"$proj_type\"} --view"
log_info "If the preview looks good, apply the changes with:"
echo "  $0 --table $target_table --application $app --to $new_status ${samples:+--samples \"$samples\"} ${proj_type:+--project_run_type \"$proj_type\"} --apply"
log_info "The apply step will update the database, back it up, and sync it to the destination server"
echo ""
exit 0