#!/usr/bin/env bash

set -euo pipefail

# Load required modules
module use --append /gt/research_development/qifa/elion/modulefiles
module load node/8.6.0
module load duckdb/1.2.2

OUT="/gt/data/seqdma/GTwebMetricsTables"
duckDB_PATH="$OUT/GTdashboardMetrics.duckdb"
duckDB_lastpush="$OUT/.last_import_push/GTdashboardMetrics.lastpush.duckdb"
push_server="ctgenometech03:/srv/shiny-server/.InputDatabase/duckDB"
duckDB_lockfile="$OUT/.duckdb.lock"

# Color-coded log functions
log_info()    { echo -e "\033[1;34m[INFO]\033[0m $1"; }
log_warn()    { echo -e "\033[1;33m[WARN]\033[0m $1"; }
log_error()   { echo -e "\033[1;31m[ERROR]\033[0m $1"; }

# Help text
print_help() {
  cat <<EOF
Usage: $0 [OPTIONS]

Options:
  --samples ID1,ID2,...       Comma-separated GT_QC_Sample_ID (.e.g S53_GT25-03619) or Sample_Name (e.g. Islet229_0h_GT25-03619_TCCTACCTNNNNNNNNN-ACTCTCCA_S53_L002_R1_001.fastq.gz)
  --project_run_type TYPE     Exact match for Project_run_type (e.g. GT25-StitzelM-123-run2 or GT25-StitzelM-123 if single run)
  --application APPNAME       Application name (e.g., rnaseq, wgs)
  --to Delivered|Undelivered  Target ProjStatus value .e.g. Delivered or Undelivered
  --table TABLENAME           Target table: qc_illumina_metrics, qc_pacbio_metrics, or qc_ont_metrics
  --view                      Preview changes only
  --apply                     Apply the changes
  --help                      Show this help message

Table Descriptions:
  qc_illumina_metrics   - Contains short-read (Illumina) data
  qc_pacbio_metrics     - Contains long-read (PacBio) data
  qc_ont_metrics        - Contains long-read (Oxford Nanopore) data
EOF
}

# Parse arguments
samples=""
proj_type=""
new_status=""
target_table=""
app=""
do_view=false
do_apply=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    --samples) samples="$2"; shift 2 ;;
    --project_run_type) proj_type="$2"; shift 2 ;;
    --application) app="$2"; shift 2 ;;
    --to) new_status="$2"; shift 2 ;;
    --table) target_table="$2"; shift 2 ;;
    --view) do_view=true; shift ;;
    --apply) do_apply=true; shift ;;
    --help) print_help; exit 0 ;;
    *) echo "Unknown option: $1"; print_help; exit 1 ;;
  esac
done

# Prevent using both --samples and --project_run_type together
if [[ -n "$samples" && -n "$proj_type" ]]; then
  echo ""
  log_error "The options --samples and --project_run_type cannot be used together."
  echo "        As each targets a different set of matching rows, please specify only one to avoid unintended updates."
  echo ""
  exit 1
fi

# Validate required inputs
if [[ -z "$target_table" || -z "$new_status" || -z "$app" ]]; then
  log_error "Must specify --table, --to, [--samples or --project_run_type], and --application or check options with --help"
  exit 1
fi

if [[ -z "$samples" && -z "$proj_type" ]]; then
  log_error "You must specify --samples or --project_run_type"
  exit 1
fi

# Prepare WHERE clause
IFS=',' read -r -a items <<< "${samples:-$proj_type}"
quoted_items=$(printf "'%s'," "${items[@]}")
quoted_items="${quoted_items%,}"  # remove trailing comma


if [[ -n "$samples" ]]; then
  where_clause="Application = '$app' AND (GT_QC_Sample_ID IN ($quoted_items) OR Sample_Name IN ($quoted_items))"
else
  where_clause="Application = '$app' AND Project_run_type IN ($quoted_items)"
fi

#where_clause="Application = '$app' AND (GT_QC_Sample_ID IN ($quoted_items) OR Sample_Name IN ($quoted_items) OR Project_run_type IN ($quoted_items))"

# VIEW mode
if [[ "$do_view" == true ]]; then
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
  exit 0
fi

# APPLY mode
if [[ "$do_apply" == true ]]; then
  echo ""
  log_info "Preparing to apply ProjStatus update to $target_table for Application = '$app'..."

  # Check if any changes are needed
change_count=$(duckdb "$duckDB_PATH" -csv -noheader -c "
  SELECT COUNT(*) FROM $target_table
  WHERE $where_clause AND ProjStatus != '$new_status';
")


  if [[ "$change_count" == "0" ]]; then
    log_info "No changes needed."
    log_info "All matching rows already have ProjStatus = '$new_status'."
    log_info "To view these rows, run:"
    echo ""
    echo "  $0 --table $target_table --application $app --to $new_status ${samples:+--samples \"$samples\"} ${proj_type:+--project_run_type \"$proj_type\"} --view"
    echo ""
    exit 0
  fi

  # Locking
  if [[ -f "$duckDB_lockfile" ]]; then
    log_warn "The database is currently locked. Another process is writing to it."
    log_warn "Please try again in a few minutes."
    exit 1
  fi

  touch "$duckDB_lockfile"
  trap 'rm -f "$duckDB_lockfile"' EXIT

  # Apply update
  log_info "Updating $change_count row(s) in $target_table..."
  duckdb "$duckDB_PATH" -c "
    UPDATE $target_table
    SET ProjStatus = '$new_status'
    WHERE $where_clause AND ProjStatus != '$new_status'
  "
  log_info "Update complete."

  # Backup + push
  log_info "Backing up to: $duckDB_lastpush"
  cp "$duckDB_PATH" "$duckDB_lastpush"

  log_info "Syncing to server: $push_server"
  rsync -av "$duckDB_PATH" "$push_server"
  
  log_info "All done."
  echo ""
  exit 0
fi

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