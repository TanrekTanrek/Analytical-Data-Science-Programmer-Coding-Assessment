#!/usr/bin/env Rscript
# ============================================================================
# Script : 01_create_ae_summary_table.R
# Purpose: Generate a hierarchical summary table of Treatment-Emergent
#          Adverse Events (TEAEs) by System Organ Class (SOC) and
#          Preferred Term, stratified by treatment arm.
# Author : Philipp Tschannen
#
# Description:
#   Filters ADAE to treatment-emergent AEs (TRTEMFL == "Y"), then produces
#   a hierarchical frequency table (SOC → AETERM) with an overall row and
#   an overall column using {gtsummary}.  The table is sorted by descending
#   frequency and exported as both PDF and HTML.
#
# Inputs (all from {pharmaverseadam}):
#   - adae : Analysis Dataset — Adverse Events
#   - adsl : Analysis Dataset — Subject Level
#
# Outputs:
#   - question_3_tlg/ae_summary_table.pdf  : Summary table (PDF)
#   - question_3_tlg/ae_summary_table.html : Summary table (HTML)
#   - question_3_tlg/01_create_ae_summary_table.log : Execution log
#
# Style  : https://style.tidyverse.org/
# ============================================================================

# --- Configuration -----------------------------------------------------------
output_dir <- file.path(
  "~/Analytical-Data-Science-Programmer-Coding-Assessment",
  "question_3_tlg"
)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# --- Setup logging -----------------------------------------------------------
log_file <- file.path(output_dir, "01_create_ae_summary_table.log")
log_con <- file(log_file, open = "wt")
sink(log_con, type = "output")
sink(log_con, type = "message")

cat("=== AE Summary Table Creation Log ===\n")
cat("Execution started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R version:", R.version.string, "\n\n")

tryCatch({
  
  # ---------------------------------------------------------------------------
  # 1. Load Packages
  # ---------------------------------------------------------------------------
  library(pharmaverseadam)
  library(gtsummary)
  library(dplyr)
  library(gt)
  
  cat("Loaded packages:\n")
  for (pkg in c("pharmaverseadam", "gtsummary", "dplyr", "gt")) {
    cat("  ", pkg, ":", as.character(packageVersion(pkg)), "\n")
  }
  cat("\n")
  
  # ---------------------------------------------------------------------------
  # 2. Load Source Data
  # ---------------------------------------------------------------------------
  adae <- pharmaverseadam::adae
  adsl <- pharmaverseadam::adsl
  
  cat("Source data loaded:\n")
  cat("  ADAE:", nrow(adae), "records,", n_distinct(adae$USUBJID), "subjects\n")
  cat("  ADSL:", nrow(adsl), "subjects\n\n")
  
  # ---------------------------------------------------------------------------
  # 3. Filter to Treatment-Emergent AEs
  # ---------------------------------------------------------------------------
  # TRTEMFL == "Y" identifies AEs that started on or after first dose and
  # up to a protocol-defined window after last dose.
  adae_te <- adae %>%
    filter(TRTEMFL == "Y")
  
  cat("TEAEs after filter:", nrow(adae_te), "records,",
      n_distinct(adae_te$USUBJID), "subjects\n\n")
  
  # ---------------------------------------------------------------------------
  # 4. Build Hierarchical Summary Table
  # ---------------------------------------------------------------------------
  # Rows:    SOC (AESOC) → Preferred Term (AETERM)
  # Columns: Treatment arm (ACTARM) + Overall
  # Counts:  Unique subjects (USUBJID); denominator from ADSL
  tbl <- tbl_hierarchical(
    data        = adae_te,
    variables   = c(AESOC, AETERM),
    by          = ACTARM,
    denominator = adsl,
    id          = USUBJID,
    label       = "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs",
    overall_row = TRUE
  ) %>%
    add_overall()
  
  cat("Hierarchical table constructed.\n")
  
  # Sort by descending frequency for clinical readability
  tbl_sorted <- sort_hierarchical(tbl)
  
  cat("Table sorted by descending frequency.\n")
  
  # ---------------------------------------------------------------------------
  # 5. Export Outputs
  # ---------------------------------------------------------------------------
  gt_obj <- tbl_sorted %>%
    as_gt()
  
  # PDF output
  pdf_path <- file.path(output_dir, "ae_summary_table.pdf")
  gt::gtsave(gt_obj, filename = pdf_path)
  cat("PDF saved:", pdf_path, "\n")
  
  # HTML output
  html_path <- file.path(output_dir, "ae_summary_table.html")
  gt::gtsave(gt_obj, filename = html_path)
  cat("HTML saved:", html_path, "\n")
  
  # ---------------------------------------------------------------------------
  # 6. Completion
  # ---------------------------------------------------------------------------
  cat("\n=== AE Summary Table creation completed successfully ===\n")
  cat("Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
}, error = function(e) {
  cat("\n!!! ERROR encountered !!!\n")
  cat("Message: ", conditionMessage(e), "\n")
  cat("Call:    ", deparse(conditionCall(e)), "\n")
  cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  stop(e)
  
}, warning = function(w) {
  cat("WARNING: ", conditionMessage(w), "\n")
  invokeRestart("muffleWarning")
  
}, finally = {
  sink(type = "message")
  sink(type = "output")
  close(log_con)
})

message("AE Summary Table complete. See log: ", log_file)