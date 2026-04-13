#!/usr/bin/env Rscript
# ============================================================================
# Script : create_adsl.R
# Purpose: Derive the ADaM ADSL (Subject-Level Analysis Dataset) from SDTM
#          source domains using the {admiral} framework.
# Author : Philipp Tschannen
#
# Description:
#   Starting from SDTM DM, this script enriches subject-level data with:
#     - Planned & actual treatment arms (TRT01P, TRT01A)
#     - Age group categorisation (AGEGR9, AGEGR9N)
#     - Treatment start/end datetimes (TRTSDTM, TRTEDTM) from EX
#     - Intent-to-treat flag (ITTFL)
#     - Last available date across VS, AE, DS, and EX (LSTAVLDT)
#
# Inputs (all from {pharmaversesdtm}):
#   - dm : Demographics
#   - vs : Vital Signs
#   - ex : Exposure
#   - ds : Disposition
#   - ae : Adverse Events
#
# Outputs:
#   - question_2_adam/adsl.rds        : Final ADSL dataset (RDS format)
#   - question_2_adam/adsl.csv        : Final ADSL dataset (CSV format)
#   - question_2_adam/create_adsl.log : Execution log (evidence of error-free run)
#
# Style : https://style.tidyverse.org/
# Testing: testthat skeleton provided at end of file
# ============================================================================

# --- Configuration -----------------------------------------------------------
output_dir <- file.path(
  
  "~/Analytical-Data-Science-Programmer-Coding-Assessment",
  "question_2_adam"
)

# Create output directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# --- Setup logging -----------------------------------------------------------
log_file <- file.path(output_dir, "create_adsl.log")
log_con <- file(log_file, open = "wt")
sink(log_con, type = "output")
sink(log_con, type = "message")

cat("=== ADSL Creation Log ===\n")
cat("Execution started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R version:", R.version.string, "\n")
cat("Output directory:", output_dir, "\n\n")

tryCatch({
  
  # ---------------------------------------------------------------------------
  # 1. Load Packages
  # ---------------------------------------------------------------------------
  library(admiral)
  library(dplyr, warn.conflicts = FALSE)
  library(pharmaversesdtm)
  library(lubridate)
  library(stringr)
  
  cat("Loaded packages:\n")
  for (pkg in c("admiral", "dplyr", "pharmaversesdtm", "lubridate", "stringr")) {
    cat("  ", pkg, ":", as.character(packageVersion(pkg)), "\n")
  }
  cat("\n")
  
  # ---------------------------------------------------------------------------
  # 2. Read Source SDTM Domains
  # ---------------------------------------------------------------------------
  dm <- pharmaversesdtm::dm
  vs <- pharmaversesdtm::vs
  ex <- pharmaversesdtm::ex
  ds <- pharmaversesdtm::ds
  ae <- pharmaversesdtm::ae
  
  # Convert blank strings ("") to NA for consistent downstream handling
  
  dm <- convert_blanks_to_na(dm)
  vs <- convert_blanks_to_na(vs)
  ex <- convert_blanks_to_na(ex)
  ds <- convert_blanks_to_na(ds)
  ae <- convert_blanks_to_na(ae)
  
  cat("Source data loaded and blanks converted to NA:\n")
  cat("  DM:", nrow(dm), "subjects\n")
  cat("  VS:", nrow(vs), "records\n")
  cat("  EX:", nrow(ex), "records\n")
  cat("  DS:", nrow(ds), "records\n")
  cat("  AE:", nrow(ae), "records\n\n")
  
  # ---------------------------------------------------------------------------
  # 3. Initialise ADSL from DM
  # ---------------------------------------------------------------------------
  # DOMAIN is dropped as it is not an ADaM variable; all DM columns carry over.
  adsl <- dm %>%
    select(-DOMAIN)
  
  cat("ADSL initialised from DM:", nrow(adsl), "subjects,",
      ncol(adsl), "variables.\n")
  
  # ---------------------------------------------------------------------------
  # 4. Derive Treatment Variables (TRT01P, TRT01A)
  # ---------------------------------------------------------------------------
  # TRT01P = Planned treatment for period 1 (from ARM)
  
  # TRT01A = Actual  treatment for period 1 (from ACTARM)
  adsl <- adsl %>%
    mutate(
      TRT01P = ARM,
      TRT01A = ACTARM
    )
  
  cat("Derived TRT01P, TRT01A.\n")
  
  # ---------------------------------------------------------------------------
  # 5. Derive Age Groupings (AGEGR9, AGEGR9N)
  # ---------------------------------------------------------------------------
  # Categories: <18, 18–50, >50.  Uses admiral::derive_vars_cat().
  agegr9_lookup <- exprs(
    ~condition,              ~AGEGR9,       ~AGEGR9N,
    AGE < 18,               "<18",          1,
    between(AGE, 18, 50),   "18 - 50",      2,
    AGE > 50,               ">50",          3,
    is.na(AGE),             NA_character_,  NA_real_
  )
  
  adsl <- adsl %>%
    derive_vars_cat(definition = agegr9_lookup)
  
  cat("Derived AGEGR9, AGEGR9N.\n")
  
  # ---------------------------------------------------------------------------
  # 6. Derive Treatment Start / End Datetimes from EX
  # ---------------------------------------------------------------------------
  # Impute start time to 00:00:00 (first) and end time to 23:59:59 (last)
  # to obtain complete datetimes for exposure windows.
  ex_ext <- ex %>%
    derive_vars_dtm(
      new_vars_prefix = "EXST",
      time_imputation = "first",
      dtc = EXSTDTC
    ) %>%
    derive_vars_dtm(
      new_vars_prefix = "EXEN",
      time_imputation = "last",
      dtc = EXENDTC
    )
  
  cat("EX datetimes derived (EXSTDTM, EXENDTM).\n")
  
  # TRTSDTM: First valid exposure start per subject
  
  # Valid = dose > 0, or dose == 0 for placebo
  adsl <- adsl %>%
    derive_vars_merged(
      dataset_add = ex_ext,
      filter_add = (EXDOSE > 0 |
                      (EXDOSE == 0 &
                         str_detect(EXTRT, "PLACEBO"))) &
        !is.na(EXSTDTM),
      new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
      order    = exprs(EXSTDTM, EXSEQ),
      mode     = "first",
      by_vars  = exprs(STUDYID, USUBJID)
    )
  
  cat("Derived TRTSDTM (first valid exposure start).\n")
  
  # TRTEDTM: Last valid exposure end per subject
  adsl <- adsl %>%
    derive_vars_merged(
      dataset_add = ex_ext,
      filter_add = (EXDOSE > 0 |
                      (EXDOSE == 0 &
                         str_detect(EXTRT, "PLACEBO"))) &
        !is.na(EXENDTM),
      new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
      order    = exprs(EXENDTM, EXSEQ),
      mode     = "last",
      by_vars  = exprs(STUDYID, USUBJID)
    )
  
  cat("Derived TRTEDTM (last valid exposure end).\n")
  
  # ---------------------------------------------------------------------------
  # 7. Derive Intent-to-Treat Flag (ITTFL)
  # ---------------------------------------------------------------------------
  # A subject is in the ITT population if they were randomised (ARM is non-null).
  adsl <- adsl %>%
    mutate(
      ITTFL = if_else(!is.na(ARM), "Y", "N")
    )
  
  cat("Derived ITTFL.\n")
  
  # ---------------------------------------------------------------------------
  # 8. Derive Last Available Date (LSTAVLDT)
  # ---------------------------------------------------------------------------
  # Across VS, AE, DS, and treatment end, find the chronologically last
  # complete date per subject.  Only records with ≥10-char date strings
  # (i.e. full yyyy-mm-dd) are considered.
  adsl <- adsl %>%
    derive_vars_extreme_event(
      by_vars  = exprs(STUDYID, USUBJID),
      events   = list(
        # Source 1: Vital Signs — last complete date with a valid result
        event(
          dataset_name = "vs",
          order        = exprs(VSDTC, VSSEQ),
          condition    = !is.na(VSDTC) &
            nchar(VSDTC) >= 10 &
            !(is.na(VSSTRESN) & is.na(VSSTRESC)),
          set_values_to = exprs(
            LSTAVLDT = convert_dtc_to_dt(VSDTC),
            seq      = VSSEQ
          )
        ),
        # Source 2: Adverse Events — last complete onset date
        event(
          dataset_name = "ae",
          order        = exprs(AESTDTC, AESEQ),
          condition    = !is.na(AESTDTC) &
            nchar(AESTDTC) >= 10,
          set_values_to = exprs(
            LSTAVLDT = convert_dtc_to_dt(AESTDTC),
            seq      = AESEQ
          )
        ),
        # Source 3: Disposition — last complete disposition date
        event(
          dataset_name = "ds",
          order        = exprs(DSSTDTC, DSSEQ),
          condition    = !is.na(DSSTDTC) &
            nchar(DSSTDTC) >= 10,
          set_values_to = exprs(
            LSTAVLDT = convert_dtc_to_dt(DSSTDTC),
            seq      = DSSEQ
          )
        ),
        # Source 4: Treatment end date (already on ADSL)
        event(
          dataset_name  = "adsl",
          condition     = !is.na(TRTEDTM),
          set_values_to = exprs(
            LSTAVLDT = TRTEDTM,
            seq      = 0
          )
        )
      ),
      source_datasets  = list(vs = vs, ae = ae, ds = ds, adsl = adsl),
      tmp_event_nr_var = event_nr,
      order            = exprs(LSTAVLDT, seq, event_nr),
      mode             = "last",
      new_vars         = exprs(LSTAVLDT)
    )
  
  cat("Derived LSTAVLDT (last available date across VS, AE, DS, EX).\n")
  
  # ---------------------------------------------------------------------------
  # 9. Final Dataset Summary
  # ---------------------------------------------------------------------------
  cat("\n--- ADSL Summary ---\n")
  cat("Rows:    ", nrow(adsl), "\n")
  cat("Columns: ", ncol(adsl), "\n")
  cat("Variables:\n  ", paste(names(adsl), collapse = ", "), "\n")
  cat("TRT01P distribution:\n")
  print(table(adsl$TRT01P, useNA = "ifany"))
  cat("ITTFL distribution:\n")
  print(table(adsl$ITTFL, useNA = "ifany"))
  cat("AGEGR9 distribution:\n")
  print(table(adsl$AGEGR9, useNA = "ifany"))
  cat("\n")
  
  # ---------------------------------------------------------------------------
  # 10. Write Output Datasets
  # ---------------------------------------------------------------------------
  rds_path <- file.path(output_dir, "adsl.rds")
  csv_path <- file.path(output_dir, "adsl.csv")
  
  saveRDS(adsl, rds_path)
  write.csv(adsl, csv_path, row.names = FALSE)
  
  cat("Outputs written:\n")
  cat("  RDS:", rds_path, "\n")
  cat("  CSV:", csv_path, "\n")
  
  # ---------------------------------------------------------------------------
  # 11. Completion
  # ---------------------------------------------------------------------------
  cat("\n=== ADSL creation completed successfully ===\n")
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
  # Always restore console sinks
  sink(type = "message")
  sink(type = "output")
  close(log_con)
})

# Print confirmation to interactive console
message("ADSL creation complete. See log: ", log_file)