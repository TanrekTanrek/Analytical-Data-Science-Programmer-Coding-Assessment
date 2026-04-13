#!/usr/bin/env Rscript
# ============================================================================
# Script: ds_sdtm_mapping.R
# Purpose: Map raw DS (Disposition) data to SDTM DS domain using {sdtm.oak}
# Author: Philip Tschannen
# Dependencies: sdtm.oak, pharmaverseraw, pharmaversesdtm, dplyr
#
# Description:
#   This script transforms raw disposition data (ds_raw) into a CDISC SDTM-
#   compliant DS domain dataset. It applies controlled terminology mappings,
#   datetime conversions, and standard derivations (DSSEQ, study day).
#
# Inputs:
#   - pharmaverseraw::ds_raw        : Raw disposition source data
#   - pharmaversesdtm::dm           : DM domain (for study day derivation)
#   - sdtm_ct.csv                   : Study-level controlled terminology
#
# Outputs:
#   - DS.csv                        : Final SDTM DS domain dataset
#   - ds_sdtm_mapping.log           : Execution log (evidence of error-free run)
# ============================================================================

# --- Setup logging -----------------------------------------------------------
log_file <- file.path(
  "~/Analytical-Data-Science-Programmer-Coding-Assessment/question_1_stdm",
  "ds_sdtm_mapping.log"
)
log_con <- file(log_file, open = "wt")
sink(log_con, type = "output")
sink(log_con, type = "message")

cat("=== DS SDTM Mapping Log ===\n")
cat("Execution started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R version:", R.version.string, "\n\n")

tryCatch({
  
  # --- Load packages ---------------------------------------------------------
  library(sdtm.oak)
  library(pharmaverseraw)
  library(pharmaversesdtm)
  library(dplyr)
  
  cat("Packages loaded successfully.\n\n")
  
  # --- Load source data ------------------------------------------------------
  ds_raw <- pharmaverseraw::ds_raw
  dm <- pharmaversesdtm::dm
  
  cat("Source data loaded.\n")
  cat("  ds_raw: ", nrow(ds_raw), " rows x ", ncol(ds_raw), " cols\n")
  cat("  dm:     ", nrow(dm), " rows x ", ncol(dm), " cols\n\n")
  
  # --- Load controlled terminology -------------------------------------------
  ct_path <- file.path(
    "~/Analytical-Data-Science-Programmer-Coding-Assessment",
    "sdtm_ct.csv"
  )
  study_ct <- read.csv(ct_path)
  
  cat("Controlled terminology loaded from:", ct_path, "\n")
  cat("  study_ct: ", nrow(study_ct), " rows\n\n")
  
  # --- Reference date configuration (for documentation) ----------------------
  # Not used downstream in this script but retained for traceability.
  ref_date_conf_df <- tibble::tribble(
    ~raw_dataset_name, ~date_var,  ~time_var,       ~dformat,      ~tformat, ~sdtm_var_name,
    "ds_raw",          "DSDTCOL",  "DSTMCOL",   "mm-dd-yyyy",        "H:M",    "RFPENDTC",
    "ds_raw",          "DEATHDT",  NA_character_, "mm/dd/yyyy", NA_character_,      "DTHDTC"
  )
  
  # --- Generate oak ID variables ---------------------------------------------
  ds_raw <- ds_raw %>%
    generate_oak_id_vars(
      pat_var = "PATNUM",
      raw_src = "ds_raw"
    )
  
  cat("Oak ID variables generated.\n")
  
  # --- Map topic variable: DSDECOD (with CT) ---------------------------------
  # When OTHERSP is missing, map IT.DSDECOD -> DSDECOD via controlled terminology
  # codelist C66727 (Disposition Event).
  ds <- assign_ct(
    raw_dat  = condition_add(ds_raw, is.na(ds_raw$OTHERSP)),
    raw_var  = "IT.DSDECOD",
    tgt_var  = "DSDECOD",
    ct_spec  = study_ct,
    ct_clst  = "C66727",
    id_vars  = oak_id_vars()
  )
  
  cat("Topic variable DSDECOD mapped (CT codelist C66727).\n")
  
  # --- Map remaining variables -----------------------------------------------
  ds <- ds %>%
    # DSDECOD from free-text (OTHERSP) when populated
    assign_no_ct(
      raw_dat = condition_add(ds_raw, !is.na(ds_raw$OTHERSP)),
      raw_var = "OTHERSP",
      tgt_var = "DSDECOD",
      id_vars = oak_id_vars()
    ) %>%
    # DSTERM from OTHERSP (free-text)
    assign_no_ct(
      raw_dat = condition_add(ds_raw, !is.na(ds_raw$OTHERSP)),
      raw_var = "OTHERSP",
      tgt_var = "DSTERM",
      id_vars = oak_id_vars()
    ) %>%
    # DSTERM from IT.DSTERM (standard term) when OTHERSP is missing
    assign_no_ct(
      raw_dat = condition_add(ds_raw, is.na(ds_raw$OTHERSP)),
      raw_var = "IT.DSTERM",
      tgt_var = "DSTERM",
      id_vars = oak_id_vars()
    ) %>%
    # DSCAT = "PROTOCOL MILESTONE" for randomised records
    hardcode_no_ct(
      raw_dat = condition_add(
        ds_raw,
        !is.na(ds_raw$IT.DSDECOD) & ds_raw$IT.DSDECOD == "Randomized"
      ),
      raw_var = "IT.DSDECOD",
      tgt_val = "PROTOCOL MILESTONE",
      tgt_var = "DSCAT",
      id_vars = oak_id_vars()
    ) %>%
    # DSCAT = "DISPOSITION EVENT" for non-randomised standard records
    hardcode_no_ct(
      raw_dat = condition_add(
        ds_raw,
        is.na(ds_raw$IT.DSDECOD) | ds_raw$IT.DSDECOD != "Randomized"
      ),
      raw_var = "IT.DSDECOD",
      tgt_val = "DISPOSITION EVENT",
      tgt_var = "DSCAT",
      id_vars = oak_id_vars()
    ) %>%
    # DSCAT = "OTHER EVENT" for free-text records
    hardcode_no_ct(
      raw_dat = condition_add(ds_raw, !is.na(ds_raw$OTHERSP)),
      raw_var = "OTHERSP",
      tgt_val = "OTHER EVENT",
      tgt_var = "DSCAT",
      id_vars = oak_id_vars()
    ) %>%
    # VISIT from INSTANCE via CT
    assign_ct(
      raw_dat = ds_raw,
      raw_var = "INSTANCE",
      tgt_var = "VISIT",
      ct_spec = study_ct,
      ct_clst = "VISIT",
      id_vars = oak_id_vars()
    ) %>%
    # VISITNUM from INSTANCE via CT
    assign_ct(
      raw_dat = ds_raw,
      raw_var = "INSTANCE",
      tgt_var = "VISITNUM",
      ct_spec = study_ct,
      ct_clst = "VISITNUM",
      id_vars = oak_id_vars()
    ) %>%
    # DSSTDTC datetime from IT.DSSTDAT
    assign_datetime(
      raw_dat = ds_raw,
      raw_var = "IT.DSSTDAT",
      tgt_var = "DSSTDTC",
      raw_unk = c("UN", "UNK"),
      raw_fmt = "m-d-y"
    ) %>%
    # DSDTC datetime from DSDTCOL + DSTMCOL
    assign_datetime(
      raw_dat = ds_raw,
      raw_var = c("DSDTCOL", "DSTMCOL"),
      tgt_var = "DSDTC",
      raw_unk = c("UN", "UNK"),
      raw_fmt = c("m-d-y", "H:M")
    )
  
  cat("All variable mappings complete.\n")
  
  # --- Derive standard SDTM variables ---------------------------------------
  ds <- ds %>%
    dplyr::mutate(
      STUDYID = ds_raw$STUDY,
      DOMAIN  = "DS",
      USUBJID = paste0("01-", ds_raw$PATNUM),
      DSDECOD = toupper(DSDECOD)
    ) %>%
    derive_seq(
      tgt_var  = "DSSEQ",
      rec_vars = c("USUBJID", "DSDECOD")
    ) %>%
    derive_study_day(
      sdtm_in       = .,
      dm_domain     = dm,
      tgdt          = "DSSTDTC",
      refdt         = "RFSTDTC",
      study_day_var = "DSSTDY"
    ) %>%
    dplyr::select(
      "STUDYID", "DOMAIN", "USUBJID", "DSSEQ",
      "DSTERM", "DSDECOD", "DSCAT", "VISITNUM",
      "VISIT", "DSDTC", "DSSTDTC", "DSSTDY"
    )
  
  cat("Derived DSSEQ, DSSTDY, and finalised column selection.\n")
  cat("Final DS dataset: ", nrow(ds), " rows x ", ncol(ds), " cols\n\n")
  
  # --- Write output ----------------------------------------------------------
  output_path <- file.path(
    "~/Analytical-Data-Science-Programmer-Coding-Assessment",
    "question_1_stdm",
    "DS.csv"
  )
  write.csv(ds, output_path, row.names = FALSE)
  
  cat("Output written to:", output_path, "\n")
  cat("\n=== Execution completed successfully ===\n")
  cat("Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
}, error = function(e) {
  cat("\n!!! ERROR encountered !!!\n")
  cat("Message:", conditionMessage(e), "\n")
  cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  stop(e)  # re-raise so non-zero exit code is returned
  
}, finally = {
  # Restore console output
  sink(type = "message")
  sink(type = "output")
  close(log_con)
})

# Print confirmation to console
message("DS SDTM mapping complete. See log: ", log_file)