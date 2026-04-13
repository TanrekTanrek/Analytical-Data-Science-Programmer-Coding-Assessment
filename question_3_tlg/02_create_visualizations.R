#!/usr/bin/env Rscript
# ============================================================================
# Script : 02_create_visualizations.R
# Purpose: Generate two AE visualizations:
#            1. Stacked bar chart — AE severity distribution by treatment arm
#            2. Forest-style dot plot — Top 10 most frequent TEAEs with 95% CI
# Author : Philipp Tschannen
#
# Description:
#   Both plots use treatment-emergent AEs (TRTEMFL == "Y") from ADAE.
#   Plot 1 shows the count of AE records by severity (MILD/MODERATE/SEVERE)
#   stacked within each treatment arm.
#   Plot 2 shows the incidence rate (%) of the 10 most frequent preferred
#   terms across all arms, with exact (Clopper–Pearson) 95% confidence
#   intervals.
#
# Inputs (all from {pharmaverseadam}):
#   - adae : Analysis Dataset — Adverse Events
#   - adsl : Analysis Dataset — Subject Level
#
# Outputs:
#   - question_3_tlg/ae_severity_by_arm.png     : Stacked bar chart (PNG)
#   - question_3_tlg/ae_top10_forest.png         : Top 10 TEAEs dot plot (PNG)
#   - question_3_tlg/02_create_visualizations.log : Execution log
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

# Plot dimensions (inches)
plot_width  <- 10
plot_height <- 7
plot_dpi    <- 300

# --- Setup logging -----------------------------------------------------------
log_file <- file.path(output_dir, "02_create_visualizations.log")
log_con <- file(log_file, open = "wt")
sink(log_con, type = "output")
sink(log_con, type = "message")

cat("=== AE Visualizations Creation Log ===\n")
cat("Execution started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R version:", R.version.string, "\n\n")

tryCatch({
  
  # ---------------------------------------------------------------------------
  # 1. Load Packages
  # ---------------------------------------------------------------------------
  library(ggplot2)
  library(pharmaverseadam)
  library(dplyr)
  
  cat("Loaded packages:\n")
  for (pkg in c("ggplot2", "pharmaverseadam", "dplyr")) {
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
  adae_te <- adae %>%
    filter(TRTEMFL == "Y")
  
  cat("TEAEs after filter:", nrow(adae_te), "records,",
      n_distinct(adae_te$USUBJID), "subjects\n\n")
  
  # ===========================================================================
  # PLOT 1: Stacked Bar Chart — AE Severity Distribution by Treatment Arm
  # ===========================================================================
  cat("--- Plot 1: AE Severity by Arm ---\n")
  
  # Count AE *records* (not unique subjects) by arm and severity
  # for a stacked bar representation of total AE burden.
  ae_sev_stacked <- adae_te %>%
    count(ACTARM, AESEV, name = "ae_count") %>%
    mutate(
      AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
    )
  
  cat("Severity counts by arm:\n")
  print(ae_sev_stacked)
  cat("\n")
  
  p1 <- ggplot(ae_sev_stacked, aes(fill = AESEV, y = ae_count, x = ACTARM)) +
    geom_bar(position = "stack", stat = "identity") +
    xlab("Treatment Arm") +
    ylab("Count of AEs") +
    labs(fill = "Severity/Intensity") +
    ggtitle("AE Severity Distribution by Treatment Arm") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 15, hjust = 1),
      plot.title = element_text(face = "bold", size = 14)
    )
  
  p1_path <- file.path(output_dir, "ae_severity_by_arm.png")
  ggsave(
    filename = p1_path,
    plot     = p1,
    width    = plot_width,
    height   = plot_height,
    dpi      = plot_dpi
  )
  
  cat("Plot 1 saved:", p1_path, "\n\n")
  
  # ===========================================================================
  # PLOT 2: Top 10 Most Frequent TEAEs with 95% Clopper–Pearson CI
  # ===========================================================================
  cat("--- Plot 2: Top 10 TEAEs with CI ---\n")
  
  # Total ITT population as the denominator
  n_total <- n_distinct(adsl$USUBJID)
  cat("Overall N (unique subjects in ADSL):", n_total, "\n")
  
  # Count unique subjects per preferred term, take the top 10, then compute
  # exact binomial (Clopper–Pearson) 95% CI for the incidence proportion.
  ae_top10 <- adae_te %>%
    distinct(USUBJID, AETERM) %>%
    count(AETERM, name = "n") %>%
    slice_max(n, n = 10, with_ties = FALSE) %>%
    rowwise() %>%
    mutate(
      pct      = n / n_total * 100,
      ci_lower = binom.test(n, n_total)$conf.int[1] * 100,
      ci_upper = binom.test(n, n_total)$conf.int[2] * 100
    ) %>%
    ungroup() %>%
    mutate(AETERM = reorder(AETERM, pct))
  
  cat("Top 10 TEAEs:\n")
  print(as.data.frame(ae_top10))
  cat("\n")
  
  p2 <- ggplot(ae_top10, aes(x = pct, y = AETERM)) +
    geom_errorbar(
      aes(xmin = ci_lower, xmax = ci_upper),
      width  = 0.2,
      colour = "grey40"
    ) +
    geom_point(size = 2.5) +
    labs(
      title    = "Top 10 Most Frequent Treatment-Emergent Adverse Events",
      subtitle = paste0(
        "Incidence rate with 95% Clopper\u2013Pearson CI (N = ", n_total, ")"
      ),
      x = "Percentage of Patients (%)",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, colour = "grey30")
    )
  
  p2_path <- file.path(output_dir, "ae_top10_forest.png")
  ggsave(
    filename = p2_path,
    plot     = p2,
    width    = plot_width,
    height   = plot_height,
    dpi      = plot_dpi
  )
  
  cat("Plot 2 saved:", p2_path, "\n\n")
  
  # ---------------------------------------------------------------------------
  # Completion
  # ---------------------------------------------------------------------------
  cat("=== Visualization creation completed successfully ===\n")
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

message("Visualizations complete. See log: ", log_file)