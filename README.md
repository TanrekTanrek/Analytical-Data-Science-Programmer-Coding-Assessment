# Analytical Data Science Programmer — Coding Assessment

> **Candidate:** [Your Name]
> **Date:** [Date]

---

## Overview

This repository contains solutions to the four-part Analytical Data Science
Programmer Coding Assessment, spanning the full clinical programming pipeline:

| # | Task | Language | Framework |
|---|---|---|---|
| 1 | SDTM Mapping (DS domain) | R | `sdtm.oak` |
| 2 | ADaM ADSL Creation | R | `admiral` |
| 3 | Tables, Listings & Graphs | R | `gtsummary`, `ggplot2` |
| 4 | Generative AI AE Agent | Python | `langchain`, `pandas` |

Every script produces a **log file** as evidence of an error-free run.

---

## Repository Structure

├── README.md
├── question_1_sdtm/
│ ├── ds_sdtm_mapping.R # Main script
│ ├── sdtm_ct.csv # Controlled terminology (input)
│ ├── DS.csv # Output: SDTM DS dataset
│ └── ds_sdtm_mapping.log # Execution log
│
├── question_2_adam/
│ ├── create_adsl.R # Main script
│ ├── adsl.rds # Output: ADSL (RDS)
│ ├── adsl.csv # Output: ADSL (CSV)
│ └── create_adsl.log # Execution log
│
├── question_3_tlg/
│ ├── 01_create_ae_summary_table.R # AE summary table script
│ ├── 02_create_visualizations.R # Visualization script
│ ├── ae_summary_table.html # Output: Summary table
│ ├── ae_summary_table.pdf # Output: Summary table (PDF)
│ ├── ae_severity_by_arm.png # Output: Stacked bar chart
│ ├── ae_top10_forest.png # Output: Top 10 TEAEs plot
│ ├── 01_create_ae_summary_table.log
│ └── 02_create_visualizations.log
│
└── question_4_genai/
├── clinical_trial_agent.py # Main script
├── adae.csv # Input: ADAE dataset
├── .env # OpenAI API key (not committed)
└── clinical_trial_agent.log # Execution log


---

## Setup

### R (Questions 1–3)

```r
install.packages(c(
  "sdtm.oak", "admiral", "pharmaverseraw", "pharmaversesdtm",
  "pharmaverseadam", "dplyr", "lubridate", "stringr",
  "gtsummary", "gt", "ggplot2"
))

### Python (Question 4)

pip install pandas python-dotenv langchain-openai langchain-core

Create question_4_genai/.env:
OPENAI_API_KEY=sk-xxxxxxxxxxxxxxxxxxxxxxxx

## How to Run Everything

### Question 1 — SDTM DS Mapping
Rscript question_1_sdtm/ds_sdtm_mapping.R
### Question 2 — ADaM ADSL
Rscript question_2_adam/create_adsl.R
### Question 3 — TLGs
Rscript question_3_tlg/01_create_ae_summary_table.R
Rscript question_3_tlg/02_create_visualizations.R
### Question 4 — LLM Agent
cd question_4_genai && python clinical_trial_agent.py

