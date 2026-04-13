# Analytical Data Science Programmer — Coding Assessment

> **Candidate:** [Your Name]
> **Date:** [Date]
> **Reviewer:** Roche Analytical Data Science Team

---

## Table of Contents

- [Overview](#overview)
- [Repository Structure](#repository-structure)
- [Environment Setup](#environment-setup)
- [Question 1 — SDTM Mapping (`question_1_sdtm/`)](#question-1--sdtm-mapping)
- [Question 2 — ADaM ADSL Creation (`question_2_adam/`)](#question-2--adam-adsl-creation)
- [Question 3 — Tables, Listings & Graphs (`question_3_tlg/`)](#question-3--tables-listings--graphs)
- [Question 4 — Generative AI / LLM Agent (`question_4_genai/`)](#question-4--generative-ai--llm-agent)
- [How to Run Everything](#how-to-run-everything)
- [Testing Strategy](#testing-strategy)
- [Style Guides & References](#style-guides--references)
- [Notes for the Reviewer](#notes-for-the-reviewer)

---

## Overview

This repository contains my solutions to the four-part Analytical Data Science
Programmer Coding Assessment. The work spans the full clinical programming
pipeline:

| # | Task | Domain | Language |
|---|---|---|---|
| 1 | SDTM Mapping | Raw → SDTM DS domain | R (`sdtm.oak`) |
| 2 | ADaM Dataset Creation | SDTM → ADaM ADSL | R (`admiral`) |
| 3 | Tables, Listings & Graphs | ADaM → TLGs | R (`gtsummary`, `ggplot2`) |
| 4 | Generative AI Agent | LLM-powered AE query agent | Python (`langchain`) |

Every script produces a **log file** as evidence of an error-free run, and all
outputs (datasets, tables, figures) are saved to their respective folders.

---

## Repository Structure
