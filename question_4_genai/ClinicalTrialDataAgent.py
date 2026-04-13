#!/usr/bin/env python3
# ============================================================================
# Script : clinical_trial_agent.py
# Purpose: Provide a natural-language interface over an Adverse Events (AE)
#          dataset using an LLM-backed agent.  User questions are translated
#          into structured JSON filters, which are then executed on a Pandas
#          DataFrame to return matching subject counts and IDs.
#
# Author : Philipp Tschannen
#
# Architecture:
#   1. User poses a plain-English question about adverse events.
#   2. The LLM (GPT-4o-mini via LangChain) maps the question to a JSON
#      structure: { target_column, filter_value }.
#   3. The agent applies the filter to the AE DataFrame (partial match for
#      AESOC/AETERM; exact match otherwise) and returns unique subjects.
#
# Key design decisions:
#   - All string columns are uppercased at load time so that LLM-generated
#     filter values match without case-sensitivity issues.
#   - AESOC and AETERM use `str.contains()` to handle partial matches
#     (e.g., "cardiac" matches "CARDIAC DISORDERS").
#   - The LLM is instructed to return raw JSON only — no markdown fences —
#     but a defensive strip is applied in case it does.
#
# Inputs:
#   - adae.csv              : Adverse Events analysis dataset (ADaM ADAE)
#   - .env                  : Environment file containing OPENAI_API_KEY
#
# Outputs:
#   - Console output        : Parsed intent, subject counts, subject IDs
#   - clinical_trial_agent.log : Full execution log (evidence of error-free run)
#
# Dependencies:
#   pandas, python-dotenv, langchain-openai, langchain-core
#
# Style:
#   - PEP 8 compliant (https://peps.python.org/pep-0008/)
#   - Type hints on class methods
#   - Docstrings on all public methods
#   - Defensive error handling throughout
# ============================================================================

import os
import sys
import json
import logging
from datetime import datetime

import pandas as pd
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI
from langchain_core.messages import SystemMessage, HumanMessage

# =============================================================================
# Logging Configuration
# =============================================================================
# Determine the output directory relative to this script's location.
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT_DIR = os.path.join(
    os.path.expanduser("~"),
    "Analytical-Data-Science-Programmer-Coding-Assessment",
    "question_4_llm",
)
os.makedirs(OUTPUT_DIR, exist_ok=True)

LOG_FILE = os.path.join(OUTPUT_DIR, "clinical_trial_agent.log")

# Configure root logger to write to both file and console
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler(LOG_FILE, mode="w", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
logger = logging.getLogger(__name__)

# =============================================================================
# Schema Description (System Prompt Context)
# =============================================================================
# This constant is injected into the LLM system prompt so the model
# understands the column semantics and mapping rules.

SCHEMA_DESCRIPTION = """
You are a clinical trial data assistant. The dataset is an Adverse Events (AE) dataset
with the following relevant columns:
- USUBJID: Unique Subject Identifier (e.g., "AB12345-001"). Used to identify and count patients.
- AETERM: Reported Term for the Adverse Event (e.g., "HEADACHE", "NAUSEA", "DIZZINESS").
  This is the specific adverse event name as reported.
- AESEV: Severity/Intensity of the Adverse Event. Values: "MILD", "MODERATE", "SEVERE".
- AESOC: System Organ Class — the body system category (e.g., "CARDIAC DISORDERS",
  "SKIN AND SUBCUTANEOUS TISSUE DISORDERS", "NERVOUS SYSTEM DISORDERS",
  "GASTROINTESTINAL DISORDERS").
- AESER: Serious Adverse Event flag. Values: "Y" (yes, serious) or "N" (no, not serious).
- AEREL: Causality / Relationship to study drug. Values include: "RELATED", "NOT RELATED",
  "POSSIBLY RELATED", "PROBABLY RELATED".
- AEACN: Action Taken with Study Treatment. Values include: "DOSE NOT CHANGED",
  "DRUG WITHDRAWN", "DOSE REDUCED", "DOSE INCREASED", "NOT APPLICABLE".
- AESTDTC: Start Date/Time of the Adverse Event (character date).
- AEENDTC: End Date/Time of the Adverse Event (character date).
IMPORTANT RULES for mapping user questions:
- "severity" or "intensity" or "mild/moderate/severe" → target_column = "AESEV"
- A specific condition name (e.g., "headache", "nausea", "rash") → target_column = "AETERM"
- A body system (e.g., "cardiac", "skin", "nervous system", "gastrointestinal") → target_column = "AESOC"
- "serious" or "SAE" → target_column = "AESER"
- "related" or "causality" or "relationship" → target_column = "AEREL"
- "action taken" or "drug withdrawn" or "dose reduced" → target_column = "AEACN"
"""


# =============================================================================
# Agent Class
# =============================================================================
class ClinicalTrialDataAgent:
    """
    Translates natural language questions about adverse events
    into structured JSON queries, then executes them on a Pandas DataFrame.

    Attributes:
        df (pd.DataFrame): The AE dataset with string columns uppercased.
        llm (ChatOpenAI): LangChain LLM client for question parsing.

    Usage:
        >>> agent = ClinicalTrialDataAgent(df)
        >>> result = agent.ask("Which patients had severe AEs?")
    """

    def __init__(self, dataframe: pd.DataFrame):
        self.df = dataframe.copy()

        # Normalize string columns to uppercase for consistent matching
        str_cols = ["AETERM", "AESEV", "AESOC", "AESER", "AEREL", "AEACN"]
        for col in str_cols:
            if col in self.df.columns:
                self.df[col] = self.df[col].astype(str).str.upper().str.strip()

        self.llm = ChatOpenAI(
            model="gpt-4o-mini",
            temperature=0,
            openai_api_key=os.getenv("OPENAI_API_KEY"),
        )

    # -------------------------------------------------------------------------
    # Parsing: Natural language → structured JSON
    # -------------------------------------------------------------------------
    def parse_question(self, question: str) -> dict:
        """
        Send the user's natural language question to the LLM.
        Returns a structured dict with 'target_column' and 'filter_value'.

        Args:
            question: Free-text clinical question from the user.

        Returns:
            dict with keys 'target_column' (str) and 'filter_value' (str).

        Raises:
            json.JSONDecodeError: If the LLM response is not valid JSON.
        """
        system_prompt = f"""{SCHEMA_DESCRIPTION}
Given a user's natural language question, extract the following as JSON:
{{
    "target_column": "<the column name to filter on>",
    "filter_value": "<the value to filter for, UPPERCASED>"
}}
Rules:
- filter_value must be UPPERCASED.
- For AESOC, infer the full standard SOC name (e.g., "cardiac" → "CARDIAC DISORDERS").
- For AESER, map "serious" → "Y" and "not serious" / "non-serious" → "N".
- Return ONLY valid JSON. No explanation, no markdown fences.
"""
        messages = [
            SystemMessage(content=system_prompt),
            HumanMessage(content=question),
        ]

        response = self.llm.invoke(messages)
        raw = response.content.strip()

        # Strip markdown code fences if present
        if raw.startswith("```"):
            raw = raw.split("\n", 1)[1] if "\n" in raw else raw[3:]
            if raw.endswith("```"):
                raw = raw[:-3]
            raw = raw.strip()

        parsed = json.loads(raw)
        return parsed

    # -------------------------------------------------------------------------
    # Execution: Structured filter → DataFrame query
    # -------------------------------------------------------------------------
    def execute_query(self, parsed: dict) -> dict:
        """
        Apply the structured filter to the DataFrame.
        Returns the count of unique subjects and their IDs.

        Args:
            parsed: Dict with 'target_column' and 'filter_value' keys.

        Returns:
            dict with 'subject_count', 'subject_ids', and filter metadata.
            If the column is not found, returns an 'error' key instead.
        """
        target_column = parsed["target_column"].upper().strip()
        filter_value = parsed["filter_value"].upper().strip()

        if target_column not in self.df.columns:
            return {
                "error": f"Column '{target_column}' not found in dataset.",
                "available_columns": list(self.df.columns),
            }

        # Use 'contains' for AESOC and AETERM to handle partial matches
        if target_column in ("AESOC", "AETERM"):
            mask = self.df[target_column].str.contains(filter_value, case=False, na=False)
        else:
            mask = self.df[target_column] == filter_value

        matched = self.df.loc[mask]
        unique_subjects = sorted(matched["USUBJID"].unique().tolist())

        return {
            "target_column": target_column,
            "filter_value": filter_value,
            "subject_count": len(unique_subjects),
            "subject_ids": unique_subjects,
        }

    # -------------------------------------------------------------------------
    # End-to-end interface
    # -------------------------------------------------------------------------
    def ask(self, question: str) -> dict:
        """
        End-to-end: natural language question → parsed intent → query result.

        Args:
            question: Free-text clinical question.

        Returns:
            dict with query results (subject_count, subject_ids, etc.).
        """
        print(f"\n{'='*70}")
        print(f"QUESTION : {question}")
        print(f"{'='*70}")

        # Step 1: LLM parsing
        parsed = self.parse_question(question)
        print(f"LLM OUTPUT: {json.dumps(parsed, indent=2)}")

        # Step 2: Pandas execution
        result = self.execute_query(parsed)
        print(f"RESULT    : {result['subject_count']} unique subject(s) found")
        if result["subject_count"] > 0:
            print(f"SUBJECTS  : {result['subject_ids']}")

        return result


# =============================================================================
# TEST SCRIPT
# =============================================================================
if __name__ == "__main__":
    logger.info("=" * 70)
    logger.info("Clinical Trial Data Agent — Execution Start")
    logger.info("=" * 70)
    logger.info("Timestamp : %s", datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
    logger.info("Python    : %s", sys.version)
    logger.info("Log file  : %s", LOG_FILE)
    logger.info("")

    try:
        # Load environment variables
        load_dotenv()

        # Load data
        df = pd.read_csv("adae.csv")
        print(f"Dataset loaded: {df.shape[0]} rows, {df.shape[1]} columns")
        print(f"Columns: {list(df.columns)}\n")

        logger.info("Dataset loaded: %d rows, %d columns", df.shape[0], df.shape[1])
        logger.info("Columns: %s", list(df.columns))
        logger.info("")

        # Initialize agent
        agent = ClinicalTrialDataAgent(df)

        # --- Test Query 1: Severity-based ---
        result1 = agent.ask("Give me the subjects who had Adverse Events of Moderate severity")

        # --- Test Query 2: Specific condition (AETERM) ---
        result2 = agent.ask("Which patients experienced headache?")

        # --- Test Query 3: Body system (AESOC) ---
        result3 = agent.ask("How many subjects had cardiac adverse events?")

        # --- Summary ---
        print(f"\n{'='*70}")
        print("SUMMARY")
        print(f"{'='*70}")
        print(f"Query 1 (Moderate severity) : {result1['subject_count']} subjects")
        print(f"Query 2 (Headache)          : {result2['subject_count']} subjects")
        print(f"Query 3 (Cardiac)           : {result3['subject_count']} subjects")

        logger.info("")
        logger.info("=" * 70)
        logger.info("SUMMARY")
        logger.info("=" * 70)
        logger.info("Query 1 (Moderate severity) : %d subjects", result1["subject_count"])
        logger.info("Query 2 (Headache)          : %d subjects", result2["subject_count"])
        logger.info("Query 3 (Cardiac)           : %d subjects", result3["subject_count"])
        logger.info("")
        logger.info("=== Execution completed successfully ===")
        logger.info("Finished: %s", datetime.now().strftime("%Y-%m-%d %H:%M:%S"))

    except Exception as exc:
        logger.error("!!! ERROR encountered !!!")
        logger.error("Type    : %s", type(exc).__name__)
        logger.error("Message : %s", str(exc))
        logger.error("Timestamp: %s", datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
        raise