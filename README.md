# Pharmaverse Coding Assessment

## Overview
This repository contains solutions to the Analytical Data Science Programmer Coding Assessment, demonstrating expertise in:
- **Pharmaverse Ecosystem**: SDTM and ADaM dataset creation using open-source R packages
- **Clinical Reporting**: Tables, Listings, and Graphs (TLGs) for regulatory submissions
- **Python & GenAI**: LLM-based clinical data assistant using LangChain
- **Best Practices**: Clean, reproducible, and well-documented code

## Repository Structure

```
pharmaverse-coding-assessment/
├── README.md                                # This file
├── question_1_sdtm/                         # SDTM DS Domain Creation
│   ├── 01_create_ds_domain.R                # Main script
│   ├── ds.rds                               # Resulting R dataset
│   ├── ds.csv                               # Resulting CSV dataset
│   ├── sdtm_ct.csv                          # Controlled terminologies
│   └── q1_log.txt                           # Execution log
├── question_2_adam/                         # ADaM ADSL Dataset Creation
│   ├── create_adsl.R                        # Main script
│   ├── adsl.rds                             # Resulting R dataset
│   ├── adsl.csv                             # Resulting CSV dataset
│   ├── adams-specs.xlsx                     # ADaM specification file
│   └── q2_log.txt                           # Execution log
├── question_3_tlg/                          # TLG - Adverse Events Reporting
│   ├── 01_create_ae_summary_table.R         # Summary table script
│   ├── 02_create_visualizations.R           # Visualization script
│   ├── ae_summary_table.html                # Table output
│   ├── p1_aeseverity_barchart.png           # Plot 1 - Group Bar Chart
│   ├── p1_aeseverity_stackedbarchart.png    # Plot 1 - Group Bar Chart
│   ├── p2_top10aes_overall.png              # Plot 2 Overall
│   ├── p2_top10aes_bytrt.png                # Plot 2 by Treatment
│   ├── q3_log1.txt                          # Execution log for Summary table
│   └── q3_log2.txt                          # Execution log for Visualization
├── question_4_python/                       # GenAI Clinical Data Assistant
│   ├── clinical_data_agent.py               # Main implementation
│   ├── test_agent.py                        # Test script
│   ├── requirements.txt                     # Python dependencies
│   └── q4_log.txt                           # Execution log
└── others/                                  # Other files (if needed)
```

## Question 1: SDTM DS Domain Creation

**Objective**: Create an SDTM Disposition (DS) domain dataset using `{sdtm.oak}` package.

**Input**: 
- Raw data: `pharmaverseraw::ds_raw`
- Study controlled terminology: `study_ct` dataframe

**Output Variables**: STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY

**Key Approach**:
- Utilized `{sdtm.oak}` functions for SDTM domain creation
- Applied controlled terminology mapping from `study_ct`
- Followed CDISC SDTM Implementation Guide v3.4 specifications
- Based on the AE domain example from Pharmaverse Examples

**Files**: See `question_1_sdtm/` folder

---

## Question 2: ADaM ADSL Dataset Creation

**Objective**: Create an ADSL (Subject Level) dataset using `{admiral}` and tidyverse tools.

**Input SDTM Domains**: 
- `pharmaversesdtm::dm` (Demographics - base for ADSL)
- `pharmaversesdtm::vs` (Vital Signs)
- `pharmaversesdtm::ex` (Exposure)
- `pharmaversesdtm::ds` (Disposition)
- `pharmaversesdtm::ae` (Adverse Events)

**Derived Variables**:
- **AGEGR9 & AGEGR9N**: Age grouping ("<18", "18 - 50", ">50")
- **TRTSDTM & TRTSTMF**: Treatment start date-time with time imputation
- **ITTFL**: Intent-to-Treat flag ("Y" if randomized)
- **LSTAVLDT**: Last known alive date from multiple sources

**Key Approach**:
- Used `{admiral}` derivation functions
- Implemented complex datetime imputation logic
- Combined data from multiple SDTM domains
- Followed ADaM Implementation Guide specifications

**Files**: See `question_2_adam/` folder

---

## Question 3: TLG - Adverse Events Reporting

**Objective**: Create regulatory-compliant adverse event summaries and visualizations.

**Input Datasets**:
- `pharmaverseadam::adae` (Adverse Events Analysis)
- `pharmaverseadam::adsl` (Subject Level Analysis)

**Deliverables**:
1. **Summary Table** (`{cards, gt}`):
   - Treatment-emergent AEs (TRTEMFL == "Y")
   - Counts and percentages by treatment group
   - HTML/DOCX/PDF output

2. **Visualizations** (`{ggplot2}`):
   - Plot 1: AE severity distribution by treatment
   - Plot 2: Top 10 most frequent AEs with 95% CI

**Key Approach**:
- Leveraged `{cards}` and `{gt}` for professional table formatting
- Created publication-ready visualizations with `{ggplot2}`
- Followed FDA TLG Catalogue standards
- Calculated confidence intervals for incidence rates

**Files**: See `question_3_tlg/` folder

---

## Question 4: GenAI Clinical Data Assistant (Bonus)

**Objective**: Develop a Generative AI assistant that translates natural language questions into structured Pandas queries.

**Technology Stack**:
- Python with Pandas for data manipulation
- LangChain for LLM integration
- OpenAI API (or mocked responses)

**Key Features**:
- Schema-aware question understanding
- Dynamic mapping of user intent to dataset variables
- Natural language to structured query translation
- Example queries:
  - "Give me subjects who had Adverse events of Moderate severity"
  - "Give me the list of subjects with severe headache."
  - "How many unique subjects experienced nausea?"

**Key Approach**:
- Defined data schema for LLM context
- Implemented `ClinicalTrialDataAgent` class
- Structured JSON output parsing
- Pandas query execution with subject counting

**Files**: See `question_4_python/` folder

---

## Installation & Setup

### R Environment
```r
# Install required packages
install.packages(c(
  "admiral", 
  "sdtm.oak", 
  "pharmaverseraw",
  "pharmaversesdtm",
  "pharmaverseadam",
  "dplyr", 
  "tidyr",
  "gtsummary",
  "gt",
  "ggplot2",
  "lubridate"
  "rlang"
  "stringr"
  "cards"
  "tibble"
  "tfrmt"
  "scales"
  "logr"
))
```

### Python Environment
```bash
cd question_4_python
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

---

## Running the Code

### Question 1
```r
source("question_1_sdtm/01_create_ds_domain.R")
```

### Question 2
```r
source("question_2_adam/create_adsl.R")
```

### Question 3
```r
source("question_3_tlg/01_create_ae_summary_table.R")
source("question_3_tlg/02_create_visualizations.R")
```

### Question 4
```bash
cd question_4_python
source .venv/bin/activate
python test_agent.py
```

---

## Key Learnings

1. **Pharmaverse Ecosystem**: Gained deep understanding of how `{sdtm.oak}` and `{admiral}` streamline clinical data programming through standardized functions and workflows.

2. **CDISC Standards**: Applied SDTM and ADaM implementation guides to create compliant datasets with proper variable derivations and controlled terminology.

3. **Complex Derivations**: Implemented sophisticated logic for datetime imputation, multi-source date derivations, and conditional flagging using tidyverse and admiral functions.

4. **Regulatory Reporting**: Created publication-ready outputs using modern R packages that meet FDA submission standards.

5. **GenAI Integration**: Explored practical applications of LLMs in clinical data analysis, enabling non-technical users to query complex datasets.

6. **Code Quality**: Emphasized clean code principles, comprehensive documentation, and reproducible workflows throughout the assessment.

---

## Challenges Overcome

- **Learning Curve**: Quickly familiarized myself with Pharmaverse packages and CDISC standards
- **Data Derivations**: Implemented complex multi-step derivations following strict specifications
- **LLM Integration**: Designed robust schema understanding and query translation logic
- **Visualization**: Created informative, publication-quality graphics with confidence intervals

---

## Tools & Technologies

- **R**: 4.2.5
- **Python**: 3.14.2
- **Key R Packages**: admiral, sdtm.oak, gt, gtsummary, ggplot2, dplyr, tidyr
- **Key Python Packages**: pandas, langchain, openai
- **Version Control**: Git/GitHub
- **AI Assistants**: Claude (Anthropic) for learning and problem-solving

---

## Acknowledgments

- **Pharmaverse Community**: For excellent documentation and examples
- **CDISC**: For comprehensive implementation guides
- **R in Pharma**: For educational videos and workshops
