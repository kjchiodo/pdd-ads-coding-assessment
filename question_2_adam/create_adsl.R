# ==============================================================================
# Study Name/Protocol: CDISCPILOT01 
# Purpose: Create an ADSL (Subject Level) dataset using SDTM source data
# Input Files: pharmaversesdtm::dm, vs, ex, ds, ae
# Reference Files: adam-specs.xlsx, 
# Output Files: adsl.rds, adsl.csv, q2_log.txt
# Usage Notes: Created using R version 4.5.2
# Author: Karen Chiodo
# Date: 08 February 2026
#===============================================================================
library(logr)
# Set up log file 
log_file <- "question_2_adam/q2_log.txt"
sink(log_file, split = TRUE)

cat("=======================================================================\n")
cat("ADSL Creation\n")
cat("Start Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=====================================================================\n\n")

tryCatch({
  ###### LOAD SDTM DATASETS ======================================================
  cat("Loading required packages...\n")
  # Load required packages
  library(logr)
  library(admiral)
  library(rlang)
  library(dplyr, warn.conflicts = FALSE)
  library(tidyr)
  library(lubridate)
  library(pharmaversesdtm)
  library(stringr)
  cat("Packages loaded successfully\n\n")
  
  cat("Loading SDTM datasets...\n")
  # Read in input SDTM data
  dm <- pharmaversesdtm::dm
  ds <- pharmaversesdtm::ds
  ex <- pharmaversesdtm::ex
  ae <- pharmaversesdtm::ae
  vs <- pharmaversesdtm::vs
  suppdm <- pharmaversesdtm::suppdm
  cat("  DM records:", nrow(dm), "\n")
  cat("  DS records:", nrow(ds), "\n")
  cat("  EX records:", nrow(ex), "\n")
  cat("  AE records:", nrow(ae), "\n")
  cat("  VS records:", nrow(vs), "\n")
  cat("  SUPPDM records:", nrow(suppdm), "\n\n")
  
  cat("Converting blanks to NA...\n")
  # Converting missing values to NA
  dm <- convert_blanks_to_na(dm)
  ds <- convert_blanks_to_na(ds)
  ex <- convert_blanks_to_na(ex)
  ae <- convert_blanks_to_na(ae)
  vs <- convert_blanks_to_na(vs)
  suppdm <- convert_blanks_to_na(suppdm)
  cat("Blank conversion complete\n\n")
  
  ###### START ADSL CREATION =====================================================
  cat("Starting ADSL creation...\n")
  
  # Start with DM as the basis for ADSL
  adsl <- dm %>%
    select(-DOMAIN)
  cat("Initial ADSL subjects:", nrow(adsl), "\n\n")
  
  
  cat("Deriving treatment variables...\n")
  # Derive Treatment Variables (TRT01P, TRT01A, TRT01PN, TRT01AN)
  adsl <- adsl %>%
    mutate(
      TRT01P = ARM,
      TRT01A = ACTARM,
      TRT01PN = case_when(
        grepl("PLACEBO", toupper(ARM)) ~ 1,
        grepl("LOW", toupper(ARM)) ~ 2,
        grepl("HIGH", toupper(ARM)) ~ 3,
        TRUE ~ NA_integer_
      ),
      TRT01AN = case_when(
        grepl("PLACEBO", toupper(ACTARM)) ~ 1,
        grepl("LOW", toupper(ACTARM)) ~ 2,
        grepl("HIGH", toupper(ACTARM)) ~ 3,
        TRUE ~ NA_integer_
      )
    )
  
  cat("Deriving age grouping...\n")
  # Derive Age Grouping (AGEGR9: "<18", "18 - 50", ">50"; AGEGR9N: 1, 2, 3)
  agegr9_lookup <- exprs(
    ~condition,              ~AGEGR9,   ~AGEGR9N,
    AGE < 18,                "<18",        1,
    between(AGE, 18, 50),    "18 - 50",    2,
    AGE > 50,                ">50",        3,
    is.na(AGE),              NA_character_, NA_integer_
  )
  
  adsl <- adsl %>%
    derive_vars_cat(agegr9_lookup)
  
  ###### DERIVATION OF TIMING VARIABLES===========================================
  cat("Deriving timing variables...\n")
  
  # Derive SCRFDT (Screen Failure Date) and FRVDT (Final Retrieval Visit Date)
  adsl <- adsl %>%
    mutate(
      SCRFDT = ifelse(
        TRT01A == "Screen Failure",
        format(as.Date(DMDTC), "%Y-%m-%d"),
        as.Date(NA_character_)
      ),
      FRVDT = pmax(
        as.Date(RFXENDTC, "%Y-%m-%d"),
        as.Date(RFPENDTC, "%Y-%m-%d"),
        as.Date(DTHDTC, "%Y-%m-%d"),
        na.rm = TRUE
      ) %>%
        format("%Y-%m-%d")
    )
  
  # Derive RANDDT (Date of Randomization) 
  adsl <- adsl %>%
    derive_vars_merged(
      dataset_add = ds,
      filter_add = DSDECOD == "RANDOMIZED",
      by_vars = exprs(STUDYID, USUBJID),
      new_vars = exprs(RANDDT = DSSTDTC)
    ) %>%
    mutate(
      RANDDT = as.Date(RANDDT)
    )
  
  # Derive TRTSDTM, TRTSTMF, TRTEDTM, TRTEDMF
  # Valid dose function
  valid_dose <- function(exdose, extrt) {
    exdose > 0 | (exdose == 0 & str_detect(toupper(extrt), "PLACEBO"))
  }
  
  # Date completeness check (YYYY-MM-DD present)
  date_complete <- function(dtc) {
    !is.na(dtc) & str_detect(dtc, "^\\d{4}-\\d{2}-\\d{2}")
  }
  
  # Check if only seconds missing
  seconds_only_missing <- function(dtc) {
    str_detect(dtc, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}$")
  }
  
  #Create EX Datetime variables
  ex_ext <- ex %>%
    derive_vars_dtm(
      dtc = EXSTDTC,
      new_vars_prefix = "EXST",
      time_imputation = "first"
    ) %>%
    derive_vars_dtm(
      dtc = EXENDTC,
      new_vars_prefix = "EXEN",
      time_imputation = "last"
    )
  
  # Derive TRTSDTM and TRTSTMF
  adsl <- adsl %>%
    derive_vars_merged(
      dataset_add = ex_ext,
      by_vars = exprs(STUDYID, USUBJID),
      filter_add =
        valid_dose(EXDOSE, EXTRT) &
        date_complete(EXSTDTC) &
        !is.na(EXSTDTM),
      new_vars = exprs(
        TRTSDTM = EXSTDTM,
        TRTSTMF = if_else(
          seconds_only_missing(EXSTDTC),
          NA_character_,
          EXSTTMF
        )
      ),
      order = exprs(EXSTDTM, EXSEQ),
      mode = "first"
    )
  
  # Derive TRTEDTM and TRTETMF      
  adsl <- adsl %>%
    derive_vars_merged(
      dataset_add = ex_ext,
      by_vars = exprs(STUDYID, USUBJID),
      filter_add =
        valid_dose(EXDOSE, EXTRT) &
        date_complete(EXENDTC) &
        !is.na(EXENDTM),
      new_vars = exprs(
        TRTEDTM = EXENDTM,
        TRTETMF = EXENTMF
      ),
      order = exprs(EXENDTM, EXSEQ),
      mode = "last"
    )
  
  # Derive TRTSDT and TRTEDT
  adsl <- adsl %>%
    derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM))
  
  # Derive TRTDURD
  adsl <- adsl %>%
    derive_var_trtdurd()
  
  ###### DERIVATION OF END OF STUDY VARIABLES ====================================
  cat("Deriving end of study variables...\n")
  
  # Derive EOSDT
  ds_ext <- derive_vars_dt(
    ds,
    dtc = DSSTDTC,
    new_vars_prefix = "DSST"
  )
  
  adsl <- adsl %>%
    derive_vars_merged(
      dataset_add = ds_ext,
      by_vars = exprs(STUDYID, USUBJID),
      new_vars = exprs(EOSDT = DSSTDT),
      filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
    )
  
  # Derive EOSSTT
  format_eosstt <- function(x) {
    case_when(
      x %in% c("COMPLETED") ~ "COMPLETED",
      x %in% c("SCREEN FAILURE") ~ NA_character_,
      TRUE ~ "DISCONTINUED"
    )
  }
  
  adsl <- adsl %>%
    derive_vars_merged(
      dataset_add = ds,
      by_vars = exprs(STUDYID, USUBJID),
      filter_add = DSCAT == "DISPOSITION EVENT",
      new_vars = exprs(EOSSTT = format_eosstt(DSDECOD)),
      missing_values = exprs(EOSSTT = "ONGOING")
    )
  
  # Derive DCSREAS and DCSREASP
  adsl <- adsl %>%
    derive_vars_merged(
      dataset_add = ds,
      by_vars = exprs(USUBJID),
      new_vars = exprs(DCSREAS = DSDECOD, DCSREASP = DSTERM),
      filter_add = DSCAT == "DISPOSITION EVENT" &
        !(DSDECOD %in% c("SCREEN FAILURE", "COMPLETED", NA))
    )
  
  ##### DERIVATION OF DEATH VARIABLES ============================================
  cat("Deriving death variables...\n")
  
  # Derive DTHDT
  adsl <- adsl %>%
    derive_vars_dt(
      new_vars_prefix = "DTH",
      dtc = DTHDTC
    )
  
  # Derive DTHDF
  adsl <- adsl %>%
    mutate(
      DTHDTF = case_when(
        # Complete date → no flag
        stringr::str_detect(DTHDTC, "^\\d{4}-\\d{2}-\\d{2}$") ~ NA_character_,
        # Missing day → Day imputed
        stringr::str_detect(DTHDTC, "^\\d{4}-\\d{2}$") ~ "D",
        # Missing month + day → Month/Day imputed
        stringr::str_detect(DTHDTC, "^\\d{4}$") ~ "M",
        TRUE ~ NA_character_
      )
    )
  
  # Derive DTHCAUS
  adsl <- adsl %>%
    derive_vars_extreme_event(
      by_vars = exprs(STUDYID, USUBJID),
      events = list(
        event(
          dataset_name = "ae",
          condition = AEOUT == "FATAL",
          set_values_to = exprs(DTHCAUS = AEDECOD),
        ),
        event(
          dataset_name = "ds",
          condition = DSDECOD == "DEATH" & grepl("DEATH DUE TO", DSTERM),
          set_values_to = exprs(DTHCAUS = DSTERM),
        )
      ),
      source_datasets = list(ae = ae, ds = ds),
      tmp_event_nr_var = event_nr,
      order = exprs(event_nr),
      mode = "first",
      new_vars = exprs(DTHCAUS)
    )
  
  # Derive DTHCAUS, DTHDOM, DTHSEQ
  adsl <- adsl %>%
    select(-DTHCAUS) %>% # Remove it before deriving it again
    derive_vars_extreme_event(
      by_vars = exprs(STUDYID, USUBJID),
      events = list(
        event(
          dataset_name = "ae",
          condition = AEOUT == "FATAL",
          set_values_to = exprs(DTHCAUS = AEDECOD, DTHDOM = "AE", DTHSEQ = AESEQ),
        ),
        event(
          dataset_name = "ds",
          condition = DSDECOD == "DEATH" & grepl("DEATH DUE TO", DSTERM),
          set_values_to = exprs(DTHCAUS = DSTERM, DTHDOM = "DS", DTHSEQ = DSSEQ),
        )
      ),
      source_datasets = list(ae = ae, ds = ds),
      tmp_event_nr_var = event_nr,
      order = exprs(event_nr),
      mode = "first",
      new_vars = exprs(DTHCAUS, DTHDOM, DTHSEQ)
    )
  
  # Derive DTHCGRP1
  adsl <- adsl %>%
    mutate(DTHCGR1 = case_when(
      is.na(DTHDOM) ~ NA_character_,
      DTHDOM == "AE" ~ "ADVERSE EVENT",
      str_detect(DTHCAUS, "(PROGRESSIVE DISEASE|DISEASE RELAPSE)") ~ "PROGRESSIVE DISEASE",
      TRUE ~ "OTHER"
    ))
  
  # Derive DTHADY
  adsl <- adsl %>%
    derive_vars_duration(
      new_var = DTHADY,
      start_date = TRTSDT,
      end_date = DTHDT
    )
  
  # Derive LDDTHELD
  adsl <- adsl %>%
    derive_vars_duration(
      new_var = LDDTHELD,
      start_date = TRTEDT,
      end_date = DTHDT,
      add_one = FALSE
    )
  
  # Derive LDDTHGR1
  adsl <- adsl %>%
    mutate(
      LDDTHGR1 = case_when(
        is.na(LDDTHELD) ~ NA_character_,
        LDDTHELD <= 30 ~ "<=30",
        LDDTHELD > 30 ~ ">30",
        TRUE ~ NA_character_
      )
    )
  
  # Derive DTH30FL
  adsl <- adsl %>%
    mutate(
      DTH30FL = if_else(!is.na(DTHDT) & !is.na(LDDTHELD) & LDDTHELD <= 30, "Y", NA_character_)
    )
  
  # Derive DTHA30FL 
  adsl <- adsl %>%
    mutate(
      DTHA30FL = if_else(!is.na(DTHDT) & !is.na(LDDTHELD) & LDDTHELD > 30, "Y", NA_character_)
    )
  
  # Derive DTHB30FL
  adsl <- adsl %>%
    mutate(
      DTHB30FL = if_else(!is.na(DTHDT) & !is.na(DTHADY) & DTHADY <= 30, "Y", NA_character_)
    )
  
  ###### DERIVATION OF LSTALVDT===================================================
  cat("Deriving last known alive date...\n")
  
  # Date complete function
  date_complete <- function(dtc) {
    !is.na(dtc) & stringr::str_detect(dtc, "^\\d{4}-\\d{2}-\\d{2}")
  }
  
  # Pre-Derive VS Dates
  vs_ext <- vs %>%
    filter(
      !(is.na(VSSTRESN) & is.na(VSSTRESC)),
      date_complete(VSDTC)
    ) %>%
    mutate(
      VSDT = convert_dtc_to_dt(VSDTC, highest_imputation = "M")
    )
  
  # Pre-Derive AE Dates
  ae_ext <- ae %>%
    filter(date_complete(AESTDTC)) %>%
    mutate(
      AESTDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "M")
    )
  # Pre-Derive DS Dates
  ds_ext <- ds %>%
    filter(date_complete(DSSTDTC)) %>%
    mutate(
      DSSTDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "M")
    )
  
  # Derivation
  adsl <- adsl %>%                         # See programming notes for LSTAVLDT
    derive_vars_extreme_event(
      by_vars = exprs(STUDYID, USUBJID),
      events = list(
        event(
          dataset_name = "vs",             # Vitals complete
          order = exprs(VSDT, VSSEQ),
          set_values_to = exprs(
            LSTALVDT = VSDT,
            seq = VSSEQ
          )
        ),
        event(
          dataset_name = "ae",              # AE onset complete
          order = exprs(AESTDT, AESEQ),
          set_values_to = exprs(
            LSTALVDT = AESTDT,
            seq = AESEQ
          )
        ),
        event(
          dataset_name = "ds",               # Disposition complete
          order = exprs(DSSTDT, DSSEQ),
          set_values_to = exprs(
            LSTALVDT = DSSTDT,
            seq = DSSEQ
          )
        ),
        event(
          dataset_name = "adsl",             # Treatment complete
          condition = !is.na(TRTEDT),
          set_values_to = exprs(
            LSTALVDT = TRTEDT,
            seq = 0
          )
        )
      ),
      source_datasets = list(
        vs = vs_ext,
        ae = ae_ext,
        ds = ds_ext,
        adsl = adsl
      ),
      tmp_event_nr_var = event_nr,
      order = exprs(LSTALVDT, seq, event_nr),
      mode = "last",
      new_vars = exprs(LSTALVDT)
    )
  
  
  ###### DERIVATION OF POPULATION FLAGS===========================================
  cat("Deriving population flags...\n")
  
  # ITTFL: Intent-to-Treat Flag                           
  # Set to "Y" if ARM is not missing, otherwise "N"
  adsl <- adsl %>%
    mutate(
      ITTFL = if_else(!is.na(ARM), "Y", "N")
    )
  
  # SAFFL: Safety Population Flag
  # Set to "Y" if patient received at least one dose of study treatment
  # (i.e., TRTSDT is not missing), otherwise "N"
  adsl <- adsl %>%
    mutate(
      SAFFL = if_else(!is.na(TRTSDT), "Y", "N")
    )
  
  # RANDFL: Randomized Population Flag
  # Set to "Y" if patient was randomized (RANDDT is not missing), otherwise "N"
  adsl <- adsl %>%
    mutate(
      RANDFL = if_else(!is.na(RANDDT), "Y", "N")
    )
  
  # FASFL: Full Analysis Set Flag
  # Typically same as ITTFL unless specified differently in SAP
  adsl <- adsl %>%
    mutate(
      FASFL = ITTFL
    )
  
  # COMPLFL: Completers Population Flag
  # Set to "Y" if patient completed the study, otherwise "N"
  adsl <- adsl %>%
    derive_var_merged_exist_flag(
      dataset_add = ds,
      by_vars = exprs(STUDYID, USUBJID),
      new_var = COMPLFL,
      condition = DSDECOD == "COMPLETED",
      false_value = "N",
      missing_value = "N"
    )
  
  
  # DISCONFL: Discontinued Flag
  # Set to "Y" if patient discontinued from study
  adsl <- adsl %>%
    derive_var_merged_exist_flag(
      dataset_add = ds,
      by_vars = exprs(STUDYID, USUBJID),
      new_var = DISCONFL,
      condition = DSDECOD != "COMPLETED" & DSCAT == "DISPOSITION EVENT",
      false_value = "N",
      missing_value = "N"
    )
  
  ###### APPLYING VARIABLE LABELS ================================================
  cat("Applying variable labels...\n")
  
  variable_labels <- list(
    STUDYID = "Study Identifier",
    USUBJID = "Unique Subject Identifier",
    SUBJID = "Subject Identifier for the Study",
    SITEID = "Study Site Identifier",
    COUNTRY = "Country",
    RFSTDTC = "Subject Reference Start Date/Time",
    RFENDTC = "Subject Reference End Date/Time",
    RFXSTDTC = "Date/Time of First Study Treatment",
    RFXENDTC = "Date/Time of Last Study Treatment",
    RFPENDTC = "Date/Time of End of Participation",
    SCRFDT = "Screen Failure Date",
    FRVDT = "Final Retrieval Visit Date",
    DTHDTC = "Date/Time of Death",
    DTHDT = "Date of Death",
    DTHADY = "Relative Day of Death",
    DTHFL = "Subject Death Flag",
    LDDTHELD = "Elapsed Days from Last Dose to Death",
    LDDTHGR1 = "Last Dose to Death - Days Elapsed Grp 1",
    DTHCAUS = "Cause of Death",
    DTHCGR1 = "Cause of Death Reason 1",
    DTHDOM = "Domain for Date of Death Collection",
    DTHSEQ = "Sequence Number",
    DTHDTF = "Date of Death Imputation Flag",
    LDDTHGR1 = "Last Dose to Death - Days Elapsed Grp 1",
    DTH30FL = "Death Within 30 Days of Last Trt Flag",
    DTHA30FL = "Death After 30 Days from Last Trt Flag",
    DTHB30FL = "Death Within 30 Days of First Trt Flag",
    RGID001 = "Geographic Region 1",
    DMDTC = "Date/Time of Collection",
    DMDY = "Study Day of Collection",
    AGE = "Age",
    AGEU = "Age Units",
    AGEGR1 = "Pooled Age Group 1",
    SEX = "Sex",
    RACE = "Race",
    RACEGR1 = "Pooled Race Group 1",
    ETHNIC = "Ethnicity",
    SAFFL = "Safety Population Flag",
    ARM = "Description of Planned Arm",
    ARMCD = "Planned Arm Code",
    ACTARM = "Description of Actual Arm",
    ACTARMCD = "Actual Arm Code",
    TRT01P = "Planned Treatment for Period 01",
    TRT01A = "Actual Treatment for Period 01",
    TRT01PN = "Planned Treatment for Period 01 (N)",
    TRT01AN = "Actual Treatment for Period 01 (N)",
    TRTSDT = "Date of First Exposure to Treatment",
    TRTSDTM = "Datetime of First Exposure to Treatment",
    TRTSTMF = "Time of First Exposure Imput. Flag",
    TRTEDT = "Date of Last Exposure to Treatment",
    TRTEDTM = "Datetime of Last Exposure to Treatment",
    TRTETMF = "Time of Last Exposure Imput. Flag",
    TRTDURD = "Total Treatment Duration (Days)",
    EOSDT = "End of Study Date",
    EOSSTT = "End of Study Status",
    RFICDT = "Date/Time of Informed Consent",
    RANDDT = "Date of Randomization",
    LSTALVDT = "Date Last Known Alive",
    ITTFL = "Intent-to-Treat Population Flag",
    RANDFL = "Randomized Population Flag",
    FASFL = "Full Analysis Set Population Flag",
    COMPLFL = "Completers Population Flag",
    DISCONFL = "Discontinued Flag",
    DCSREAS = "Reason for Discontinuation",
    DCSREASP = "Reason for Disc from Study (Spec)",
    AGEGR9 = "Pooled Age Group 9",
    AGEGR9N = "Pooled Age Group 9 (N)"
  )
  
  # Apply labels to variables that exist in the dataset
  for (var in names(variable_labels)) {
    if (var %in% names(adsl)) {
      attr(adsl[[var]], "label") <- variable_labels[[var]]
    }
  }
  
  ###### SAVING DATASET AND LOG FILE =============================================
  # Save the dataset
  cat("Saving ADSL dataset...\n")
  saveRDS(adsl, "question_2_adam/adsl.rds")
  write.csv(adsl, "question_2_adam/adsl.csv", row.names = FALSE)
  cat("Dataset saved successfully\n\n")
  
  # Print summary
  cat("Dataset Summary:\n")
  print(str(adsl))
  cat("\n")
  print(summary(adsl))
  
  cat("\n========================================\n")
  cat("Script completed successfully!\n")
  cat("Completion time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("========================================\n")
  
}, error = function(e) {
  cat("\n!!! ERROR OCCURRED !!!\n")
  cat("Error message:", conditionMessage(e), "\n")
  cat("========================================\n")
}, finally = {
  sink()
})


