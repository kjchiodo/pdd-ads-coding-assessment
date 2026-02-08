# ==============================================================================
# Study Name/Protocol: CDISCPILOT01 
# Purpose: Create an SDTM Disposition (DS) domain dataset using sdtm.oak
# Input Files: pharmaverse::ds_raw, sdtm_ct.csv
# Reference Files: Subject_Disposition_aCRF.pdf
# Output Files: ds.rds, q1_log.txt
# Usage Notes: Created using R version 4.5.2
# Author: Karen Chiodo
# Date: 08 February 2026
#===============================================================================

###### LOAD RAW DATA AND CONTROLLED TERMINOLOGY FILE ===========================

# Load required packages
library(logr)
library(dplyr)
library(sdtm.oak)
library(admiral)
library(pharmaverseraw)

# Set up log file 
log_file <- "question_1_sdtm/q1_log.txt"
sink(log_file, split = TRUE)

cat("==============================================================================\n")
cat("SDTM DS Domain Creation\n")
cat("Start Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("==============================================================================\n\n")

tryCatch({
# Load input data --------------------------------------------------------------
cat("Loading input data...\n")

# Load raw disposition data
ds_raw <- pharmaverseraw::ds_raw
ds_raw <- convert_blanks_to_na(ds_raw)
cat("  - ds_raw loaded:", nrow(ds_raw), "records\n")

# Create study controlled terminology
study_ct <- read.csv(
    "/cloud/project/question_1_sdtm/sdtm_ct.csv",
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
    )
cat("  - study_ct created:", nrow(study_ct), "records\n\n")

###### PREPARATION OF VARIABLES FOR MAPPING ====================================
cat("Preparation of variables before mapping...\n")
# Creation of needed variables (DSSEQ, DSCAT, DSSTDY)

# DSSEQ      
ds_raw <- ds_raw %>%                            
    group_by(PATNUM) %>%
      mutate(DSSEQ = row_number()) %>%
    ungroup()

### DSCAT (Refer to the Subject Disposition aCRF for programming notes)
ds_raw <- ds_raw %>%            
    mutate(
      IT.DSDECOD = if_else(       # If OTHERSP is null, map IT.DSDECOD to DSDECOD.
        is.na(OTHERSP),           # If OTHERSP is not null, map the value to DSDECOD and DSTERM.
        IT.DSDECOD, 
        OTHERSP
      ),
      IT.DSTERM = if_else(        
        is.na(OTHERSP),
        IT.DSTERM,
        OTHERSP
      ),
      DSCAT = case_when(
        !is.na(OTHERSP) ~ "OTHER EVENT",       # DSCAT if OTHERSP is not null
        IT.DSDECOD == "Randomized" ~ "PROTOCOL MILESTONE",      #DSCAT if DSDECOD is Randomized
        TRUE ~ "DISPOSITION EVENT"             # DSCAT for other cases
      )
    )

### DSSTDY (and RFSTDTC to compute for the study days)
rfstdtc_derived <- ds_raw %>%
    filter(IT.DSDECOD == "Randomized") %>%
    select(PATNUM, RFSTDTC = IT.DSSTDAT)

ds_raw <- ds_raw %>%
    left_join(rfstdtc_derived, by = "PATNUM")

ds_raw <- ds_raw %>%
    mutate( 
      DSSTDT = as.Date(IT.DSSTDAT, format = "%m-%d-%Y"),  # Convert to Date format
      RFSTDT= as.Date(RFSTDTC, format = "%m-%d-%Y"),
      # Calculate DSSTDY following CDISC rules:
      # - If DSSTDT >= RFSTDT: DSSTDY = DSSTDT - RFSTDT + 1
      # - If DSSTDT < RFSTDT: DSSTDY = DSSTDT - RFSTDT
      DSSTDY = case_when(
        is.na(DSSTDT) | is.na(RFSTDT) ~ NA_integer_,
        DSSTDT >= RFSTDT ~ as.integer(DSSTDT - RFSTDT) + 1,
        DSSTDT < RFSTDT ~ as.integer(DSSTDT - RFSTDT)
      )
    ) %>%
    select(-DSSTDT, -RFSTDT)       # Remove temporary date columns

cat("  - Needed variables generated \n")

###### CREATION OF DS DOMAIN USING SDTM.OAK ====================================
cat("Creating DS domain...\n")

# Derive oak_id_vars
ds_raw <-ds_raw %>%
    generate_oak_id_vars(
      pat_var = "PATNUM",
      raw_src = "ds_raw"
    ) 

# Mapping of Variables
### Topic and Identifier Variables  
ds <- assign_no_ct(                         #DSTERM                 
          raw_dat = ds_raw,
          raw_var = "IT.DSTERM",
          tgt_var = "DSTERM",
          id_vars = oak_id_vars()
    ) %>%                                   #STUDYID, DOMAIN, USUBJID, DSSEQ
      dplyr::mutate( 
          STUDYID = ds_raw$STUDY,
          DOMAIN = "DS",
          USUBJID = paste0(ds_raw$STUDY, "-", ds_raw$PATNUM),
          DSSEQ = ds_raw$DSSEQ
    ) %>%                                   #DSDECOD
      assign_ct( 
          raw_dat = ds_raw,
          raw_var = "IT.DSDECOD",
          tgt_var = "DSDECOD",
          ct_spec = study_ct,
          ct_clst = "C66727",
          id_vars = oak_id_vars()
      ) %>%                                 #DSCAT
      assign_no_ct(                 
          raw_dat = ds_raw,
          raw_var = "DSCAT",
          tgt_var = "DSCAT",
          id_vars = oak_id_vars()
      ) %>%                                 #VISITNUM
      assign_ct(
          raw_dat = ds_raw,
          raw_var = "INSTANCE",
          tgt_var = "VISITNUM",
          ct_spec = study_ct,
          ct_clst = "VISITNUM",
          id_vars = oak_id_vars()
      ) %>%                                 #VISIT
      assign_ct(
          raw_dat = ds_raw,
          raw_var = "INSTANCE",
          tgt_var = "VISIT",
          ct_spec = study_ct,
          ct_clst = "VISIT",
          id_vars = oak_id_vars()
      ) %>%                                 #DSDTC
      assign_datetime(
          raw_dat = ds_raw,
          raw_var = "DSDTCOL",
          tgt_var = "DSDTC",
          raw_fmt = c("m-d-y"),
          id_vars = oak_id_vars()
      ) %>%                                 #DSSTDTC
      assign_datetime(
          raw_dat = ds_raw,
          raw_var = "IT.DSSTDAT",
          tgt_var = "DSSTDTC",
          raw_fmt = c("m-d-y"),
          id_vars = oak_id_vars()
      ) %>%
      assign_no_ct(                         #DSSTDY
          raw_dat = ds_raw,
          raw_var = "DSSTDY",
          tgt_var = "DSSTDY",
          id_vars = oak_id_vars()
      ) %>%
      dplyr::select("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", 
                    "DSTERM", "DSDECOD", "DSCAT", "VISITNUM", 
                    "VISIT", "DSDTC", "DSSTDTC", "DSSTDY")
cat("DS domain created with", nrow(ds), "records\n\n")


###### SAVING DATASET AND LOG FILE ====================================  
# Save the dataset
cat("Saving DS domain...\n")
saveRDS(ds, "question_1_sdtm/ds.rds")
write.csv(ds, "question_1_sdtm/ds.csv", row.names = FALSE)
cat("Dataset saved successfully\n\n")

# Print summary
cat("Dataset Summary:\n")
print(str(ds))
cat("\n")
print(summary(ds))

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