# ==============================================================================
# Study Name/Protocol: CDISCPILOT01 
# Purpose: Create a summary table of TEAEs by SOC and PT
# Input Files: pharmaverseadam::adsl, adae
# Reference Files: https://pharmaverse.github.io/cardinal/quarto/index-catalog.html
#             https://pharmaverse.github.io/examples/tlg/adverse_events.html
# Output Files: ae_summary_table.html, q3_log1.txt
# Usage Notes: Created using R version 4.5.2
# Author: Karen Chiodo
# Date: 09 February 2026
#===============================================================================
library(logr)
# Set up log file 
log_file <- "question_3_tlg/q3_log1.txt"
sink(log_file, split = TRUE)

cat("=======================================================================\n")
cat("TEAE Table Creation\n")
cat("Start Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=====================================================================\n\n")

tryCatch({
  ###### LOAD ADAM DATASETS ======================================================
  cat("Loading required packages...\n")
  # Load required packages
  library(gt)
  library(gtsummary)
  library(dplyr)
  library(pharmaverseadam)
  library(cards)
  library(tibble)
  library(tfrmt)
  
  # Load input data 
  cat("Loading input datasets...\n")
  # Filter to include only subjects marked as part of the safety population
  adsl <- pharmaverseadam::adsl %>%
    filter(SAFFL == "Y")
  # Load adverse event data with treatment-emergent AE records
  adae <- pharmaverseadam::adae %>%
    filter(SAFFL == "Y" & TRTEMFL == "Y")
  
  cat("  - ADSL records (Safety Population):", nrow(adsl), "\n")
  cat("  - ADAE records (TEAEs):", nrow(adae), "\n\n")
  
  ###### DATA PREPARATION ========================================================
  
  # Create an ARD that stacks hierarchical data of adverse events
  cat("Creating adverse event ARD...\n")
  ae_ard <- ard_stack_hierarchical(
    data = adae,
    by = TRT01A,                       # Derived from ACTARM
    variables = c(AEBODSYS, AETERM),
    statistic = ~ c("n", "p"),         # Calculate count and percentage
    denominator = adsl,
    id = USUBJID,
    over_variables = TRUE,
    overall = TRUE
  )
  
  # Filter adae and adsl with trt01a set to "Total"
  cat("Creating Total column ARD...\n")
  adae2 <- adae |>
    mutate(TRT01A = "Total")
  adsl2 <- adsl |>
    mutate(TRT01A = "Total")
  
  ae2_ard <- ard_stack_hierarchical(
    data = adae2,
    by = TRT01A, 
    variables = c(AEBODSYS, AETERM),
    denominator = adsl2,
    statistic = ~ c("n", "p"),
    id = USUBJID,
    over_variables = TRUE,
    overall = TRUE
  ) |>
    filter(group2 == "TRT01A" | variable == "TRT01A") # filter to stats we need
  
  ###### TIDY FOR TABLE ==========================================================
  cat("Combining and reshaping data...\n")
  ae3_ard <- bind_ard(ae_ard, ae2_ard) |>
    # reshape the data
    shuffle_card(fill_hierarchical_overall = "TEAE") |>
    # transform group-level freqs/pcts into a singular "bigN" row
    prep_big_n(vars = "TRT01A") |>
    # for nested variables, fill any missing values with "Any event"
    prep_hierarchical_fill(vars = c("AEBODSYS", "AETERM"), fill = "Any event") |>
    mutate(TRT01A = ifelse(TRT01A == "Overall TRT01A", "Total", TRT01A))
  
  # Create ordering columns, sort by AEBODSYS
  cat("Creating ordering columns...\n")
  ordering_aebodsys <- ae3_ard |>
    filter(TRT01A == "Total", stat_name == "n", AETERM == "Any event") |>
    arrange(desc(stat)) |>
    mutate(ord1 = row_number()) |>
    select(AEBODSYS, ord1)
  
  # Sort by AETERM after AEBODSYS order
  ordering_aeterm <- ae3_ard |>
    filter(TRT01A == "Total", stat_name == "n") |>
    group_by(AEBODSYS) |>
    arrange(desc(stat)) |>
    mutate(ord2 = row_number()) |>
    select(AEBODSYS, AETERM, ord2)
  
  # Join on our ordering columns and keep required columns
  ae4_ard <- ae3_ard |>
    full_join(ordering_aebodsys, by = "AEBODSYS") |>
    full_join(ordering_aeterm, by = c("AEBODSYS", "AETERM")) |>
    select(AEBODSYS, AETERM, ord1, ord2, stat, stat_name, TRT01A)
  
  
  ####### FORMATTING THE TABLE FOR GT ==========================================
  
  # Get the big N values for headers
  cat("Extracting denominators for headers...\n")
  big_n <- ae4_ard |>
    filter(stat_name == "bigN") |>
    select(TRT01A, stat) |>
    distinct() |>
    deframe()
  
  # Format n (p%) for each treatment
  format_n_pct <- function(n, p) {
    case_when(
      is.na(p) | is.na(n) ~ "",
      p == 0 ~ "0 (0%)",
      p == 1 ~ sprintf("%d (100%%)", n),
      p >= 0.995 ~ sprintf("%d (>99%%)", n),
      p <= 0.01 ~ sprintf("%d (<1%%)", n),
      TRUE ~ sprintf("%d (%.1f%%)", n, p * 100)
    )
  }
  
  # Extract TEAE row - this goes at the very top
  cat("Preparing TEAE summary row...\n")
  teae_row <- ae4_ard |>
    filter(AETERM == "TEAE",
           stat_name %in% c("n", "p")) |>
    select(AEBODSYS, AETERM, TRT01A, stat_name, stat) |>
    pivot_wider(
      names_from = c(TRT01A, stat_name),
      values_from = stat
    ) |>
    mutate(
      AETERM = "Treatment Emergent AEs",
      Placebo = format_n_pct(Placebo_n, Placebo_p),
      `Xanomeline High Dose` = format_n_pct(`Xanomeline High Dose_n`, `Xanomeline High Dose_p`),
      `Xanomeline Low Dose` = format_n_pct(`Xanomeline Low Dose_n`, `Xanomeline Low Dose_p`),
      Total = format_n_pct(Total_n, Total_p),
      ord1 = -1,
      ord2 = 0,
      is_soc = TRUE
    ) |>
    select(AEBODSYS, AETERM, ord1, ord2, Placebo, `Xanomeline High Dose`, `Xanomeline Low Dose`, Total, is_soc)
  
  # Extract the SOC-level statistics (the "Any event" rows)
  cat("Preparing SOC-level statistics...\n")
  soc_stats <- ae4_ard |>
    filter(AETERM == "Any event",
           stat_name %in% c("n", "p")) |>
    select(AEBODSYS, TRT01A, stat_name, stat, ord1) |>
    pivot_wider(
      names_from = c(TRT01A, stat_name),
      values_from = stat
    ) |>
    mutate(
      AETERM = AEBODSYS,
      Placebo = format_n_pct(Placebo_n, Placebo_p),
      `Xanomeline High Dose` = format_n_pct(`Xanomeline High Dose_n`, `Xanomeline High Dose_p`),
      `Xanomeline Low Dose` = format_n_pct(`Xanomeline Low Dose_n`, `Xanomeline Low Dose_p`),
      Total = format_n_pct(Total_n, Total_p),
      ord2 = 0,
      is_soc = TRUE
    ) |>
    select(AEBODSYS, AETERM, ord1, ord2, Placebo, `Xanomeline High Dose`, `Xanomeline Low Dose`, Total, is_soc)
  
  # Prepare term-level data
  cat("Preparing term-level data...\n")
  ae_table_data <- ae4_ard |>
    filter(stat_name %in% c("n", "p"),
           AETERM != "Any event",
           AETERM != "TEAE") |>
    pivot_wider(
      id_cols = c(AEBODSYS, AETERM, ord1, ord2),
      names_from = c(TRT01A, stat_name),
      values_from = stat
    ) |>
    mutate(
      Placebo = format_n_pct(Placebo_n, Placebo_p),
      `Xanomeline High Dose` = format_n_pct(`Xanomeline High Dose_n`, `Xanomeline High Dose_p`),
      `Xanomeline Low Dose` = format_n_pct(`Xanomeline Low Dose_n`, `Xanomeline Low Dose_p`),
      Total = format_n_pct(Total_n, Total_p),
      is_soc = FALSE
    ) |>
    select(AEBODSYS, AETERM, ord1, ord2, Placebo, `Xanomeline High Dose`, `Xanomeline Low Dose`, Total, is_soc)
  
  # Combine rows
  cat("Combining all rows...\n")
  ae_combined <- bind_rows(teae_row, soc_stats, ae_table_data) |>
    arrange(ord1, ord2) |>
    select(-ord1, -ord2, -AEBODSYS)
  
  ###### CREATING THE TEAE TABLE ===================================================
  
  # Create TEAE table
  cat("Creating formatted table...\n")
  TEAE_table <- ae_combined |>
    gt() |>
    tab_header(
      title = "Table 14.3.X Summary of Treatment-Emergent Adverse Events (TEAEs)",
      subtitle = "by System Organ Class (SOC) and Preferred Term (PT) - Safety Population"
    ) |>
    cols_label(
      AETERM = md("**Primary System Organ Class**<br>&nbsp;&nbsp;&nbsp;Reported Term for the Adverse Event"),
      Placebo = md(sprintf("**Placebo**<br>N = %d", big_n["Placebo"])),
      `Xanomeline High Dose` = md(sprintf("**Xanomeline High Dose**<br>N = %d", big_n["Xanomeline High Dose"])),
      `Xanomeline Low Dose` = md(sprintf("**Xanomeline Low Dose**<br>N = %d", big_n["Xanomeline Low Dose"])),
      Total = md(sprintf("**Overall**<br>N = %d", big_n["Total"]))
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = AETERM,
        rows = is_soc == TRUE
      )
    ) |>
    tab_style(
      style = cell_text(indent = px(10)),  # Changed from 20 to 10
      locations = cells_body(
        columns = AETERM,
        rows = is_soc == FALSE
      )
    ) |>
    cols_hide(columns = is_soc) |>
    tab_footnote(
      footnote = "n (%)",
      locations = cells_column_labels(columns = c(Placebo, `Xanomeline High Dose`, `Xanomeline Low Dose`, Total))
    ) |>
    tab_options(
      table.font.size = px(11),
      heading.align = "center",
      heading.title.font.size = px(14),
      heading.subtitle.font.size = px(14),
      column_labels.border.top.color = "black",
      column_labels.border.top.width = px(2),
      column_labels.border.bottom.color = "black",
      table.border.bottom.color = "black",
      table.border.bottom.width = px(2),
      footnotes.font.size = px(9)
    ) |>
    cols_align(
      align = "center",
      columns = c(Placebo, `Xanomeline High Dose`, `Xanomeline Low Dose`, Total)
    ) |>
    cols_align(
      align = "left",
      columns = AETERM
    )
  
  # Display table
  print(TEAE_table)
  
  ###### SAVING TABLE AND LOG FILE ===============================================
  
  # Save as HTML file
  cat("\nSaving table to file...\n")
  output_file <- "question_3_tlg/ae_summary_table.html"
  TEAE_table |>
    gtsave(filename = output_file)
  
  cat("Table saved successfully to:", output_file, "\n")
  cat("\nEnd Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("=======================================================================\n")
  
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
