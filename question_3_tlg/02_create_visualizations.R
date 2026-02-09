# ==============================================================================
# Study Name/Protocol: CDISCPILOT01 
# Purpose: Create AE Figures using ggplot2
# Input Files: pharmaverseadam::adsl, adae
# Reference Files: https://pharmaverse.github.io/cardinal/quarto/index-catalog.html
#             https://pharmaverse.github.io/examples/tlg/adverse_events.html
# Output Files: p1_aeseverity_barchart.png, p1_aeseverity_stackedbarchart.png,
#               p2_top10aes_overall.png, p2_top10aes_bytrt.png, q3_log2.txt
# Usage Notes: Created using R version 4.5.2
# Author: Karen Chiodo
# Date: 09 February 2026
#===============================================================================
library(logr)
# Set up log file 
log_file <- "question_3_tlg/q3_log2.txt"
sink(log_file, split = TRUE)

cat("=======================================================================\n")
cat("AE Graphs Creation\n")
cat("Start Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=====================================================================\n\n")

tryCatch({
  cat("Loading required packages...\n")
  # Load required packages 
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(pharmaverseadam)
  library(scales)
  
  # Load input data\
  cat("Loading input datasets...\n")
  # Filter to include only subjects marked as part of the safety population
  adsl <- pharmaverseadam::adsl %>%
    filter(SAFFL == "Y")
  adae <- pharmaverseadam::adae %>%
    filter(SAFFL == "Y")
  
  ###### PLOT 1: AE SEVERITY DISTRIBUTION BY TREATMENT ===========================
  cat("Creation of AE Severity Distribution Graphs...\n")
  
  # Prepare data for severity plot
  cat("Preparing data for severity plot...\n")
  severity_data <- adae %>%
    filter(!is.na(AESEV) & !is.na(ACTARM)) %>%
    count(ACTARM, AESEV) %>%
    group_by(ACTARM) %>%
    mutate(
      total = sum(n),
      pct = 100 * n / total
    ) %>%
    ungroup() %>%
    mutate(
      AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
    )
  
  # PLOT1_BC: Create grouped bar chart by treatment group
  cat("Creating grouped bar chart...\n")
  plot1_bc <- ggplot(severity_data, aes(x = ACTARM, y = n, fill = AESEV)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_text(
      aes(label = paste0(n, "\n(", round(pct, 1), "%)")),
      position = position_dodge(width = 0.7),
      vjust = -0.5,
      size = 3
    ) +
    scale_fill_manual(
      values = c("MILD" = "#90EE90", "MODERATE" = "#FFD700", "SEVERE" = "#FF6347"),
      name = "Severity"
    ) +
    labs(
      title = "Distribution of Adverse Events Severity by Treatment Group",
      subtitle = "Safety Population",
      x = "Treatment Group",
      y = "Number of Adverse Events",
      caption = "Note: Percentages represent proportion within each treatment group"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "right",
      panel.grid.major.x = element_blank(), 
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  
  # PLOT1_SBC: Create stacked bar chart by treatment group
  cat("Creating stacked bar chart...\n")
  plot1_sbc <- ggplot(
    severity_data,
    aes(x = ACTARM, y = n, fill = AESEV)
  ) +
    geom_bar(
      stat = "identity",
      position = "stack",
      width = 0.7
    ) +
    geom_text(
      aes(label = paste0(n, " (", round(pct, 1), "%)")),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    scale_fill_manual(
      values = c("MILD" = "#90EE90", "MODERATE" = "#FFD700", "SEVERE" = "#FF6347"),
      name = "Severity"
    ) +
    labs(
      title = "Distribution of Adverse Events Severity by Treatment Group",
      subtitle = "Safety Population",
      x = "Treatment Group",
      y = "Number of Adverse Events",
      caption = "Note: Percentages represent proportion within each treatment group"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      axis.text.x = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank()
    )
  
  ###### PLOT 2: TOP 10 MOST FREQUENT AES WITH 95% CI (FOREST PLOT)===============
  cat("Creation of Top 10 Most Frequent AEs with 95% CIs Plots...\n")
  
  # Clopper–Pearson (Exact) CI function
  cat("Function for creating CIs...\n")
  cp_ci <- function(x, n, conf_level = 0.95) {
    alpha <- 1 - conf_level
    lower <- ifelse(
      x == 0,
      0,
      qbeta(alpha / 2, x, n - x + 1)
    )
    upper <- ifelse(
      x == n,
      1,
      qbeta(1 - alpha / 2, x + 1, n - x)
    )
    data.frame(
      lower = 100 * lower,
      upper = 100 * upper
    )
  }
  
  # PLOT 2: Overall Incidence ----------------------------------------------------
  cat("Preparing data for Top 10 AEs with 95% CIs...\n")
  # Overall subject count (denominator)
  N_overall <- adsl %>%
    distinct(USUBJID) %>%
    nrow()
  
  # Overall AE incidence by term
  ae_overall <- adae %>%
    select(USUBJID, AETERM) %>%
    distinct() %>%
    count(AETERM, name = "n_events") %>%
    mutate(
      N = N_overall,
      incidence_rate = 100 * n_events / N
    )
  
  # Top 10 most frequent AEs
  ae_top10 <- ae_overall %>%
    arrange(desc(n_events)) %>%
    slice_head(n = 10)
  
  # Add Clopper–Pearson CIs
  ae_top10 <- ae_top10 %>%
    rowwise() %>%
    mutate(
      ci = list(cp_ci(n_events, N))
    ) %>%
    ungroup() %>%
    unnest(ci)
  
  # Order AE terms by incidence rate
  ae_top10 <- ae_top10 %>%
    arrange(incidence_rate) %>%
    mutate(
      AETERM = factor(AETERM, levels = AETERM)
    )
  
  # Plot Overall incidence with 95% CI
  cat("Creating Top 10 AEs with 95% CIs Plot...\n")
  plot2 <- ggplot(
    ae_top10,
    aes(x = AETERM, y = incidence_rate)
  ) +
    geom_point(size = 3, color = "#0072B2") +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      width = 0.3,
      linewidth = 0.8,
      color = "#0072B2"
    ) +
    labs(
      title = "Top 10 Most Frequent Adverse Events",
      subtitle = "Overall Incidence Rates with 95% Clopper-Pearscon CIs - Safety Population",
      x = "Adverse Event Term",
      y = "Incidence Rate (%)",
      caption = paste0(
        "Note: Bars represent 95% Clopper–Pearson confidence intervals\n",
        "Incidence rate = (Number of subjects with AE / Total subjects) × 100\n",
        "N = ", N_overall
      )
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.text.x = element_text(size = 9),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_flip()
  
  # PLOT 2: Incidence per Treatment Group ----------------------------------------
  cat("Preparing data for Top 10 AEs by Treatment with 95% CIs...\n")
  # Denominator per treatment group
  N_by_trt <- adsl %>%
    distinct(USUBJID, TRT01A) %>%
    count(TRT01A, name = "N")
  
  # AE incidence by treatment and term
  ae_by_trt <- adae %>%
    select(USUBJID, TRT01A, AETERM) %>%
    distinct() %>%
    count(TRT01A, AETERM, name = "n_events") %>%
    left_join(N_by_trt, by = "TRT01A") %>%
    mutate(
      incidence_rate = 100 * n_events / N
    )
  
  # Determine top 10 AEs by OVERALL incidence
  ae_overall_rank <- ae_by_trt %>%
    group_by(AETERM) %>%
    summarise(
      overall_events = sum(n_events),
      overall_N = sum(N),
      overall_incidence = 100 * overall_events / overall_N,
      .groups = "drop"
    ) %>%
    arrange(desc(overall_incidence)) %>%
    slice_head(n = 10)
  
  # Keep top 10 AEs and add Clopper–Pearson CIs
  ae_plot <- ae_by_trt %>%
    semi_join(ae_overall_rank, by = "AETERM") %>%
    rowwise() %>%
    mutate(
      ci = list(cp_ci(n_events, N))
    ) %>%
    ungroup() %>%
    unnest(ci)
  
  # Order AE terms by incidence rate
  ae_plot <- ae_plot %>%
    mutate(
      AETERM = factor(
        AETERM,
        levels = rev(ae_overall_rank$AETERM)
      )
    )
  
  # N caption per treatment
  N_caption <- N_by_trt %>%
    arrange(TRT01A) %>%
    mutate(trt_N = paste0(TRT01A, ": ", N)) %>%
    summarise(
      caption_text = paste(trt_N, collapse = " | ")
    ) %>%
    pull(caption_text)
  
  # Plot Incidence Rate by Treatment Group
  cat("Creating Top 10 AEs by Treatment with 95% CIs Plot...\n")
  plot2_trt <- ggplot(
    ae_plot,
    aes(x = AETERM, y = incidence_rate, color = TRT01A)
  ) +
    geom_point(
      position = position_dodge(width = 0.6),
      size = 3
    ) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      width = 0.3,
      linewidth = 0.8,
      position = position_dodge(width = 0.6)
    ) +
    scale_color_manual(
      values = c("#E69F00", "#56B4E9", "#009E73")
    ) +
    labs(
      title = "Top 10 Most Frequent Adverse Events by Treatment Group",
      subtitle = "Incidence Rates with 95% Clopper–Pearson CIs - Safety Population",
      x = "Adverse Event Term",
      y = "Incidence Rate (%)",
      color = "Treatment Group",
      caption = paste0(
        "Note: Error bars represent 95% Clopper–Pearson confidence intervals\n",
        "Incidence rate = (Subjects with AE / Total subjects in Treatment Group) × 100\n",
        "N per treatment: ", N_caption
      )
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.text.y = element_text(size = 9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_flip()
  
  ###### SAVING GRAPHS AND LOG FILE ===============================================
  
  # Save Plot 1 Outputs
  cat("Saving Plot 1 Outputs...\n")
  output_plot1bc <- "question_3_tlg/p1_aeseverity_barchart.png"
  ggsave(output_plot1bc, plot1_bc, width = 10, height = 6, dpi = 300)
  cat("  Plot 1 - Bar Chart saved to:", output_plot1bc, "\n\n")
  
  output_plot1sbc <- "question_3_tlg/p1_aeseverity_stackedbarchart.png"
  ggsave(output_plot1sbc, plot1_sbc, width = 10, height = 6, dpi = 300)
  cat("  Plot 1 - Stacked Bar Chart saved to:", output_plot1sbc, "\n\n")
  
  # Save Plot 2 Outputs
  cat("Saving Plot 2 Outputs...\n")
  output_plot2 <- "question_3_tlg/p2_top10aes_overall.png"
  ggsave(output_plot2, plot2, width = 10, height = 8, dpi = 300)
  cat("  Plot 2 Overall saved to:", output_plot2, "\n\n")
  
  output_plot2trt <- "question_3_tlg/p2_top10aes_bytrt.png"
  ggsave(output_plot2trt, plot2_trt, width = 10, height = 8, dpi = 300)
  cat("  Plot 2 by Treatment saved to:", output_plot2trt, "\n\n")
  
  # Log completion
  cat("=======================================================================\n")
  cat("All plots created successfully\n")
  cat("End Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
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
