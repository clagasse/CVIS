# Export key CVIS outputs as CSV files for publication/git upload
library(here)
source(file.path(here(), "code", "0_setup.R"))

library(dplyr)
library(readr)

cat("=========================================\n")
cat("EXPORTING KEY RESULTS TO CSV\n")
cat("=========================================\n")

# 1. Load scoring results
scoring_path <- file.path(paths$output, "scoring_results.Rdata")
if (file.exists(scoring_path)) {
  load(scoring_path)
  
  if (exists("scores_tidy_baseline")) {
    write_csv(scores_tidy_baseline, file.path(paths$output, "scores_tidy_baseline.csv"))
    cat("Saved: output/scores_tidy_baseline.csv\n")
  }
  
  if (exists("all_std_long_baseline")) {
    write_csv(all_std_long_baseline, file.path(paths$output, "all_std_long_baseline.csv"))
    cat("Saved: output/all_std_long_baseline.csv\n")
  }
} else {
  cat("Warning: scoring_results.Rdata not found!\n")
}

# 2. Load uncertainty analysis results
uncertainty_path <- file.path(paths$output, "uncertainty_analysis_results.Rdata")
if (file.exists(uncertainty_path)) {
  load(uncertainty_path)
  
  if (exists("cu_summary")) {
    write_csv(cu_summary, file.path(paths$output, "cu_summary.csv"))
    cat("Saved: output/cu_summary.csv\n")
  }
  
  if (exists("anova_unc")) {
    # Convert list/df to clean data frame
    write_csv(as.data.frame(anova_unc), file.path(paths$output, "anova_unc.csv"))
    cat("Saved: output/anova_unc.csv\n")
  }
} else {
  cat("Warning: uncertainty_analysis_results.Rdata not found!\n")
}

cat("=========================================\n")
cat("EXPORT COMPLETE!\n")
cat("=========================================\n")
