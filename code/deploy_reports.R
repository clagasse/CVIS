# ==============================================================================
# CVIS Reports Builder and Deployer (code/deploy_reports.R)
#
# Description:
#   Compiles the Climate Vulnerability Indicators for Salmon (CVIS) supplemental
#   reports (6a and 6b) locally and copies the final HTML outputs and required
#   assets to the 'docs/' folder. This facilitates deploying the reports to
#   GitHub Pages (which can serve files out of '/docs' on the main branch).
#
# Usage:
#   Set the toggles below and run the script.
# ==============================================================================

# ==================== 0. Configurations ====================

# Toggles for what to build
build_6a <- TRUE       # Supplement S1: Description of Indicators
build_6b <- TRUE       # Supplement S2: CU Reports Dashboard
build_6e <- FALSE      # Supplement S3: Visual Results Overview

# Build option for speed vs completeness
# FALSE will compile only the first 2 CUs (great for quick testing/validation)
# TRUE will compile all 50 CUs (required for final production/deployment)
compile_all_cus <- TRUE

# Output destination directory (default is the workspace '/docs' directory)
docs_dir <- file.path(here::here(), "docs")

# ==================== 1. Initialization ====================

library(here)

# Load environment configuration and paths
if (!exists("paths")) {
  source(file.path(here::here(), "code", "0_setup.R"))
}

# Ensure the docs directory exists
if (!dir.exists(docs_dir)) {
  dir.create(docs_dir, showWarnings = FALSE, recursive = TRUE)
}

cat("\n======================================================\n")
cat("Starting CVIS Report Compilation & Deployment Script\n")
cat("Target Deployment Directory: ", docs_dir, "\n")
cat("======================================================\n\n")

# ==================== 2. Build Supplemental Report 6a ====================
if (build_6a) {
  cat("--- Building Supplemental Report 6a (S1 Indicators Description) ---\n")
  
  temp_out_file <- paste0(Sys.Date(), "_S1_indicators_report.html")
  
  # Render Rmd file to output/reports/
  rmarkdown::render(
    file.path(paths$code, "6a_S1_indicators_description.Rmd"),
    output_file = temp_out_file,
    output_dir = paths$reports,
    output_format = "html_document",
    envir = globalenv()
  )
  
  # Copy compiled HTML to docs folder with static filename
  src_html <- file.path(paths$reports, temp_out_file)
  dest_html <- file.path(docs_dir, "6a_S1_indicators_description.html")
  
  if (file.exists(src_html)) {
    file.copy(src_html, dest_html, overwrite = TRUE)
    cat("✓ Report 6a copied to:", dest_html, "\n\n")
  } else {
    warning("Could not find compiled Report 6a at ", src_html, "\n")
  }
}

# ==================== 3. Build Supplemental Report 6b ====================
if (build_6b) {
  cat("--- Building Supplemental Report 6b (S2 CU Reports Dashboard) ---\n")
  
  # Ensure target CU reports output directory exists
  cu_reports_dir <- file.path(paths$reports, "CU_reports")
  dir.create(cu_reports_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Load cu_run if not loaded
  if (!exists("cu_run")) {
    load(file.path(paths$CU, "cu_run.Rds"))
  }
  
  # Determine which CUs to compile
  if (compile_all_cus) {
    cus_to_compile <- cu_run$FULL_CU_IN
    cat("Compiling ALL", length(cus_to_compile), "Conservation Units. This may take some time...\n")
  } else {
    cus_to_compile <- cu_run$FULL_CU_IN[1:2]
    cat("Testing mode: Compiling only the first", length(cus_to_compile), "CUs (", paste(cus_to_compile, collapse = ", "), ").\n")
    cat("Set 'compile_all_cus <- TRUE' in the script to compile all CUs for production.\n")
  }
  
  # Compile individual reports
  for (CU_IN_i in cus_to_compile) {
    cat("Compiling HTML profile for:", CU_IN_i, "\n")
    default_rcp <- "45"
    default_period <- 3
    
    rmarkdown::render(
      file.path(paths$code, "6b_S2_CU_reports.Rmd"),
      output_file = paste0(CU_IN_i, "_CVIS_Data_report.html"),
      output_dir = cu_reports_dir,
      output_format = "html_document",
      output_options = list(self_contained = FALSE, lib_dir = file.path(cu_reports_dir, "libs")),
      params = list(
        FULL_CU_IN = CU_IN_i,
        default_rcp = default_rcp,
        default_period = default_period
      ),
      envir = globalenv()
    )
  }
  
  # Generate the master HTML dashboard combining the individual reports
  cat("Stitching individual reports into master dashboard...\n")
  source(file.path(paths$code, "6d_generate_dashboard.R"))
  
  # Destination directory inside docs for CU reports resources
  docs_cu_dir <- file.path(docs_dir, "CU_reports")
  dir.create(docs_cu_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 1. Copy the master dashboard HTML with static name
  src_dash <- file.path(paths$reports, "CVIS_CU_Supplemental_Report.html")
  dest_dash <- file.path(docs_dir, "6b_S2_CU_reports.html")
  
  if (file.exists(src_dash)) {
    file.copy(src_dash, dest_dash, overwrite = TRUE)
    cat("✓ Stitched dashboard copied to:", dest_dash, "\n")
  } else {
    warning("Could not find stitched dashboard at ", src_dash, "\n")
  }
  
  # 2. Copy the libs folder (contains JS/CSS libraries used by htmlwidgets)
  src_libs <- file.path(cu_reports_dir, "libs")
  dest_libs <- file.path(docs_cu_dir, "libs")
  
  if (dir.exists(src_libs)) {
    # Delete existing libs first to prevent merge issues
    if (dir.exists(dest_libs)) {
      unlink(dest_libs, recursive = TRUE, force = TRUE)
    }
    dir.create(dest_libs, showWarnings = FALSE, recursive = TRUE)
    file.copy(src_libs, docs_cu_dir, recursive = TRUE, overwrite = TRUE)
    cat("✓ HTML widget libraries copied to:", dest_libs, "\n")
  } else {
    warning("Could not find libs folder at ", src_libs, "\n")
  }
  
  # 3. Copy figure files directories (*_files)
  cat("Copying generated figure folders for each CU...\n")
  all_dirs <- list.dirs(cu_reports_dir, full.names = TRUE, recursive = FALSE)
  fig_dirs <- all_dirs[grepl("_files$", all_dirs)]
  
  for (f_dir in fig_dirs) {
    dir_name <- basename(f_dir)
    dest_f_dir <- file.path(docs_cu_dir, dir_name)
    
    if (dir.exists(dest_f_dir)) {
      unlink(dest_f_dir, recursive = TRUE, force = TRUE)
    }
    
    dir.create(dest_f_dir, showWarnings = FALSE, recursive = TRUE)
    file.copy(f_dir, docs_cu_dir, recursive = TRUE, overwrite = TRUE)
  }
  cat("✓ Figure files copied to:", docs_cu_dir, "\n\n")
}

# ==================== 4. Build Visual Results Overview (6e) ====================
if (build_6e) {
  cat("--- Building Visual Results Overview (6e) ---\n")
  
  temp_out_file <- "6e_CVIS_visual_overview.html"
  
  # Render Rmd file to output/reports/
  rmarkdown::render(
    file.path(paths$code, "6e_CVIS_visual_overview.Rmd"),
    output_file = temp_out_file,
    output_dir = paths$reports,
    output_format = "html_document",
    envir = globalenv()
  )
  
  # Copy compiled HTML to docs folder with static filename
  src_html <- file.path(paths$reports, temp_out_file)
  dest_html <- file.path(docs_dir, "6e_CVIS_visual_overview.html")
  
  if (file.exists(src_html)) {
    file.copy(src_html, dest_html, overwrite = TRUE)
    cat("✓ Report 6e copied to:", dest_html, "\n")
  } else {
    warning("Could not find compiled Report 6e at ", src_html, "\n")
  }
  
  # Copy files directory if generated (for figures/assets)
  src_files <- file.path(paths$reports, "6e_CVIS_visual_overview_files")
  dest_files <- file.path(docs_dir, "6e_CVIS_visual_overview_files")
  
  if (dir.exists(src_files)) {
    if (dir.exists(dest_files)) {
      unlink(dest_files, recursive = TRUE, force = TRUE)
    }
    dir.create(dest_files, showWarnings = FALSE, recursive = TRUE)
    file.copy(src_files, docs_dir, recursive = TRUE, overwrite = TRUE)
    cat("✓ Report 6e figures folder copied to:", dest_files, "\n\n")
  }
}

cat("======================================================\n")
cat("Deployment Sync Completed Successfully!\n")
cat("======================================================\n")
