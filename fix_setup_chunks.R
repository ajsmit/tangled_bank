#!/usr/bin/env Rscript

# Script to fix R setup chunks that are incorrectly placed inside YAML headers
# They should be moved to immediately after the closing --- of the YAML header

# List of all files that need to be checked
files_to_check <- c(
  "/Users/ajsmit/Documents/R_local/tangled_bank/about.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Biostatistics_Assessment_Instructions_2025.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Biostatistics_Self-Assessment.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Biostats_Prac_Exam_2025.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Biostats_Theory_Test_2025.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Exam_2025_rewrite.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Final_Assessment_2023.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Final_Assessment_2024_Q&A.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Intro_R_Presentations.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Intro_R_Self-Assessment.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Intro_R_Test_2025.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Mid_Assessment_2023.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Prac_Exam_Rubric_2025.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Research_Project_2024.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Task_A.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Task_B.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Task_Bonus.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Task_C.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Task_D.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Task_E.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Task_F.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Task_G.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/BCB744_Task_H.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/examples/BCB744_BioStats_Example_1.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/examples/BCB744_Intro_R_Example_1.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/examples/BCB744_Intro_R_Example_2.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/assessments/Sheet.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/assessments/_05-spp_dissimilarity.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/assessments/BCB743_intgrative_assignment.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/assessments/Task_A1.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/assessments/Task_A2.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/assessments/Task_B.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/assessments/Task_C.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/assessments/Task_D.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/assessments/Task_E.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/assessments/Task_F.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/assessments/Task_G.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/BCB743_index.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/CA.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/cluster_analysis.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/constrained_ordination.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/correlations.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/DCA.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/deep_dive.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/dis-metrics.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/model_building.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/multiple_regression.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/nMDS_diatoms.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/nMDS.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/ordination.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/PCA_examples.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/PCA_SDG_example.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/PCA.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/PCoA.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/randomisation.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/review.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/two_oceans_appendices.qmd",
  "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/unconstrained-summary.qmd"
)

# Function to fix a single file
fix_file <- function(file_path) {
  if (!file.exists(file_path)) {
    cat("File does not exist:", file_path, "\n")
    return(FALSE)
  }
  
  # Read the file content
  content <- readLines(file_path, warn = FALSE)
  
  # Find the YAML header boundaries
  yaml_start <- which(content == "---")[1]
  yaml_ends <- which(content == "---")
  
  if (length(yaml_ends) < 2) {
    cat("Could not find complete YAML header in:", file_path, "\n")
    return(FALSE)
  }
  
  yaml_end <- yaml_ends[2]
  
  # Check if the setup chunk is inside YAML (between first and second ---)
  setup_lines <- which(grepl("```\\{r, setup, include=FALSE\\}", content))
  knit_lines <- which(grepl("knitr::opts_knit\\$set\\(root\\.dir = '~/Documents/R_local/tangled_bank'\\)", content))
  setup_end_lines <- which(content == "```" & 1:length(content) > max(setup_lines, 0))
  
  if (length(setup_lines) == 0 || length(knit_lines) == 0) {
    cat("No setup chunk found in:", file_path, "\n")
    return(FALSE)
  }
  
  setup_line <- setup_lines[1]
  knit_line <- knit_lines[1]
  setup_end_line <- setup_end_lines[setup_end_lines > setup_line][1]
  
  # Check if setup chunk is inside YAML
  if (setup_line > yaml_start && setup_end_line < yaml_end) {
    cat("Fixing:", file_path, "\n")
    
    # Remove setup chunk from inside YAML
    new_content <- content[-(setup_line:setup_end_line)]
    
    # Adjust yaml_end position after removal
    yaml_end_new <- yaml_end - (setup_end_line - setup_line + 1)
    
    # Add setup chunk after the closing ---
    setup_chunk <- c("", content[setup_line:setup_end_line])
    
    # Insert after yaml end
    final_content <- c(new_content[1:yaml_end_new], setup_chunk, new_content[(yaml_end_new + 1):length(new_content)])
    
    # Write back to file
    writeLines(final_content, file_path)
    return(TRUE)
  } else {
    cat("Setup chunk already correctly positioned in:", file_path, "\n")
    return(FALSE)
  }
}

# Count files processed
files_fixed <- 0
total_files <- length(files_to_check)

cat("Starting to process", total_files, "files...\n\n")

for (file in files_to_check) {
  if (fix_file(file)) {
    files_fixed <- files_fixed + 1
  }
}

cat("\nSummary:\n")
cat("Total files checked:", total_files, "\n")
cat("Files fixed:", files_fixed, "\n")
cat("Files already correct or skipped:", total_files - files_fixed, "\n")