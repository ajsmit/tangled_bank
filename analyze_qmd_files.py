#!/usr/bin/env python3
"""
Comprehensive analysis of .qmd files to find setup chunks inside YAML headers.

This script will:
1. Read all .qmd files
2. Parse YAML headers and identify setup chunks
3. Categorize files as CORRECT, INCORRECT, or NO_SETUP
4. Generate detailed report
"""

import re
import os
from pathlib import Path

def analyze_qmd_file(file_path):
    """
    Analyze a single .qmd file to determine setup chunk placement.
    
    Returns:
        tuple: (category, details) where category is 'CORRECT', 'INCORRECT', or 'NO_SETUP'
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
    except UnicodeDecodeError:
        try:
            with open(file_path, 'r', encoding='latin-1') as f:
                content = f.read()
        except Exception as e:
            return 'ERROR', f'Could not read file: {e}'
    except Exception as e:
        return 'ERROR', f'Could not read file: {e}'
    
    # Check if file has YAML header (starts with ---)
    if not content.strip().startswith('---'):
        # No YAML header, check for setup chunks anywhere
        setup_pattern = r'```\{r[^}]*setup[^}]*\}'
        if re.search(setup_pattern, content, re.IGNORECASE):
            return 'CORRECT', 'No YAML header, setup chunk found elsewhere'
        else:
            return 'NO_SETUP', 'No YAML header, no setup chunk'
    
    # Find YAML header boundaries
    yaml_start = content.find('---')
    if yaml_start == -1:
        return 'NO_SETUP', 'No YAML header found'
    
    # Find the closing --- of YAML header
    yaml_end_pattern = r'\n---\s*\n'
    yaml_end_match = re.search(yaml_end_pattern, content[yaml_start + 3:])
    
    if not yaml_end_match:
        return 'ERROR', 'YAML header not properly closed'
    
    yaml_end_pos = yaml_start + 3 + yaml_end_match.end()
    
    # Extract YAML header content
    yaml_content = content[yaml_start:yaml_end_pos]
    
    # Extract content after YAML
    post_yaml_content = content[yaml_end_pos:]
    
    # Look for setup chunks in YAML header
    setup_pattern = r'```\{r[^}]*setup[^}]*\}'
    setup_in_yaml = re.search(setup_pattern, yaml_content, re.IGNORECASE)
    setup_after_yaml = re.search(setup_pattern, post_yaml_content, re.IGNORECASE)
    
    if setup_in_yaml:
        return 'INCORRECT', 'Setup chunk found inside YAML header'
    elif setup_after_yaml:
        return 'CORRECT', 'Setup chunk found after YAML header'
    else:
        return 'NO_SETUP', 'No setup chunk found in file'

def main():
    # List of all .qmd files to analyze
    files = [
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
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB743/unconstrained-summary.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/01-scientific-philosophy.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/02-summarise-and-describe.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/03-visualise.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/04-distributions.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/05-inference.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/06-assumptions.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/07-t_tests.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/08-anova.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/09-regressions.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/10-correlations.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/11-decision_guide.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/12-glance.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/13-confidence.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/basic_stats/14-transformations.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/BCB744_index.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/_02-github.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/01-RStudio.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/02-working-with-data.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/03-data-in-R.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/04-workflow.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/05-graphics.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/06-faceting.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/07-brewing.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/08-mapping.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/09-mapping_style.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/10-mapping_rnaturalearth.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/11-mapping_quakes.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/12-tidy.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/13-tidier.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/14-tidiest.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/15-recap.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/16-functions.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/17-base_r.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/18-dates.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/BDC223_FAQ.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/BDC223_index.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/L00-introduction.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/L01-worldmapper.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/L02-SA_V.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/L03-plant_stress.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/L04-carbon_cycle.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/L05-light.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/L06a-pigments_photosynthesis.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/L06b-jassby_platt.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/L07-chromatic_adaptation.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/L08a-nutrient_uptake.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/L08b-nutrients_michaelis_menten.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/Lab1_SA_V.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/Lab2_misc_calcs.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/Lab3_PI_curves.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC223/Lab4_nitrogen_uptake.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/assessments/BDC334_Class_Test_2_2025.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/assessments/BDC334_Class_tests_1.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/assessments/Prac_assessment_2025.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/BDC334_AI_Tutor.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/BDC334_index.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/BDC334_syllabus.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/BDC334-Lecture-Transcripts.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/IKS.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/Lab-01-introduction.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/Lab-02a-r_rstudio.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/Lab-02b-env_dist.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/Lab-03-biodiversity.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/Lab-04-biodiversity.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/Lec-01-introduction.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/Lec-02-ecosystems.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/Lec-03-gradients.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/Lec-04-biodiversity.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/Lec-06-unified-ecology.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/r_markdown_example.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/blog.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/blog/2023-11-13-basic-mhw-detect/index.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/blog/2023-11-22-run-lengths/index.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/blog/2023-11-23-heatwaver/index.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/index.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/ABNJ.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/AI4AI.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/assessment_theory.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/case_for_promotion.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/genAI.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/graduate_attributes.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/heatwaveR_publ.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/How_to_learn.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/kaggle_earthquakes.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/NRF_ratings.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/promotion_index.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/reproducible_research.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/research_grants.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/technology_infusion.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/pages/Transboundary_systems.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/resources/ecology_resources_web.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/resources/general_resources_web.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/resources/spatial_resources_web.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/alt_method.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/buffer_data_extract.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/chl_ERDDAP.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/chl_localisation.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/chl_sightings.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/download_earthdata.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/elem_ts_methods.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/gridded_data_intro.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/gridded_data.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/heatwaveR_issues.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/MHW_MCS_horizonplots.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/netCDF_dates.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/PBSPro_users.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/prep_NOAA_OISST.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/README_Lengau.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/README_PBSPro.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/README_tmux.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/regridding.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/Wind_stress_curl.qmd"
    ]
    
    # Categorize files
    correct_files = []
    incorrect_files = []
    no_setup_files = []
    error_files = []
    
    print("COMPREHENSIVE .QMD FILE ANALYSIS")
    print("=" * 50)
    print(f"Total files to analyze: {len(files)}")
    print()
    
    for i, file_path in enumerate(files, 1):
        if not os.path.exists(file_path):
            error_files.append((file_path, "File not found"))
            print(f"[{i}/{len(files)}] ERROR: {file_path} - File not found")
            continue
            
        category, details = analyze_qmd_file(file_path)
        relative_path = file_path.replace('/Users/ajsmit/Documents/R_local/tangled_bank/', '')
        
        if category == 'CORRECT':
            correct_files.append((relative_path, details))
        elif category == 'INCORRECT':
            incorrect_files.append((relative_path, details))
        elif category == 'NO_SETUP':
            no_setup_files.append((relative_path, details))
        else:  # ERROR
            error_files.append((relative_path, details))
            
        print(f"[{i}/{len(files)}] {category}: {relative_path}")
    
    # Generate report
    print("\n" + "=" * 80)
    print("COMPREHENSIVE ANALYSIS REPORT")
    print("=" * 80)
    print(f"Total files analyzed: {len(files)}")
    print(f"Files with CORRECT setup chunk placement: {len(correct_files)}")
    print(f"Files with INCORRECT setup chunk placement: {len(incorrect_files)}")
    print(f"Files with NO setup chunk: {len(no_setup_files)}")
    print(f"Files with errors: {len(error_files)}")
    print()
    
    if incorrect_files:
        print("🚨 FILES WITH INCORRECT SETUP CHUNK PLACEMENT (NEED FIXING):")
        print("-" * 60)
        for file_path, details in incorrect_files:
            print(f"  • {file_path}")
            print(f"    └─ {details}")
        print()
    
    if correct_files:
        print("✅ FILES WITH CORRECT SETUP CHUNK PLACEMENT:")
        print("-" * 50)
        for file_path, details in correct_files:
            print(f"  • {file_path}")
        print()
    
    if no_setup_files:
        print("ℹ️  FILES WITH NO SETUP CHUNK:")
        print("-" * 35)
        for file_path, details in no_setup_files:
            print(f"  • {file_path}")
        print()
    
    if error_files:
        print("❌ FILES WITH ERRORS:")
        print("-" * 25)
        for file_path, details in error_files:
            print(f"  • {file_path}: {details}")
        print()
    
    # Focus on BDC334 as mentioned by user
    print("🔍 SPECIAL FOCUS ON BDC334 DIRECTORY:")
    print("-" * 40)
    bdc334_files = [f for f in files if '/BDC334/' in f]
    bdc334_incorrect = [(f, d) for f, d in incorrect_files if 'BDC334/' in f]
    bdc334_correct = [(f, d) for f, d in correct_files if 'BDC334/' in f]
    bdc334_no_setup = [(f, d) for f, d in no_setup_files if 'BDC334/' in f]
    
    print(f"Total BDC334 files: {len(bdc334_files)}")
    print(f"BDC334 files needing fixes: {len(bdc334_incorrect)}")
    print(f"BDC334 files correct: {len(bdc334_correct)}")
    print(f"BDC334 files with no setup: {len(bdc334_no_setup)}")
    print()
    
    if bdc334_incorrect:
        print("BDC334 files that need fixing:")
        for file_path, details in bdc334_incorrect:
            print(f"  • {file_path}")
    
    return incorrect_files

if __name__ == "__main__":
    incorrect_files = main()