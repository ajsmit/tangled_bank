#!/usr/bin/env python3
"""
Fix .qmd files that have setup chunks incorrectly placed inside YAML headers.

This script will:
1. Read each file with incorrect setup chunk placement
2. Extract the setup chunk from inside the YAML header
3. Move it to after the closing YAML `---`
4. Write the corrected file back
"""

import re
import os
from pathlib import Path

def fix_qmd_file(file_path):
    """
    Fix a single .qmd file by moving setup chunk from inside YAML to after YAML.
    
    Returns:
        bool: True if file was fixed, False if no changes needed or error
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
    except UnicodeDecodeError:
        try:
            with open(file_path, 'r', encoding='latin-1') as f:
                content = f.read()
        except Exception as e:
            print(f"ERROR reading {file_path}: {e}")
            return False
    except Exception as e:
        print(f"ERROR reading {file_path}: {e}")
        return False
    
    # Check if file has YAML header
    if not content.strip().startswith('---'):
        return False
    
    # Find YAML header boundaries
    yaml_start = content.find('---')
    if yaml_start == -1:
        return False
    
    # Find the closing --- of YAML header
    yaml_end_pattern = r'\n---\s*\n'
    yaml_end_match = re.search(yaml_end_pattern, content[yaml_start + 3:])
    
    if not yaml_end_match:
        print(f"ERROR: YAML header not properly closed in {file_path}")
        return False
    
    yaml_end_pos = yaml_start + 3 + yaml_end_match.start()
    yaml_close_pos = yaml_start + 3 + yaml_end_match.end()
    
    # Extract YAML header content (without the closing ---)
    yaml_content = content[yaml_start:yaml_end_pos]
    yaml_close = content[yaml_end_pos:yaml_close_pos]
    post_yaml_content = content[yaml_close_pos:]
    
    # Look for setup chunks in YAML header using more flexible regex
    setup_pattern = r'(```\{r[^}]*setup[^}]*\}[^`]*```)'
    setup_matches = re.findall(setup_pattern, yaml_content, re.DOTALL | re.IGNORECASE)
    
    if not setup_matches:
        return False
    
    print(f"FIXING: {file_path}")
    print(f"  Found {len(setup_matches)} setup chunk(s) inside YAML")
    
    # Remove all setup chunks from YAML content
    yaml_content_fixed = yaml_content
    for setup_chunk in setup_matches:
        yaml_content_fixed = yaml_content_fixed.replace(setup_chunk, '')
    
    # Clean up any extra blank lines in YAML
    yaml_content_fixed = re.sub(r'\n\n+', '\n\n', yaml_content_fixed)
    yaml_content_fixed = yaml_content_fixed.rstrip() + '\n'
    
    # Combine all setup chunks (in case there are multiple)
    all_setup_chunks = '\n'.join(setup_matches)
    
    # Reconstruct the file: YAML + closing --- + setup chunks + rest of content
    fixed_content = yaml_content_fixed + yaml_close + all_setup_chunks + '\n\n' + post_yaml_content
    
    # Write the fixed content back
    try:
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(fixed_content)
        print(f"  ✅ Successfully fixed {file_path}")
        return True
    except Exception as e:
        print(f"ERROR writing {file_path}: {e}")
        return False

def main():
    # List of files that need fixing (from the analysis)
    files_to_fix = [
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
        "/Users/ajsmit/Documents/R_local/tangled_bank/BCB744/intro_r/_02-github.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/BDC334/Lab-03-biodiversity.qmd",
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
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/heatwaveR_issues.qmd",
        "/Users/ajsmit/Documents/R_local/tangled_bank/vignettes/MHW_MCS_horizonplots.qmd"
    ]
    
    print("QMD FILE FIXER")
    print("=" * 40)
    print(f"Files to fix: {len(files_to_fix)}")
    print()
    
    fixed_count = 0
    error_count = 0
    
    for i, file_path in enumerate(files_to_fix, 1):
        print(f"[{i}/{len(files_to_fix)}] Processing: {os.path.basename(file_path)}")
        
        if not os.path.exists(file_path):
            print(f"  ❌ File not found: {file_path}")
            error_count += 1
            continue
            
        success = fix_qmd_file(file_path)
        if success:
            fixed_count += 1
        else:
            error_count += 1
            print(f"  ❌ Failed to fix: {file_path}")
    
    print("\n" + "=" * 60)
    print("FIX SUMMARY")
    print("=" * 60)
    print(f"Total files processed: {len(files_to_fix)}")
    print(f"Successfully fixed: {fixed_count}")
    print(f"Errors encountered: {error_count}")
    print()
    
    if fixed_count > 0:
        print("✅ All files have been processed!")
        print("🔍 Run the analysis script again to verify fixes...")
    
    return fixed_count, error_count

if __name__ == "__main__":
    fixed, errors = main()