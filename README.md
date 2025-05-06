# Date: 2025-05-05
# Author: Quentin Chenot
# Article: "Evaluating the Space Fortress task for executive functions training applications: Psychometric evidence and cognitive correlates"

This repository contains the R scripts used for data preprocessing and analysis in the associated scientific article.

## **Directory Structure**
```
project/
├── data/
│   └── sub-<participant_id>/
│       └── ses-<session_number>/
│           └── behavior/
├── tmp_data/
│   └── sub-<participant_id>/
│       └── ses-<session_number>/
│           └── behavior/
├── results/
│   ├── combined_data/
│   │   └── behavior/
│   └── figures/
└── src/
    ├── preprocessing/
    │   └── behavior/
    │       ├── _main.R
    │       ├── SF.R
    │       ├── antisaccade.R
    │       ├── categoryswitch.R
    │       ├── colorshape.R
    │       ├── dualnback.R
    │       ├── keeptrack.R
    │       ├── lettermemory.R
    │       ├── numberletter.R
    │       ├── stopsignal.R
    │       └── stroop.R  
    └── analyses/
        ├── P01_combine_data.R
        ├── P02_behavioral_data_transformation.R
        ├── P03_demographics.R
        ├── P04_SF_psychometrics.R
        ├── P05_hypotheses.R
        └── P06_SupplementaryMaterial.R
```

## **Scripts Overview**

### **Preprocessing Scripts (`src/preprocessing/behavior/`)**

1. **Main Script (`_main.R`)**
   - Manages directory creation and file organization
   - Sources and runs all task-specific preprocessing scripts
   - Saves preprocessed data in `/results/combined_data/behavior/`

2. **Space Fortress Processing (`SF.R`)**
   - Processes raw Space Fortress data following BIDS format
   - Computes performance scores for each game session
   - Saves processed data in `/tmp_data/` and `/results/combined_data/behavior/`

3. **Task-Specific Scripts**
   - Preprocess data from executive function tasks:
     - Antisaccade
     - Category Switch
     - Color Shape
     - Dual N-back
     - Keep Track
     - Letter Memory
     - Number Letter
     - Stop Signal
     - Stroop

### **Analysis Scripts (`src/analyses/`)**

1. **Data Combination (`P01_combine_data.R`)**
   - Combines demographics and task performance data
   - Creates unified dataset for analysis

2. **Data Transformation (`P02_behavioral_data_transformation.R`)**
   - Performs data transformations
   - Calculates z-scores for task performance
   - Saves output in /combined_data/behavior/

3. **Demographics Analysis (`P03_demographics.R`)**
   - Extracts and summarizes participant demographics

4. **Space Fortress Psychometrics (`P04_SF_psychometrics.R`)**
   - Analyzes Space Fortress sensitivity and reliability
   - Generates and saves associated figures

5. **Hypotheses Testing (`P05_hypotheses.R`)**
   - Tests main hypotheses about relationships between:
     - Space Fortress performance and executive functions
     - Space Fortress performance and demographics
   - Generates and saves associated figures

6. **Supplementary Analyses (`P06_SupplementaryMaterial.R`)**
   - Creates statisticals analyses available in supplementary materials
   - Generates and saves associated figures

## **Instructions**

1. **Setup**
   - Ensure raw data are under `data/`

2. **Preprocessing**
   - Run `_main.R` to process all behavioral data
   - Check `tmp_data/` for intermediate results

3. **Analysis**
   - From raw data: Run `P01_combine_data.R` and `P02_behavioral_data_transformation.R` to obtain preprocessed data stored in `/results/combined_data`
   - From preprocessed data: Run `P03_demographics.R`, `P04_SF_psychometrics.R`, `P05_hypotheses.R` and `P06_SupplementaryMaterial.R`
   - Generated figures will be saved in `results/figures/`

## **Dependencies**

### **Statistical analysis software**
- R version: 4.2.0 or higher
- Required packages:
  - **Data manipulation:** `dplyr`, `tidyr`, `reshape2`
  - **Visualization:** `ggplot2`, `ggpubr`, `ggExtra`, `cowplot`
  - **Statistics:** `e1071`, `nortest`, `psych`, `broom`
  - **Utilities:** `rstudioapi`, `stringr`, `foreach`, `bestNormalize`

### **Computational Environment**
- OS: Windows 10
- R version: 4.2.0
- RStudio version: 2023.06.0

## **License**
This project is licensed under [MIT License](LICENSE.md) - see LICENSE.md file for details

## **Preregistration**
The analysis plan for this study was preregistered on OSF: [[link to preregistration](https://osf.io/5t3re/)]

## **Data Availability**
- Raw data are available upon request (contact: quentinchenot@gmail.com)
- Preprocessed data are available on Github: [[link to Github](https://github.com/Chenot/SF_EFs/)]

## **Contact**
For questions or issues, please contact Quentin Chenot [quentinchenot@gmail.com].