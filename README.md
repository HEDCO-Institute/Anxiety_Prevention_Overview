## Anxiety Prevention Overview Repository 

This repository contains the data and R code to reproduce the results of our Anxiety Prevention Overview technical report titled "Do School-Based AnxietyPrevention Programs Support Youth? Analyzing School-Based Interventions for Primary and Secondary Prevention of Anxiety" <br>
Additional materials, including the protocol, study materials, and dissemination products are available at <https://osf.io/8nea2/>.

## Computational Environment

All of our analyses were ran on a Windows 10 Enterprise platform (x86_64-w64-mingw32/x64). <br>
Our analyses were conducted in R (version 4.2.2) and are reported in Rmarkdown files (version 2.20) <br>
We use a variety of R packages that need to be installed. Code to install and load all required packages are included in the beginning of the coding scripts. See below for the full list of packages and versions used:

- `pacman` (0.5.1)
- `devtools` (2.4.5)
- `rio` (0.5.29)
- `here` (1.0.1)
- `readxl` (1.4.0)
- `janitor` (2.1.0)
- `tidyverse` (1.3.2)
- `openxlsx` (4.2.5)
- `robumeta` (2.0)
- `metafor` (3.8-1)
- `lubridate` (1.8.0)
- `gt` (0.8.0.9000)
- `webshot2` (0.1.0)
- `stringi` (1.7.6)
- `htmltools` (0.5.4)
- `ccaR` (0.1.0)
- `robvis` (0.3.0.900)
- `writexl` (1.5.0)

## Data

The excel data for this overview are included in the `data` folder of this repository. All data files are also publicly available at <https://osf.io/8nea2/>. The value `-999` indicates a missing value that was not extracted for all data files. See below for a description of each data file:  <br>
<br>

#### Review Level Data:

| Data File | Description | Data Structure |
|-----------|:----------:|-------------|
| `APO_all_citations.csv` | Citation information for all references | One row per reference ID (citation) |
| `APO_review_eligibility_citations.xlsx` | Citation information for each reference assessed for eligibility | One row per reference ID (citation) |
| `APO_review_eligibility.xlsx` | Full-text screening decisions | One row per reference ID |
| `APO_review_data_collection.xlsx` | Extracted descriptive data, study quality, and risk of bias assessments on eligible reviews | One row per review |
<br>

#### Primary Study Level Data:

| Data File | Description | File Structure |
|-----------|:----------:|-------------|
| `Anxiety_Overview_Eligibility_Data.xlsx` | Primary study overlap across reviews | One row per primary study; columns are review IDs |
| `APO_study_eligibility_citations.xlsx` | Citation information from each study assessed for eligibility | One row per reference ID (study citation) |
| `APO_linked_references.xlsx` | Reference information for additional reports of studies/reviews | One row per main reference and linked reference combo |
| `APO_study_eligibility.xlsx` | Full-text screening decisions | One row per reference ID (study citation) |
| `APO_study_level.xlsx` | Extracted descriptive data on eligible primary studies | One row per primary study |
| `APO_study_ROB.xlsx` | Risk of bias assessments on eligible primary studies | One row per primary study |
| `APO_group_level.xlsx` | Extracted descriptive data for each study group | One row per user and group ID combo |
| `APO_outcome_level.xlsx` | Extracted descriptive data for each outcome of interest | One row per user and outcome ID combo |
| `APO_effect_level.csv` | Extracted effect size data for each group contrast and outcome combination | One row per user and effect size ID combo |
| `Anxiety_Symptoms.xlsx` | Meta-analytic data for anxiety symptoms meta-analysis | One row per effect size - created via `merge_anxiety_for_meta_analysis.R` script|
| `Depression_Symptoms.xlsx` | Meta-analytic data for depression symptoms meta-analysis | One row per effect size - created via `merge_anxiety_for_meta_analysis.R` script|
| `Well_being.xlsx` | Meta-analytic data for well-being meta-analysis | One row per follow-up effect size - created via `merge_anxiety_for_meta_analysis.R` script|
| `Anxiety_diagnosis.xlsx` | Meta-analytic data for anxiety diagnosis meta-analysis | One row per effect size - created via `merge_anxiety_for_meta_analysis.R` script|

## Code

All code necessary to reproduce our findings are included in 3 Rmarkdown files in the `code` folder: 

- `analysis_script_descriptives.Rmd`: Cleaning and analysis script to reproduce descriptive results
- `merge_anxiety_for_meta_analysis.Rmd`: Cleaning script to produce the data set to run meta-analysis script on
- `analysis_script_meta_analysis.Rmd`: Analysis script to reproduce meta-analytic results

## Replication Steps

To replicate our results: 

**If you have Rstudio and Git installed and connected to your GitHub account:**

1. Clone this repository to your local machine ([click for help](https://book.cds101.com/using-rstudio-server-to-clone-a-github-repo-as-a-new-project.html#step---2))
1. Run `analysis_script_descriptives.Rmd` for descriptive results and `analysis_script_meta_analysis.Rmd` for meta-analytic results in the `code` folder

**If you need to install or connect R, Rstudio, Git, and/or GitHub:**

1. [Create a GitHub account](https://happygitwithr.com/github-acct.html#github-acct)
1. [Install R and RStudio](https://happygitwithr.com/install-r-rstudio.html)
1. [Install git](https://happygitwithr.com/install-git.html)
1. [Link git to your GitHub account](https://happygitwithr.com/hello-git.html)
1. [Sign into GitHub in Rstudio](https://happygitwithr.com/https-pat.html)

**To reproduce our results without using Git and GitHub, you may use the following steps:** 

1. Create an R project on your local machine ([click for help](https://rpubs.com/Dee_Chiluiza/create_RProject))
1. Create the following folders in your R project root directory: `data`, `code`
1. Download all data files listed above from the repository and put them in the `data` folder you created
1. Download the `analysis_script_descriptives.Rmd` and `analysis_script_meta_analysis.Rmd` listed above from the repository and put it in the `code` folder you created
1. Run `analysis_script_descriptives.Rmd` for descriptive results and `analysis_script_meta_analysis.Rmd` for meta-analytic results

## Tables, Figures, and Appendices

All tables, figures, and appendices are located in the `outputs` folder of the repository in their respective sub-folders. All tables, figures, and appendices can be reproduced in HTML by running the `analysis_script_descriptives.Rmd` file.

## Contact

If you have any questions, concerns, or feedback, feel free to email Shaina Trevino at [strevino\@uoregon.edu](mailto:strevino@uoregon.edu)

