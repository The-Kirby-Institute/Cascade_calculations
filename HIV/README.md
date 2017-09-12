## Australian HIV Cascade Calculations ##

This directory contains all the code and materials used for estimating each step of the Australian HIV diagnosis and care cascade. Estimates are reported annually in the HIV, viral hepatitis and sexually transmissible infections in the Australia Annual Surveillance Report (ASR). The HIV cascade is treated as a separate sub-project within the overall Australian cascades project. 

### Aims ###

The aim of the HIV cascade calculations is to estimate the total number of people living with HIV, the total number of people living with diagnosed HIV, the number of people retained in care, the number of people on antiretrovial treatment (ART), and the number of people on treatment with suppressed viral load for each calendar year. The purpose of the cascade is to highlight any potential gaps in care and treatment at a cross-sectional population level and to achieve the maximal level of viral suppression. HIV cascades can be estimated for specific populations based on region, sex, demographics, ethnicity, age, and risk of HIV exposure. 

### Project organization ###

All the files related to the HIV cascade estimates and calculations are stored in this directory and 6 sub-directories. Some of these materials are stored locally and are not available from the repository. 

_Main directory scripts_

The following R markdown scripts are the main scripts used for the Australian HIV cascade and related calculations. Further details are available as comments in the scripts. The files are numbered from 0 to specify the order they need to be run to produce all the HIV cascade results. Scripts starting with the same number can be run independently of each other. 

Note as these are scripts, not functions, care should be taken to ensure the project specifications are correct before running a script especially after updates have been pulled from the repository.

- **0-ArtAnalysis.Rmd**: This script was used to explore the number of people taking ART in Australia over 2000-2014. Its primary purpose was to estimate the number taking ART during the year merging data from the Australian HIV Observational Database (AHOD) for 2000-2012 and a 10% longitudinal sample of Pharmaceutical Benefits Scheme (PBS) data provided by the company Prospection for 2013-2014. This was used once off as the PBS data since 2014 is considered to be the most accurate so the script was only used for the crossover. 
	- The resulting estimates are saved as: `data/ART_Estimates-2014.csv`. This file needs to be generated for the `1-HivTreatment.Rmd` script to run.
- **0-GenerateAdjustments.Rmd**: This scripts calculates the proportion of notifications that are duplicates and population movement and death rates for specific populations of people living with HIV in Australia. These calculations use data from the `data/` directory as input.
    - Results are stored in two files in the `data/` directory: `data/HIVbaseEstimates-YEAR.csv` and `data/HIVcadjustments-YEAR.csv`. These files are then read in by `1-PldhivCalculations.Rmd`.
- **1-HivTreatment.Rmd**: This script produces estimates for the number of people who had HIV treatment each year in Australia nationally, for each state and territory, and for males and females. The user needs to specify the final year (YEAR) for the estimates and other calculation options in the _initialization_ R markdown chunk at the start of the script. The calculations use multiple input files from the main Cascade_calculations `~/data/` (described in the parent directory) and the HIV `data/` directories.
    - The resulting estimates are saved as: `output/HIVtreatment-(YEAR).csv`. This file needs to be generated for the `3-HivCascadeMerge.Rmd` script to run.
- **1-PldhivCalculations.Rmd**: This script produces estimates for the number of people living with diagnosed HIV (PLDHIV) and input files for the [ECDC HIV Modelling Tool](https://ecdc.europa.eu/en/publications-data/hiv-modelling-tool). It is the main script for calculating the HIV diagnosis and care cascade. If specified it also estimates the number of HIV positive people retained in care. Estimates are produced nationally or for specific subpopulations specified by the user. The user also needs to specify the final year (YEAR) for the estimates and other calculation options in the _Script parameters_ R markdown chunk at the start of the script. The script sources functions from the main Cascade_calculations `~/code/` (described in the parent directory) and the HIV `code/` directories. For a given year (YEAR) the script loads input data files from the `data/` directory. 
    - Results are saved in a user specified directory (with the YEAR appended) in the `output/` directory as `.csv` files. This directory is then used to save all the population specific HIV cascade outputs. 
    - If specified, additional files are created and stored in a directory in `output/ECDC_calculations`. These files are input files for the [ECDC HIV Modelling Tool](https://ecdc.europa.eu/en/publications-data/hiv-modelling-tool) which is run separately. 
- **2-UndiagnosedCalculations.Rmd**: This script calculates the number of people living with HIV in Australia for specified sub-populations using the estimated PLDHIV produced by `1-PldhivCalculations.Rmd` and the number/percentage undiagnosed estimated by the ECDC HIV Modelling tool. The user needs to specify the final year of results (YEAR), the name of the output directory produced by `1-PldhivCalculations.Rmd`, and the appropriate ECDC HIV Modelling Tool directory. For this script to run the ECDC HIV Modelling Tool must be run first with the results for the specified population saved in the appropriate directory as `All_charts.xlsx` (with all rows unhidden). 
    - This script save results in the specified directory in `output/`. It also copies and renames the `All_charts.xlsx` file into this directory. 
- **3-HivCascadeMerge.Rmd**: This script merges all the HIV cascade results produced by other scripts into population specific `.csv` files. The user needs to specify the final year (YEAR) for the estimates and sub-population cascades to be merged in the _Initialization_ R markdown chunk at the start of the script. Input files are then read from the `output\` directory.
    - Results for each sub-population specified are saved in the appropriate directory in `output\`. the script also produces a single `output/HIVcascadeEstimates-YEAR.csv` file containing the HIV cascade estimates for all the populations. This file needs to be generated for the `4-HivCascadePlots.Rmd` script to run.
- **4-HivCascadePlots.Rmd**: This script loads the result file `output/HIVcascadeEstimates-YEAR.csv` for a specified year (YEAR) and produces all the figures for the HIV cascade and associated results. These are stored in the `output/figures/YEAR` directory. The user needs to specify the final year for the estimates and other calculation options in the _setup_ R markdown chunk at the start of the script. When run using `knitr` this scripts produces a summary document presenting the results with all the methods and data sources (with references) used for the calculations. The resulting Word document is stored in the `docs` directory. Additional files required to produce the Word document are only available locally but all the final results and figures can be generated using the files in the repository.  

#### data ####

Contains the cleaned input data sets for the HIV cascade calculations. Only files containing publicly available data are stored in the online repository with other files stored locally. A README file within the directory describes the data files in detail.

#### docs (local) ####

Contains manuscript, report, and presentation files based on the HIV cascade results and related documents. The R markdown generated documents produced by `4-HivCascadePlots.Rmd` and `hivaging.Rmd` are saved in this directory along with the associated styles and bibtex files to produce these documents. These files are stored locally and are not available in the online repository. 

#### misc (local) ####

Contains miscellaneous files such references, correspondence, general information, examples, working documents, and general kibble  of relevance to the HIV cascade. These files are stored locally and are not available in the online repository. 

#### output ####

Contains output files storing the results produced by the main directory scripts. The results are stored in a number of `.csv` files for each specified sub-population and year (YEAR) with associated figures saved in the `/figures/YEAR` directory. The online repository only contains the overall results for each sub-population cascade calculated for the 2017 ASR: `HIVcascadeEstimates-2016.csv` and the HIV cascade output directories for Australia overall and for the state of NSW in 2016. The results in these directories are associated with journal manuscripts and are stored for archival and reproducibility purposes. 

#### subprojects (local) ####

Contains additional projects associated with the main HIV cascade such as cascade calculations for specific jurisdictions or sub-populations. These sub-projects are stored locally and are not available in the online repository. 








 

