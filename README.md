## Australian Diagnosis and Care Cascade Calculations

This project repository contains all the code and cleaned (publicly available) input data used to estimate the steps of various diagnosis and care cascades for Australia and for associated calculations. The outputs of these calculations are reported annually in the HIV, viral hepatitis and sexually transmissible infections in [Australia Annual Surveillance Report](https://kirby.unsw.edu.au/report-type/annual-surveillance-reports) (ASR). Additional materials are stored in this repository for associated publications related to the cascades for transparency and reproducibility purposes.  

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1478453.svg)](https://doi.org/10.5281/zenodo.1478453)

This repository currently contains materials for two separate cascades: the Australian HIV cascade and the Australian Chlamydia cascade. This project also provides estimates for a viral hepatitis C (HCV) cascade but materials for this cascade are stored locally and not available in this online repository. Cascade estimates obtained from this project have been reported in the ASR since 2015. Additional calculations related to these cascades have also been used to produce a number of publications listed below. 

All calculations are primarily conducted using R (currently version 3.4.3) with associated packages (forcats 0.3.0; stringr 1.3.0; dplyr 0.7.4; purrr 0.2.4; readr 1.1.1; readxl 1.0.0; tidyr 0.8.0; tibble 1.4.2; ggplot2 2.2.1; 
tidyverse 1.2.1; Hmisc 4.1.1; Formula 1.2.2; survival 2.41.3; lattice 0.20.35; gridExtra 2.3; RColorBrewer 1.1.2; scales 0.5.0; captioner 2.2.3; cowplot 0.9.2). Some simple calculations are conducted using Microsoft Excel. Many of the data files required to run the scripts are not publicly available and have been excluded from the repository (but can be available on Request). Please report an issue if you want to run the code or contact Rgray@kirby.unsw.edu.au. 

**Repository owner:** Richard T. Gray 

**ORCID ID:** orcid.org/0000-0002-2885-0483

**Affiliation:** [_The Kirby Institute_](https://kirby.unsw.edu.au/), UNSW Sydney, Sydney NSW 2052, Australia

### Aims ###

The aim of the cascade calculations is to estimate the total number of people living with an infection, have been diagnosed, are receiving appropriate care, are receiving treatment, and have achieved the desired clinical outcome/endpoint. The purpose of these steps are to highlight any potential gaps in care and treatment at a cross-sectional population level. Each infection can have different steps in its cascade depending on its key characteristics and clinical pathway and may focus on specific populations by region, demographics, or exposure risk. 

### Contributors ###

The main developer for this project is:

- Dr Richard T. Gray: Principle developer and coder and maintainer of the repository.

who is based in the Surveillance, Evaluation and Research Program (SERP) at the [The Kirby Institute](https://kirby.unsw.edu.au/). 

Other developers:

- Dr Neil Bretana: Model developer and coder while at the Kirby Institute during 2017-2018. 

The development of the diagnosis and care cascades and the project overall is overseen by other researchers within SERP and the Kirby Institute and by three reference groups: the Australian HIV Cascade Reference Group, the Australian STI Cascade Reference Group, and the Australian HCV Cascade Reference Group.

### Project organization ###

The code and materials for the HIV, Chlamydia, and HCV cascade are stored in three separate directories and treated as separate projects. They are described separately via README files contained in the corresponding directories. The directories and files contained in the main directory contain material that is relevant to all cascade calculations. Some of these materials are stored locally (for use by collaborators) and are not available from the online repository. 

_Main directory sub-directories_ 

#### code ####

Contains specific R functions and scripts used for all cascade calculations.  

#### dashboards ####

Contains R markdown scripts for generating shiny dashboards for specific cascades. It currently only has a draft/preliminary dashboard script for the HIV cascade.

#### data (local) ####

Contains raw and cleaned data files of relevance to all cascade calculations. Most of these files are stored locally and are not available in this online repository as they contain data that can not be shared publicly. Currently this folder only contains cleaned Australian Bureau of Statistics (ABS) population movement data. 

#### docs (local) ####

Contains manuscript, report, and presentation files based on cascade results and related documents. These files are stored locally and are not available in this online repository. 

#### HCV cascade (local) ####

Contains all the materials relevant to the HCV cascade calculations. The estimates are produced in collaboration with the [Center for Disease Analysis](http://centerforda.com/) using their CDA Bright model (which uses Microsoft Excel) of HCV transmission with updated HCV epidemiological, demographic, and clinical data for Australia. Along with the cascade estimates additional epidemiological and morbidity estimates are produced by the model for reporting in the ASR. Currently these files are stored locally and are not available in this online repository.

#### misc (local) ####

Contains miscellaneous files such as references, correspondence, general information, examples, working documents, and general kibble of relevance to the project. These files are stored locally and are not available in this online repository. 

#### Chlamydia ####

Contains all the code and materials used for the Australian chlamydia diagnosis and care cascade project. It contains a specific README file describing the files within this directory. 

#### HIV #####

Contains all the code and materials used for the Australian HIV diagnosis and care cascade project. It contains a specific README file describing the files within this directory. 

### Publications ###

The following publications are associated with this project and used code in this repository to generate the results and figures. 

- The Kirby Institute. HIV, viral hepatitis and sexually transmissible infections in Australia: Annual Surveillance Report. The Kirby Institute, UNSW Australia, Sydney NSW 2052.
	- For the years 2015-2018. Available from the [Kirby Institute website](https://kirby.unsw.edu.au/report-type/annual-surveillance-reports)
	- The HIV and Chlamydia cascade estimates were produced using [version 1.0_2015_ASR](https://github.com/leftygray/Cascade_calculations/releases/tag/v1.0_2015_ASR) in 2015, [version v2.0_2016_ASR](https://github.com/leftygray/Cascade_calculations/releases/tag/v2.0_2016_ASR) in 2016, [version 3.0_2017_ASR](https://github.com/leftygray/Cascade_calculations/releases/tag/v3.0_2017_ASR) in 2017, and [version v3.6_2018_ASR](https://github.com/leftygray/Cascade_calculations/releases/tag/v3.6_2018_ASR) in 2018. 
<br></br>
- B Hajarizadeh, J Grebely, H McManus, C Estes, H Razavi, RT Gray, et al. Chronic hepatitis C burden and care cascade in Australia in the era of interferon-based treatment. Journal of gastroenterology and hepatology 2017; 32:229–236.
<br></br>
- P keen et al. The 2016 HIV diagnosis and care cascade in New South Wales, Australia: meeting the UNAIDS 90-90-90 targets. Journal of the International AIDS Society 2018; 21:e25109.
	- Manuscript results generated using [version 3.0_2017_ASR](https://github.com/leftygray/Cascade_calculations/releases/tag/v3.0_2017_ASR); doi:[10.5281/zenodo.998280](http://doi.org/10.5281/zenodo.998280)
<br></br>
- N Bretana et al. An ageing population of people living with diagnosed HIV in Australia. JAIDS 2018; In press. 
    - Submitted paper results generated using [version 3.3_HIV_ageing_paper](https://github.com/leftygray/Cascade_calculations/releases/tag/v3.3_HIV_ageing_paper); doi:[10.5281/zenodo.1260377](http://doi.org/10.5281/zenodo.1260377)
    - Accepted paper results generated using [version 3.5_HIV_ageing_paper-Final](https://github.com/leftygray/Cascade_calculations/releases/tag/v3.5_HIV_ageing_paper-Final); doi:[10.5281/zenodo.1478452](https://doi.org/10.5281/zenodo.1478452)
<br></br>
- RT Gray, D Callander, J Hocking, S McGregor, H McManus, A Dyda, C Moreira, S Braat, B Hngel, J Ward, DP Wilson, B Donovan, J Kaldor, RJ Guy on behalf of the Australian STI Diagnosis and Care Cascade Reference Group. A population-level diagnosis and care cascade for genital chlamydia in Australia. _Submitted to Sexually Transmitted Infections_
    - Submitted paper results generated using [version 3.4_sti_cascade_paper](https://github.com/leftygray/Cascade_calculations/releases/tag/v3.4_sti_cascade_paper); doi:[10.5281/zenodo.1283746](http://doi.org/10.5281/zenodo.998280)
<br></br>
- RT Gray et al. The Australia HIV cascade. _In preparation_ 
    - Manuscript results generated using [version v3.6_2018_ASR](https://github.com/leftygray/Cascade_calculations/releases/tag/v3.0_2017_ASR); doi:[10.5281/zenodo.1478453](https://doi.org/10.5281/zenodo.1478453)



