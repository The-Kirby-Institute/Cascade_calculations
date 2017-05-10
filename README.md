## Australian Diagnosis and Care Cascade Calculations

This project repository contains all the code and input data used to estimate the steps of various diagnosis and care cascades for Australia and for associated calculations. The outputs of these calculations are reported annually in the HIV, viral hepatitis and sexually transmissible infections in [Australia Annual Surveillance Report](https://kirby.unsw.edu.au/report-type/annual-surveillance-reports) (ASR). Additional materials are stored in this repository for associated publications related to the cascades for transparency and reproduciblity purposes.  

This repository currently contains materials for two separate cascades: the Australian HIV cascade and the Australian Chlamydia cascade. Cascade estimates obtained from this project have been reported in the ASR since 2015. Additional calculations related to these cascades have also been used to produce a number of publications listed below. 

**Repository owner:** Richard T. Gray

**ORCID ID:** orcid.org/0000-0002-2885-0483

**Affiliation:** [_The Kirby Institute_](https://kirby.unsw.edu.au/), UNSW Sydney, Sydney NSW 2052, Australia

## Aims ##

The aim of the cascade calculations is to estimate the total number of people living with an infection, have been diagnosed, are receiving appropriate care, receiving treatment, and have achieved the desired clinical outcome/endpoint. The purpose of these steps is highlight any potential gaps in care and treatment at a cross-sectional population level. Each infection can have different steps in its cascade depending on its key characteristics and clinical pathway  and may focus on specific populations by region, demographics, or exposure risk. 

### Contributors ###

The two main developers for this project are:

- Dr Richard T. Gray: Principle developer and coder and maintainer of the repository.
- Dr Neil Bretana: Model developer and coder.

who are both based in the Surveillance, Evaluation and Research Program (SERP) at the 
[The Kirby Institute](https://kirby.unsw.edu.au/). 

The development of the diagnosis and care cascades and the project overall is overseen by other researchers within SERP and the Kirby Institute and by two reference groups: the Australian HIV Cascade Reference Group and the Australian STI Cascade Reference Group. 

### Project organization ###

The code and materials for the HIV and Chlamydia cascade are stored in two separate folders and treated as separate projects. They are described separately via README files contained in the corresponding folders. The folders and files contained in the main directory contain material that is relevant to all cascade calculations.

_Main directory sub-directories_ 

#### code ####

Contains specific R functions and scripts used for all cascade calculations.  

#### dashboards ####

Contains R markdown scripts for generating shiny dashboards for specific cascades. It currently only has a dashboard script for the HIV cascade.

#### Chlamydia ####

Contains all the code and materials used for the Australian chlamydia diagnosis and care cascade project. It contains a specific README file describing the specific materials within this directory. 

#### HIV #####

Contains all the code and materials used for the Australian HIV diagnosis and care cascade project. It contains a specific README file describing the specific materials within this directory. 

### Publications ###

The following publications are associated with this project and used code in this repository to generate the results and figures. 

- The Kirby Institute. HIV, viral hepatitis and sexually transmissible infections in Australia: Annual Survellance Report. The Kirby Institute, UNSW Australia, Sydney NSW 2052.
	- For the years 2015-2016. Available from the [Kirby Institute website](https://kirby.unsw.edu.au/report-type/annual-surveillance-reports)
	- The HIV and Chlamydia cascade estimates were produced using version XX in 2015 and version XX in 2016
<br></br>
- RT Gray, D Callander, S McGregor, J Hocking, H McManus, A Dyda, C Moreira, DP Wilson, B Donovan, J Kaldor, RJ Guy on behalf of the Australian STI Diagnosis and Care Cascade Reference Group. A population-level diagnosis and care cascade for curable STIs: The Australian Chlamydia Cascade. _In preparation_
	- Current manuscript based on version XX
<br></br>
- RT Gray et al. The Australia HIV cascade. _In preparation_ 
	- Current manuscript based on version XX


