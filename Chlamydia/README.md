## Australian Chlamydia Cascade Calculations ##

This directory contains all the code and materials used for estimating each step of the Australian Chlamydia diagnosis and care cascade. Annual estimates are reported annually in the HIV, viral hepatitis and sexually transmissible infections in the Australia Annual Surveillance Report (ASR). The chlamydia cascade is treated as a separate sub-project within the overall Australian cascades project. 

### Project organization ###

All the chlamydia cascade project files are stored in this directory and 3 sub-directories.

_Main directory files_

The files `Chlamydia_Cascade_Methods.Rmd` and `ChlamydiaCascade.bib` are minor files used for storing a description of the methods and data sources used in the chlamydia cascade calculations.

**ChlamydiaCascade.Rmd** is a R markdown script and is the main file for the chlamydia calculations. The user needs to specify a specific year for the cascade estimates. The script then reads in the associated cleaned data files stored in the data/ directory, performs the cascade calculations and stores the resulting estimates and figures in the `output/` directory.  

#### code ####

Contains a R markdown script used to load and clean the raw chlamydia related data files from various sources for the user specified year. The resulting cleaned data sets are saved as `.csv `files in the `data/` directory. These files are described in detail in a data/ directory README file.

#### data ####

Contains the raw and cleaned data sets for the years 2014 and 2015 used in the cascade calculations. A detailed README file within the directory describes each of the data files in detail. Some of the data files are stored locally and are not available in the online repository. 

#### output ####

Contains output files storing the results produced by the `ChlamydiaCascade.Rmd` script. The results are stored in three `.csv` files for each specified year and as figures within a figure/ folder (created by `ChlamydiaCascade.Rmd`). The repository currently only holds the 2015 cascade results which were published in the Australian Chlamydia Cascade manuscript. 









 

