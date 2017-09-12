## HIV Cascade Data ##

This directory contains all the data inputs used to calculate the Australian HIV diagnosis and care cascade.

Most of the files store input by year (specified by YEAR below). Many of the data files are not available in the online repository as they contain data which is not available publicly. 

### Publicly available data ####

These files are available in the online repository. 

- `countryRegionCodes.csv`: Simply contains the country and global region code for each country used by the Australian Bureau of Statistics (ABS) for data collection. These codes are used to extract HIV notifications by country or region of birth.  
- `Hard_coded_estimates.csv`: Contains separate estimates for specific populations and stages of the HIV cascade obtained through alternative methods and data collection processes. These estimates replace the results obtained using the general HIV cascade methodology and scripts if they are thought to be more accurate for the specified population. 
- `HIVadjustments-YEAR.csv`: Contains the estimated relative change in population movement rate and death rate compared to the base estimates (for the overall national HIV-positive population) for specific populations from 1980 to YEAR. These relative changes are multiplied by the base estimates to obtained population specific population movement and death rates. 
- `HIVbaseEstimates-YEAR.csv`: Contains the estimated proportion of notifications which are unique, population movement rates, and death rates for the overall population of people living with HIV in Australia from 1980 to YEAR. These estimates form the base estimates for all sub-populations and are adjusted accordingly based on data in `HIVadjustments-YEAR.csv`. 
- `individualParameters.csv`: Specific HIV related estimates produced or published in separate work used in the HIV cascade calculations. 

### Local data ###

These data files are only available locally as they are not publicly available but can be provided in an appropriate format on request (Rgray@kirby.unsw.edu.au). 

- ``: 

