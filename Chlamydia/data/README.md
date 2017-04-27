## Chlamydia Cascade Data ##

This folder contains all the data inputs used to calculate the Australian Chlamydia diagnosis and care cascade.

Each file stores input on incidence/new infections, notifications, treatment and retesting by year:

- chlamincidenceYEAR.csv: Contains annual estimates for new infections by gender and age group. This was obtained using the model from Ali H, Cameron E, Drovandi C, McCaw J, Guy R, Middleton M, et al. A new approach to estimating trends in chlamydia incidence. Sexually transmitted infections 2015.
- chlamnotificationsYEAR.csv: Contains the raw notifications data for each gender and age group obtained from the NNDSS. 
- chlamtreatmentYEAR.csv: These files contain the treatment and re-testing inputs for the cascade calculations. These inputs are entered manually from various sources and individual studies as provided in the source and reference columns. 

Note the estimates in chlamtreatment2014.csv were used in the 2014 cascade. These values were updated for the 2015 cascade as shown in chlamtreatment2015.csv. The 2015 values were used for the Australian Chlamydia Cascade manuscript.  

The retesting inputs in the chlamtreatmentYEAR.csv files are entered manually from three specific files:  

- retestsAccessYEAR.csv: Contains results for sexual health clinics in ACCESS by region nationally. These values were used for sexual health clinic estimates in the cascade calculations. For regional and remote areas we used the combined regional+remote values due to the small numbers for the remote settings. 
- restetsNSWgp2015.csv: Contains results for general practice clinics in ACCESS by region for the state of NSW. These results were used for urban general practice estimates in the 2015 cascade calculations.
- retestsAccept2014.csv: Contains results for general practice clinics in regional areas from the control arm of the ACCEPt study. These results were used for the regional and remote general practice estimates in the 2015 cascade calculations.

These there files are produced by cleaning the raw data files provided by collaborators using the ~/code/ChlamydiaClean.Rmd script. The raw data files are stored in the 'raw' sub-folder which is not included in the git repository.
