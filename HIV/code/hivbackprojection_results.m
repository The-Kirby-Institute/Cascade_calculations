%% MSM and non-MSM back projected estimates
% This script reads in the results files produced by James Jannson's
% backprojection model and converts them into table format for use in the
% HIV cascade estimates.
%
% Citation: Jansson, James, et al. "Inferring HIV incidence from case 
% surveillance with CD4+ cell counts." AIDS (London, England) (2015).
% 
% Written by: Richard T. Gray, The Kirby Institute, Faculty of Medicine,
% UNSW
%
% Last updated: 2015-07-08
%

%% Initialization

clc; clear all; close all; 

analysisYear = 2013;

dataFolder = 'C:/Users/Rgray/Documents/Research/!Evaluation_Modelling/project_care_cascades/data/raw/';
outputFolder = 'C:/Users/Rgray/Documents/Research/!Evaluation_Modelling/project_care_cascades/data/backprojection-clean/';
outputFolder = [outputFolder num2str(analysisYear) '/'];

timestep = 0.1;
stepsYear = 1/timestep;

%% Read in the .mat result files 

load([dataFolder 'James_Model_MSMStruct']);
load([dataFolder 'James_Model_NonMSMStruct']);

%% Extract the results we want

msmResults = MSMResultsRG.Local;

% Use undiagnosed for setting things up
msmUndiagnosed = msmResults.Undiagnosed;

% Number of years run and index of data we want — common across indicators
numyears = length(msmUndiagnosed.Time)*timestep;
startyear = min(msmUndiagnosed.Time); % effectively start 1965 to start of 2014
                                      % Year specified is for the 1st of
                                      % Jan. I am going to assume this is
                                      % equal to 31st Dec.
                                      

years = startyear:(startyear+numyears-1);

% Take the results just before the end of the year to represent the number
% in the current year
yearIndex = stepsYear:stepsYear:(numyears*stepsYear);

headings = {'year','mean','median','lowerCI','UpperCI'};

%% MSM results

% Undiagnosed results
undiagnosedArray = zeros(numyears,5); 
undiagnosedArray(:,1) = years;
undiagnosedArray(:,2) = msmUndiagnosed.Mean(yearIndex);
undiagnosedArray(:,3) = msmUndiagnosed.Median(yearIndex);
undiagnosedArray(:,4) = msmUndiagnosed.LCI(yearIndex);
undiagnosedArray(:,5) = msmUndiagnosed.UCI(yearIndex);

% Write to separate file
xlswrite([outputFolder 'undiagnosedmsm.xlsx'], [headings; num2cell(undiagnosedArray)]);

% Incidence
msmIncidence = msmResults.Incidence;
incidenceArray = zeros(numyears,5);
incidenceArray(:,1) = years;
incidenceArray(:,2) = msmIncidence.Mean;
incidenceArray(:,3) = msmIncidence.Median;
incidenceArray(:,4) = msmIncidence.LCI;
incidenceArray(:,5) = msmIncidence.UCI;

% Write to separate file
xlswrite([outputFolder 'incidencemsm.xlsx'], [headings; num2cell(incidenceArray)]);

% Deaths
msmDeaths = msmResults.Deaths;
deathsArray = zeros(numyears,5);
deathsArray(:,1) = years;
deathsArray(15:end,2) = msmDeaths.Mean;
deathsArray(15:end,3) = msmDeaths.Median;
deathsArray(15:end,4) = msmDeaths.LCI;
deathsArray(15:end,5) = msmDeaths.UCI;

% Write to separate file
xlswrite([outputFolder 'deathsmsm.xlsx'], [headings; num2cell(deathsArray)]);

% Diagnosed
msmDiagnosed = msmResults.CurrentlyDiagnosed;
diagsArray = zeros(numyears,5);
diagsArray(:,1) = years;
diagsArray(15:end,2) = msmDiagnosed.Mean;
diagsArray(15:end,3) = msmDiagnosed.Median;
diagsArray(15:end,4) = msmDiagnosed.LCI;
diagsArray(15:end,5) = msmDiagnosed.UCI;

% Write to separate file
xlswrite([outputFolder 'diagnosesmsm.xlsx'], [headings; num2cell(diagsArray)]);

%% Non-MSM results

nmsmResults = NonMSMResultsRG.Local;

% Undiagnosed results
nmsmUndiagnosed = nmsmResults.Undiagnosed;
undiagnosedArray = zeros(numyears,5); 
undiagnosedArray(:,1) = years;
undiagnosedArray(:,2) = nmsmUndiagnosed.Mean(yearIndex);
undiagnosedArray(:,3) = nmsmUndiagnosed.Median(yearIndex);
undiagnosedArray(:,4) = nmsmUndiagnosed.LCI(yearIndex);
undiagnosedArray(:,5) = nmsmUndiagnosed.UCI(yearIndex);

% Write to separate file
xlswrite([outputFolder 'undiagnosednon-msm.xlsx'], [headings; num2cell(undiagnosedArray)]);

% Incidence
nmsmIncidence = nmsmResults.Incidence;
incidenceArray = zeros(numyears,5);
incidenceArray(:,1) = years;
incidenceArray(:,2) = nmsmIncidence.Mean;
incidenceArray(:,3) = nmsmIncidence.Median;
incidenceArray(:,4) = nmsmIncidence.LCI;
incidenceArray(:,5) = nmsmIncidence.UCI;

% Write to separate file
xlswrite([outputFolder 'incidencenon-msm.xlsx'], [headings; num2cell(incidenceArray)]);

% Deaths
nmsmDeaths = nmsmResults.Deaths;
deathsArray = zeros(numyears,5);
deathsArray(:,1) = years;
deathsArray(15:end,2) = nmsmDeaths.Mean;
deathsArray(15:end,3) = nmsmDeaths.Median;
deathsArray(15:end,4) = nmsmDeaths.LCI;
deathsArray(15:end,5) = nmsmDeaths.UCI;

% Write to separate file
xlswrite([outputFolder 'deathsnon-msm.xlsx'], [headings; num2cell(deathsArray)]);

% Diagnosed
nmsmDiagnosed = nmsmResults.CurrentlyDiagnosed;
diagsArray = zeros(numyears,5);
diagsArray(:,1) = years;
diagsArray(15:end,2) = nmsmDiagnosed.Mean;
diagsArray(15:end,3) = nmsmDiagnosed.Median;
diagsArray(15:end,4) = nmsmDiagnosed.LCI;
diagsArray(15:end,5) = nmsmDiagnosed.UCI;

% Write to separate file
xlswrite([outputFolder 'diagnosesnon-msm.xlsx'], [headings; num2cell(diagsArray)]);













