# GroupLevelObjectivePerformance

The preprint can be found at: https://osf.io/preprints/psyarxiv/b967v_v1

# Simulations code readme

##### General folders structure

1. Common - includes the R scripts used by both the simulations and datasets reanalysis codes:
1.1. Awareness Tests - includes the implementation of all awareness tests used in our study, including the proposed ones.
1.2. Definitions - the common definitions used either across simulations and datasets reanalysis codes or are general data structes used across different scripts. 

2. Simulations - includes all scripts used to run the simulations in our study:
2.1. Analysis Types - includes an implementation of all tested scenarios.
2.2. AUCAnalysis - analyzed the area under the curve (AUC) analysis reported in the paper regarding the overall sensitivity of each test to awareness in the given tested scenarios.
2.3. GrandScatterPlot - produces the main figure for presenting the simulations results of our study.
2.4. Run - the main entry point to the simulations analysis. See "How to run?" below.
2.6. Simulation - the core simulation function, producing simulated data in each iteration and runs all tests implementing the 'awareness_test_class' (see Common\AwarenessTests.R) to estimate their sensitivity and specificity.

3. DatasetsReanalysis - includes all scripts used to run the datasets reanalysis in our study:
3.1. Run - the main script that runs the analysis. See "How to run?" below.
3.2. CheckAwareness - this function runs all objective awareness tests on each dataset 
3.3. ValidateDataset - this function compares the dataset's statistics with the ones reported in the relevant paper saved in the table ComparisonToPaperTable.csv in 'Data' folder.
3.4. compareDataToPaper - this function compares numbers: the statistical value calculated and the statistical number reported in the manuscript. It is called by the function ValidateDataset. 
3.5. LoadDataSets - this function loads all datasets to one dataframe
3.6. PlottingFunctions - this function generates all plots for the datasets reanalysis   
3.7. PreprocessResultsForPlots - this function preprocesses the data of awareness tests calculated for all datasets, to prepare it for plotting.
3.8. There is one script for each paper (named based on the authors names and publication year) that extracts data from this paper's folder in 'Datasets' folder, and saves it as an RData file in the 'Data' folder. 

##### How to run the simulations?

1. Open the 'Rproj' file 'GroupLevelObjectivePerformance' in R Studio.
2. Make sure Rtools for the relevant R version is installed. This is required for the next step where groundhog will install all required libraries
3. Make sure the library 'groundhog' is installed. This step is important since all packaged used in the simulations project rely on the environment loaded by groundhog.
4. Navigate to the 'Simulations' folder
5. Open the script 'Run.R'
6. Running the script will:
6.1. Install and load all libraries required by our code using the groudhog library.
6.2. Source all R scripts needed to run our code.
6.3. Load the simulations configuration (defined in Common\Definitions.R; see 'fixed_params' parameter and class).
6.4. Run all simulations according to the simulation configuration.
6.5. Save the results of the simulations to an RData file. 
6.6 In order to produce plots for the results run 'GrandScatterPlot.R'. Chose which plots to produce with the variable optionImage.
optionImage=1 - test types: GBC, T, MMLR, GBBayes, TBayes
create also a Bayesian image:  GBBayes, TBayes, with H0 for supplementary
optionImage=2 - test types: GBC, GB, Chi
optionImage=3 - GBC, GlobalNull
optionImage=4 - GBBayes, TBayes, GB_Bayes_Uninformative
6.7 In order to run the AUC analysis and produce plots run 'AUCAnalysis.R'

##### How to run the datasets Reanalysis?

1. Open the 'Rproj' file 'GroupLevelObjectivePerformance' in R Studio.
2. Make sure Rtools for the relevant R version is installed. This is required for the next step where groundhog will install all required libraries
3. Make sure the library 'groundhog' is installed. This step is important since all packaged used in the datset relanalysis project rely on the environment loaded by groundhog.
4. Navigate to the 'DatasetsReanalysis' folder
5. Open the script 'Run.R'
6. Running the script will:
6.1. Install and load all libraries required by our code using the groudhog library.
6.2. Source all R scripts needed to run our code.
6.3. Define parameters used in the analysis. The analysis can be run with objective aware participants (incSubjObjTest=1) and without (incSubjObjTest=0) 
6.4. Load all datasets to one dataframe
6.5. Compare all test's statistics to the ones reported in the manuscript and run all tests for each dataset.
6.5. Produce plots for the results.
