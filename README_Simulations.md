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

3. DatasetsAnalysis

##### How to run the simulations?

1. Open the 'Rproj' file 'GroupLevelObjectivePerformance' in R Studio.
2. Make sure the library 'groundhog' is installed. This step is important since all packaged used in the simulations project rely on the environment loaded by groundhog.
2. Navigate to the 'Simulations' folder
3. Open the script 'Run.R'
4. Running the script will:
4.1. Install and load all libraries required by our code using the groudhog library.
4.2. Source all R scripts needed to run our code.
4.3. Load the simulations configuration (defined in Common\Definitions.R; see 'fixed_params' parameter and class).
4.4. Run all simulations according to the simulation configuration.
4.5. Save the results of the simulations to an RData file, and produce plots for the results.
