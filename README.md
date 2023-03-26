# Model Choice - Smooth or Linear?
In this Github repository you can find the code to run the simluations from the paper "Linear or smooth? Enhanced model choice in boosting via deselection of base-learners". You can also create your own setups to test the proposed procedure for variable selection and model choice.
<br/>
<br/>
Before running, you should delete the "temp" files in the folders "Results" and "Plots". These files are only necessary to create the folders within the repository and to maintain the structure of the project after downloading the code as a zip file. 
<br/>
<br/>
In order to run the simulations, you need to execute the the code in the file "testSetups.R". All necessary results are saved in the in folder "Results". The code only runs simulations for setups for which no results are saved in the folder "Results" yet. It is important to note that if you want to modify a simulation setup, you need to remove the respective results files from the "Results" folder first. Alternatively, you can also adjust the setup and just save it as a new setup file in the "Setups" folder. Tables with proportions of how many base-learners of which predictors are included in all simulations and boxplots with RMSE values after each step of the procedure are created after executing the code in the file "generatePlots.R". The tables and boxplots are saved in the folder "Plots" as pdf-files.
<br/>
<br/>
Necessary R-packages: mboost, plyr
Make sure to install these packages prior to running the files "testSetups.R" and "generatePlots.R".

## Creating your own setups
Custom setups can be created with the file "#000Default.xlsx". The total number of linear and uninformative predictors is determined in the column "nslopes". The slopes of all informative linear predictors are set in the column "beta".  Each slope is included in one respective cell. The columns "nObs_fit" and "nObs_test" determine the sample sizes of the training and testing datasets for the simulations. The non-linear effects of all smooth predictors are determined in the column "funs". The effect of each smooth predictor has to be included in one cell each. The smooth functions have to be defined in R-syntax with "x" the variable name. The standard deviation for the normally distributed error term, the total number of simulations that should be performed and the number of maximum boosting iterations for all simulations are determined in the columns "se", "nDatasets" and "maxIter" respectively. The setup file needs to be saved as a csv-file. Note that the custom setup file must not have a name so that it appears before the "#000Default.xlsx" file.<br/>
A setup file for a dataset with 5 linear predictors with slopes 1,2,3,4 and 5, 5 uninformative predictors, 100 observations for the training dataset, 200 observations for the testing dataset, a sine function for a smooth predictor, a cosine function for another smooth predictor, a standard deviation of 1 for the normally distributed error term, 100 simulation runs and 1000 boosting iterations for each simulation is created with the following table that has to be saved as a csv-file:<br/>
<br/>
nslopes | beta | nObs_fit | nObs_test | funs | se | nDatasets | maxIter
:---: | :---: | :---: | :---: | :---: | :---: | :---: | :---:
10 | 1 | 100 | 200 | sin(x) | 1 | 100 | 1000
   | 2 |   |   | cos(x) |   |   |  
  | 3 |   |   |   |   |   |  
  | 4 |   |   |   |   |   |  
  | 5 |   |   |   |   |   |  
