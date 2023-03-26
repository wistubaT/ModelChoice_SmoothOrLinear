# ModelChoice_SmoothOrLinear
In this Github repository you can find the code to run the simluations from the paper "Linear or smooth? Enhanced model choice in boosting via deselection of base-learners". You can also create your own setups to test the proposed procedure for variable selection and model choice.

Before running, you should delete the "temp" files in the folders "Results" and "Plots". These files are only necessary to create the folders within the repository and to maintain the structure of the project after downloading the code as a zip file. 

In order to run the simulations, you need to execute the the code in the file "testSetups.R". All necessary results are saved in the in folder "Results". The code only runs simulations for setups for which no results are saved in the folder "Results" yet. It is important to note that if you want to modify a simulation setup, you need to remove the respective results files from the "Results" folder first. Alternatively, you can also adjust the setup and just save it as a new setup file in the "Setups" folder. Tables with proportions of how many base-learners of which predictors are included in all simulations and boxplots with RMSE values after each step of the procedure are created after executing the code in the file "generatePlots.R". The tables and boxplots are saved in the folder "Plots" as pdf-files.

Necessary R-packages: mboost, plyr
Make sure to install these packages prior to running the files "testSetups.R" and "generatePlots.R".

## Creating your own setups
Hello
