# Code for the paper: "Predicting Common Audiological Functional Parameters as intermediate representation in a clinical decision support system for audiology"
#---------------------------------------------------------------------------------------------
Folders are described below, each contains data and plot folders. Scripts for plots are within 
plot folders 

Main scripts can be find in each folder. labeled_scripts contain a folder varImp_scripts, which
contains the scripts for the extraction of variable importance

#------ MB ---------------------#
Scripts for labeled dataset 
- Preprocessing
- Model building (Lasso, Elastic Net and Random Forests)
- Variable Importance
- Corresponding plots 

#------ EV ---------------------#
Scripts for the unlabeled dataset 

- Preprocessing 
- Prediction of CAFPAs
- Expert cluster prediction 
- Unlabeled data prediction 
- Corresponding plots 

#------ CAFPA_PLOT -------------#
contains the MATLAB scripts to generate the CAFPA plots 

- seperately for each model and generally 

#------ PSM_R ------------------# 
scripts for propensity score matching 
-> and CAFPA prediction with matched dataset 
