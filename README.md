# "Predicting Common Audiological Functional Parameters as intermediate representation in a clinical decision support system for audiology"

### Samira K. Saak, Mareike Buhl, Andrea Hildebrandt, Birger Kollmeier
---------------------------------------------------------------------------------------------

## ABSTRACT
The application of machine learning for the development of clinical decision-support systems in audiology provides the potential to improve the objectivity and precision of clinical experts’ diagnostic decisions. However, for successful clinical application, such a tool needs to be accurate, as well as accepted and trusted by physicians. In the field of audiology, large amounts of patients’ data are being measured, but these are distributed over local clinical databases and are heterogeneous with respect to the applied assessment tools. For the purpose of integrating across different databases, the Common Audiological Functional Parameters (CAFPAs) were recently established as abstract representations of the contained audiological information describing relevant functional aspects of the human auditory system. As an intermediate layer in a clinical decision-support system for audiology, the CAFPAs aim at maintaining interpretability to the potential users. Thus far, the CAFPAs were derived by experts from audiological measures. For designing a clinical decision-support system, in a next step the CAFPAs need to be automatically derived from available data of individual patients. Therefore, the present study aims at predicting the expert generated CAFPA labels using three different machine learning models, namely the lasso regression, elastic nets, and random forests. Furthermore, the importance of different audiological measures for the prediction of specific CAFPAs is examined and interpreted. The trained models are then used to predict CAFPAs for unlabeled data not seen by experts. Prediction of unlabeled cases is evaluated by means of model-based clustering methods. Results indicate an adequate prediction of the ten distinct CAFPAs. All models perform comparably and turn out to be suitable choices for the prediction of CAFPAs. They also generalize well to unlabeled data. Additionally, the extracted relevant features are plausible for the respective CAFPAs, facilitating interpretability of the predictions. Based on the trained models, a prototype of a clinical decision-support system in audiology can be implemented and extended towards clinical databases in the future. 


----------------------------------------------------------------------------------------------
## The code for reproducing the analysis can be found in the folders, that is:  

## MB
Scripts for labeled dataset 
- Preprocessing
- Model building (Lasso, Elastic Net and Random Forests)
- Variable Importance
- Corresponding plots 

## EV 
Scripts for the unlabeled dataset 
- Preprocessing 
- Propensity score matching
- Prediction of CAFPAs
- Clusters for labels 
- Clusters for unlabeled data 
- Corresponding plots 

## CAFPA_PLOT 
contains the MATLAB scripts to generate the CAFPA plots 
- seperately for each model

#### Data availability
According to the data usage agreement of the authors, the datasets analyzed in this study can only be shared upon motivated request.
