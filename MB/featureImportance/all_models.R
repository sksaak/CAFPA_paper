################################################################################################
#        Feature Importance Agreement across models                  
#
#        - 1 random imputation  
#        - across LOOCV folds
#
################################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()
graphics.off()

# set seed
set.seed(22)

cafpa_names <- c("ca1", "ca2", "ca3", "ca4", "cu1", "cu2", "cb", "cn", "cc", "ce")

# load data and prepare for plotting
#----------------------------------------------------------------------
#   Lasso Regression
#----------------------------------------------------------------------

load("MB\\data\\VarImp\\lassoVarImp.Rdata")

#----------------------------------------------------------------------
#   Elastic Net 
#----------------------------------------------------------------------
# load data
load("MB\\data\\VarImp\\elastic_vim.Rdata")

#----------------------------------------------------------------------
#   Random forest
#----------------------------------------------------------------------

# load the data 
load("MB\\data\\VarImp\\rf.varImp_1.Rdata")
load("MB\\data\\VarImp\\rf.varImp_2.Rdata")
load("MB\\data\\VarImp\\rf.varImp_3.Rdata")
load("MB\\data\\VarImp\\rf.varImp_4.Rdata")
load("MB\\data\\VarImp\\rf.varImp_5.Rdata")
load("MB\\data\\VarImp\\rf.varImp_6.Rdata")
load("MB\\data\\VarImp\\rf.varImp_7.Rdata")
load("MB\\data\\VarImp\\rf.varImp_8.Rdata")
load("MB\\data\\VarImp\\rf.varImp_9.Rdata")
load("MB\\data\\VarImp\\rf.varImp_10.Rdata")

# combine seperated rf variable imp and delete the double saved things
rf.varImp <- list(rf.varImp_1, rf.varImp_2, rf.varImp_3, rf.varImp_4,
                  rf.varImp_5, rf.varImp_6, rf.varImp_7, rf.varImp_8,
                  rf.varImp_9, rf.varImp_10)

# extract those with VImp larger than 50 
for (i in 1:10){
  rf.varImp[[i]] <-  rf.varImp[[i]][[1]][!(rf.varImp[[i]][[1]][["Imp"]] < 50),]
  
}

rm(rf.varImp_1, rf.varImp_2, rf.varImp_3, rf.varImp_4,
   rf.varImp_5, rf.varImp_6, rf.varImp_7, rf.varImp_8,
   rf.varImp_9, rf.varImp_10)

#------------------------------------------------------------------------------
#  COMBINE MODELS  (just add the other models)
#-------------------------------------------------------------------------------

num = 3 # number of models
cafpas <- list()

for (name in cafpa_names){
  tmp <- c(as.character(lasso.Imp[[name]][["Var1"]]), as.character(elastic.vim[[name]][["Var1"]]), rf.varImp[[name]][["Var"]])
  tmp <- table(tmp)
  cafpas[[name]] <- as.data.frame(tmp[tmp == num])
  
}

save(cafpas, file ="MB\\data\\VarImp\\all_models_vim.Rdata")
