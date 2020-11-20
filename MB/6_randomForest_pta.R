################################################################################################
#        Random Forests for CAFPA prediction                  
#
#     
#
################################################################################################

# updated to use the same sample indices across iterations 

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# load libraries
library(caret)
library(randomForest)

# set up folders and directories
FOLDER <- "MB\\data\\le_ri_combine_pta\\"
dir.create(file.path(FOLDER), recursive = TRUE) 
dir.create(file.path(paste0(FOLDER, "data\\")), recursive = TRUE) 
dir.create(file.path(paste0(FOLDER, "data\\randomForest\\")), recursive = TRUE) 
dir.create(file.path(paste0(FOLDER, "models\\randomForest\\")), recursive = TRUE) 
dir.create(file.path("MB\\plots\\le_ri_combine_pta\\randomForest\\"), recursive = TRUE) 

path_data <- paste0(FOLDER, "data\\")
path_save <- paste0(FOLDER, "data\\randomForest\\")
path_model<- paste0(FOLDER, "models\\randomForest\\")
path_plot <- "MB\\plots\\le_ri_combine_pta\\randomForest\\"

set.seed (22)

# load output var
load("MB\\data\\preprocess\\preprocessed_data_final.Rdata")
input.var <- NULL # not imputed one is loaded as well, delete! 


rf.results <- list()
cafpa = c("c.c_a1", "c.c_a2", "c.c_a3", "c.c_a4", "c.c_u1", "c.c_u2", "c.c_b", "c.c_n", "c.c_c", "c.c_e")
imp.num = 20

# make indices dependent on each cafpa 
indices <- list()

for (i in 1:length(output.var)){
  output <- as.vector(output.var[subj.use[,i],i])
 indices[i]<- list(caret::createDataPartition(output, p=0.8, list=FALSE))

}


for (m in 1:imp.num){
  
  load(paste0(path_data,"data_le_ri_combined_pta_", m ,".Rdata"))
  
  pdf(paste0(path_plot, m, "_rf_plots.pdf") , onefile=T )
  
  
  # predefine vars for data storage
  # train
  train_MSE_all <- NULL
  train_MAE_all <- NULL
  train_rsq_all <- NULL
  train_ssr_all <- NULL
  train_mtry_all <- NULL
  
  # test
  test_MSE_all <- NULL
  test_MAE_all <- NULL
  test_rsq_all <- NULL
  test_ssr_all <- NULL
  test_mtry_all <- NULL
  
  # var importance
  varImportance <- list()
  
  # predictions
  predAct <- list()
  
  # loop across different CAFPAS -------------------------------------------------------------
  for (i in 1:length(output.var)){
    # make input & output dependent on subjects to use
    input <- data[subj.use[,i],]
    output <- output.var[subj.use[,i],i]
    
    # put input vars and specified cafpa output into dataframe
    dat <- data.frame(input, output)
    
    ind <- unlist(indices[i])
    # split data
    train.data <- dat[ind,]
    test.data <- dat[-ind,] 
    
    # set up cross validation
    cross.val <- trainControl(method="cv", number = 10)
    
    # set up hyperparameter with ML algorithm (searches for best mtry given data)
    bestmtry <- tuneRF(input[ind,], output[ind], stepFactor=1.5, improve=1e-5, ntree=500)

    print(bestmtry)
    mtry = bestmtry[,1]
    tuning <- expand.grid(mtry = c(bestmtry[,1]))
    
    # train models
    pred.mod = train(output ~ .,
                     data=train.data, 
                     method = 'rf', 
                     metric = "RMSE",
                     tuneGrid = tuning,
                     trControl = cross.val,
                     importance = TRUE)
    
    # summary of model 
    print(pred.mod)
    plot(pred.mod, main = paste0(cafpa[i], "   iteration: ", m))
    
    # variable importance
    varImportance[[i]] <- varImp(pred.mod)
    
    
    # train set -------------------------------------------------------------------
    train_pred <- predict(pred.mod, newdata = train.data[-45])
    
    train_MSE_all[i] <- mean(( train_pred -train.data$output)^2)
    train_MAE_all[i] <- mean(abs(train_pred -train.data$output))
    train_rsq_all[i] <- cor(train_pred, train.data$output)^2
    train_ssr_all[i] <- t(train.data$output - train_pred) %*% (train.data$output - train_pred)
    train_mtry_all[i] <- pred.mod$bestTune$mtry
    
    # test set -------------------------------------------------------------------
    test_pred <- predict(pred.mod, newdata = test.data[-45])
    
    test_MSE_all[i] <- mean(( test_pred -test.data$output)^2)
    test_MAE_all[i] <- mean(abs(test_pred -test.data$output))
    test_rsq_all[i] <- cor(test_pred, test.data$output)^2
    test_ssr_all[i] <- t(test.data$output - test_pred) %*% (test.data$output - test_pred)
    test_mtry_all[i] <- pred.mod$bestTune$mtry
    
    # save predicted and actual values 
    df <- data.frame(test.data$output, test_pred)
    names(df) <- c("actual", "preds")
    predAct[i] <- list(df)
    
    # store models
    save(pred.mod, file = (paste0(path_model, "rf_imp_pta_", m , "_cafpa_", i, ".Rdata")))
    
    print(c(m, i))
    
  }
  
 
  
  
  
  # store data
  rf.results$test_MAE[m] <- list(test_MAE_all)
  rf.results$test_MSE[m] <- list(test_MSE_all)
  rf.results$test_rsq[m] <- list(test_rsq_all)
  rf.results$test_ssr[m] <- list(test_ssr_all)
  rf.results$train_MAE[m] <- list(train_MAE_all)
  rf.results$train_MSE[m] <- list(train_MSE_all)
  rf.results$train_rsq[m] <- list(train_rsq_all)
  rf.results$train_ssr[m] <- list(train_ssr_all)
  rf.results$varImportance[m] <- list(varImportance)
  rf.results$varNames <- list(pred.mod$coefnames)
  # predicted value
  rf.results$predAct[m] <- list(predAct)
  
  dev.off()
}

# save output 

save(rf.results, file = (paste0(path_save, "rf.results_pta.Rdata")))









