################################################################################################
#        Elastic net for CAFPA prediction                  
#
#     
#
################################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# load libraries 
library(glmnet)
library(caret)
library(tidyverse)

# set up folders and directories
FOLDER <- "MB\\data\\le_ri_combine_pta\\"
dir.create(file.path(FOLDER), recursive = TRUE) 
dir.create(file.path(paste0(FOLDER, "data\\")), recursive = TRUE) 
dir.create(file.path(paste0(FOLDER, "data\\elasticNet\\")), recursive = TRUE) 
dir.create(file.path(paste0(FOLDER, "models\\elasticNet\\")), recursive = TRUE) 
dir.create(file.path("MB\\plots\\le_ri_combine_pta\\elastic_net\\"), recursive = TRUE) 

path_data <- paste0(FOLDER, "data\\")
path_save <- paste0(FOLDER, "data\\elasticNet\\")
path_model<- paste0(FOLDER, "models\\elasticNet\\")
path_plot <- "MB\\plots\\le_ri_combine_pta\\elastic_net\\"

set.seed (22)

### FUNCTIONS
# function to extract best results
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

###
# iterations of mice
imp.num = 20

# predefine vars
elastic_net.train <-list()
elastic_net.coef <- list()
elastic_net.results <- list()
cafpa = c("c.c_a1", "c.c_a2", "c.c_a3", "c.c_a4", "c.c_u1", "c.c_u2", "c.c_b", "c.c_n", "c.c_c", "c.c_e")


# load output var
load("MB\\data\\preprocess\\preprocessed_data_final.Rdata")
input.var <- NULL # not imputed one is loaded as well, delete! 

# make indices dependent on each cafpa 
indices <- list()

for (i in 1:length(output.var)){
  output <-  as.vector(output.var[subj.use[,i],i])
  indices[i]<- list(caret::createDataPartition(output, p=0.8, list=FALSE))
  
}


# load preprocessed data in loop 
for (m in 1:imp.num){
  
  load(paste0(path_data,"data_le_ri_combined_pta_", m ,".Rdata"))
  
  # make a pdf for each imp iteration containing all the plots for the cafpas  
  pdf(paste0(path_plot, m, "_elastic_net_plots.pdf") , onefile=T )
  
  # train
  train_MAE_all <- NULL
  train_rsq_all <- NULL

  # test
  test_MAE_all <- NULL
  test_rsq_all <- NULL
  predAct <- list()

  # bestTune
  bestTune <- NULL
 
  for(i in 1:length(output.var)) {
    
    # make input & output dependent on subjects to use
    output <- output.var[subj.use[,i],i]
    input <- data[subj.use[,i],]
    
    input$output <- output
    
    # build train and test set
    train = unlist(indices[i])
    test = (-train)
    output.test=output[test]
    
    # Set training control
    cv_5 = trainControl(method = "cv", number = 10)
    # train set -------------------------------------------------------------------

    # Train the model
    elastic_net_model <- train(output~ .,data = input[train,],
                               method = "glmnet",
                               metric = "MSE",
                               trControl = cv_5,
                               tuneLength = 10
                               )
   
    elastic_net_model$bestTune

    elastic_net.train[i]<- list(get_best_result(elastic_net_model))
   
    train_rsq_all[i] <- elastic_net.train[[i]][["Rsquared"]]
    train_MAE_all[i] <- elastic_net.train[[i]][["MAE"]]
    
    bestTune[i] <- list(elastic_net_model$bestTune)
    
    # extract coefficients 
    tmp.coef <- coef(elastic_net_model$finalModel, elastic_net_model$bestTune$lambda)
    idx <- as.vector(tmp.coef@i+1)
    rname<- tmp.coef@Dimnames[[1]][idx]
    val <- as.vector(tmp.coef@x)
    elastic_net.coef[i] <- list(data.frame(rname,val))
    
    # test set -------------------------------------------------------------------
    predictions <- elastic_net_model %>% predict(input[test,])
    
    test_rsq_all[i] <- R2(predictions, output.test)
    test_MAE_all[i] <- MAE(predictions, output.test)        
    
    # save predicted and actual values 
    df <- data.frame(output.test, predictions)
    names(df) <- c("actual", "preds")
    
    residual <- output.test-predictions
    df$res <- residual
    
    predAct[i] <- list(df)
    
    
    if(i == 1){c.c_a1.coef <- elastic_net.coef[i]}
    if(i == 2){c.c_a2.coef <- elastic_net.coef[i]}
    if(i == 3){c.c_a3.coef <- elastic_net.coef[i]}
    if(i == 4){c.c_a4.coef <- elastic_net.coef[i]}
    if(i == 5){c.c_u1.coef <- elastic_net.coef[i]}
    if(i == 6){c.c_u2.coef <- elastic_net.coef[i]}
    if(i == 7){c.c_b.coef  <- elastic_net.coef[i]}
    if(i == 8){c.c_n.coef  <- elastic_net.coef[i]}
    if(i == 9){c.c_c.coef  <- elastic_net.coef[i]}
    if(i == 10){c.c_e.coef <- elastic_net.coef[i]}

    
    save(elastic_net_model, file = (paste0(path_model, "elastic_imp_pta_", m , "_cafpa_", i, ".Rdata")))
    
        
    print(c(m,i))

  }
    
  # STORE DATA 
  
    # cafpas
    elastic_net.results$cafpas$c.c_a1[m] <- c.c_a1.coef
    elastic_net.results$cafpas$c.c_a2[m] <- c.c_a2.coef
    elastic_net.results$cafpas$c.c_a3[m] <- c.c_a3.coef
    elastic_net.results$cafpas$c.c_a4[m] <- c.c_a4.coef
    elastic_net.results$cafpas$c.c_u1[m] <- c.c_u1.coef
    elastic_net.results$cafpas$c.c_u2[m] <- c.c_u2.coef
    elastic_net.results$cafpas$c.c_b[m] <- c.c_b.coef
    elastic_net.results$cafpas$c.c_n[m] <- c.c_n.coef
    elastic_net.results$cafpas$c.c_c[m] <- c.c_c.coef
    elastic_net.results$cafpas$c.c_e[m] <- c.c_e.coef
    # model fit
    elastic_net.results$test_MAE[m] <- list(test_MAE_all)
    elastic_net.results$test_rsq[m] <- list(test_rsq_all)
    elastic_net.results$train_MAE[m] <- list(train_MAE_all)
    elastic_net.results$train_rsq[m] <- list(train_rsq_all)
    # tune parameter
    elastic_net.results$bestTune[m] <- list(bestTune)
    # predicted value
    elastic_net.results$predAct[m] <- list(predAct)
    
  
  
}


# save output 

save(elastic_net.results, file = (paste0(path_save, "elastic_net_pta.results.Rdata")))






