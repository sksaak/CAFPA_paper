################################################################################################
#        Feature Importance with Random Forests                  
#
#        - 1 random imputation selected 
#        - LOOCV to extract frequencies 
#
################################################################################################


# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()
graphics.off()

# libraries
library(randomForest)
library(caret)
library(ggplot2)

# set up folders and directories
dir.create(file.path("MB\\data\\VarImp"), recursive = TRUE) 
dir.create(file.path("MB\\plots\\VarImp\\"), recursive = TRUE) 
path_data <- "MB\\data\\VarImp\\"
path_plot <- "MB\\plots\\VarImp\\"

path_data <- paste0(MAIN, "MB\\data\\")


# set seed
set.seed(22)

# load output var
load("MB\\data\\preprocess\\preprocessed_data_final.Rdata")

# select random imputation
imp <- round(runif(1, min=1, max=20))

cafpa_names <- c("ca1", "ca2", "ca3", "ca4", "cu1", "cu2", "cb", "cn", "cc", "ce")
tmp.varImp <- list()
varImp <- list()
rf.varImp <- list()

# load preprocessed data   
load(paste0("MB\\data\\le_ri_combine_pta\\data\\data_le_ri_combined_pta_", imp ,".Rdata"))

# set up cross validation
cross.val <- trainControl(method="cv", number = 10)

data.imputed <- data

for(i in 1:length(output.var)) {
  
  # make input & output dependent on subjects to use
  output <- output.var[subj.use[,i],i]
  input <- data.imputed[subj.use[,i],] 
  
  # put input vars and specified cafpa output into dataframe
  data <- data.frame(input, output)
  
  # set up hyperparameter with ML algorithm (searches for best mtry given data)
  bestmtry <- tuneRF(input, output, stepFactor=1.5, improve=1e-5, ntree=500)
  mtry = bestmtry[,1]
  tuning <- expand.grid(mtry = c(bestmtry[,1]))
  
  # LOO-CV 
    
    n <- output.var[subj.use[,i],i]
    
    for(c in 1:2){#length(n)){
      
  
      # train models
      pred.mod = train(output ~ .,
                       data= data[-c,], 
                       method = 'rf', 
                       metric = "RMSE",
                       tuneGrid = tuning,
                       trControl = cross.val,
                       importance = TRUE)
      
      
      
      # variable importance
      tmp.varImp[[c]] <- varImp(pred.mod)
      varImp[[c]] <- tmp.varImp[[c]][["importance"]][["Overall"]]
    
      
      #a <- importance(pred.mod)
      
      
      print(paste(i, c))
      
    } # cv loop
    
    rf.varImp[i] <- list(varImp)
    
  
} # cafpa loop



rf.names <- pred.mod$coefnames
names(rf.varImp) <- cafpa_names


# loop through cv results
cafpas_tmp <-list()
cafpas <- list()
for(name in cafpa_names){
  
  cafpas_tmp[[name]] <- as.data.frame(rf.varImp[[name]])
  
  tmp <- as.data.frame(rowMeans(cafpas_tmp[[name]]))
  tmp$Var1 <- rf.names
  names(tmp) <- c("Imp", "Var")
  cafpas[[name]] <- tmp
  cafpas[[name]] <- cafpas[[name]][!(cafpas[[name]]$Imp < 50),]
  
  }

save(rf.varImp, file = paste0(path_data, "rf.varImp.Rdata"))



#### PLOTS 
pdf(paste0(path_plot, "RF_Variable_Importance_by_occurence.pdf"), onefile = TRUE)

for (name in cafpa_names){
  print(ggplot(data = cafpas[[name]], aes(x = reorder(cafpas[[name]]$Var, - cafpas[[name]]$Imp), y = cafpas[[name]]$Imp)) + 
          geom_bar(stat = "identity", fill = "darkblue") +
          labs(title= paste(name, " - Variable occurence across iteration"), 
               x="", y = "Importance")+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  )
  dev.off()
}