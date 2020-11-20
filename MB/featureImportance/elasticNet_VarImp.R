################################################################################################
#        Feature Importance with Elastic Net                  
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

library(ggplot2)
library(glmnet)

# set up folders and directories
dir.create(file.path("MB\\data\\VarImp"), recursive = TRUE) 
dir.create(file.path("MB\\plots\\VarImp\\"), recursive = TRUE) 
path_data <- "MB\\data\\VarImp\\"
path_plot <- "MB\\plots\\VarImp\\"

# set seed
set.seed(22)

# load output var
load(paste0("MB\\data\\preprocess\\preprocessed_data_final.Rdata"))

# select random imputation
imp <- round(runif(1, min=1, max=20))

varImp <- list()
elasticNet.varImp <- list()

# load preprocessed data   
load(paste0( "MB\\data\\le_ri_combine_pta\\data\\data_le_ri_combined_pta_", imp ,".Rdata"))


for(i in 1:length(output.var)) {
  
  # LOO-CV 
  
  n <- output.var[subj.use[,i],i]
  elastic_net.coef <- list()
  
  for(c in 1:length(n)){
    
    # make input & output dependent on subjects to use
    output <- output.var[subj.use[,i],i]
    input <- data[subj.use[,i],]
    
    input$output <- output
    
    # Set training control
    cv_5 = trainControl(method = "cv", number = 10)
    
    # Train the model
    elastic_net_model <- train(output~ .,data = input[-c,],
                               method = "glmnet",
                               metric = "RMSE",
                               trControl = cv_5,
                               tuneLength = 10
    )
    

    # extract coefficients 
    tmp.coef <- coef(elastic_net_model$finalModel, elastic_net_model$bestTune$lambda)
    idx <- as.vector(tmp.coef@i+1)
    rname<- tmp.coef@Dimnames[[1]][idx]
    val <- as.vector(tmp.coef@x)
    varImp[c] <- list(data.frame(rname,val))
    
    print(paste(i, c))
    
  } # cv loop
  
  elasticNet.varImp[i] <- list(varImp)
  
  
} # cafpa loop

save(elasticNet.varImp, file = paste0(path_data,"elasticnet.Rdata"))

#load("elasticnet.Rdata")

# Extract features across LOOCV folds and order them ----------------------------

# extract varnames 
cafpa_names <- c("ca1", "ca2", "ca3", "ca4", "cu1", "cu2", "cb", "cn", "cc", "ce")
cafpas <- list()
cafpas_tmp <- list()
count = 1
half <- 115

for (name in cafpa_names){
  for (i in 1:length(elasticNet.varImp[[1]])){
  cafpas_tmp[[name]][[i]] <- as.character(elasticNet.varImp[[count]][[i]][["rname"]])
  }
  cafpas[[name]] <- as.data.frame(table(unlist(cafpas_tmp[[name]])))
  cafpas[[name]] <- cafpas[[name]][-1,]
  cafpas[[name]] <- cafpas[[name]][order(cafpas[[name]]$Freq, decreasing = TRUE),]
  cafpas[[name]] <-  cafpas[[name]][!(cafpas[[name]]$Freq < half), ]
  
  count = count = count+1
}

# save feature importance for elastic net
elastic.vim <- cafpas
save(elastic.vim, file = paste0(path_data,"elastic_vim.Rdata"))

#### PLOTS ----------------------------------------------------------------------
pdf(paste0(path_plot, "Enet_Var_Imp_by_occurence.pdf"), onefile = TRUE)

for (name in cafpa_names){
 print(ggplot(data = cafpas[[name]], aes(x = reorder(cafpas[[name]]$Var1, - cafpas[[name]]$Freq), y = cafpas[[name]]$Freq)) + 
    geom_bar(stat = "identity", fill = "darkblue") +
    labs(title= paste(name, " - Variable occurence across iteration"), 
         x="", y = "Frequency")+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
 )
  dev.off()
}





