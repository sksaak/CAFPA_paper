################################################################################################
#        Feature Importance with Lasso regression                  
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

library(glmnet)
library(ggplot2)

# set up folders and directories
dir.create(file.path("MB\\data\\VarImp"), recursive = TRUE) 
dir.create(file.path("MB\\plots\\VarImp\\"), recursive = TRUE) 
path_data <- "MB\\data\\VarImp\\"
path_plot <- "MB\\plots\\VarImp\\"

# set seed
set.seed(22)

# load output var
load("MB\\data\\preprocess\\preprocessed_data_final.Rdata")

# select random imputation
imp <- round(runif(1, min=1, max=20))

cafpa_names <- c("ca1", "ca2", "ca3", "ca4", "cu1", "cu2", "cb", "cn", "cc", "ce")
varImp <- list()
lasso.varImp <- list()

# load preprocessed data   
load(paste0("MB\\data\\le_ri_combine_pta\\data\\data_le_ri_combined_pta_", imp ,".Rdata"))



for(i in 1:length(output.var)) {
  
  # LOO-CV 
  
  n <- output.var[subj.use[,i],i]
  
  for(c in 1:length(n)){
    
    # make input & output dependent on subjects to use
    output <- output.var[subj.use[,i],i]
    input <- data[subj.use[,i],]
    
    input$output <- output
    
    # Build x-Matrix and y-vector with one of the CAFPAs as dependent variable
    x <- model.matrix(output~., input)
    y <- output
    
    # define lambda-grid: 10^10 bis 10^-2 (contains all possibilities from OLS to null model)
    grid <- 10^seq(10,-2, length =100)
    # performing cross validation to extract the best lambda
    cv.out =cv.glmnet (x[-c ,],y[-c],alpha =1, lambda = grid)
    # extract the best lambda value from the cross-validation
    bestlam =cv.out$lambda.min
    # rebuilding the model with best lambda value identified
    lasso.best <- glmnet(x[-c,],y[-c],alpha =1, lambda=bestlam)
    
    lasso.coef=predict(lasso.best, type="coefficients", s=bestlam)[1:length(input),]
    
    
    varImp[c]  <- list(lasso.coef[lasso.coef != 0] )
    
    print(paste(i, c))
    
    
  } # cv loop
  
  lasso.varImp[i] <- list(varImp)
  
  
} # cafpa loop

# Extract features across LOOCV folds and order them ----------------------------

half <- 115
cafpas_tmp <- lasso.varImp
cafpas <- list()
for (i in 1:10){
  cafpas_tmp[[i]] <- lapply(cafpas_tmp[[i]], function(x) x[-1])
  cafpas_tmp[[i]] <- lapply(cafpas_tmp[[i]], function(x) sort(abs(x), decreasing = TRUE))
  
  tmp <- as.data.frame(table(names(unlist(cafpas_tmp[[i]]))))
  cafpas[[i]] <- tmp[!(tmp$Freq < half), ]
}

 names(cafpas) <- cafpa_names
 lasso.Imp <- cafpas
 
 save(lasso.Imp, file = paste0(path_data, "VarImp\\lassoVarImp.Rdata"))

#### PLOTS -----------------------------------------------------------------------
pdf(paste0(path_plot, "Lasso_Variable_Importance_by_occurence.pdf"), onefile = TRUE)

for (name in cafpa_names){
  print(ggplot(data = cafpas[[name]], aes(x = reorder(cafpas[[name]]$Var1, - cafpas[[name]]$Freq), y = cafpas[[name]]$Freq)) + 
          geom_bar(stat = "identity", fill = "darkblue") +
          labs(title= paste(name, " - Variable occurence across iteration"), 
               x="", y = "Frequency")+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  )
  dev.off()
}

