################################################################################################
#         Imputing remaining missing features with MICE                    
#
#
################################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# load packages
library(mice)
library(VIM)


# set up folders and directories
dir.create(file.path("MB\\data\\preprocess\\"), recursive = TRUE) 
dir.create(file.path("MB\\data\\MICE_imputation\\"), recursive = TRUE) 
path_data <- "MB\\data\\preprocess\\"
path_save <- "MB\\data\\MICE_imputation\\"

# load the data
data <- load("MB\\data\\preprocess\\preprocessed_data_final.Rdata")

# calculate proportions of missing values for subjects and variables
propMiss <- list()
propMiss$variables <- list( colMeans(  is.na(input.var) ))
propMiss$subjects <- list( rowMeans(  is.na(input.var) ))
#write.csv2(propMiss, "ProportionMissings.csv", quote = F)


# plot missing data and save in pdf format
pdf(paste0(path_save, "input.var_missing_pattern_final.pdf") , onefile=T )
aggr_plot <- aggr(input.var, col=c('navyblue','red'), only.miss = TRUE, 
                  numbers=TRUE, sortVars=TRUE, labels=names(input.var), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
dev.off()

#----------------------------------------------------------------------------------------------
# MICE IMPUTATION                                                               

# kick out patient ID
input.var$patientID <- NULL

# initialize predictor matrix 
pred.Matrix <- 1 - diag( 1, ncol(input.var) )
rownames( pred.Matrix ) <- colnames( pred.Matrix) <- colnames(input.var )


#************************************************************
# MICE imputation with method randomForest
mi.res <- mice( input.var , predictorMatrix = pred.Matrix , 
                method = c(rep( "rf" , ncol(input.var ) ))  ,
                m = 20  , maxit= 20   
)

# plot variable convergence (mean and SD of imputations across iterations)

pdf( "input.var_imputation_mice_convergence_final_rf.pdf" , onefile=T )
plot(mi.res,ask=F) 

dev.off()

# store variables that have missing cases
indmiss <- which( mi.res$nmis > 0 )
# number of iterations
M <- mi.res$m 

# Summary of imputation 
summary( mi.res )

# head of first imputed dataset 
head( mice::complete( mi.res , action=1 ) )

# calculate still existing proportions (for control)
#imp_set = 1 # determines imputation set (1:20)
#data.imputed <- mice::complete(mi.res, action = imp_set)
#propMiss <- list()
#propMiss$variables <- list( colMeans(  is.na(data.imputed[,1:73]) ))
#propMiss$subjects <- list( rowMeans(  is.na(data.imputed[,1:73]) ))

#.....................
## save datasets

for (i in 1:M){
  data.imputed <- mice::complete(mi.res, action = i)
  save(data.imputed, file = (paste0(path_save, paste0("MEAS_mice_imputation_rf_", i, ".Rdata"))))
}

# save mi.res
save(mi.res, file = (paste0(path_save, paste0("mi.res_MODEL_final_rf.Rdata"))))

