#################################################
#         Preprocessing new CAFPA data 
#         without expert labels
#   
#       -> MICE
#       
#################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# set up folders and directories
dir.create(file.path("EV\\data\\imp_data_new\\"), recursive = TRUE)  

path_data <- "data_original\\"
path_database <- "MB\\data\\MICE_imputation\\"
path_save <- "EV\\data\\imp_data_new\\"

#libraries 
library(mice)

# for reproducibility of sample drawn
set.seed(22) 


# load the data 
dat <- read.csv(paste0(path_data, "CAFPA_dataset_meas_unlabeled_18-Jun-2020.txt"), header = TRUE, sep = "")

# reorder column names (delete X and assign the age to m.age)
header_names <- names(dat)
header_names <- header_names[-1]

dat[,83] <- NULL
colnames(dat) <- header_names

# load database (old data) (random imputation)
imp <- round(runif(1, min=1, max=20))
dataMEAS <- load(paste0(path_database, "MEAS_mice_imputation_rf_", imp, ".Rdata"))

# Make new data consistent with variables used for the models
var_names <- names(data.imputed)
new_dat <- dat[,var_names]

# make data consistent
new_dat[new_dat == "NULL"] <- NA
new_dat[new_dat == "NaN"] <- NA
# change gender to [0,1]
new_dat$m.gender <- as.numeric(new_dat$m.gender)
new_dat$m.gender[new_dat$m.gender == 1] <- 0 # male 
new_dat$m.gender[new_dat$m.gender == 2] <- 1 # women


# count NaNs for each column 
nan.column <-sapply(new_dat, function(y) sum(length(which(is.na(y)))))
nan.column.frame <-data.frame(nan.column)

# count nans in each row
nan.row.all <- rowSums(is.na(dat))
nan.row.all.frame <- data.frame(nan.row.all)


###############################################
# make factors out of categorical variables   #
###############################################

# unordered
dat$m.tinnitus_ri <- as.factor(dat$m.tinnitus_ri)
dat$m.tinnitus_le <- as.factor(dat$m.tinnitus_le)
dat$m.language    <- as.factor(dat$m.language)
dat$m.gender      <- as.factor(dat$m.gender)

# factors with levels (but not yet ordered)
dat$m.hp_noise          <- as.ordered(dat$m.hp_noise) 
dat$m.hp_quiet          <- as.ordered(dat$m.hp_quiet) 



#----------------------------------------
# combine old and imputed dataset for further imputations

base_newDat <- rbind(data.imputed,new_dat)

# initialize predictor matrix 
pred.Matrix <- 1 - diag( 1, ncol(base_newDat) )
rownames( pred.Matrix ) <- colnames( pred.Matrix) <- colnames(base_newDat )


#************************************************************
# MICE imputation with method randomForest
mi.res <- mice( base_newDat , predictorMatrix = pred.Matrix , 
                method = c(rep( "rf" , ncol(base_newDat) ))  ,
                m = 20  , maxit= 20   
)

# plot variable convergence (mean and SD of imputations across iterations)


pdf(paste0(path_save, "input.var_imputation_mice_convergence_final_rf.pdf") , onefile=T )
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



################################
#   Save variables             #
################################

#.....................
## save datasets
M = 20
for (i in 1:M){
  data.imputed <- mice::complete(mi.res, action = i)
  save(data.imputed, file = (paste0(path_save, paste0("MEAS_mice_imputation_rf_", i, ".Rdata"))))
}

# save mi.res
save(mi.res, file = (paste0(path_save, paste0("mi.res_MODEL_final_rf.Rdata"))))


