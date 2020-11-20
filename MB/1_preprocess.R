################################################################################################
#         General preprocessing of the labeled dataset                   
#
#         - i.e. reading in data+
#         - select random patient IDs from the patients rated by multiple experts 
#         - excluding excessive NaNs
#
################################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

#library(xlsx)
library(openxlsx)

# set up folders and directories
dir.create(file.path("MB\\data\\"), recursive = TRUE)  
path_save <- "MB\\data\\"

# load the data 
dat <- read.xlsx("DATA_ALL.xlsx", 1)

# change column names for further processing (- to .))
colnames(dat) <- gsub("-", ".", colnames(dat))

# delete empty columns
dat[,1] <- NULL
dat[,117] <- NULL

# find the indexes with the same patient ID and choose one of those patients randomly
set.seed(22) # for reproducibiliy of sample drawn
subjects <- table(dat$patientID)
double_subjects <- subjects > 1
subjects.more <- names(subjects[double_subjects])
random.subj <- NULL
count <- 1

# select random subjects of the the multiple subjects
for (i in 1:length(subjects)){
  if(subjects[i] > 1){
    random.subj[count] <- sample(1:subjects[i],1)
    count = count + 1
   }
}

multiple.ID <- data.frame()
subj.index_delete <- NULL

# store the indexes of subjects to delete
for (i in 1:length(subjects.more)){
subj.index <- which(dat$patientID == subjects.more[i])
multiple.ID <- data.frame(c(multiple.ID, subj.index))

subj.index <- subj.index[(-random.subj[i])]
subj.index_delete <- c(subj.index_delete, subj.index)

}

# delete multiple subjects
subj.index_delete <- sort(subj.index_delete)

dat <- dat[-subj.index_delete,] 


# make data consistent
dat[dat == "NULL"] <- NA
dat[dat == "NaN"] <- NA
# change gender to [0,1]
dat$m.gender[dat$m.gender == "m"] <- 0 # male 
dat$m.gender[dat$m.gender == "w"] <- 1 # women
dat$m.gender <- as.numeric(dat$m.gender)



############################
# clear excessive NaNs     #
############################
var.count <-NULL
var.count[1] <- ncol(dat)


# check for columns with all NA values & delete
all.nan <- sapply(dat, function(x)all((is.na(x)))) 
deleted.vars <- list()
deleted.vars$all.nan <- list(names(dat[all.nan]))
dat[all.nan] <- NULL

var.count[2] <- ncol(dat)

# count NaNs for each column and kick out vars that have more than 30% missing variables
nan.column <-sapply(dat, function(y) sum(length(which(is.na(y)))))
nan.column.frame <-data.frame(nan.column)

# count proportion missing for each variable
propMiss <- (colSums(  is.na(dat) )/nrow(dat))
propMiss.list <- list()
propMiss.list$vars$all <- list(propMiss)
propMiss.list$vars$ten <- list(propMiss[propMiss < 0.1])
propMiss.list$vars$twenty <- list(propMiss[propMiss > 0.1 & propMiss< 0.2])
propMiss.list$vars$thirty <- list(propMiss[propMiss > 0.2 & propMiss< 0.3])
propMiss.list$vars$forty <- list(propMiss[propMiss > 0.3 & propMiss<0.4])
propMiss.list$vars$more <- list(propMiss[propMiss > 0.4])


# count subject & define cutoff 
subject.sum <- nrow(dat)
cutoff <- ceiling((subject.sum/100)*40)

# delete vars with more than 40% missing values
delete.var <- nan.column > cutoff

dat[,delete.var] <- NULL

var.count[3] <- ncol(dat)

# save variable names 
deleted.tmp <- c()
for (i in 1:length(delete.var)){
  if (any(grepl("TRUE", delete.var[i]) == T)){
    deleted.tmp  <- c(deleted.tmp, names(delete.var)[i])
  }
} 

# save deleted names in list
deleted.vars$moreThanThirty <- list(deleted.tmp)

# count nans in each column
nan.column <-sapply(dat, function(y) sum(length(which(is.na(y)))))
nan.column.frame <-data.frame(nan.column)

# count nans in each row for complete dataset
nan.row.all <- rowSums(is.na(dat))
nan.row.all.frame <- data.frame(nan.row.all)

subj.count <- c()
subj.count[1] <- nrow(dat)

# check subj missings without pred vars 
input.var <- dat[,1:which(colnames(dat)=="m.age")]

# count proportion missing for each variable
propMiss.subj <- (rowSums(  is.na(input.var) )/ncol(input.var))
propMiss.list$subj$all <- list(propMiss.subj)
propMiss.list$subj$ten <- list(propMiss.subj[propMiss.subj < 0.1])
propMiss.list$subj$twenty <- list(propMiss.subj[propMiss.subj > 0.1 & propMiss.subj< 0.2])
propMiss.list$subj$thirty <- list(propMiss.subj[propMiss.subj > 0.2 & propMiss.subj< 0.3])
propMiss.list$subj$forty <- list(propMiss.subj[propMiss.subj > 0.3 & propMiss.subj<0.4])
propMiss.list$subj$more <- list(propMiss.subj[propMiss.subj > 0.4])

# count nans in each row after vars are deleted
nan.row <-rowSums(is.na(input.var))
nan.row.frame <- data.frame(nan.row)

# delete subjects with more than 40% missing data points
cutoff.subj <-  (ncol(input.var)/100)*40
keep.SUBJ <- ifelse(nan.row > cutoff.subj, FALSE, TRUE)
input.var <- input.var[keep.SUBJ,]

subj.count[2] <-nrow(input.var)


################################
#   Make dependent vars        #
################################

output.var <- dat[,which(colnames(dat) == "c.c_a1"):which(colnames(dat) == "c.c_e")]

# delete the subjects with too many missing values as above
output.var <- output.var[keep.SUBJ,]

nan.column <-sapply(output.var, function(y) sum(length(which(is.na(y)))))
nan.column.frame <-data.frame(nan.column)


# make logical for subjects that should be used for each CAFPA due to missing value 
subj.use <- ifelse(is.na(output.var), FALSE, TRUE)


###############################################
# make factors out of categorical variables   #
###############################################

# unordered
input.var$m.tinnitus_ri <- as.factor(input.var$m.tinnitus_ri)
input.var$m.tinnitus_le <- as.factor(input.var$m.tinnitus_le)
input.var$m.language    <- as.factor(input.var$m.language)
input.var$m.gender      <- as.factor(input.var$m.gender)

# factors with levels (but not yet ordered)
input.var$m.hp_noise          <- as.ordered(input.var$m.hp_noise) 
input.var$m.hp_quiet          <- as.ordered(input.var$m.hp_quiet) 

################################
#   Save variables             #
################################

save(input.var, output.var, subj.use, file = paste0(path_save, 'preprocessed_data_final.Rdata'))
save(propMiss.list,deleted.vars, nan.column.frame, nan.row.frame,file = paste0(path_save, 'missing_value_final.Rdata'))

