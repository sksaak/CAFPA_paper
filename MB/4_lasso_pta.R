################################################################################################
#        LASSO regression for CAFPA prediction                  
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
library(ggplot2)
library(caret)

# set up folders and directories
FOLDER <- "MB\\data\\le_ri_combine_pta\\"
dir.create(file.path(FOLDER), recursive = TRUE) 
dir.create(file.path(paste0(FOLDER, "data\\")), recursive = TRUE) 
dir.create(file.path(paste0(FOLDER, "data\\lasso\\")), recursive = TRUE) 
dir.create(file.path(paste0(FOLDER, "models\\lasso\\")), recursive = TRUE) 
dir.create(file.path("MB\\plots\\le_ri_combine_pta\\lasso\\"), recursive = TRUE) 

path_data <- paste0(FOLDER, "data\\")
path_save <- paste0(FOLDER, "data\\lasso\\")
path_model <- paste0(FOLDER, "models\\lasso\\")
path_plot <- "MB\\plots\\le_ri_combine_pta\\lasso\\"
 
      
# set seed
set.seed(22)

# iterations of mice
imp.num = 20

# predefine vars
lasso.results <- list()
cafpa = c("c.c_a1", "c.c_a2", "c.c_a3", "c.c_a4", "c.c_u1", "c.c_u2", "c.c_b", "c.c_n", "c.c_c", "c.c_e")

# load output var
load("MB\\data\\preprocess\\preprocessed_data_final.Rdata")

# make indices dependent on each cafpa 
indices <- list()

for (i in 1:length(output.var)){
  output <- as.vector(output.var[subj.use[,i],i])
  indices[i]<- list(caret::createDataPartition(output, p=0.8, list=FALSE))
}


# load preprocessed data in loop 
for (m in 1:imp.num){

load(paste0(path_data,"data_le_ri_combined_pta_", m ,".Rdata"))


# make a pdf for each imp iteration containing all the plots for the cafpas  
pdf(paste0(path_plot, m, "_lasso_plots.pdf") , onefile=T )

# LASSO -----------------------------------------------------------------------               

# train
train_MSE_all <- NULL
train_MAE_all <- NULL
train_rsq_all <- NULL

# test
MSE_all <- NULL
MAE_all <- NULL
rsq_test <- NULL

predAct <- list()

# loop across CAFPAs
for(i in 1:length(output.var)) {

# make input & output dependent on subjects to use
output <- output.var[subj.use[,i],i]
input.var <- NULL
input.var <- data

# make input and concatenate output for model formulae
input <- input.var[subj.use[,i],]
input$output <- output

# Build x-Matrix and y-vector with one of the CAFPAs as dependent variable
x <- model.matrix(output~., input)
y <- output

# build train and test set
train = indices[[i]]
test = (-train)
y.test=y[test]

# define lambda-grid: 10^10 bis 10^-2 (contains all possibilities from OLS to null model)
grid <- 10^seq(10,-2, length =100)

# building the lasso model with the lambda grid
lasso.mod <- glmnet(x[train,],y[train],alpha =1, lambda=grid)
plot(lasso.mod, label = TRUE)
title(main = cafpa[i], line = 3)

# performing cross validation to extract the best lambda
cv.out =cv.glmnet(x[train ,],y[train],alpha =1, lambda = grid)
plot(cv.out)
title(main = cafpa[i], line = 3)
# extract the best lambda value from the cross-validation
bestlam =cv.out$lambda.min

# rebuilding the model with best lambda value identified
lasso.best <- glmnet(x[train,],y[train],alpha =1, lambda=bestlam)

# train set -------------------------------------------------------------------
train.lasso.pred=predict (lasso.best ,s=bestlam ,newx=x[train ,])
train_actual <- y[train]
train_preds <- train.lasso.pred

train_rsq <- cor(train_actual, train_preds)^2
train_rsq_all[i] <- train_rsq

train_MAE <- mean(abs(train.lasso.pred -y[train]))
train_MAE_all[i] <- train_MAE

train_MSE <- mean(( train.lasso.pred -y[train])^2)
train_MSE_all[i] <- train_MSE


# test set -------------------------------------------------------------------
lasso.pred=predict (lasso.best ,s=bestlam ,newx=x[test ,])

# look at actual deviation of values and predicted values
final <- cbind(y[test], lasso.pred)
head(final)

# calculate Rsquared test
actual <- y[test]
preds <- lasso.pred

rsq.test <- cor(actual, preds)^2

df <- data.frame(actual, preds)
names(df) <- c("actual", "preds")

ggplot(df, aes(df$preds,df$actual)) + 
  geom_point(color = "darkblue") + 
  geom_abline() + 
  theme_light() + 
  xlim(0,1.05) + 
  ylim(0,1.05) + 
  xlab("Predicted") + 
  ylab("Actual") + 
  title(main = paste(cafpa[i], "Imp", m))
  
  
# Test set MSE and MAE
MSE <- mean(( lasso.pred -y.test)^2)
MSE_all[i] <- MSE
MAE <- mean(abs(lasso.pred -y.test))
MAE_all[i] <- MAE

# retrieve coefficients
lasso.coef=predict(lasso.best, type="coefficients", s=bestlam)[1:length(input),]
plot(lasso.coef)
title(main = cafpa[i], line = 3)


coefficients <- lasso.coef[lasso.coef != 0]

if(i == 1){c.c_a1.coef <- list(coefficients)}
if(i == 2){c.c_a2.coef <- list(coefficients)}
if(i == 3){c.c_a3.coef <- list(coefficients)}
if(i == 4){c.c_a4.coef <- list(coefficients)}
if(i == 5){c.c_u1.coef <- list(coefficients)}
if(i == 6){c.c_u2.coef <- list(coefficients)}
if(i == 7){c.c_b.coef  <- list(coefficients)}
if(i == 8){c.c_n.coef  <- list(coefficients)}
if(i == 9){c.c_c.coef  <- list(coefficients)}
if(i == 10){c.c_e.coef <- list(coefficients)}


# save model
save(lasso.best, file = (paste0(path_model, "lasso_imp_pta_", m , "_cafpa_", i, ".Rdata")))
save(cv.out, file = (paste0(path_model, "lasso_cv_out_", m , "_cafpa_", i, ".Rdata")))

} # cafpa loop

# store data

# cafpas
lasso.results$cafpas$c.c_a1[m] <- c.c_a1.coef
lasso.results$cafpas$c.c_a2[m] <- c.c_a2.coef
lasso.results$cafpas$c.c_a3[m] <- c.c_a3.coef
lasso.results$cafpas$c.c_a4[m] <- c.c_a4.coef
lasso.results$cafpas$c.c_u1[m] <- c.c_u1.coef
lasso.results$cafpas$c.c_u2[m] <- c.c_u2.coef
lasso.results$cafpas$c.c_b[m] <- c.c_b.coef
lasso.results$cafpas$c.c_n[m] <- c.c_n.coef
lasso.results$cafpas$c.c_c[m] <- c.c_c.coef
lasso.results$cafpas$c.c_e[m] <- c.c_e.coef
#
lasso.results$test_MAE[m] <- list(MAE_all)
lasso.results$test_MSE[m] <- list(MSE_all)
lasso.results$test_rsq[m] <- list(rsq_test)
lasso.results$train_MAE[m] <- list(train_MAE_all)
lasso.results$train_MSE[m] <- list(train_MSE_all)
lasso.results$train_rsq[m] <- list(train_rsq_all)

lasso.results$predAct[m] <- list(predAct)

dev.off()

} # imp loop 


# save output 


save(lasso.results, file = (paste0(path_save, "lasso.results_pta.Rdata")))






