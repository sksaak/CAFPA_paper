#####################################################################
#    Plot MSE vs lambda and alpha for Lasso/Elastic Net             #
#                                                                   #
#                                                                   #
#                                                                   #      
#####################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()
graphics.off()

# libraries
library(ggplot2)
library(ggpubr)
library(viridis)
library(randomForest)

# set seed
set.seed(22)

# set up working directory 
MAIN <- "C:\\Users\\Samira\\Desktop\\Neurocognitive Psychology\\CAFPA_pub\\analysis_complete\\"
FOLDER <- "MB\\data\\le_ri_combine_pta\\"
path_save <- paste0(MAIN, "MB\\plots\\cv_lasso_enet\\" )

# select random imp set
imp <- round(runif(1, min=1, max=20))

axis_size = 34
axis_face = "bold"

# load data LASSO ----------------------------------------------------------------------------------------------------------
plots <- list()
for (i in 1:10){
load(paste0(MAIN, FOLDER, "models\\lasso\\lasso_cv_out_", as.character(imp), "_cafpa_", as.character(i), ".Rdata"))

df <-  data.frame(lambda = cv.out$lambda,
                  cvm = cv.out$cvm,
                  cvsd = cv.out$cvsd,
                  cvup = cv.out$cvup,
                  cvlo = cv.out$cvlo,
                  nzero = as.vector(unlist(cv.out$nzero)),
                  lambda.min = rep(cv.out$lambda.min, times =length(cv.out$lambda)))

png(paste0(path_save, "Lambda_lasso_", as.character(i), ".png"), width = 550, height = 550 )
print(ggplot(data = df,aes(x = log(lambda), y = cvm)) + 
  geom_errorbar(aes(ymin = cvlo, ymax= cvup), width = 1.5, color = "black")+
  geom_point(color = "darkblue", size = 4) +
  geom_vline(aes(xintercept = log(lambda.min[1])), lty = "dotted", size = 1) +
  theme_bw() + 
  scale_x_continuous(limits = c(min(log(df$lambda)), max(log(df$lambda))))+
 # ylim(0,0.09)+
  xlab("")+
  ylab("")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(face = axis_face, size = axis_size), 
        axis.text.y = element_text(face = axis_face, size = axis_size)) + 
    ggtitle(as.character(i))
)

dev.off()
}


# load data ELASTIC NET ---------------------------------------------------------------------------------------------------------------

plots <- list()
for (i in 1:10){
 load(paste0(MAIN, FOLDER, "models\\elasticNet\\elastic_imp_pta_", as.character(imp), "_cafpa_", as.character(i), ".Rdata"))
 # elastic_net_model <- models[[i]]
  df <-  data.frame(lambda = elastic_net_model[["results"]][["lambda"]],
                    Alpha = as.factor(elastic_net_model[["results"]][["alpha"]]),
                    MSE   = elastic_net_model[["results"]][["RMSE"]]^2,
                    best_lambda = rep(elastic_net_model[["bestTune"]][["lambda"]], times = length(elastic_net_model[["results"]][["lambda"]])),
                    best_alpha = rep(elastic_net_model[["bestTune"]][["alpha"]], times = length(elastic_net_model[["results"]][["lambda"]]))
  )
  
  min_mse <- df[df$lambda == df$best_lambda,]
  min_mse <- min_mse[min_mse$Alpha == df$best_alpha[1],]

png(paste0(path_save, "Lambda_alpha_enet_", as.character(i), ".png"), width = 580, height = 550 )
  print(ggplot(data = df,aes(x = log(lambda), y = MSE, color = Alpha)) + 
          geom_point(size = 4, alpha = 0.5) +
          geom_line() +
          geom_vline(aes(xintercept = log(min_mse$lambda)), lty = "dotted", size = 1)+
          geom_hline(aes(yintercept = min_mse$MSE), lty = "dotted", size = 1)+
          scale_color_viridis(discrete = TRUE) +
          theme_bw() + 
          ylim(0,0.09)+
          xlim(-10, 0)+
          xlab("")+
          ylab("") +
          ggtitle(as.character(i))+
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"), 
                axis.text.x = element_text(face = axis_face, size = axis_size+2), 
                axis.text.y = element_text(face = axis_face, size = axis_size+2)) + 
              #  if(i == 11 || i == 12){
              #  theme(legend.text = element_text(face = axis_face, size = axis_size-5),
              #  legend.title = element_text(face = axis_face, size = axis_size-6))
               # } else {
                theme(legend.position = "none")
               # }
              
         
  )
  
  dev.off()
}


# Random Forest ---------------------------------------------------------------------------------------
  load(paste0(MAIN, FOLDER, "data\\data_le_ri_combined_pta_", imp ,".Rdata"))

  # loop across different CAFPAS
  for (i in 1:length(output.var)){
    # make input & output dependent on subjects to use
    input <- data[subj.use[,i],]
    ind <- unlist(indices[i])
    
    dat <- data.frame(input, output)
    # split data
    train.data <- dat[ind,]
    test.data <- dat[-ind,] 
    
    # set up cross validation
    cross.val <- trainControl(method="cv", number = 10)
    
    # set up hyperparameter with ML algorithm (searches for best mtry given data)
    set.seed(22)
    bestmtry <- tuneRF(input[ind,], output[ind], stepFactor=1.5, improve=1e-5, ntree=500)
    set.seed(22)
    bestmtry <- tuneRF(train.data[,-45], train.data$output, stepFactor=1.5, improve=1e-5, ntree=500)
    
    
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
    
    
    
  }

