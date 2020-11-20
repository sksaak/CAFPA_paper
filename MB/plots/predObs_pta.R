#####################################################################
#    Plot predicted vs. observed for all models                     #
#                                                                   #
# -> averaged across imputations with pta_asym                      #
#                                                                   #      
#####################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()
graphics.off()

# libraries
library(dplyr)
library(ggplot2)
library(matrixStats)
library(stats)
library(ggpubr)

# set seed
set.seed(22)

# set paths
MAIN <- "C:\\Users\\Samira\\Desktop\\Neurocognitive Psychology\\CAFPA_pub\\analysis_complete\\"
path_data <- paste0(MAIN, "MB\\data\\")
path_plot <- paste0(MAIN, "MB\\plots\\PredObs_MB\\")
setwd(path)

# load results data
load(file = paste0(path_data, "lasso\\lasso.results_pta.Rdata"))
load(file = paste0(path_data, "randomForest\\rf.results_pta.Rdata"))
load(file = paste0(path_data, "elasticNet\\elastic_net_pta.results.Rdata"))


# predefine vars
eNet <- list()
lasso <- list()
rf <- list()

for (i in 1:20){
  # LASSO
  lasso$ca1[i] <- list(lasso.results$predAct[[i]][[1]][["preds"]])
  lasso$ca2[i] <- list(lasso.results$predAct[[i]][[2]][["preds"]])
  lasso$ca3[i] <- list(lasso.results$predAct[[i]][[3]][["preds"]])
  lasso$ca4[i] <- list(lasso.results$predAct[[i]][[4]][["preds"]])
  lasso$cu1[i] <- list(lasso.results$predAct[[i]][[5]][["preds"]])
  lasso$cu2[i] <- list(lasso.results$predAct[[i]][[6]][["preds"]])
  lasso$cb[i] <- list(lasso.results$predAct[[i]][[7]][["preds"]])
  lasso$cn[i] <- list(lasso.results$predAct[[i]][[8]][["preds"]])
  lasso$cc[i] <- list(lasso.results$predAct[[i]][[9]][["preds"]])
  lasso$ce[i] <- list(lasso.results$predAct[[i]][[10]][["preds"]])
  
  # ELASTIC NET 
  eNet$ca1[i] <- list(elastic_net.results$predAct[[i]][[1]][["preds"]])
  eNet$ca2[i] <- list(elastic_net.results$predAct[[i]][[2]][["preds"]])
  eNet$ca3[i] <- list(elastic_net.results$predAct[[i]][[3]][["preds"]])
  eNet$ca4[i] <- list(elastic_net.results$predAct[[i]][[4]][["preds"]])
  eNet$cu1[i] <- list(elastic_net.results$predAct[[i]][[5]][["preds"]])
  eNet$cu2[i] <- list(elastic_net.results$predAct[[i]][[6]][["preds"]])
  eNet$cb[i] <- list(elastic_net.results$predAct[[i]][[7]][["preds"]])
  eNet$cn[i] <- list(elastic_net.results$predAct[[i]][[8]][["preds"]])
  eNet$cc[i] <- list(elastic_net.results$predAct[[i]][[9]][["preds"]])
  eNet$ce[i] <- list(elastic_net.results$predAct[[i]][[10]][["preds"]])
  
  # RANDOM FOREST
  rf$ca1[i] <- list(rf.results$predAct[[i]][[1]][["preds"]])
  rf$ca2[i] <- list(rf.results$predAct[[i]][[2]][["preds"]])
  rf$ca3[i] <- list(rf.results$predAct[[i]][[3]][["preds"]])
  rf$ca4[i] <- list(rf.results$predAct[[i]][[4]][["preds"]])
  rf$cu1[i] <- list(rf.results$predAct[[i]][[5]][["preds"]])
  rf$cu2[i] <- list(rf.results$predAct[[i]][[6]][["preds"]])
  rf$cb[i] <- list(rf.results$predAct[[i]][[7]][["preds"]])
  rf$cn[i] <- list(rf.results$predAct[[i]][[8]][["preds"]])
  rf$cc[i] <- list(rf.results$predAct[[i]][[9]][["preds"]])
  rf$ce[i] <- list(rf.results$predAct[[i]][[10]][["preds"]])
  
}

# extract mean and standard error across imputation 
# Ca1
ca1 <- data.frame()
ca1 <- data.frame( rowMeans(as.data.frame(lasso$ca1)))
ca1[2] <- data.frame(rowSds(matrix(unlist(lasso$ca1), ncol = 20, byrow = FALSE )))
ca1[3] <- data.frame(rowMeans(as.data.frame(eNet$ca1)))
ca1[4] <- data.frame(rowSds(matrix(unlist(eNet$ca1), ncol = 20, byrow = FALSE )))
ca1[5] <- data.frame(rowMeans(as.data.frame(rf$ca1)))
ca1[6] <- data.frame(rowSds(matrix(unlist(rf$ca1), ncol = 20, byrow = FALSE )))
ca1[7] <- data.frame(lasso.results[["predAct"]][[1]][[1]][["actual"]])
names(ca1) <- c("lasso_mean", "lasso_sd", "elastic_mean", "elastic_sd", "rf_mean", "rf_sd", "observed")

# Ca1
ca2 <- data.frame()
ca2 <- data.frame( rowMeans(as.data.frame(lasso$ca2)))
ca2[2] <- data.frame(rowSds(matrix(unlist(lasso$ca2), ncol = 20, byrow = FALSE )))
ca2[3] <- data.frame(rowMeans(as.data.frame(eNet$ca2)))
ca2[4] <- data.frame(rowSds(matrix(unlist(eNet$ca2), ncol = 20, byrow = FALSE )))
ca2[5] <- data.frame(rowMeans(as.data.frame(rf$ca2)))
ca2[6] <- data.frame(rowSds(matrix(unlist(rf$ca2), ncol = 20, byrow = FALSE )))
ca2[7] <- data.frame(lasso.results[["predAct"]][[1]][[2]][["actual"]])
names(ca2) <- c("lasso_mean", "lasso_sd", "elastic_mean", "elastic_sd", "rf_mean", "rf_sd", "observed")

# Ca1
ca3 <- data.frame()
ca3 <- data.frame( rowMeans(as.data.frame(lasso$ca3)))
ca3[2] <- data.frame(rowSds(matrix(unlist(lasso$ca3), ncol = 20, byrow = FALSE )))
ca3[3] <- data.frame(rowMeans(as.data.frame(eNet$ca3)))
ca3[4] <- data.frame(rowSds(matrix(unlist(eNet$ca3), ncol = 20, byrow = FALSE )))
ca3[5] <- data.frame(rowMeans(as.data.frame(rf$ca3)))
ca3[6] <- data.frame(rowSds(matrix(unlist(rf$ca3), ncol = 20, byrow = FALSE )))
ca3[7] <- data.frame(lasso.results[["predAct"]][[1]][[3]][["actual"]])
names(ca3) <- c("lasso_mean", "lasso_sd", "elastic_mean", "elastic_sd", "rf_mean", "rf_sd", "observed")

# Ca1
ca4 <- data.frame()
ca4 <- data.frame( rowMeans(as.data.frame(lasso$ca4)))
ca4[2] <- data.frame(rowSds(matrix(unlist(lasso$ca4), ncol = 20, byrow = FALSE )))
ca4[3] <- data.frame(rowMeans(as.data.frame(eNet$ca4)))
ca4[4] <- data.frame(rowSds(matrix(unlist(eNet$ca4), ncol = 20, byrow = FALSE )))
ca4[5] <- data.frame(rowMeans(as.data.frame(rf$ca4)))
ca4[6] <- data.frame(rowSds(matrix(unlist(rf$ca4), ncol = 20, byrow = FALSE )))
ca4[7] <- data.frame(lasso.results[["predAct"]][[1]][[4]][["actual"]])
names(ca4) <- c("lasso_mean", "lasso_sd", "elastic_mean", "elastic_sd", "rf_mean", "rf_sd", "observed")

# Ca1
cu1 <- data.frame()
cu1 <- data.frame( rowMeans(as.data.frame(lasso$cu1)))
cu1[2] <- data.frame(rowSds(matrix(unlist(lasso$cu1), ncol = 20, byrow = FALSE )))
cu1[3] <- data.frame(rowMeans(as.data.frame(eNet$cu1)))
cu1[4] <- data.frame(rowSds(matrix(unlist(eNet$cu1), ncol = 20, byrow = FALSE )))
cu1[5] <- data.frame(rowMeans(as.data.frame(rf$cu1)))
cu1[6] <- data.frame(rowSds(matrix(unlist(rf$cu1), ncol = 20, byrow = FALSE )))
cu1[7] <- data.frame(lasso.results[["predAct"]][[1]][[5]][["actual"]])
names(cu1) <- c("lasso_mean", "lasso_sd", "elastic_mean", "elastic_sd", "rf_mean", "rf_sd", "observed")

# Ca1
cu2 <- data.frame()
cu2 <- data.frame( rowMeans(as.data.frame(lasso$cu2)))
cu2[2] <- data.frame(rowSds(matrix(unlist(lasso$cu2), ncol = 20, byrow = FALSE )))
cu2[3] <- data.frame(rowMeans(as.data.frame(eNet$cu2)))
cu2[4] <- data.frame(rowSds(matrix(unlist(eNet$cu2), ncol = 20, byrow = FALSE )))
cu2[5] <- data.frame(rowMeans(as.data.frame(rf$cu2)))
cu2[6] <- data.frame(rowSds(matrix(unlist(rf$cu2), ncol = 20, byrow = FALSE )))
cu2[7] <- data.frame(lasso.results[["predAct"]][[1]][[6]][["actual"]])
names(cu2) <- c("lasso_mean", "lasso_sd", "elastic_mean", "elastic_sd", "rf_mean", "rf_sd", "observed")

# Ca1
cb <- data.frame()
cb <- data.frame( rowMeans(as.data.frame(lasso$cb)))
cb[2] <- data.frame(rowSds(matrix(unlist(lasso$cb), ncol = 20, byrow = FALSE )))
cb[3] <- data.frame(rowMeans(as.data.frame(eNet$cb)))
cb[4] <- data.frame(rowSds(matrix(unlist(eNet$cb), ncol = 20, byrow = FALSE )))
cb[5] <- data.frame(rowMeans(as.data.frame(rf$cb)))
cb[6] <- data.frame(rowSds(matrix(unlist(rf$cb), ncol = 20, byrow = FALSE )))
cb[7] <- data.frame(lasso.results[["predAct"]][[1]][[7]][["actual"]])
names(cb) <- c("lasso_mean", "lasso_sd", "elastic_mean", "elastic_sd", "rf_mean", "rf_sd", "observed")

# Ca1
cn <- data.frame()
cn <- data.frame( rowMeans(as.data.frame(lasso$cn)))
cn[2] <- data.frame(rowSds(matrix(unlist(lasso$cn), ncol = 20, byrow = FALSE )))
cn[3] <- data.frame(rowMeans(as.data.frame(eNet$cn)))
cn[4] <- data.frame(rowSds(matrix(unlist(eNet$cn), ncol = 20, byrow = FALSE )))
cn[5] <- data.frame(rowMeans(as.data.frame(rf$cn)))
cn[6] <- data.frame(rowSds(matrix(unlist(rf$cn), ncol = 20, byrow = FALSE )))
cn[7] <- data.frame(lasso.results[["predAct"]][[1]][[8]][["actual"]])
names(cn) <- c("lasso_mean", "lasso_sd", "elastic_mean", "elastic_sd", "rf_mean", "rf_sd", "observed")

# Ca1
cc <- data.frame()
cc <- data.frame( rowMeans(as.data.frame(lasso$cc)))
cc[2] <- data.frame(rowSds(matrix(unlist(lasso$cc), ncol = 20, byrow = FALSE )))
cc[3] <- data.frame(rowMeans(as.data.frame(eNet$cc)))
cc[4] <- data.frame(rowSds(matrix(unlist(eNet$cc), ncol = 20, byrow = FALSE )))
cc[5] <- data.frame(rowMeans(as.data.frame(rf$cc)))
cc[6] <- data.frame(rowSds(matrix(unlist(rf$cc), ncol = 20, byrow = FALSE )))
cc[7] <- data.frame(lasso.results[["predAct"]][[1]][[9]][["actual"]])
names(cc) <- c("lasso_mean", "lasso_sd", "elastic_mean", "elastic_sd", "rf_mean", "rf_sd", "observed")

# Ca1
ce <- data.frame()
ce <- data.frame( rowMeans(as.data.frame(lasso$ce)))
ce[2] <- data.frame(rowSds(matrix(unlist(lasso$ce), ncol = 20, byrow = FALSE )))
ce[3] <- data.frame(rowMeans(as.data.frame(eNet$ce)))
ce[4] <- data.frame(rowSds(matrix(unlist(eNet$ce), ncol = 20, byrow = FALSE )))
ce[5] <- data.frame(rowMeans(as.data.frame(rf$ce)))
ce[6] <- data.frame(rowSds(matrix(unlist(rf$ce), ncol = 20, byrow = FALSE )))
ce[7] <- data.frame(lasso.results[["predAct"]][[1]][[10]][["actual"]])
names(ce) <- c("lasso_mean", "lasso_sd", "elastic_mean", "elastic_sd", "rf_mean", "rf_sd", "observed")


# Correlation between labeled and predicted with p.value ----------------------------------------------------
all_models <- list(ca1, ca2, ca3, ca4, cu1, cu2, cb, cn, cc, ce)

lasso_cor <- as.data.frame(matrix(data = NA, nrow = 10, ncol = 3))
elastic_cor <- as.data.frame(matrix(data = NA, nrow = 10, ncol = 3))
rf_cor <- as.data.frame(matrix(data = NA, nrow = 10, ncol = 3))

for (i in 1:10){
  tmp_lasso <- cor.test(all_models[[i]][["lasso_mean"]], all_models[[i]][["observed"]])
  tmp_elastic <- cor.test(all_models[[i]][["elastic_mean"]], all_models[[i]][["observed"]])
  tmp_rf <- cor.test(all_models[[i]][["rf_mean"]], all_models[[i]][["observed"]])
  
  lasso_cor[i,]   <- c(round(tmp_lasso$estimate,2),  tmp_lasso$p.value, NA)
  if(lasso_cor[i,2] < 0.05 && lasso_cor[i,2] > 0.01){lasso_cor[i,3] = "*" 
  }else if (lasso_cor[i,2] < 0.01 && lasso_cor[i,2] > 0.001){lasso_cor[i,3] = "**"
  }else if (lasso_cor[i,2] < 0.001){lasso_cor[i,3] = "***"}
  
  elastic_cor[i,] <- c(round(tmp_elastic$estimate,2),  tmp_elastic$p.value, NA)
  if(elastic_cor[i,2] < 0.05 && elastic_cor[i,2] > 0.01){elastic_cor[i,3] = "*" 
  }else if (elastic_cor[i,2] < 0.01 && elastic_cor[i,2] > 0.001){elastic_cor[i,3] = "**"
  }else if (elastic_cor[i,2] < 0.001){elastic_cor[i,3] = "***"}
  
  rf_cor[i,]      <- c(round(tmp_rf$estimate,2),  tmp_rf$p.value, NA)
  if(rf_cor[i,2] < 0.05 && lasso_cor[i,2] > 0.01){rf_cor[i,3] = "*" 
  }else if (rf_cor[i,2] < 0.01 && rf_cor[i,2] > 0.001){rf_cor[i,3] = "**"
  }else if (rf_cor[i,2] < 0.001){rf_cor[i,3] = "***"}
  
}
names(lasso_cor) <- c("R", "pvalue", "sig")
names(elastic_cor) <- c("R", "pvalue", "sig")
names(rf_cor) <- c("R", "pvalue", "sig")

#--------------------------------------------------------------------

cafpa_names <- c("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB","CN", "CC","CE")

corr_size = 14
corr_x = 0.2
corr_x_rf = 0.07 
corr_x_lasso = 0.02
corr_y = 1
axis_size = 42
axis_face = "bold"
point_size = 8
point_alpha = 0.6

col_lasso = "#56B4E9" #"lightblue"
col_elastic = "darkblue"#"#006666"
col_rf = "darkgreen"

plot_width = 700
plot_height = 700

setwd(path_plot)

#- store plots in p1-10 -----------------------------------------------------------

png("CA1_preds_obs.png", width = plot_width, height = plot_height)
ggplot(data= ca1, aes(y = observed)) + 
          
  geom_point(aes(x = rf_mean),      color = col_rf, alpha= point_alpha, size = point_size ) + 
  geom_point(aes(x = elastic_mean), color = col_elastic, alpha= point_alpha, size = point_size ) +         
  geom_point(aes(x = lasso_mean),   color = col_lasso, alpha= point_alpha, size = point_size ) + 
          
          geom_smooth(aes(x = lasso_mean), method = "lm", se = FALSE, color = col_lasso, alpha = point_alpha)+
          geom_smooth(aes(x = elastic_mean), method = "lm", se = FALSE, color = col_elastic, alpha = point_alpha)+
          geom_smooth(aes(x = rf_mean), method = "lm", se = FALSE, color = col_rf, alpha = point_alpha)+
          
          annotate("text", label = paste("rLR = ", as.character(lasso_cor$R[1]), lasso_cor$sig[1]),
                   colour = col_lasso, x = corr_x, y = corr_y, size = corr_size, fontface = "italic", face = axis_face) +
          annotate("text", label = paste("rEN = ", as.character(elastic_cor$R[1]), elastic_cor$sig[1]),
                   colour = col_elastic, x = corr_x, y = corr_y-0.1, size = corr_size,fontface = "italic") +
          annotate("text", label = paste("rRF = ", as.character(rf_cor$R[1]), rf_cor$sig[1]),
                   colour = col_rf, x = corr_x, y = corr_y-0.2, size = corr_size,fontface = "italic") +
          annotate("text", label = lasso_cor$sig[1], colour = "black", x = corr_x+0.188, y = corr_y-0.2, 
                   size = corr_size, fontface = "italic", face = axis_face)+
          
          geom_abline(intercept = 0, slope = 1, lty = 2) +
          scale_y_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          scale_x_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          
          theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             axis.text.x = element_text(face = axis_face, size = axis_size), 
                             axis.text.y = element_text(face = axis_face, size = axis_size)) +
          ggtitle("1")+
          xlab("") + 
          ylab("") 
dev.off()

png("CA2_preds_obs.png", width = plot_width, height = plot_height)
ggplot(data= ca2, aes(y = observed)) + 
          
  geom_point(aes(x = rf_mean),      color = col_rf, alpha= point_alpha, size = point_size ) + 
  geom_point(aes(x = elastic_mean), color = col_elastic, alpha= point_alpha, size = point_size ) +         
  geom_point(aes(x = lasso_mean),   color = col_lasso, alpha= point_alpha, size = point_size ) +   
  
          geom_smooth(aes(x = lasso_mean), method = "lm", se = FALSE, color = col_lasso, alpha = point_alpha)+
          geom_smooth(aes(x = elastic_mean), method = "lm", se = FALSE, color = col_elastic, alpha = point_alpha)+
          geom_smooth(aes(x = rf_mean), method = "lm", se = FALSE, color = col_rf, alpha = point_alpha)+
          
          annotate("text", label = paste("rLR = ",  as.character(lasso_cor$R[2]), lasso_cor$sig[2]),
                   colour = col_lasso, x = corr_x, y = corr_y, size = corr_size, fontface = "italic") +
          annotate("text", label = paste("rEN = ", as.character(elastic_cor$R[2]), elastic_cor$sig[2]),
                   colour = col_elastic, x = corr_x, y = corr_y-0.1, size = corr_size,fontface = "italic") +
          annotate("text", label = paste("rRF = ", as.character(rf_cor$R[2]), rf_cor$sig[2]),
                   colour = col_rf, x = corr_x, y = corr_y-0.2, size = corr_size,fontface = "italic") +
          
          geom_abline(intercept = 0, slope = 1, lty = 2) +
          scale_y_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          scale_x_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+        
  
          theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             axis.text.x = element_text(face = axis_face, size = axis_size), 
                             axis.text.y = element_text(face = axis_face, size = axis_size))+
                             #axis.title = element_text(face = axis_face, size = axis_size)) +
          ggtitle("2")+
          xlab("") + 
          ylab("") 
dev.off()

png("CA3_preds_obs.png", width = plot_width, height = plot_height)
ggplot(data= ca3, aes(y = observed)) + 
           
   geom_point(aes(x = rf_mean),      color = col_rf, alpha= point_alpha, size = point_size ) + 
   geom_point(aes(x = elastic_mean), color = col_elastic, alpha= point_alpha, size = point_size ) +         
   geom_point(aes(x = lasso_mean),   color = col_lasso, alpha= point_alpha, size = point_size ) + 
   
          geom_smooth(aes(x = lasso_mean), method = "lm", se = FALSE, color = col_lasso, alpha = point_alpha)+
          geom_smooth(aes(x = elastic_mean), method = "lm", se = FALSE, color = col_elastic, alpha = point_alpha)+
          geom_smooth(aes(x = rf_mean), method = "lm", se = FALSE, color = col_rf, alpha = point_alpha)+
          
          annotate("text", label = paste("rLR = ", as.character(lasso_cor$R[3]), lasso_cor$sig[3]),
                   colour = col_lasso, x = corr_x, y = corr_y, size = corr_size, fontface = "italic") +
          annotate("text", label = paste("rEN = ", as.character(elastic_cor$R[3]), elastic_cor$sig[3]),
                   colour = col_elastic, x = corr_x, y = corr_y-0.1, size = corr_size,fontface = "italic") +
          annotate("text", label = paste("rRF = ", as.character(rf_cor$R[3]), rf_cor$sig[3]),
                   colour = col_rf, x = corr_x, y = corr_y-0.2, size = corr_size,fontface = "italic") +
          
          geom_abline(intercept = 0, slope = 1, lty = 2) +
          scale_y_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          scale_x_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          
          theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             axis.text.x = element_text(face = axis_face, size = axis_size), 
                             axis.text.y = element_text(face = axis_face, size = axis_size)) +
           ggtitle("3")+
          xlab("") + 
          ylab("")  
dev.off()
 
png("CA4_preds_obs.png", width = plot_width, height = plot_height)
ggplot(data= ca4, aes(y = observed)) + 
         
   geom_point(aes(x = rf_mean),      color = col_rf, alpha= point_alpha, size = point_size ) + 
   geom_point(aes(x = elastic_mean), color = col_elastic, alpha= point_alpha, size = point_size ) +         
   geom_point(aes(x = lasso_mean),   color = col_lasso, alpha= point_alpha, size = point_size ) + 
          
          geom_smooth(aes(x = lasso_mean), method = "lm", se = FALSE, color = col_lasso, alpha = point_alpha)+
          geom_smooth(aes(x = elastic_mean), method = "lm", se = FALSE, color = col_elastic, alpha = point_alpha)+
          geom_smooth(aes(x = rf_mean), method = "lm", se = FALSE, color = col_rf, alpha = point_alpha)+
          
          annotate("text", label = paste("rLR = ",  as.character(lasso_cor$R[4]), lasso_cor$sig[4]),
                   colour = col_lasso, x = corr_x, y = corr_y, size = corr_size, fontface = "italic") +
          annotate("text", label = paste("rEN = ", as.character(elastic_cor$R[4]), elastic_cor$sig[4]),
                   colour = col_elastic, x = corr_x, y = corr_y-0.1, size = corr_size,fontface = "italic") +
          annotate("text", label = paste("rRF = ", as.character(rf_cor$R[4]), rf_cor$sig[4]),
                   colour = col_rf, x = corr_x-0.015, y = corr_y-0.2, size = corr_size,fontface = "italic") +
          
          geom_abline(intercept = 0, slope = 1, lty = 2) +
          scale_y_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          scale_x_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          
          theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             axis.text.x = element_text(face = axis_face, size = axis_size), 
                             axis.text.y = element_text(face = axis_face, size = axis_size)) +
  ggtitle("4")+
          xlab("") + 
          ylab("") 
dev.off()
 
png("CU1_preds_obs.png", width = plot_width, height = plot_height)
ggplot(data= cu1, aes(y = observed)) + 
   geom_point(aes(x = rf_mean),      color = col_rf, alpha= point_alpha, size = point_size ) + 
   geom_point(aes(x = elastic_mean), color = col_elastic, alpha= point_alpha, size = point_size ) +         
   geom_point(aes(x = lasso_mean),   color = col_lasso, alpha= point_alpha, size = point_size ) + 
          
          geom_smooth(aes(x = lasso_mean), method = "lm", se = FALSE, color = col_lasso, alpha = point_alpha)+
          geom_smooth(aes(x = elastic_mean), method = "lm", se = FALSE, color = col_elastic, alpha = point_alpha)+
          geom_smooth(aes(x = rf_mean), method = "lm", se = FALSE, color = col_rf, alpha = point_alpha)+
          
          annotate("text", label = paste("rLR = ", as.character(lasso_cor$R[5]), lasso_cor$sig[5]),
                   colour = col_lasso, x = corr_x, y = corr_y, size = corr_size, fontface = "italic") +
          annotate("text", label = paste("rEN = ", as.character(elastic_cor$R[5]), elastic_cor$sig[5]),
                   colour = col_elastic, x = corr_x, y = corr_y-0.1, size = corr_size,fontface = "italic") +
          annotate("text", label = paste("rRF = ", as.character(rf_cor$R[5]), rf_cor$sig[5]),
                   colour = col_rf, x = corr_x, y = corr_y-0.2, size = corr_size,fontface = "italic") +
          
          geom_abline(intercept = 0, slope = 1, lty = 2) +
          scale_y_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          scale_x_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          
          theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             axis.text.x = element_text(face = axis_face, size = axis_size), 
                             axis.text.y = element_text(face = axis_face, size = axis_size)) +
  ggtitle("5")+
          xlab("") + 
          ylab("")  
 dev.off()

png("CU2_preds_obs.png", width = plot_width, height = plot_height)
ggplot(data= cu2, aes(y = observed)) + 
  geom_point(aes(x = rf_mean),      color = col_rf, alpha= point_alpha, size = point_size ) + 
  geom_point(aes(x = elastic_mean), color = col_elastic, alpha= point_alpha, size = point_size ) +         
  geom_point(aes(x = lasso_mean),   color = col_lasso, alpha= point_alpha, size = point_size ) + 
          
          geom_smooth(aes(x = lasso_mean), method = "lm", se = FALSE, color = col_lasso, alpha = point_alpha)+
          geom_smooth(aes(x = elastic_mean), method = "lm", se = FALSE, color = col_elastic, alpha = point_alpha)+
          geom_smooth(aes(x = rf_mean), method = "lm", se = FALSE, color = col_rf, alpha = point_alpha)+
          
          annotate("text", label = paste("rLR = ", as.character(lasso_cor$R[6]), lasso_cor$sig[6]),
                   colour = col_lasso, x = corr_x, y = corr_y, size = corr_size, fontface = "italic") +
          annotate("text", label = paste("rEN = ", as.character(elastic_cor$R[6]), elastic_cor$sig[6]), 
                   colour = col_elastic, x = corr_x, y = corr_y-0.1, size = corr_size,fontface = "italic") +
          annotate("text", label = paste("rRF = ", as.character(rf_cor$R[6]), rf_cor$sig[6]),
                   colour = col_rf, x = corr_x, y = corr_y-0.2, size = corr_size,fontface = "italic") +
          
          geom_abline(intercept = 0, slope = 1, lty = 2) +
          scale_y_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          scale_x_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          
          theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             axis.text.x = element_text(face = axis_face, size = axis_size), 
                             axis.text.y = element_text(face = axis_face, size = axis_size))+
                            # axis.title = element_text(face = axis_face, size = axis_size)) +
  ggtitle("6")+
          xlab("") + 
          ylab("")  
dev.off()

png("CB_preds_obs.png", width = plot_width, height = plot_height)
ggplot(data= cb, aes(y = observed)) + 
  geom_point(aes(x = rf_mean),      color = col_rf, alpha= point_alpha, size = point_size ) + 
  geom_point(aes(x = elastic_mean), color = col_elastic, alpha= point_alpha, size = point_size ) +         
  geom_point(aes(x = lasso_mean),   color = col_lasso, alpha= point_alpha, size = point_size ) + 
          
          geom_smooth(aes(x = lasso_mean), method = "lm", se = FALSE, color = col_lasso, alpha = point_alpha)+
          geom_smooth(aes(x = elastic_mean), method = "lm", se = FALSE, color = col_elastic, alpha = point_alpha)+
          geom_smooth(aes(x = rf_mean), method = "lm", se = FALSE, color = col_rf, alpha = point_alpha)+
          
          annotate("text", label = paste("rLR = ",  as.character(lasso_cor$R[7]), lasso_cor$sig[7]),
                   colour = col_lasso, x = corr_x, y = corr_y, size = corr_size, fontface = "italic") +
          annotate("text", label = paste("rEN = ",as.character(elastic_cor$R[7]), elastic_cor$sig[7]),
                   colour = col_elastic, x = corr_x-0.015, y = corr_y-0.1, size = corr_size,fontface = "italic") +
          annotate("text", label = paste("rRF = ",as.character(rf_cor$R[7]), rf_cor$sig[7]),
                   colour = col_rf, x = corr_x, y = corr_y-0.2, size = corr_size,fontface = "italic") +
          
          geom_abline(intercept = 0, slope = 1, lty = 2) +
          scale_y_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          scale_x_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          
          theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             axis.text.x = element_text(face = axis_face, size = axis_size), 
                             axis.text.y = element_text(face = axis_face, size = axis_size))+
                             #axis.title = element_text(face = axis_face, size = axis_size)) +
  ggtitle("7")+
          xlab("") + 
          ylab("")  
dev.off()


png("CN_preds_obs.png", width = plot_width, height = plot_height)
ggplot(data= cn, aes(y = observed)) + 
  geom_point(aes(x = rf_mean),      color = col_rf, alpha= point_alpha, size = point_size ) + 
  geom_point(aes(x = elastic_mean), color = col_elastic, alpha= point_alpha, size = point_size ) +         
  geom_point(aes(x = lasso_mean),   color = col_lasso, alpha= point_alpha, size = point_size ) + 
          
          geom_smooth(aes(x = lasso_mean), method = "lm", se = FALSE, color = col_lasso, alpha = point_alpha)+
          geom_smooth(aes(x = elastic_mean), method = "lm", se = FALSE, color = col_elastic, alpha = point_alpha)+
          geom_smooth(aes(x = rf_mean), method = "lm", se = FALSE, color = col_rf, alpha = point_alpha)+
          
          annotate("text", label = paste("rLR = ", as.character(lasso_cor$R[8]), lasso_cor$sig[8]),
                   colour = col_lasso, x = corr_x, y = corr_y, size = corr_size, fontface = "italic", face = "bold") +
          annotate("text", label = paste("rEN = ", as.character(elastic_cor$R[8]), elastic_cor$sig[8]),
                   colour = col_elastic, x = corr_x, y = corr_y-0.1, size = corr_size,fontface = "italic") +
          annotate("text", label = paste("rRF = ", as.character(rf_cor$R[8]), rf_cor$sig[8]),
                   colour = col_rf, x = corr_x, y = corr_y-0.2, size = corr_size,fontface = "italic") +
          
          geom_abline(intercept = 0, slope = 1, lty = 2) +
          scale_y_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          scale_x_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          
          theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             axis.text.x = element_text(face = axis_face, size = axis_size), 
                             axis.text.y = element_text(face = axis_face, size = axis_size)) +
  ggtitle("8")+
          xlab("") + 
          ylab("")  
dev.off()

png("CC_preds_obs.png", width = plot_width, height = plot_height)
ggplot(data= cc, aes(y = observed)) + 
   geom_point(aes(x = rf_mean),      color = col_rf, alpha= point_alpha, size = point_size ) + 
   geom_point(aes(x = elastic_mean), color = col_elastic, alpha= point_alpha, size = point_size ) +         
   geom_point(aes(x = lasso_mean),   color = col_lasso, alpha= point_alpha, size = point_size ) + 
   
          geom_smooth(aes(x = lasso_mean), method = "lm", se = FALSE, color = col_lasso, alpha = point_alpha)+
          geom_smooth(aes(x = elastic_mean), method = "lm", se = FALSE, color = col_elastic, alpha = point_alpha)+
          geom_smooth(aes(x = rf_mean), method = "lm", se = FALSE, color = col_rf, alpha = point_alpha)+
          
          annotate("text", label = paste("rLR = ", as.character(lasso_cor$R[9]), lasso_cor$sig[9]),
                   colour = col_lasso, x = corr_x, y = corr_y, size = corr_size, fontface = "italic") +
          annotate("text", label = paste("rEN = ",as.character(elastic_cor$R[9]), elastic_cor$sig[9]),
                   colour = col_elastic, x = corr_x, y = corr_y-0.1, size = corr_size,fontface = "italic") +
          annotate("text", label = paste("rRF = ",as.character(rf_cor$R[9]), rf_cor$sig[9]), 
                   colour = col_rf, x = corr_x, y = corr_y-0.2, size = corr_size,fontface = "italic") +
          
          geom_abline(intercept = 0, slope = 1, lty = 2) +
          scale_y_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          scale_x_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          
          theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             axis.text.x = element_text(face = axis_face, size = axis_size), 
                             axis.text.y = element_text(face = axis_face, size = axis_size)) +
  ggtitle("9")+
          xlab("") + 
          ylab("")  

 dev.off()
 

png("CE_preds_obs.png", width = plot_width, height = plot_height)
ggplot(data= ce, aes(y = observed)) + 
   geom_point(aes(x = rf_mean),      color = col_rf, alpha= point_alpha, size = point_size ) + 
   geom_point(aes(x = elastic_mean), color = col_elastic, alpha= point_alpha, size = point_size ) +         
   geom_point(aes(x = lasso_mean),   color = col_lasso, alpha= point_alpha, size = point_size ) + 
          
          geom_smooth(aes(x = lasso_mean), method = "lm", se = FALSE, color = col_lasso, alpha = point_alpha)+
          geom_smooth(aes(x = elastic_mean), method = "lm", se = FALSE, color = col_elastic, alpha = point_alpha)+
          geom_smooth(aes(x = rf_mean), method = "lm", se = FALSE, color = col_rf, alpha = point_alpha)+
          
          annotate("text", label = paste("rLR = ",  as.character(lasso_cor$R[10]), lasso_cor$sig[10]),
                   colour = col_lasso, x = corr_x, y = corr_y, size = corr_size, fontface = "italic") +
          annotate("text", label = paste("rEN = ", as.character(elastic_cor$R[10]), elastic_cor$sig[10]),
                   colour = col_elastic, x = corr_x, y = corr_y-0.1, size = corr_size,fontface = "italic") +
          annotate("text", label = paste("rRF = ", as.character(rf_cor$R[10]), rf_cor$sig[10]), 
                   colour = col_rf, x = corr_x, y = corr_y-0.2, size = corr_size,fontface = "italic") +
          
          geom_abline(intercept = 0, slope = 1, lty = 2) +
          scale_y_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          scale_x_continuous(breaks=c(0,0.5, 1), limits = c(0,1))+
          
          theme_bw() + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             axis.text.x = element_text(face = axis_face, size = axis_size), 
                             axis.text.y = element_text(face = axis_face, size = axis_size)) +
  ggtitle("10")+
          xlab("") + 
          ylab("")  
       
dev.off()


#---------- end store plots -------------------------

ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, nrow = 2, ncol = 5)





# CA1 LASSO -----------------------------------------

setwd(path_plot)

cafpas <- list(ca1,ca2,ca3,ca4,cu1,cu2,cb,cn,cc,ce)
c_name <- c("ca1","ca2","ca3","ca4","cu1","cu2","cb","cn","cc","ce")

# colours #
#labels=c("Elastic Net", "Lasso", "Neural Net", "Random Forest"),
#values=c("#006666", "lightblue", "#66CC00", "darkgreen"))+

# vars for data handling 

corr_col = "red"
corr_size = 9
corr_x = 0.15
corr_y = 0.9
axis_size = 20
axis_face = "bold"


graphics.off()

for (i in 1:10){
  
  png(filename = paste0(c_name[i], "_Lasso.png"), width = 441, height = 422 )
  print(ggscatter(cafpas[[i]], y = "observed", x = "lasso_mean", 
                  add = "reg.line", 
                  conf.int = TRUE,
                  add.params = list(color = "lightblue",
                                    fill = "lightgray"),
                  xlim = c(0,1), 
                  ylim = c(0,1),
                  xlab = "",
                  ylab = "") + 
          #geom_errorbar(aes(xmin = cafpas[[i]][["lasso_mean"]]-cafpas[[i]][["lasso_sd"]], xmax= cafpas[[i]][["lasso_mean"]]+cafpas[[i]][["lasso_sd"]]), width = 0.01, color = "black")+
          
          annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
          annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[i]]$lasso_mean, cafpas[[i]]$observed ),2))), colour = corr_col,
                   x = corr_x, y = 0.9, size = corr_size) +
          ggtitle("Lasso") + 
          theme(plot.title = element_text(face = "bold", colour = "black", size = axis_size), 
                #axis.text.x = element_blank(),
                axis.text.x = element_text(face = axis_face, size = axis_size), 
                axis.text.y = element_text(face = axis_face, size = axis_size)) )
  
  dev.off()
  
  png(filename = paste0(c_name[i], "_ElasticNet.png"), width = 441, height = 422 )
  print(ggscatter(cafpas[[i]], y = "observed", x = "elastic_mean", 
                  add = "reg.line", 
                  conf.int = TRUE,
                  add.params = list(color = "#006666",
                                    fill = "lightgray"),
                  xlim = c(0,1), 
                  ylim = c(0,1),
                  xlab = "",
                  ylab = "") + 
          # geom_errorbar(aes(ymin = cafpas[[i]][["elastic_mean"]]-cafpas[[i]][["elastic_sd"]], ymax= cafpas[[i]][["elastic_mean"]]+cafpas[[i]][["elastic_sd"]]), width = 0.01, color = "black")+
          annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
          annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[i]]$elastic_mean, cafpas[[i]]$observed),2))), colour = corr_col,
                   x = corr_x, y = 0.9, size = corr_size)  + 
          ggtitle("Elastic Net") + 
          theme(plot.title = element_text(face = "bold", colour = "black", size = axis_size),
                #axis.text.x = element_blank(),
                axis.text.x = element_text(face = axis_face, size = axis_size), 
                axis.text.y = element_blank())
                #axis.text.y = element_text(face = axis_face, size = axis_size)) 
  )
  
  
  dev.off()
  
  png(filename = paste0(c_name[i], "_RandomForest.png"), width = 441, height = 422 )
  print(ggscatter(cafpas[[i]], y = "observed", x = "rf_mean", 
                  add = "reg.line", 
                  conf.int = TRUE,
                  add.params = list(color = "darkgreen",
                                    fill = "lightgray"),
                  xlim = c(0,1), 
                  ylim = c(0,1),
                  xlab = "",
                  ylab = ""
                 ) + 
    #geom_errorbar(aes(ymin = cafpas[[i]][["rf_mean"]]-cafpas[[i]][["rf_sd"]], ymax= cafpas[[i]][["rf_mean"]]+cafpas[[i]][["rf_sd"]]), width = 0.01, color = "black")+
    
    annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
    annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[i]]$rf_mean, cafpas[[i]]$observed),2))), colour = corr_col,
             x = corr_x, y = 0.9, size = corr_size)  + 
    ggtitle("Random Forest") + 
    theme(plot.title = element_text(face = "bold", colour = "black", size = axis_size),
          
          axis.text.x = element_text(face = axis_face, size = axis_size),
          axis.text.y = element_blank())
          #axis.text.y = element_text(face = axis_face, size = axis_size)) 
  )
  dev.off()
  
 
  
}

#---------------------- Multiple plots on one page ----------------------------

# vars for data handling 

corr_col = "red"
corr_size = 10
corr_x = 0.1
corr_y = 0.9
axis_size = 28
axis_face = "bold"

# LASSO for all 10 cafpas # -------------------------------------------------------------------------

l1 <- ggscatter(cafpas[[1]], y = "observed", x = "lasso_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "lightblue",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(xmin = cafpas[[i]][["lasso_mean"]]-cafpas[[i]][["lasso_sd"]], xmax= cafpas[[i]][["lasso_mean"]]+cafpas[[i]][["lasso_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[1]]$lasso_mean, cafpas[[1]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) +
  #ggtitle("Lasso") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        #axis.text.x = element_text(face=axis_face,size=axis_size), 
        axis.text.y = element_text(face=axis_face, size=axis_size)
  )

l2 <- ggscatter(cafpas[[2]], y = "observed", x = "lasso_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "lightblue",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(xmin = cafpas[[i]][["lasso_mean"]]-cafpas[[i]][["lasso_sd"]], xmax= cafpas[[i]][["lasso_mean"]]+cafpas[[i]][["lasso_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[2]]$lasso_mean, cafpas[[2]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) +
  #ggtitle("Lasso") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        #axis.text.x = element_text(face=axis_face,size=axis_size), 
        axis.text.y = element_text(face=axis_face, size=axis_size))

l3 <- ggscatter(cafpas[[3]], y = "observed", x = "lasso_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "lightblue",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(xmin = cafpas[[i]][["lasso_mean"]]-cafpas[[i]][["lasso_sd"]], xmax= cafpas[[i]][["lasso_mean"]]+cafpas[[i]][["lasso_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[3]]$lasso_mean, cafpas[[3]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) +
  # ggtitle("Lasso") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        #axis.text.x = element_text(face=axis_face,size=axis_size), 
        axis.text.y = element_text(face=axis_face, size=axis_size))

l4 <- ggscatter(cafpas[[4]], y = "observed", x = "lasso_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "lightblue",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(xmin = cafpas[[i]][["lasso_mean"]]-cafpas[[i]][["lasso_sd"]], xmax= cafpas[[i]][["lasso_mean"]]+cafpas[[i]][["lasso_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[4]]$lasso_mean, cafpas[[4]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) +
  #ggtitle("Lasso") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        #axis.text.x = element_text(face=axis_face,size=axis_size), 
        axis.text.y = element_text(face=axis_face, size=axis_size))

l5 <- ggscatter(cafpas[[5]], y = "observed", x = "lasso_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "lightblue",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(xmin = cafpas[[i]][["lasso_mean"]]-cafpas[[i]][["lasso_sd"]], xmax= cafpas[[i]][["lasso_mean"]]+cafpas[[i]][["lasso_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[5]]$lasso_mean, cafpas[[5]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) +
  #ggtitle("Lasso") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_text(face=axis_face,size=axis_size), 
        axis.text.y = element_text(face=axis_face, size=axis_size))

l6 <- ggscatter(cafpas[[6]], y = "observed", x = "lasso_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "lightblue",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(xmin = cafpas[[i]][["lasso_mean"]]-cafpas[[i]][["lasso_sd"]], xmax= cafpas[[i]][["lasso_mean"]]+cafpas[[i]][["lasso_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[6]]$lasso_mean, cafpas[[6]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) +
  # ggtitle("Lasso") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        #axis.text.x = element_text(face=axis_face,size=axis_size), 
        axis.text.y = element_text(face=axis_face, size=axis_size))

l7 <- ggscatter(cafpas[[7]], y = "observed", x = "lasso_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "lightblue",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(xmin = cafpas[[i]][["lasso_mean"]]-cafpas[[i]][["lasso_sd"]], xmax= cafpas[[i]][["lasso_mean"]]+cafpas[[i]][["lasso_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[7]]$lasso_mean, cafpas[[7]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) +
  # ggtitle("Lasso") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        #axis.text.x = element_text(face=axis_face,size=axis_size), 
        axis.text.y = element_text(face=axis_face, size=axis_size))

l8 <- ggscatter(cafpas[[8]], y = "observed", x = "lasso_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "lightblue",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(xmin = cafpas[[i]][["lasso_mean"]]-cafpas[[i]][["lasso_sd"]], xmax= cafpas[[i]][["lasso_mean"]]+cafpas[[i]][["lasso_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[8]]$lasso_mean, cafpas[[8]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) +
  #ggtitle("Lasso") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        #axis.text.x = element_text(face=axis_face,size=axis_size), 
        axis.text.y = element_text(face=axis_face, size=axis_size))

l9 <- ggscatter(cafpas[[9]], y = "observed", x = "lasso_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "lightblue",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(xmin = cafpas[[i]][["lasso_mean"]]-cafpas[[i]][["lasso_sd"]], xmax= cafpas[[i]][["lasso_mean"]]+cafpas[[i]][["lasso_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[9]]$lasso_mean, cafpas[[9]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) +
  #ggtitle("Lasso") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        #axis.text.x = element_text(face=axis_face,size=axis_size), 
        axis.text.y = element_text(face=axis_face, size=axis_size))

l10 <- ggscatter(cafpas[[10]], y = "observed", x = "lasso_mean", 
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "lightblue",
                                   fill = "lightgray"),
                 xlim = c(0,1), 
                 ylim = c(0,1),
                 xlab = "",
                 ylab = "") + 
  #geom_errorbar(aes(xmin = cafpas[[i]][["lasso_mean"]]-cafpas[[i]][["lasso_sd"]], xmax= cafpas[[i]][["lasso_mean"]]+cafpas[[i]][["lasso_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[10]]$lasso_mean, cafpas[[10]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) +
  #ggtitle("Lasso") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        #axis.text.x = element_blank(),
        axis.text.x = element_text(face=axis_face,size=axis_size), 
        axis.text.y = element_text(face=axis_face, size=axis_size))

# ELASTIC for all 10 CAFPAS # -----------------------------------------------------------------------------------------------


e1 <- ggscatter(cafpas[[1]], y = "observed", x = "elastic_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "#006666",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  # geom_errorbar(aes(ymin = cafpas[[i]][["elastic_mean"]]-cafpas[[i]][["elastic_sd"]], ymax= cafpas[[i]][["elastic_mean"]]+cafpas[[i]][["elastic_sd"]]), width = 0.01, color = "black")+
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[1]]$elastic_mean, cafpas[[1]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Elastic Net") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

e2 <- ggscatter(cafpas[[2]], y = "observed", x = "elastic_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "#006666",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  # geom_errorbar(aes(ymin = cafpas[[i]][["elastic_mean"]]-cafpas[[i]][["elastic_sd"]], ymax= cafpas[[i]][["elastic_mean"]]+cafpas[[i]][["elastic_sd"]]), width = 0.01, color = "black")+
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[2]]$elastic_mean, cafpas[[2]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Elastic Net") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 


e3 <- ggscatter(cafpas[[3]], y = "observed", x = "elastic_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "#006666",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  # geom_errorbar(aes(ymin = cafpas[[i]][["elastic_mean"]]-cafpas[[i]][["elastic_sd"]], ymax= cafpas[[i]][["elastic_mean"]]+cafpas[[i]][["elastic_sd"]]), width = 0.01, color = "black")+
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[3]]$elastic_mean, cafpas[[3]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Elastic Net") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

e4 <- ggscatter(cafpas[[4]], y = "observed", x = "elastic_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "#006666",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  # geom_errorbar(aes(ymin = cafpas[[i]][["elastic_mean"]]-cafpas[[i]][["elastic_sd"]], ymax= cafpas[[i]][["elastic_mean"]]+cafpas[[i]][["elastic_sd"]]), width = 0.01, color = "black")+
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[4]]$elastic_mean, cafpas[[4]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Elastic Net") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

e5 <- ggscatter(cafpas[[5]], y = "observed", x = "elastic_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "#006666",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  # geom_errorbar(aes(ymin = cafpas[[i]][["elastic_mean"]]-cafpas[[i]][["elastic_sd"]], ymax= cafpas[[i]][["elastic_mean"]]+cafpas[[i]][["elastic_sd"]]), width = 0.01, color = "black")+
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[5]]$elastic_mean, cafpas[[5]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Elastic Net") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_text(face=axis_face,size=axis_size), 
        axis.text.y = element_blank())
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

e6 <- ggscatter(cafpas[[6]], y = "observed", x = "elastic_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "#006666",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  # geom_errorbar(aes(ymin = cafpas[[i]][["elastic_mean"]]-cafpas[[i]][["elastic_sd"]], ymax= cafpas[[i]][["elastic_mean"]]+cafpas[[i]][["elastic_sd"]]), width = 0.01, color = "black")+
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[6]]$elastic_mean, cafpas[[6]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Elastic Net") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 


e7 <- ggscatter(cafpas[[7]], y = "observed", x = "elastic_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "#006666",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  # geom_errorbar(aes(ymin = cafpas[[i]][["elastic_mean"]]-cafpas[[i]][["elastic_sd"]], ymax= cafpas[[i]][["elastic_mean"]]+cafpas[[i]][["elastic_sd"]]), width = 0.01, color = "black")+
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[7]]$elastic_mean, cafpas[[7]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Elastic Net") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

e8 <- ggscatter(cafpas[[8]], y = "observed", x = "elastic_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "#006666",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  # geom_errorbar(aes(ymin = cafpas[[i]][["elastic_mean"]]-cafpas[[i]][["elastic_sd"]], ymax= cafpas[[i]][["elastic_mean"]]+cafpas[[i]][["elastic_sd"]]), width = 0.01, color = "black")+
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[8]]$elastic_mean, cafpas[[8]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Elastic Net") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 


e9 <- ggscatter(cafpas[[9]], y = "observed", x = "elastic_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "#006666",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  # geom_errorbar(aes(ymin = cafpas[[i]][["elastic_mean"]]-cafpas[[i]][["elastic_sd"]], ymax= cafpas[[i]][["elastic_mean"]]+cafpas[[i]][["elastic_sd"]]), width = 0.01, color = "black")+
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[9]]$elastic_mean, cafpas[[9]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Elastic Net") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

e10 <- ggscatter(cafpas[[10]], y = "observed", x = "elastic_mean", 
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "#006666",
                                   fill = "lightgray"),
                 xlim = c(0,1), 
                 ylim = c(0,1),
                 xlab = "",
                 ylab = "") + 
  # geom_errorbar(aes(ymin = cafpas[[i]][["elastic_mean"]]-cafpas[[i]][["elastic_sd"]], ymax= cafpas[[i]][["elastic_mean"]]+cafpas[[i]][["elastic_sd"]]), width = 0.01, color = "black")+
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[10]]$elastic_mean, cafpas[[10]]$observed),2))),colour = corr_col, 
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Elastic Net") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_text(face=axis_face,size=axis_size), 
        #axis.text.y = element_text(face=axis_face, size=axis_size)) 
        axis.text.y = element_blank())

# RANDOM FOREST FOR ALL 10 CAFPAS # -------------------------------------------------------------------------

r1 <- ggscatter(cafpas[[1]], y = "observed", x = "rf_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "darkgreen",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["rf_mean"]]-cafpas[[i]][["rf_sd"]], ymax= cafpas[[i]][["rf_mean"]]+cafpas[[i]][["rf_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[1]]$rf_mean, cafpas[[1]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Random Forest") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
# axis.text.x = element_text(face=axis_face,size=axis_size), 
# axis.text.y = element_text(face=axis_face, size=axis_size)) 

r2 <- ggscatter(cafpas[[2]], y = "observed", x = "rf_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "darkgreen",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["rf_mean"]]-cafpas[[i]][["rf_sd"]], ymax= cafpas[[i]][["rf_mean"]]+cafpas[[i]][["rf_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[2]]$rf_mean, cafpas[[2]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Random Forest") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

r3 <- ggscatter(cafpas[[3]], y = "observed", x = "rf_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "darkgreen",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["rf_mean"]]-cafpas[[i]][["rf_sd"]], ymax= cafpas[[i]][["rf_mean"]]+cafpas[[i]][["rf_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[3]]$rf_mean, cafpas[[3]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Random Forest") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

r4 <- ggscatter(cafpas[[4]], y = "observed", x = "rf_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "darkgreen",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["rf_mean"]]-cafpas[[i]][["rf_sd"]], ymax= cafpas[[i]][["rf_mean"]]+cafpas[[i]][["rf_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[4]]$rf_mean, cafpas[[4]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Random Forest") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

r5 <- ggscatter(cafpas[[5]], y = "observed", x = "rf_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "darkgreen",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["rf_mean"]]-cafpas[[i]][["rf_sd"]], ymax= cafpas[[i]][["rf_mean"]]+cafpas[[i]][["rf_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[5]]$rf_mean, cafpas[[5]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Random Forest") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_text(face=axis_face,size=axis_size), 
        #axis.text.y = element_text(face=axis_face, size=axis_size)) 
        axis.text.y = element_blank())

r6 <- ggscatter(cafpas[[6]], y = "observed", x = "rf_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "darkgreen",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["rf_mean"]]-cafpas[[i]][["rf_sd"]], ymax= cafpas[[i]][["rf_mean"]]+cafpas[[i]][["rf_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[6]]$rf_mean, cafpas[[6]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Random Forest") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

r7 <- ggscatter(cafpas[[7]], y = "observed", x = "rf_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "darkgreen",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["rf_mean"]]-cafpas[[i]][["rf_sd"]], ymax= cafpas[[i]][["rf_mean"]]+cafpas[[i]][["rf_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[7]]$rf_mean, cafpas[[7]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Random Forest") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

r8 <- ggscatter(cafpas[[8]], y = "observed", x = "rf_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "darkgreen",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["rf_mean"]]-cafpas[[i]][["rf_sd"]], ymax= cafpas[[i]][["rf_mean"]]+cafpas[[i]][["rf_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[8]]$rf_mean, cafpas[[8]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Random Forest") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

r9 <- ggscatter(cafpas[[9]], y = "observed", x = "rf_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "darkgreen",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["rf_mean"]]-cafpas[[i]][["rf_sd"]], ymax= cafpas[[i]][["rf_mean"]]+cafpas[[i]][["rf_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[9]]$rf_mean, cafpas[[9]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #  ggtitle("Random Forest") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

r10 <- ggscatter(cafpas[[10]], y = "observed", x = "rf_mean", 
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "darkgreen",
                                   fill = "lightgray"),
                 xlim = c(0,1), 
                 ylim = c(0,1),
                 xlab = "",
                 ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["rf_mean"]]-cafpas[[i]][["rf_sd"]], ymax= cafpas[[i]][["rf_mean"]]+cafpas[[i]][["rf_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[10]]$rf_mean, cafpas[[10]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Random Forest") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_text(face=axis_face,size=axis_size), 
        #axis.text.y = element_text(face=axis_face, size=axis_size)) 
        axis.text.y = element_blank())
# NEURAL NET FOR ALL 10 CAFPAS # ------------------------------------------------------------------

n1 <- ggscatter(cafpas[[1]], y = "observed", x = "nn_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "#66CC00",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["nn_mean"]]-cafpas[[i]][["nn_sd"]], ymax= cafpas[[i]][["nn_mean"]]+cafpas[[i]][["nn_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[1]]$nn_mean, cafpas[[1]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Neural Network") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

n2 <-  ggscatter(cafpas[[2]], y = "observed", x = "nn_mean", 
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "#66CC00",
                                   fill = "lightgray"),
                 xlim = c(0,1), 
                 ylim = c(0,1),
                 xlab = "",
                 ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["nn_mean"]]-cafpas[[i]][["nn_sd"]], ymax= cafpas[[i]][["nn_mean"]]+cafpas[[i]][["nn_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[2]]$nn_mean, cafpas[[2]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Neural Network") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

n3 <-  ggscatter(cafpas[[3]], y = "observed", x = "nn_mean", 
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "#66CC00",
                                   fill = "lightgray"),
                 xlim = c(0,1), 
                 ylim = c(0,1),
                 xlab = "",
                 ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["nn_mean"]]-cafpas[[i]][["nn_sd"]], ymax= cafpas[[i]][["nn_mean"]]+cafpas[[i]][["nn_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[3]]$nn_mean, cafpas[[3]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Neural Network") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

n4 <-  ggscatter(cafpas[[4]], y = "observed", x = "nn_mean", 
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "#66CC00",
                                   fill = "lightgray"),
                 xlim = c(0,1), 
                 ylim = c(0,1),
                 xlab = "",
                 ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["nn_mean"]]-cafpas[[i]][["nn_sd"]], ymax= cafpas[[i]][["nn_mean"]]+cafpas[[i]][["nn_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[4]]$nn_mean, cafpas[[4]]$observed),2))),colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Neural Network") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

n5 <-  ggscatter(cafpas[[5]], y = "observed", x = "nn_mean", 
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "#66CC00",
                                   fill = "lightgray"),
                 xlim = c(0,1), 
                 ylim = c(0,1),
                 xlab = "",
                 ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["nn_mean"]]-cafpas[[i]][["nn_sd"]], ymax= cafpas[[i]][["nn_mean"]]+cafpas[[i]][["nn_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[5]]$nn_mean, cafpas[[5]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Neural Network") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_text(face=axis_face,size=axis_size), 
        #axis.text.y = element_text(face=axis_face, size=axis_size)) 
        axis.text.y = element_blank())

n6 <-  ggscatter(cafpas[[6]], y = "observed", x = "nn_mean", 
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "#66CC00",
                                   fill = "lightgray"),
                 xlim = c(0,1), 
                 ylim = c(0,1),
                 xlab = "",
                 ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["nn_mean"]]-cafpas[[i]][["nn_sd"]], ymax= cafpas[[i]][["nn_mean"]]+cafpas[[i]][["nn_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[6]]$nn_mean, cafpas[[6]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Neural Network") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

n7 <-  ggscatter(cafpas[[7]], y = "observed", x = "nn_mean", 
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "#66CC00",
                                   fill = "lightgray"),
                 xlim = c(0,1), 
                 ylim = c(0,1),
                 xlab = "",
                 ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["nn_mean"]]-cafpas[[i]][["nn_sd"]], ymax= cafpas[[i]][["nn_mean"]]+cafpas[[i]][["nn_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[7]]$nn_mean, cafpas[[7]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  # ggtitle("Neural Network") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

n8 <-  ggscatter(cafpas[[8]], y = "observed", x = "nn_mean", 
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "#66CC00",
                                   fill = "lightgray"),
                 xlim = c(0,1), 
                 ylim = c(0,1),
                 xlab = "",
                 ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["nn_mean"]]-cafpas[[i]][["nn_sd"]], ymax= cafpas[[i]][["nn_mean"]]+cafpas[[i]][["nn_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[8]]$nn_mean, cafpas[[8]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Neural Network") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

n9 <- ggscatter(cafpas[[9]], y = "observed", x = "nn_mean", 
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "#66CC00",
                                  fill = "lightgray"),
                xlim = c(0,1), 
                ylim = c(0,1),
                xlab = "",
                ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["nn_mean"]]-cafpas[[i]][["nn_sd"]], ymax= cafpas[[i]][["nn_mean"]]+cafpas[[i]][["nn_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[9]]$nn_mean, cafpas[[9]]$observed),2))), colour = corr_col,
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Neural Network") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
#axis.text.x = element_text(face=axis_face,size=axis_size), 
#axis.text.y = element_text(face=axis_face, size=axis_size)) 

n10 <- ggscatter(cafpas[[10]], y = "observed", x = "nn_mean", 
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "#66CC00",
                                   fill = "lightgray"),
                 xlim = c(0,1), 
                 ylim = c(0,1),
                 xlab = "",
                 ylab = "") + 
  #geom_errorbar(aes(ymin = cafpas[[i]][["nn_mean"]]-cafpas[[i]][["nn_sd"]], ymax= cafpas[[i]][["nn_mean"]]+cafpas[[i]][["nn_sd"]]), width = 0.01, color = "black")+
  
  annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = 2 ) +
  annotate("text", label = paste("r = ", as.character(round(cor(cafpas[[10]]$nn_mean, cafpas[[10]]$observed),2))),colour = corr_col, 
           x = corr_x, y = 0.9, size = corr_size) + 
  #ggtitle("Neural Network") + 
  theme(plot.title = element_text(face = "bold", colour = "black", size = "16"), 
        axis.text.x = element_text(face=axis_face,size=axis_size), 
        #axis.text.y = element_text(face=axis_face, size=axis_size)) 
        axis.text.y = element_blank())


# PLOT ALL # -----------------------------------

graphics.off()

png(filename = "PredObs_1_5.png", width = 2050, height = 2050)
ggarrange(l1, e1, r1, n1,  
          l2, e2, r2, n2, 
          l3, e3, r3, n3,
          l4, e4, r4, n4,
          l5, e5, r5, n5,
          # labels = c("Lasso", "Elastic Net", "Random Forest", "Neural Net"),
          # font.label = list(size = 30, color = "black"),
          ncol = 4, nrow = 5)

dev.off()

png(filename = "PredObs_6_10.png", width = 1920, height = 1920)
ggarrange(l6, e6, r6, n6,
          l7, e7, r7, n7,
          l8, e8, r8, n8, 
          l9, e9, r9, n9, 
          l10, e10, r10, n10,
          # labels = c("Lasso", "Elastic Net", "Random Forest", "Neural Net"), 
          ncol = 4, nrow = 5)

dev.off()


