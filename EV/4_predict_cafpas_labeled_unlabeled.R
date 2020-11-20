#########################################################################################
#
# CAFPA prediction with matched data 
#
#########################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()
graphics.off()

# libraries
library(gplots)

# set up folders and directories
dir.create(file.path("PSM_R\\data\\"), recursive = TRUE)  
dir.create(file.path("MB\\data\\le_ri_combine_pta\\models\\"), recursive = TRUE)  
dir.create(file.path("PSM_r\\data\\"), recursive = TRUE)  
dir.create(file.path("PSM_R\\plots\\"), recursive = TRUE)  

path_data <- "PSM_R\\data\\"
path_model<- "MB\\data\\le_ri_combine_pta\\models\\"
path_save <- "PSM_r\\data\\"
path_plot <- "PSM_R\\plots\\"

# set seed
set.seed(22)

# load Matched data (based on demographics)
load(paste0(path_data, "psm_data_nearest_demMatched.Rdata"))

# convert characters to numeric in matches_ID
matches_ID <- as.data.frame(apply(matches_ID, 2, as.numeric))

# reorder unlabeled in such a way, that each row matches its pair in the labeled dataset 
tmp_unlabeled <- data_unlabeled[match(matches_ID$unlabeled[1], data_unlabeled$ID),]

for (i in 2:240){
  tmp_unlabeled <- rbind(tmp_unlabeled, data_unlabeled[match(matches_ID$unlabeled[i], data_unlabeled$ID),])
}

# assign it to original data_unlabeled structure (now reordered)
data_unlabeled <- tmp_unlabeled

# iterations of mice
imp.num = 20

#pre-define labeled & unlabed vars
labeled_lasso_tmp.cafpa = matrix(data = NA, nrow = 240, ncol = 20) # 20, are the imps of the models (lasso, elastic, rf)
labeled_lasso_cafpas = matrix(data = NA, nrow = 240, ncol = 10)

labeled_elastic_tmp.cafpa = matrix(data = NA, nrow = 240, ncol = 20)
labeled_elastic_cafpas = matrix(data = NA, nrow = 240, ncol = 10)

labeled_rf_tmp.cafpa = matrix(data = NA, nrow = 240, ncol = 20)
labeled_rf_cafpas = matrix(data = NA, nrow = 240, ncol = 10)
#
unlabeled_lasso_tmp.cafpa = matrix(data = NA, nrow = 240, ncol = 20)
unlabeled_lasso_cafpas = matrix(data = NA, nrow = 240, ncol = 10)

unlabeled_elastic_tmp.cafpa = matrix(data = NA, nrow = 240, ncol = 20)
unlabeled_elastic_cafpas = matrix(data = NA, nrow = 240, ncol = 10)

unlabeled_rf_tmp.cafpa = matrix(data = NA, nrow = 240, ncol = 20)
unlabeled_rf_cafpas = matrix(data = NA, nrow = 240, ncol = 10)

# match data to model formula
labeled <- data_labeled[, match("m.swi_sum", names(data_labeled)): match("m.pta_asym", names(data_labeled))]
unlabeled <- data_unlabeled[, match("m.swi_sum", names(data_labeled)): match("m.pta_asym", names(data_labeled))]

# make model matrix of both labeled and unlabeled cases
labeled$output <- 1:240 # for dummy fiting
unlabeled$output <- 1:240 # for dummy fiting
x_labeled <- model.matrix(output~.,labeled)
x_unlabeled <- model.matrix(output~.,unlabeled)


for (c in 1:10){
  
  idx = 1

    for (m in 1:imp.num){ #loop through all models
      
      
      # load lasso model 
      load(paste0(path_model,"lasso\\lasso_imp_pta_", m ,"_cafpa_", c ,".Rdata"))
      # load elastic Net model 
      load(paste0(path_model,"elasticNet\\elastic_imp_pta_", m ,"_cafpa_", c ,".Rdata"))
      
      # load rf model 
      load(paste0(path_model,"randomForest\\rf_imp_pta_", m ,"_cafpa_", c ,".Rdata"))
      
      # make predictions and store for both labeled and unlabeled
      labeled_lasso.pred = predict(lasso.best, newx=x_labeled)
      labeled_lasso_tmp.cafpa[,idx] <- labeled_lasso.pred
      
      labeled_elastic.pred <- elastic_net_model %>% predict(labeled)
      labeled_elastic_tmp.cafpa[,idx] <- labeled_elastic.pred
      
      labeled_rf.pred <- predict(pred.mod, newdata = labeled)
      labeled_rf_tmp.cafpa[,idx] <- labeled_rf.pred
      
      #
      unlabeled_lasso.pred = predict(lasso.best, newx=x_unlabeled)
      unlabeled_lasso_tmp.cafpa[,idx] <- unlabeled_lasso.pred
      
      unlabeled_elastic.pred <- elastic_net_model %>% predict(unlabeled)
      unlabeled_elastic_tmp.cafpa[,idx] <- unlabeled_elastic.pred
      
      unlabeled_rf.pred <- predict(pred.mod, newdata = unlabeled)
      unlabeled_rf_tmp.cafpa[,idx] <- unlabeled_rf.pred
      
      # update index for cafpa
      idx = idx + 1
      
    } # model imp loop 
    
  print(c)
  
  labeled_lasso_cafpas[,c] = rowMeans(labeled_lasso_tmp.cafpa)
  labeled_elastic_cafpas[,c] = rowMeans(labeled_elastic_tmp.cafpa)
  labeled_rf_cafpas[,c] = rowMeans(labeled_rf_tmp.cafpa)
  
  unlabeled_lasso_cafpas[,c] = rowMeans(unlabeled_lasso_tmp.cafpa)
  unlabeled_elastic_cafpas[,c] = rowMeans(unlabeled_elastic_tmp.cafpa)
  unlabeled_rf_cafpas[,c] = rowMeans(unlabeled_rf_tmp.cafpa)
  
}# CAFPA loop

# add labels again 
labeled_lasso_cafpas   = cbind(labeled_lasso_cafpas, as.vector(matches_ID$labeled))
labeled_elastic_cafpas = cbind(labeled_elastic_cafpas, as.vector(matches_ID$labeled))
labeled_rf_cafpas      = cbind(labeled_rf_cafpas, as.vector(matches_ID$labeled) )

unlabeled_lasso_cafpas   = cbind(unlabeled_lasso_cafpas, as.vector(matches_ID$unlabeled))
unlabeled_elastic_cafpas = cbind(unlabeled_elastic_cafpas, as.vector(matches_ID$unlabeled))
unlabeled_rf_cafpas      = cbind(unlabeled_rf_cafpas, as.vector(matches_ID$unlabeled) )


# save CAFPAs
save(labeled_lasso_cafpas, file = paste0(path_save, "CAFPAs_lasso_labeled_demMatched.Rdata" ))
save(labeled_elastic_cafpas, file = paste0(path_save, "CAFPAs_elastic_labeled_demMatched.Rdata" ))
save(labeled_rf_cafpas, file = paste0(path_save, "CAFPAs_rf_labeled_demMatched.Rdata" ))

save(unlabeled_lasso_cafpas, file = paste0(path_save, "CAFPAs_lasso_unlabeled_demMatched.Rdata" ))
save(unlabeled_elastic_cafpas, file = paste0(path_save, "CAFPAs_elastic_unlabeled_demMatched.Rdata" ))
save(unlabeled_rf_cafpas, file = paste0(path_save, "CAFPAs_rf_unlabeled_demMatched.Rdata" ))

# save for matlab
writeMat(paste0(path_save, "pred_CAFPAS_all_models_MB.mat"),
         CAFPA_lasso_labeled = labeled_lasso_cafpas, CAFPA_elastic_labeled = labeled_elastic_cafpas, CAFPA_rf_labeled = labeled_rf_cafpas, 
         CAFPA_lasso_unlabeled = unlabeled_lasso_cafpas, CAFPA_elastic_unlabeled = unlabeled_elastic_cafpas, CAFPA_rf_unlabeled = unlabeled_rf_cafpas)


#--------------- HEATMAPS ----------------------

my_palette <- colorRampPalette(c("darkgreen", "yellow", "red"))(n = 299)
col_breaks = c(seq(0,0.3,length=100),       # for green
               seq(0.31,0.7,length=100),    # for yellow
               seq(0.71,1,length=100))      # for red 


setwd(path_plot)
# LABELED ------------------------------------------
png("Lasso_heatmap_predictions_labeled.png", width = 716, height = 634)
heatmap.2(labeled_lasso_cafpas[,-11], dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
          col=my_palette, breaks = col_breaks, 
          key = TRUE,
          density.info = c("density"),
          #trace = "none",
          tracecol="black",
          denscol = "black", 
          labCol= c("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB", "CN", "CC", "CE"), 
          srtCol = 0,
          xlab = "CAFPAs", 
          ylab = "Patients", 
          main = "Lasso prediction labeled")

dev.off()

png("Enet_heatmap_predictions_labeled.png", width = 716, height = 634)
heatmap.2(labeled_elastic_cafpas[,-11], dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
          col=my_palette, breaks = col_breaks,
          
          key = TRUE,
          density.info = c("density"),
          #trace = "none",
          tracecol="black",
          denscol = "black", 
          labCol= c("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB", "CN", "CC", "CE"), 
          srtCol = 0,
          xlab = "CAFPAs", 
          ylab = "Patients", 
          main = "Elastic Net prediction labeled")

dev.off()

png("RF_heatmap_predictions_labeled.png", width = 716, height = 634)
heatmap.2(labeled_rf_cafpas[,-11], dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
          col=my_palette, breaks = col_breaks, 
          
          key = TRUE,
          density.info = c("density"),
          
          #trace = "none",
          tracecol="black",
          denscol = "black", 
          labCol= c("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB", "CN", "CC", "CE"), 
          srtCol = 0,
          xlab = "CAFPAs", 
          ylab = "Patients", 
          main = "Random Forest prediction labeled")

dev.off()
# UNLABELED ----------------------------------

png("Lasso_heatmap_predictions_unlabeled.png", width = 716, height = 634)
heatmap.2(unlabeled_lasso_cafpas[,-11], dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
          col=my_palette, breaks = col_breaks, 
          key = TRUE,
          density.info = c("density"),
          #trace = "none",
          tracecol="black",
          denscol = "black", 
          labCol= c("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB", "CN", "CC", "CE"), 
          srtCol = 0,
          xlab = "CAFPAs", 
          ylab = "Patients", 
          main = "Lasso prediction unlabeled")

dev.off()

png("Enet_heatmap_predictions_unlabeled.png", width = 716, height = 634)
heatmap.2(unlabeled_elastic_cafpas[,-11], dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
          col=my_palette, breaks = col_breaks,
          
          key = TRUE,
          density.info = c("density"),
          #trace = "none",
          tracecol="black",
          denscol = "black", 
          labCol= c("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB", "CN", "CC", "CE"), 
          srtCol = 0,
          xlab = "CAFPAs", 
          ylab = "Patients", 
          main = "Elastic Net prediction unlabeled")

dev.off()

png("RF_heatmap_predictions_unlabeled.png", width = 716, height = 634)
heatmap.2(unlabeled_rf_cafpas[,-11], dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
          col=my_palette, breaks = col_breaks, 
          
          key = TRUE,
          density.info = c("density"),
          
          #trace = "none",
          tracecol="black",
          denscol = "black", 
          labCol= c("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB", "CN", "CC", "CE"), 
          srtCol = 0,
          xlab = "CAFPAs", 
          ylab = "Patients", 
          main = "Random Forest prediction unlabeled")

dev.off()


# load in expert data to compare 

CAFPA_labeled <- load("C:\\Users\\Samira\\Desktop\\MA\\analysis\\data\\1_preprocess\\preprocessed_data_final.Rdata")


expert_cafpa <- as.matrix(output.var)

png("CAFPA_expert_labels_heatmap.png", width = 716, height = 634)
heatmap.2(expert_cafpa, dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
          col=my_palette, breaks = col_breaks, 
          
          key = TRUE,
          density.info = c("density"),
          
          #trace = "none",
          tracecol="black",
          denscol = "black", 
          labCol= c("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB", "CN", "CC", "CE"), 
          srtCol = 0,
          xlab = "CAFPAs", 
          ylab = "Patients", 
          main = "Expert Labels")

dev.off()

