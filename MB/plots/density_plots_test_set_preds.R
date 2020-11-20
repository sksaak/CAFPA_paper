#########################################################################################
#
# Density Plots for Predicted and Expert CAFPAs
#
#########################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()
graphics.off()

library(ggplot2)
library(ggpubr) 
library(grid)

# set up folders and directories
dir.create(file.path("MB\\data\\"), recursive = TRUE)  
dir.create(file.path("MB\\plots\\density_plots\\"), recursive = TRUE)  

path_data <- "MB\\data\\"
path_plot <- "MB\\plots\\density_plots\\"



# load data for models
load(paste0(path_data, "lasso\\lasso.results_pta.Rdata"))
load(paste0(path_data, "elasticNet\\elastic_net_pta.results.Rdata"))
load(paste0(path_data, "randomForest\\rf.results_pta.Rdata"))

# pre-define models for CAFPA preds
lasso_preds   <- matrix(data = NA, nrow = 47, ncol = 10)
elastic_preds <- matrix(data = NA, nrow = 47, ncol = 10)
rf_preds      <- matrix(data = NA, nrow = 47, ncol = 10)


# make cafpa prediction matrices per model
for (i in 1:10){
  # lasso
  tmp_lasso_preds   <- sapply(lasso.results$predAct, function(x) x[i])
  tmp_lasso <- Reduce("+", tmp_lasso_preds) / length(tmp_lasso_preds)
  lasso_preds[1:length(tmp_lasso$preds),i] <- tmp_lasso$preds
  # elastic
  tmp_elastic_preds   <- sapply(elastic_net.results$predAct, function(x) x[i])
  tmp_elastic <- Reduce("+", tmp_elastic_preds) / length(tmp_elastic_preds)
  elastic_preds[1:length(tmp_elastic$preds),i] <- tmp_elastic$preds
  # rf
  tmp_rf_preds   <- sapply(rf.results$predAct, function(x) x[i])
  tmp_rf <- Reduce("+", tmp_rf_preds) / length(tmp_rf_preds)
  rf_preds[1:length(tmp_rf$preds),i] <- tmp_rf$preds
  
}
rm("tmp_lasso_preds", "tmp_elastic_preds", "tmp_rf_preds", "tmp_lasso", "tmp_elastic", "tmp_rf")


# make expert dataset (TRUE CAFPAS)
expert <- matrix(data = NA, nrow = 47, ncol = 10)

for (i in 1:10){
  expert[1:length(lasso.results$predAct[[1]][[i]][["actual"]]), i] <- lasso.results$predAct[[1]][[i]][["actual"]]
}

# colnames
cafpas <- c("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB", "CN", "CC", "CE")

colnames(lasso_preds) <- cafpas
colnames(elastic_preds) <- cafpas
colnames(rf_preds) <- cafpas
colnames(expert) <- cafpas



# plot densities for CAFPAs across models (lasso, elastic, rf) and expert ---------------------------------------

lasso   <- as.data.frame(lasso_preds)
elastic <- as.data.frame(elastic_preds)
rf      <- as.data.frame(rf_preds)
expert  <- as.data.frame(expert)

# general plot settings
col_breaks = c(seq(0,0.3,length=100),       # for green
               seq(0.31,0.7,length=100),    # for yellow
               seq(0.71,1,length=100))      # for red 

my_palette <- colorRampPalette(c("#006401", "#FCD402", "#B12322"))(n = 100)
g <- rasterGrob(t(my_palette), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

axis_size = 80
line_size = 3
y_lim <- c(0,8)
x_lim <- c(0,1)
bandwidth = 0.015
axis_x <- element_blank()
axis_rf <- element_text(face = "bold", size = axis_size, color = "black")
plot_width <- 600
plot_height <- 600


# density plots for all models ---------------------------------------
for (i in 1:10){
  if (i == 1){axis_y = element_text(face = "bold", size = axis_size, color = "black")
  } else {axis_y = element_blank()}
  
  png(paste0("Expert_", cafpas[i], ".png"), width = plot_width, height = plot_height)
  print(ggplot(expert, aes_string(x = cafpas[i])) + 
          annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
          geom_density(size = line_size, bw = bandwidth) + 
          geom_vline(aes(xintercept=mean(expert[,i], na.rm = TRUE)),
                     color="black", linetype="dashed", size=1) +
          theme(axis.line = element_line(colour = "black", size = line_size, linetype = "solid"),
                axis.text.x= axis_x, 
                axis.text.y= axis_y,
                plot.background = element_rect(fill = "#E6E6E6")) + 
          labs(x = "", y = "") + 
          xlim(x_lim) + 
          if ( i == 1){scale_y_continuous(breaks = y_lim, limits = y_lim)} else {ylim(y_lim)} 
        
  )
  dev.off()
  
  # lasso plots 
  png(paste0("Lasso_", cafpas[i], ".png"), width = plot_width, height = plot_height)
  print(ggplot(lasso, aes_string(x = cafpas[i])) + 
          annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
          geom_density(size = line_size, bw = bandwidth) + 
          geom_vline(aes(xintercept=mean(lasso[,i], na.rm = TRUE)),
                     color="black", linetype="dashed", size=1) +
          theme(axis.line = element_line(colour = "black", size = line_size, linetype = "solid"),
                axis.text.x= axis_x, 
                axis.text.y= axis_y, 
          ) + 
          labs(x = "", y = "") + 
          xlim(x_lim) +
          if ( i == 1){scale_y_continuous(breaks = y_lim, limits = y_lim)} else {ylim(y_lim)} 
  )
  dev.off()
  
  # elastic plots 
  png(paste0("Elastic_", cafpas[i], ".png"), width = plot_width, height = plot_height)
  print(ggplot(elastic, aes_string(x = cafpas[i])) + 
          annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
          geom_density(size = line_size, bw = bandwidth) + 
          geom_vline(aes(xintercept=mean(elastic[,i], na.rm = TRUE)),
                     color="black", linetype="dashed", size=1) +
          theme(axis.line = element_line(colour = "black", size = line_size, linetype = "solid"),
                axis.text.x= axis_x, 
                axis.text.y= axis_y, 
          ) + 
          labs(x = "", y = "") + 
          xlim(x_lim) +
          if ( i == 1){scale_y_continuous(breaks = y_lim, limits = y_lim)} else {ylim(y_lim)} 
  )
  dev.off()
  
  # rf plots   
  png(paste0("Rf_", cafpas[i], ".png"), width = plot_width, height = plot_height)
  print(ggplot(rf, aes_string(x = cafpas[i])) + 
          annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
          geom_density(size = line_size, bw= bandwidth) + 
          geom_vline(aes(xintercept=mean(rf[,i], na.rm = TRUE)),
                     color="black", linetype="dashed", size=1) +
          theme(axis.line = element_line(colour = "black", size = line_size, linetype = "solid"),
                axis.text.x= axis_rf, 
                axis.text.y= axis_y, 
          ) + 
          labs(x = "", y = "") + 
          scale_x_continuous(breaks = c(0,1), limits = (x_lim)) +
          if ( i == 1){scale_y_continuous(breaks = y_lim, limits = y_lim)} else {ylim(y_lim)} 
          )
  dev.off()
}






