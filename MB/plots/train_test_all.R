#####################################################################
#    Plot Train/Test error for all models                           #
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

# set seed
set.seed(22)

# set up folders and directories
dir.create(file.path("MB\\data\\le_ri_combine_pta\\"), recursive = TRUE)  
dir.create(file.path("MB\\plots\\train_test\\"), recursive = TRUE)  

path_data <- "MB\\data\\le_ri_combine_pta\\"
path_plot <- "MB\\plots\\train_test\\"

# load models
load(paste0(MAIN, FOLDER, "data\\randomForest\\rf.results_pta.Rdata"))
load(paste0(MAIN, FOLDER, "data\\elasticNet\\elastic_net_pta.results.Rdata"))
load(paste0(MAIN, FOLDER, "data\\lasso\\lasso.results_pta.Rdata"))

# select random imputation set for plotting of train/test error set
imp <- round(runif(1, min=1, max=20))



lasso_data <- data.frame("CAFPA" = rep(0:9, times = 2), 
                         "Group" = rep(c("Train", "Test"), each = 10),
                         "Error" = c(lasso.results$train_MAE[[imp]], lasso.results$test_MAE[[imp]]))
elastic_data <- data.frame("CAFPA" = rep(0:9, times = 2), 
                           "Group" = rep(c("Train", "Test"), each = 10),
                           "Error" = c(elastic_net.results$train_MAE[[imp]], elastic_net.results$test_MAE[[imp]]))
rf_data <- data.frame("CAFPA" = rep(0:9, times = 2), 
                      "Group" = rep(c("Train", "Test"), each = 10),
                      "Error" = c(rf.results$train_MAE[[imp]], rf.results$test_MAE[[imp]]))

axis_size = 28
axis_face = "bold"
point_size = 5
cafpa_names = expression("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB","CN", "CC","CE")

data <- list(lasso= lasso_data,elastic= elastic_data,rf = rf_data)

plots <- list()

for (name in names(data)){
  tmp <- ggplot(data = data[[name]], aes(x = CAFPA, y = Error, color = Group)) +
            geom_point(size = point_size) + 
            geom_line(size = point_size-4)+
            scale_x_continuous(breaks = c(0:9), labels =cafpa_names) +
            scale_color_viridis(discrete = TRUE)+
            theme_bw() +
            theme( panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   axis.text.x = element_text(face = axis_face, size = axis_size, angle = 45, vjust = 0.5, color = "black"), 
                   axis.text.y = element_text(face = axis_face, size = axis_size),
                   legend.title = element_blank(),
                   legend.text = element_text(size= axis_size, face= "bold")) +
            ylim(0,0.3)+
            xlab("") + 
            ylab("") 
  
  plots[[name]] <- tmp
  
}

png(paste0(path_save, "Train_test_baseline_all_models.png"), width = 1500, height = 500 )
ggarrange(plotlist = plots, nrow = 1, ncol = 3, 
          common.legend = TRUE, legend = "right")

dev.off()
