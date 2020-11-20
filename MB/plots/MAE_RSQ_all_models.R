################################################################################################
#         Model comparison regarding predictive performance                   
#
#         - MAE 
#         - Rsquared
#         
#
################################################################################################


# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()
graphics.off()

# libraries
library(DescTools)
library(ggplot2)

# set seed
set.seed(22)

# set up folders and directories
dir.create(file.path("MB\\data\\"), recursive = TRUE)  
dir.create(file.path("MB\\plots\\model_comparison_MAE_RSQ\\"), recursive = TRUE)  

path_data <- "MB\\data\\"
path_plot <- "MB\\plots\\model_comparison_MAE_RSQ\\"
# load data
load(file = paste0(path_data, "lasso\\lasso.df.Rdata"))
load(file = paste0(path_data, "elasticNet\\elastic.df.Rdata"))
load(file = paste0(path_data, "randomForest\\rf.df.Rdata"))

# load results data
load(file = paste0(path_data, "lasso\\lasso.results_pta.Rdata"))
load(file = paste0(path_data, "randomForest\\rf.results_pta.Rdata"))
load(file = paste0(path_data, "elasticNet\\elastic_net_pta.results.Rdata"))

# load baseline and ICC
load(file = paste0("MB\\data\\mean_baseline.Rdata"))

# predefine vars
base_MAE <- NULL

# Baseline Model 
for (i in 1:10){
  pred <- as.vector(matrix(data = unlist(base[i]), nrow = 1, ncol = length(rf.results[["predAct"]][[1]][[i]][["actual"]])))
  #  base_R2[i] <- R2(pred, nn.results[["predAct"]][[1]][[i]][["actual"]])
  base_MAE[i] <- MAE(pred, rf.results[["predAct"]][[1]][[i]][["actual"]])
  
}


cafpa_labels = c("C_a1", "C_a2", "C_a3", "C_a4", "C_u1", "C_u2", "C_b", "C_n", "C_c", "C_e")


# BAR PLOTS ON TOP # -----------------------

num = as.character(0:9)

## barplot
df.MAE <- data.frame(model=rep(c("Null Model","Lasso", "Elastic Net", "Random Forest"), each=10),
                     cafpa= rep(cafpa_labels, times = 4),
                     cafpa_num = rep(num, times = 4),
                     MAE = c(base_MAE,lasso.df$mean_MAE_test, elastic.df$mean_MAE_test, rf.df$mean_MAE_test))

df.MAE <- df.MAE[order(df.MAE$MAE, decreasing = TRUE),]


df.rsq <- data.frame(model=rep(c("Lasso", "Elastic Net", "Random Forest"), each=10),
                     cafpa= rep(cafpa_labels, times = 3),
                     cafpa_num = rep(num, times = 3),
                     rsq = c(lasso.df$mean_rsq_test, elastic.df$mean_rsq_test, rf.df$mean_rsq_test))

df.rsq <- df.rsq[order(df.rsq$rsq, decreasing = FALSE),]


# parameters
axis_size = 28
axis_face = "bold"

colblind = c("darkblue", "#56B4E9", "grey","darkgreen")


png("bar_all_MAE.png", width = 882, height = 442)
ggplot(data=df.MAE, aes(x=cafpa_num, y=MAE, fill=model)) +
  geom_bar(stat="identity", position = "identity") + 
  
  scale_fill_manual(values = colblind) +
  #scale_fill_manual(values=c("#006666", "lightblue", "grey", "darkgreen")) +
  theme_bw() + 
  scale_x_discrete(labels = cafpa_labels) + 
  
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.x = element_text(face="bold", size=axis_size),
                     axis.text.y = element_text(face="bold", size=axis_size), 
                     legend.position = "none",
                     #legend.title = element_blank(),
                     legend.text = element_blank())
                     #legend.text = element_text(face = "bold", size = 22))

dev.off()


# rsq swipped around best performance on top, less messy

png("bar_all_rsq.png", width = 882, height = 442)
ggplot(data=df.rsq, aes(x=cafpa_num, y=rsq, fill=model)) +
  geom_bar(stat="identity", position = "identity") + 
  geom_point( aes(shape = factor(df.rsq$model)), color = "black", size = 5)+ 
  scale_fill_manual(name = "Model",
                    labels=c("Elastic Net", "Lasso", "Random Forest"),
                    values=c("darkblue", "#56B4E9", "darkgreen"))+
 # scale_fill_manual(name = "Model",
 #                   labels=c("Elastic Net", "Lasso", "Random Forest"),
 #                   values=c("#006666", "lightblue", "darkgreen"))+
  
  scale_shape_manual(name = "Model",
                     labels=c("Elastic Net", "Lasso", "Random Forest"),
                     values = c(15, 16, 18))+ 
  theme_bw() + 
  scale_x_discrete(labels = cafpa_labels) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.x = element_text(face="bold", size=axis_size),
        axis.text.y = element_text(face="bold", size=axis_size), 
        legend.position = "none",
       #legend.title = element_blank(),
       legend.text = element_blank()) +
       #legend.text = element_text(face = "bold", size = 22))
  ylim(0,1)

dev.off()


## Error reduction across CAFPAs for all models ##--------------------

lasso <-lasso.df$base   - lasso.df$mean_MAE_test
enet <- elastic.df$base - elastic.df$mean_MAE_test 
rf <-   rf.df$base      - rf.df$mean_MAE_test

error_reduction <- data.frame(c(mean(lasso), mean(enet), mean(rf)))
names(error_reduction) <- "mean_reduction"
error_reduction$sd_reduction <- c(sd(lasso), sd(enet), sd(rf))
error_reduction$model_name <- c("Lasso", "Elastic Net", "Random Forest")
error_reduction$model <- c("1","2","3")


png(filename = "Error_reduction.png",width = 442, height = 442)
ggplot(data=error_reduction, aes(x=model, y=mean_reduction, fill=model)) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_errorbar(aes(ymin = mean_reduction-sd_reduction, ymax= mean_reduction+sd_reduction), width = 0.5, color = "black")+
  scale_fill_manual(values=c("darkblue", "#56B4E9", "darkgreen"))+
  scale_x_discrete(labels = error_reduction$model_name) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold", size=axis_size), 
        legend.position = "none",
        legend.text = element_blank()) +
  #legend.text = element_text(face = "bold", size = 22))
  ylab("Mean Error Reduction") #+
  #ggtitle("Mean MAE reduction across CAFPAs")
  ylim(0,0.1)

dev.off()


