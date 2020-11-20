##################################################
#
#  Clustering
#
#
##################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# libraries
library(mice)
library(factoextra)
library(mclust)
library(gplots)
library(ggpubr)
library(tidyverse)
library(viridis)
library(gridExtra)

set.seed(8)

# set up folders and directories
dir.create(file.path("EV\\plots\\expert_cafpa\\"), recursive = TRUE)  
dir.create(file.path("EV\\data\\expert_cafpa_imp\\"), recursive = TRUE) 

path_plot = "EV\\plots\\expert_cafpa\\"
path_save = "EV\\data\\expert_cafpa_imp\\"

# load in expert data to compare 
CAFPA_labeled <- load("MB\\data\\preprocess\\preprocessed_data_final.Rdata")


# either do MICE or load MICE
mice.do = FALSE

if (mice.do == TRUE){

# Impute missing data in CAFPAs 

# initialize predictor matrix 
pred.Matrix <- 1 - diag( 1, ncol(output.var) )
rownames( pred.Matrix ) <- colnames( pred.Matrix) <- colnames(output.var )

#************************************************************
# MICE imputation with method randomForest
mi.res <- mice( output.var , predictorMatrix = pred.Matrix , 
                method = c(rep( "rf" , ncol(output.var ) ))  ,
                m = 20  , maxit= 20   
)

# plot variable convergence (mean and SD of imputations across iterations)
pdf(paste0(path_plot, "expert_cafpa.imputation_mice_convergence_rf.pdf") , onefile=T )
plot(mi.res,ask=F) 
dev.off()

#.....................
## save datasets
for (i in 1:20){
  data.imputed <- mice::complete(mi.res, action = i)
  save(data.imputed, file = (paste0(path_save, paste0("expert_mice_imputation_rf_", i, ".Rdata"))))
}

# save mi.res
save(mi.res, file = (paste0(path_save, paste0("mi.res_expert_rf.Rdata"))))

model <- as.matrix(mice::complete(mi.res, action =  round(runif(1, min=1, max=20))))

#------------------
} else {
 
   # select random imputation
  imp <- round(runif(1, min=1, max=20))
  
  load(paste0(path_save, "expert_mice_imputation_rf_", imp, ".Rdata"))
  model <- as.matrix(data.imputed)}

# make model for clustering

colnames(model) = c("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB","CN", "CC","CE")

  # CLUSTERING - Model based clustering ---------------------------------------------
  # first check optimal clusters
  optimal_mc <- Mclust(model, G = 2:10)

  # Plot BIC values 
  BIC_data <- as.data.frame(optimal_mc$BIC[1:9,1:14])
  BIC_data <- round(BIC_data,2)
  BIC_data <-  BIC_data %>% gather(Identifier, BIC, EII:VVV)
  BIC_data$size = rep(2:10, times = 14)
  
  axis_face = "bold"
  axis_size = 28
  
  best_BIC <- BIC_data[BIC_data$BIC == max(BIC_data$BIC, na.rm = TRUE),]
 
  png(paste0(path_plot, imp, "_BIC_values.png"), width = 850, height = 750 )
  print(ggplot(data = BIC_data, aes(x = size, y = BIC, color = Identifier))+
    geom_point(size = 3) +
    geom_line(size = 1) + 
    geom_vline(xintercept = best_BIC$size, linetype = "dotted", size = 1)+
    theme_bw() +
    theme( panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.line = element_line(colour = "black"),
           axis.text.x = element_text(face = axis_face, size = axis_size), 
           axis.text.y = element_text(face = axis_face, size = axis_size),
           legend.text = element_text(face = axis_face, size = axis_size-5), 
           legend.title =element_text(face = axis_face, size = axis_size)) + 
    scale_color_viridis(discrete = TRUE) +
    scale_x_continuous(breaks = c(2:10))+
    xlab("") + 
    ylab(""))
  dev.off()
  
  
  # check cluster with 2:10 clusters 
  axis_size = 42
  axis_face = "bold"
  
  one    <- "#4B3232"
  two    <- "#7D4B32"
  three  <- "#966432"
  four   <- "#AF7D32"
  five   <- "#DD8918"
   
  my_pal <- c(one,two, three, four, five)
  expert_pal <- c(four, two, three, five, one)
  
  # to check differenct cluster CAFPA plots 
  for (c in 2:5){
    
    mc <- Mclust(model, G = c)
    
    summary(mc)
    
    scaled_mc <- mc
    scaled_mc$data <- scale(mc$data, center = TRUE, scale = FALSE)

    # Classification: plot showing the clustering
    png(paste0(path_plot, "expert_", as.character(c), "_clustering.png"))
    print(fviz_mclust(scaled_mc, "classification", geom = "point", stand = FALSE,
                      pointsize = 1.5, palette = expert_pal, 
                      ggtheme = theme_bw(), main = "") + 
                              theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                               axis.text.x = element_text(face = axis_face, size = axis_size), 
                               axis.text.y = element_text(face = axis_face, size = axis_size),
                               legend.position = "none")+
                               #legend.text = element_text(face = axis_face, size = axis_size)) +

            xlim(-1,1.6) +
            ylim(-0.6, 0.6)
          )
   
    dev.off()
    
    if(c == 2){mc_2 <- mc}
    if(c == 3){mc_3 <- mc}
    if(c == 4){mc_4 <- mc}
    if(c == 5){mc_5 <- mc}
    if(c == 6){mc_6 <- mc}
    if(c == 7){mc_7 <- mc}
    if(c == 8){mc_8 <- mc}
    if(c == 9){mc_9 <- mc}
    if(c == 10){mc_10 <- mc}

  
  } # cluster loop
  
  
  expert_clusts <- list(mc_2, mc_3, mc_4, mc_5, mc_6, mc_7, mc_8, mc_9, mc_10)#, mc_11, mc_12, mc_13, mc_14)
  
  save(expert_clusts, file = paste0(path_save, "expert_clusters_10.Rdata"))
  
  

