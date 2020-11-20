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
library(factoextra)
library(gplots)
library(mclust)

# set up folders and directories
dir.create(file.path("EV\\data\\clustering\\"), recursive = TRUE)  

path_data <- "PSM_R\\data\\"
path_save <- "EV\\data\\clustering\\"
path_plot <- "EV\\plots\\clustering\\"

set.seed(22)

# load CAFPA data for different models 

# matched unlabeled demographics
load(paste0(path_data, "CAFPAs_lasso_unlabeled_demMatched.Rdata"))
lasso_matched_unlabeled_dem <- as.data.frame(unlabeled_lasso_cafpas)

load(paste0(path_data, "CAFPAs_elastic_unlabeled_demMatched.Rdata"))
elastic_matched_unlabeled_dem <- as.data.frame(unlabeled_elastic_cafpas)

load(paste0(path_data, "CAFPAs_rf_unlabeled_demMatched.Rdata"))
rf_matched_unlabeled_dem <- as.data.frame(unlabeled_rf_cafpas)

# store index
ID <- rf_matched_unlabeled_dem$V11

# delete index from models
lasso_matched_unlabeled_dem[,11] <- NULL
elastic_matched_unlabeled_dem[,11] <- NULL
rf_matched_unlabeled_dem[,11] <- NULL

cafpa_names <- c("CA1", "CA2", "CA3", "CA4", "CU1", "CU2", "CB","CN", "CC","CE")

colnames(lasso_matched_unlabeled_dem) = cafpa_names 
colnames(elastic_matched_unlabeled_dem) = cafpa_names 
colnames(rf_matched_unlabeled_dem) = cafpa_names 


model_name <- c("Lasso matched unlabeled dem", "Elastic Net matched unlabeled dem", "Random Forest matched unlabeled dem")

# loop through models 
for (i in 1:3){   
  
  if        (i == 1){model <- lasso_matched_unlabeled_dem
  } else if (i == 2){model <- elastic_matched_unlabeled_dem
  } else if (i == 3){model <- rf_matched_unlabeled_dem
  }


# CLUSTERING - Model based clustering ---------------------------------------------

# color palette of for clusters of cafpas 
one    <- "#4B3232"
two    <- "#7D4B32"
three  <- "#966432"
four   <- "#AF7D32"
five   <- "#DD8918"

my_pal <- c(one,two, three, four, five)

# manually defined the palette according to the given cluster pattern for the models (for 5 clusters with VII)
pal_lasso <-colorRampPalette(c(one, three, four, two, five))(n = 5)
pal_elastic <-colorRampPalette(c(one, three, two, four, five))(n = 5)
pal_rf <-colorRampPalette(c(one, two, five, four, three))(n = 5)

# check cluster with 5 clusters (based on Expert Clustering) <- Expert cluster was defined as 5 
for (c in 5){ # can be checked with different number of clusters as well <- then color palette has to be ajusted accordingly

  # VVI model was determined by cluster number from expert labeled CAFPA dataset
  mc <- Mclust(model, G = c, modelNames = "VVI")

  summary(mc)

  # center data around mean for clustering  
  scaled_mc <- mc
  scaled_mc$data <- scale(mc$data, center = TRUE, scale = FALSE)
 
  axis_size = 42
  axis_face = "bold"
  
  if (i == 1){my_pal = pal_lasso}
  if (i == 2){my_pal = pal_elastic}
  if (i == 3){my_pal = pal_rf}
  
  path_plot <- "C:\\Users\\Samira\\Desktop\\CAFPA_pub\\analysis\\EV\\plots\\pub_cluster\\"
  
  # Classification: plot showing the clustering
  png(paste0(path_plot, model_name[i], "_", as.character(c), "_legend_clustering.png"))
  print(fviz_mclust(scaled_mc, "classification", geom = "point", stand = FALSE,
                    pointsize = 1.5, palette = my_pal, 
                    ggtheme = theme_bw(), main = "") + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          axis.text.x = element_text(face = axis_face, size = axis_size), 
          axis.text.y = element_text(face = axis_face, size = axis_size),
          legend.text = element_text(face = axis_face, size = axis_size))+
          #legend.position = "none")+
          xlim(-1,1.6) +
          ylim(-0.6, 0.6)
  )
  dev.off()
  
  # in case of larger cluster numbers (palette needs to be adjusted)
   if(c == 2){mc_2 <- mc}
   if(c == 3){mc_3 <- mc}
   if(c == 4){mc_4 <- mc}
   if(c == 5){mc_5 <- mc}
   if(c == 6){mc_6 <- mc}

} # cluster loop

# save clusters

if (i == 1){lasso_matched_unlabeled_clusts_dem   <- mc_5} #list(mc_2, mc_3, mc_4, mc_5, mc_6)}
if (i == 2){elastic_matched_unlabeled_clusts_dem <- mc_5} #list(mc_2, mc_3, mc_4, mc_5, mc_6)}
if (i == 3){rf_matched_unlabeled_clusts_dem      <- mc_5} #list(mc_2, mc_3, mc_4, mc_5, mc_6)}

} # model loop 
save(lasso_matched_unlabeled_clusts_dem, file = paste0(path_save, "Lasso_matched_unlabeled_clusters_dem.Rdata"))
save(elastic_matched_unlabeled_clusts_dem, file = paste0(path_save, "Elastic_matched_unlabeled_clusters_dem.Rdata"))
save(rf_matched_unlabeled_clusts_dem, file = paste0(path_save, "Rf_matched_unlabeled_clusters_dem.Rdata"))

#--------------- Agreement across clusters between models ---------------------------------

lasso_class <- lasso_matched_unlabeled_clusts_dem$classification
lasso_class[lasso_class== 2]<- 6
lasso_class[lasso_class== 3]<- 7
lasso_class[lasso_class== 4]<- 8
lasso_class[lasso_class== 6]<- 3
lasso_class[lasso_class== 7]<- 4
lasso_class[lasso_class== 8]<- 2

elastic_class <- elastic_matched_unlabeled_clusts_dem$classification
elastic_class[elastic_class==3] <- 6
elastic_class[elastic_class==2] <- 3
elastic_class[elastic_class==6] <- 2

rf_class <- rf_matched_unlabeled_clusts_dem$classification
rf_class[rf_class==3] <- 6
rf_class[rf_class==5] <- 3
rf_class[rf_class==6] <- 5

df.class <- data.frame(lasso = lasso_class, elastic =  elastic_class, rf = rf_class, index =c(1:240))

lr_en <- sum(lasso_class == elastic_class)/240
lr_rf <- sum(lasso_class == rf_class)/240
en_rf <- sum(elastic_class == rf_class)/240

