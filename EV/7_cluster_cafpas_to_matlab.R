##################################################
#
#  CLUSTER CAFPAS to Matlab
#
#
##################################################


# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# libraries
library(R.matlab)


# set paths 
MAIN <- "C:\\Users\\Samira\\Desktop\\Neurocognitive Psychology\\CAFPA_pub\\analysis_complete\\"
path_data = paste0(MAIN, "EV\\data\\clustering\\")
path_save = paste0(MAIN, "EV\\data\\clustering\\") 

set.seed(22)


# load Models -------------------------------------------------------------------------------------------
load(paste0(path_data, "Lasso_matched_unlabeled_clusters_dem.Rdata"))
lasso_clusts <- lasso_matched_unlabeled_clusts_dem

load(paste0(path_data, "Elastic_matched_unlabeled_clusters_dem.Rdata"))
elastic_clusts <- elastic_matched_unlabeled_clusts_dem

load(paste0(path_data, "Rf_matched_unlabeled_clusters_dem.Rdata"))
rf_clusts <- rf_matched_unlabeled_clusts_dem

for (c in 1:5){


if (c == 1){
 
  summary(lasso_clusts[[c]])
  summary(elastic_clusts[[c]])
  summary(rf_clusts[[c]])
  
  #lasso
  index_1 <- unlist(lasso_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(lasso_clusts[[c]]["classification"]) == 2
  lasso_1 <- lasso_clusts[[c]][["data"]][index_1,]
  lasso_2 <- lasso_clusts[[c]][["data"]][index_2,]
  # elastic net
  index_1 <- unlist(elastic_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(elastic_clusts[[c]]["classification"]) == 2
  elastic_1 <- elastic_clusts[[c]][["data"]][index_1,]
  elastic_2 <- elastic_clusts[[c]][["data"]][index_2,]
  # random forest
  index_1 <- unlist(rf_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(rf_clusts[[c]]["classification"]) == 2
  rf_1 <- rf_clusts[[c]][["data"]][index_1,]
  rf_2 <- rf_clusts[[c]][["data"]][index_2,]
  
  # save for matlab
  writeMat(paste0(path_save, paste0("CAFPA_Lasso_dem_", as.character(c+1), ".mat")), lasso_1 = lasso_1, lasso_2 = lasso_2)
  writeMat(paste0(path_save, paste0("CAFPA_elastic_dem_", as.character(c+1), ".mat")), elastic_1 = elastic_1, elastic_2 = elastic_2)
  writeMat(paste0(path_save, paste0("CAFPA_rf_dem_", as.character(c+1), ".mat")), rf_1 = rf_1, rf_2 = rf_2)
  
} else if (c == 2){
  
  #lasso
  index_1 <- unlist(lasso_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(lasso_clusts[[c]]["classification"]) == 2
  index_3 <- unlist(lasso_clusts[[c]]["classification"]) == 3
  lasso_1 <- lasso_clusts[[c]][["data"]][index_1,]
  lasso_2 <- lasso_clusts[[c]][["data"]][index_2,]
  lasso_3 <- lasso_clusts[[c]][["data"]][index_3,]
  # elastic net
  index_1 <- unlist(elastic_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(elastic_clusts[[c]]["classification"]) == 2
  index_3 <- unlist(elastic_clusts[[c]]["classification"]) == 3
  elastic_1 <- elastic_clusts[[c]][["data"]][index_1,]
  elastic_2 <- elastic_clusts[[c]][["data"]][index_2,]
  elastic_3 <- elastic_clusts[[c]][["data"]][index_3,]
  
  # random forest
  index_1 <- unlist(rf_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(rf_clusts[[c]]["classification"]) == 2
  index_3 <- unlist(rf_clusts[[c]]["classification"]) == 3
  rf_1 <- rf_clusts[[c]][["data"]][index_1,]
  rf_2 <- rf_clusts[[c]][["data"]][index_2,]
  rf_3 <- rf_clusts[[c]][["data"]][index_3,]
  
  # save for matlab
  writeMat(paste0(path_save, paste0("CAFPA_Lasso_dem_", as.character(c+1), ".mat")), lasso_1 = lasso_1, lasso_2 = lasso_2, lasso_3 = lasso_3)
  writeMat(paste0(path_save, paste0("CAFPA_elastic_dem_", as.character(c+1), ".mat")), elastic_1 = elastic_1, elastic_2 = elastic_2, elastic_3 = elastic_3)
  writeMat(paste0(path_save, paste0("CAFPA_rf_dem_", as.character(c+1), ".mat")), rf_1 = rf_1, rf_2 = rf_2, rf_3 = rf_3)
  
} else if (c ==3){
  # lasso
  index_1 <- unlist(lasso_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(lasso_clusts[[c]]["classification"]) == 2
  index_3 <- unlist(lasso_clusts[[c]]["classification"]) == 3
  index_4 <- unlist(lasso_clusts[[c]]["classification"]) == 4
  lasso_1 <- lasso_clusts[[c]][["data"]][index_1,]
  lasso_2 <- lasso_clusts[[c]][["data"]][index_2,]
  lasso_3 <- lasso_clusts[[c]][["data"]][index_3,]
  lasso_4 <- lasso_clusts[[c]][["data"]][index_4,]
  # elastic
  index_1 <- unlist(elastic_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(elastic_clusts[[c]]["classification"]) == 2
  index_3 <- unlist(elastic_clusts[[c]]["classification"]) == 3
  index_4 <- unlist(elastic_clusts[[c]]["classification"]) == 4
  elastic_1 <- elastic_clusts[[c]][["data"]][index_1,]
  elastic_2 <- elastic_clusts[[c]][["data"]][index_2,]
  elastic_3 <- elastic_clusts[[c]][["data"]][index_3,]
  elastic_4 <- elastic_clusts[[c]][["data"]][index_4,]
  # random forest 
  index_1 <- unlist(rf_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(rf_clusts[[c]]["classification"]) == 2
  index_3 <- unlist(rf_clusts[[c]]["classification"]) == 3
  index_4 <- unlist(rf_clusts[[c]]["classification"]) == 4
  rf_1 <- rf_clusts[[c]][["data"]][index_1,]
  rf_2 <- rf_clusts[[c]][["data"]][index_2,]
  rf_3 <- rf_clusts[[c]][["data"]][index_3,]
  rf_4 <- rf_clusts[[c]][["data"]][index_4,]
  
  # save for matlab
  writeMat(paste0(path_save, paste0("CAFPA_Lasso_dem_", as.character(c+1), ".mat")), lasso_1 = lasso_1, lasso_2 = lasso_2, lasso_3 = lasso_3, lasso_4 =lasso_4)
  writeMat(paste0(path_save, paste0("CAFPA_elastic_dem_", as.character(c+1), ".mat")), elastic_1 = elastic_1, elastic_2 = elastic_2, elastic_3 = elastic_3, elastic_4 = elastic_4)
  writeMat(paste0(path_save, paste0("CAFPA_rf_dem_", as.character(c+1), ".mat")), rf_1 = rf_1, rf_2 = rf_2, rf_3 = rf_3, rf_4 = rf_4)
  
} else if (c == 4){
  # lasso
  index_1 <- unlist(lasso_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(lasso_clusts[[c]]["classification"]) == 2
  index_3 <- unlist(lasso_clusts[[c]]["classification"]) == 3
  index_4 <- unlist(lasso_clusts[[c]]["classification"]) == 4
  index_5 <- unlist(lasso_clusts[[c]]["classification"]) == 5
  lasso_1 <- lasso_clusts[[c]][["data"]][index_1,]
  lasso_2 <- lasso_clusts[[c]][["data"]][index_2,]
  lasso_3 <- lasso_clusts[[c]][["data"]][index_3,]
  lasso_4 <- lasso_clusts[[c]][["data"]][index_4,]
  lasso_5 <- lasso_clusts[[c]][["data"]][index_5,]
  # elastic
  index_1 <- unlist(elastic_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(elastic_clusts[[c]]["classification"]) == 2
  index_3 <- unlist(elastic_clusts[[c]]["classification"]) == 3
  index_4 <- unlist(elastic_clusts[[c]]["classification"]) == 4
  index_5 <- unlist(elastic_clusts[[c]]["classification"]) == 5
  elastic_1 <- elastic_clusts[[c]][["data"]][index_1,]
  elastic_2 <- elastic_clusts[[c]][["data"]][index_2,]
  elastic_3 <- elastic_clusts[[c]][["data"]][index_3,]
  elastic_4 <- elastic_clusts[[c]][["data"]][index_4,]
  elastic_5 <- elastic_clusts[[c]][["data"]][index_5,]
  # random forest
  index_1 <- unlist(rf_clusts[[c]]["classification"]) == 1
  index_2 <- unlist(rf_clusts[[c]]["classification"]) == 2
  index_3 <- unlist(rf_clusts[[c]]["classification"]) == 3
  index_4 <- unlist(rf_clusts[[c]]["classification"]) == 4
  index_5 <- unlist(rf_clusts[[c]]["classification"]) == 5
  rf_1 <- rf_clusts[[c]][["data"]][index_1,]
  rf_2 <- rf_clusts[[c]][["data"]][index_2,]
  rf_3 <- rf_clusts[[c]][["data"]][index_3,]
  rf_4 <- rf_clusts[[c]][["data"]][index_4,]
  rf_5 <- rf_clusts[[c]][["data"]][index_5,]
  
  # save for matlab
  writeMat(paste0(path_save, paste0("CAFPA_Lasso_dem_", as.character(c+1), ".mat")), lasso_1 = lasso_1, lasso_2 = lasso_2, lasso_3 = lasso_3, lasso_4 =lasso_4, lasso_5 = lasso_5)
  writeMat(paste0(path_save, paste0("CAFPA_elastic_dem_", as.character(c+1), ".mat")), elastic_1 = elastic_1, elastic_2 = elastic_2, elastic_3 = elastic_3, elastic_4 = elastic_4, elastic_5 = elastic_5)
  writeMat(paste0(path_save, paste0("CAFPA_rf_dem_", as.character(c+1), ".mat")), rf_1 = rf_1, rf_2 = rf_2, rf_3 = rf_3, rf_4 = rf_4, rf_5 = rf_5)
  
} else if (c == 5){ 
  
# lasso
index_1 <- unlist(lasso_clusts[[c]]["classification"]) == 1
index_2 <- unlist(lasso_clusts[[c]]["classification"]) == 2
index_3 <- unlist(lasso_clusts[[c]]["classification"]) == 3
index_4 <- unlist(lasso_clusts[[c]]["classification"]) == 4
index_5 <- unlist(lasso_clusts[[c]]["classification"]) == 5
index_6 <- unlist(lasso_clusts[[c]]["classification"]) == 6
lasso_1 <- lasso_clusts[[c]][["data"]][index_1,]
lasso_2 <- lasso_clusts[[c]][["data"]][index_2,]
lasso_3 <- lasso_clusts[[c]][["data"]][index_3,]
lasso_4 <- lasso_clusts[[c]][["data"]][index_4,]
lasso_5 <- lasso_clusts[[c]][["data"]][index_5,]
lasso_6 <- lasso_clusts[[c]][["data"]][index_6,]
# elastic
index_1 <- unlist(elastic_clusts[[c]]["classification"]) == 1
index_2 <- unlist(elastic_clusts[[c]]["classification"]) == 2
index_3 <- unlist(elastic_clusts[[c]]["classification"]) == 3
index_4 <- unlist(elastic_clusts[[c]]["classification"]) == 4
index_5 <- unlist(elastic_clusts[[c]]["classification"]) == 5
index_6 <- unlist(elastic_clusts[[c]]["classification"]) == 6
elastic_1 <- elastic_clusts[[c]][["data"]][index_1,]
elastic_2 <- elastic_clusts[[c]][["data"]][index_2,]
elastic_3 <- elastic_clusts[[c]][["data"]][index_3,]
elastic_4 <- elastic_clusts[[c]][["data"]][index_4,]
elastic_5 <- elastic_clusts[[c]][["data"]][index_5,]
elastic_6 <- elastic_clusts[[c]][["data"]][index_6,]
# random forest
index_1 <- unlist(rf_clusts[[c]]["classification"]) == 1
index_2 <- unlist(rf_clusts[[c]]["classification"]) == 2
index_3 <- unlist(rf_clusts[[c]]["classification"]) == 3
index_4 <- unlist(rf_clusts[[c]]["classification"]) == 4
index_5 <- unlist(rf_clusts[[c]]["classification"]) == 5
index_6 <- unlist(rf_clusts[[c]]["classification"]) == 6
rf_1 <- rf_clusts[[c]][["data"]][index_1,]
rf_2 <- rf_clusts[[c]][["data"]][index_2,]
rf_3 <- rf_clusts[[c]][["data"]][index_3,]
rf_4 <- rf_clusts[[c]][["data"]][index_4,]
rf_5 <- rf_clusts[[c]][["data"]][index_5,]
rf_6 <- rf_clusts[[c]][["data"]][index_6,]

# save for matlab
writeMat(paste0(path_save, paste0("CAFPA_Lasso_dem_", as.character(c+1), ".mat")), lasso_1 = lasso_1, lasso_2 = lasso_2, lasso_3 = lasso_3,
                                                 lasso_4 = lasso_4, lasso_5 = lasso_5, lasso_6 = lasso_6)
writeMat(paste0(path_save, paste0("CAFPA_elastic_dem_", as.character(c+1), ".mat")), elastic_1 = elastic_1, elastic_2 = elastic_2, elastic_3 = elastic_3,
         elastic_4 = elastic_4, elastic_5 = elastic_5, elastic_6 = elastic_6)
writeMat(paste0(path_save, paste0("CAFPA_rf_dem_", as.character(c+1), ".mat")), rf_1 = rf_1, rf_2 = rf_2, rf_3 = rf_3, rf_4 = rf_4, 
         rf_5 = rf_5, rf_6 = rf_6)
}
  
} # cluster loop


# load Expert -------------------------------------------------------------------------------------------
load(paste0(MAIN, "EV\\data\\expert_cafpa_imp\\expert_clusters.Rdata"))

# determine n clusters to save for matlab (2:10)
c = 8

summary(expert_clusts[[c]])

index_1 <- unlist(expert_clusts[[c]]["classification"]) == 1
index_2 <- unlist(expert_clusts[[c]]["classification"]) == 2
index_3 <- unlist(expert_clusts[[c]]["classification"]) == 3
index_4 <- unlist(expert_clusts[[c]]["classification"]) == 4
index_5 <- unlist(expert_clusts[[c]]["classification"]) == 5
index_6 <- unlist(expert_clusts[[c]]["classification"]) == 6
index_7 <- unlist(expert_clusts[[c]]["classification"]) == 8
index_8 <- unlist(expert_clusts[[c]]["classification"]) == 8
index_9 <- unlist(expert_clusts[[c]]["classification"]) == 9
index_10 <- unlist(expert_clusts[[c]]["classification"]) == 10

expert_1 <- expert_clusts[[c]][["data"]][index_1,]
expert_2 <- expert_clusts[[c]][["data"]][index_2,]
expert_3 <- expert_clusts[[c]][["data"]][index_3,]
expert_4 <- expert_clusts[[c]][["data"]][index_4,]
expert_5 <- expert_clusts[[c]][["data"]][index_5,]
expert_6 <- expert_clusts[[c]][["data"]][index_6,]
expert_7 <- expert_clusts[[c]][["data"]][index_7,]
expert_8 <- expert_clusts[[c]][["data"]][index_8,]
expert_9 <- expert_clusts[[c]][["data"]][index_9,]
expert_10 <- expert_clusts[[c]][["data"]][index_10,]


# save for matlab
writeMat(paste0(path_save, "CAFPA_expert_", as.character(c+1), ".mat"), expert_1 = expert_1, expert_2 = expert_2, expert_3 = expert_3,
         expert_4 = expert_4, expert_5 = expert_5, expert_6 = expert_6, expert_7 = expert_7, expert_8 = expert_8,
         expert_9 = expert_9, expert_10 = expert_10)


