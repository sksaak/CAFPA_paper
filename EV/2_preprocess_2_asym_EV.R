#######################################################################
#     Select more strongly affected ear for further analysis          #
#     with a PTA asymmetry score                                      #
#                                                                     #
#######################################################################


# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()


# set seed
set.seed(22)

# set up folders and directories
dir.create(file.path("EV\\data\\pta_EV\\"), recursive = TRUE)  
path_load <- "EV\\data\\imp_data_new\\"
path_save <- "EV\\data\\pta_EV\\"


# number of imputed datasets
imp.num = 20



for (m in 1:imp.num){
  
  load(paste0(path_load,"MEAS_mice_imputation_rf_", m ,".Rdata"))
  
  # delete database from data
  data.imputed <- data.imputed[241:595,]

  data <- NULL

### PRE-DEFINE VARS ####    
  
  m.ag_ac.125      <- NULL
  m.ag_ac.250      <- NULL
  m.ag_ac.500      <- NULL
  m.ag_ac.750      <- NULL
  m.ag_ac.1000     <- NULL
  m.ag_ac.1500     <- NULL
  m.ag_ac.2000     <- NULL
  m.ag_ac.3000     <- NULL
  m.ag_ac.4000     <- NULL
  m.ag_ac.6000     <- NULL
  m.ag_ac.8000     <- NULL

  m.ag_bc.500      <- NULL
  m.ag_bc.750      <- NULL
  m.ag_bc.1000     <- NULL
  m.ag_bc.1500     <- NULL
  m.ag_bc.2000     <- NULL
  m.ag_bc.3000     <- NULL
  m.ag_bc.4000     <- NULL

  m.acalos_1_5.Lcut      <- NULL
  m.acalos_1_5.mlow      <- NULL
  m.acalos_1_5.mhigh     <- NULL
  m.acalos_1_5.L2.5      <- NULL
  m.acalos_1_5.L25       <- NULL
  m.acalos_1_5.L50       <- NULL
  
  m.acalos_4.Lcut      <- NULL
  m.acalos_4.mlow      <- NULL
  m.acalos_4.mhigh     <- NULL
  m.acalos_4.L2.5      <- NULL
  m.acalos_4.L25       <- NULL
  m.acalos_4.L50       <- NULL
  
  # Asymmetry  score based on pta 
  m.pta_asym      <- NULL
  
  
##### Loop through rows to select stronger affected ear and compute asymmetry score #####
  for (r in 1:nrow(data.imputed)){
  
  m.ag_ac.125[r]      <- max(data.imputed$m.ag_ac_ri.125[r], data.imputed$m.ag_ac_le.125[r])
  m.ag_ac.250[r]      <- max(data.imputed$m.ag_ac_ri.250[r], data.imputed$m.ag_ac_le.250[r])
  m.ag_ac.500[r]      <- max(data.imputed$m.ag_ac_ri.500[r], data.imputed$m.ag_ac_le.500[r])
  m.ag_ac.750[r]      <- max(data.imputed$m.ag_ac_ri.750[r], data.imputed$m.ag_ac_le.750[r])
  m.ag_ac.1000[r]     <- max(data.imputed$m.ag_ac_ri.1000[r], data.imputed$m.ag_ac_le.1000[r])
  m.ag_ac.1500[r]     <- max(data.imputed$m.ag_ac_ri.1500[r], data.imputed$m.ag_ac_le.1500[r])
  m.ag_ac.2000[r]     <- max(data.imputed$m.ag_ac_ri.2000[r], data.imputed$m.ag_ac_le.2000[r])
  m.ag_ac.3000[r]     <- max(data.imputed$m.ag_ac_ri.3000[r], data.imputed$m.ag_ac_le.3000[r])
  m.ag_ac.4000[r]     <- max(data.imputed$m.ag_ac_ri.4000[r], data.imputed$m.ag_ac_le.4000[r])
  m.ag_ac.6000[r]     <- max(data.imputed$m.ag_ac_ri.6000[r], data.imputed$m.ag_ac_le.6000[r])
  m.ag_ac.8000[r]     <- max(data.imputed$m.ag_ac_ri.8000[r], data.imputed$m.ag_ac_le.8000[r])
  
  m.ag_bc.500[r]      <- max(data.imputed$m.ag_bc_ri.500[r], data.imputed$m.ag_bc_le.500[r])
  m.ag_bc.750[r]      <- max(data.imputed$m.ag_bc_ri.750[r], data.imputed$m.ag_bc_le.750[r])
  m.ag_bc.1000[r]     <- max(data.imputed$m.ag_bc_ri.1000[r], data.imputed$m.ag_bc_le.1000[r])
  m.ag_bc.1500[r]     <- max(data.imputed$m.ag_bc_ri.1500[r], data.imputed$m.ag_bc_le.1500[r])
  m.ag_bc.2000[r]     <- max(data.imputed$m.ag_bc_ri.2000[r], data.imputed$m.ag_bc_le.2000[r])
  m.ag_bc.3000[r]     <- max(data.imputed$m.ag_bc_ri.3000[r], data.imputed$m.ag_bc_le.3000[r])
  m.ag_bc.4000[r]     <- max(data.imputed$m.ag_bc_ri.4000[r], data.imputed$m.ag_bc_le.4000[r])
  
  m.acalos_1_5.Lcut[r]      <- max(data.imputed$m.acalos_1_5_ri.Lcut[r], data.imputed$m.acalos_1_5_le.Lcut[r])
  m.acalos_1_5.mlow[r]      <- max(data.imputed$m.acalos_1_5_ri.mlow[r], data.imputed$acalos_1_5_le.mlow[r])
  m.acalos_1_5.mhigh[r]     <- max(data.imputed$m.acalos_1_5_ri.mhigh[r], data.imputed$m.acalos_1_5_le.mhigh[r])
  m.acalos_1_5.L2.5[r]      <- max(data.imputed$m.acalos_1_5_ri.L2.5[r], data.imputed$m.acalos_1_5_le.L2.5[r])
  m.acalos_1_5.L25[r]       <- max(data.imputed$m.acalos_1_5_ri.L25[r], data.imputed$m.acalos_1_5_le.L25[r])
  m.acalos_1_5.L50[r]       <- max(data.imputed$m.acalos_1_5_ri.L50[r], data.imputed$m.acalos_1_5_le.L50[r])

  m.acalos_4.Lcut[r]        <- max(data.imputed$m.acalos_4_ri.Lcut[r], data.imputed$m.acalos_4_le.Lcut[r])
  m.acalos_4.mlow[r]        <- max(data.imputed$m.acalos_4_ri.mlow[r], data.imputed$m.acalos_4_le.mlow[r])
  m.acalos_4.mhigh[r]       <- max(data.imputed$m.acalos_4_ri.mhigh[r], data.imputed$m.acalos_4_le.mhigh[r])
  m.acalos_4.L2.5[r]        <- max(data.imputed$m.acalos_4_ri.L2.5[r], data.imputed$m.acalos_4_le.L2.5[r])
  m.acalos_4.L25[r]         <- max(data.imputed$m.acalos_4_ri.L25[r], data.imputed$m.acalos_4_le.L25[r])
  m.acalos_4.L50[r]         <- max(data.imputed$m.acalos_4_ri.L50[r], data.imputed$m.acalos_4_le.L50[r])
  
  m.pta_asym[r] <- abs((data.imputed$m.ag_ac_ri.500[r] +  
                    data.imputed$m.ag_ac_ri.1000[r]+
                    data.imputed$m.ag_ac_ri.2000[r]+
                    data.imputed$m.ag_ac_ri.4000[r])/4 - 
                
                    (data.imputed$m.ag_ac_le.500[r] +  
                     data.imputed$m.ag_ac_le.1000[r]+
                     data.imputed$m.ag_ac_le.2000[r]+
                     data.imputed$m.ag_ac_le.4000[r])/4)
 
} # row loop
  
  
##### add variables to data.frame #####
  
  data<- data.imputed[,61:ncol(data.imputed)] 
  
  data['m.ag_ac.125']      <- m.ag_ac.125

  data['m.ag_ac.250']      <- m.ag_ac.250

  data['m.ag_ac.500']       <- m.ag_ac.500
 
  data['m.ag_ac.750']       <- m.ag_ac.750
 
  data['m.ag_ac.1000']       <- m.ag_ac.1000
 
  data['m.ag_ac.1500']       <- m.ag_ac.1500
 
  data['m.ag_ac.2000']       <- m.ag_ac.2000
 
  data['m.ag_ac.3000']       <- m.ag_ac.3000
 
  data['m.ag_ac.4000']       <- m.ag_ac.4000

  data['m.ag_ac.6000']       <- m.ag_ac.6000

  data['m.ag_ac.8000']       <- m.ag_ac.8000
 
  data['m.ag_bc.500']       <- m.ag_bc.500
 
  data['m.ag_bc.750']       <- m.ag_bc.750
  
  data['m.ag_bc.1000']       <- m.ag_bc.1000
 
  data['m.ag_bc.1500']       <- m.ag_bc.1500
 
  data['m.ag_bc.2000']       <- m.ag_bc.2000
 
  data['m.ag_bc.3000']       <- m.ag_bc.3000
  
  data['m.ag_bc.4000']       <- m.ag_bc.4000
  
  data['m.acalos_1_5.Lcut']      <- m.acalos_1_5.Lcut
 
  data['m.acalos_1_5.mlow']      <- m.acalos_1_5.mlow
 
  data['m.acalos_1_5.mhigh']      <- m.acalos_1_5.mhigh
 
  data['m.acalos_1_5.L2.5']      <- m.acalos_1_5.L2.5
 
  data['m.acalos_1_5.L25']      <- m.acalos_1_5.L25
 
  data['m.acalos_1_5.L50']      <- m.acalos_1_5.L50

  data['m.acalos_4.Lcut']      <- m.acalos_4.Lcut
 
  data['m.acalos_4.mlow']      <- m.acalos_4.mlow
 
  data['m.acalos_4.mhigh']      <- m.acalos_4.mhigh
 
  data['m.acalos_4.L2.5']      <- m.acalos_4.L2.5
 
  data['m.acalos_4.L25']      <- m.acalos_4.L25
 
  data['m.acalos_4.L50']      <- m.acalos_4.L50
  
  data['m.pta_asym']    <- m.pta_asym

  
  
  
  # save
  save(data, file = (paste0(path_save, paste0("data_pta_", m, ".Rdata"))))
  
} # imp loop