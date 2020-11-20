#########################################################################################
#
# PROPENSITY SCORE MATCHING 
#
#########################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()
graphics.off()

# libraries
library(MatchIt)
library(ggplot2)

# set up folders and directories
dir.create(file.path("MB\\data\\"), recursive = TRUE)  
dir.create(file.path("EV\\data\\"), recursive = TRUE)  
dir.create(file.path("PSM_R\\plots\\"), recursive = TRUE)  

path_labeled   <- "MB\\data\\"
path_unlabeled <-"EV\\data\\"
path_save      <- "PSM_R\\plots\\"


# set seed
set.seed(22)

# select random imputation
imp <- round(runif(1, min=1, max=20))

# load labeled data
load(paste0(path_labeled,"le_ri_combine_pta\\data\\data_le_ri_combined_pta_", imp ,".Rdata"))
labeled <- data
labeled <- cbind(Group = 1, labeled) 


# load unlabeled data 
load(paste0(path_unlabeled, "pta_EV\\data_pta_", imp, ".Rdata"))
unlabeled <- data
unlabeled <- cbind(Group = 0, unlabeled) 

# merge data 
dat <- rbind(labeled, unlabeled )
head(dat)


##########################################################
# Matching based on demographic variables

# formula of variables group ~
x <- formula(dat[c(1,13,14,6)])

# index, not included in formula, but to ensure later matching
dat$ID <- 1:595

# compare nearest and optimal method
m.out_nearest = matchit( x, data = dat, method = "nearest", discard = "none",
)
m.out_optimal = matchit( x, data = dat, method = "optimal", discard = "none",
)


nearest<- summary(m.out_nearest)
optimal <- summary(m.out_optimal)

nearest_sumDiff <- sum(abs(nearest$sum.matched$`Mean Diff`))
optimal_sumDiff <- sum(abs(optimal$sum.matched$`Mean Diff`))

# choose nearest method as overall smaller matched difference 

m.out <- m.out_nearest

# visualize matching
png(paste0(path_plot, "Jitter_nearest_demMatched.png"))
plot(m.out, type = "jitter")
dev.off()

png(paste0(path_plot,"Hist_nearest_demMatched.png"))
plot(m.out, type = "hist")
dev.off()


matches_ID <- as.data.frame(m.out$match.matrix)
matches_ID$labeled <- 1:240
names(matches_ID) <- c("unlabeled", "labeled")


data_labeled <- match.data(m.out, group ="treat")
data_unlabeled <- match.data(m.out, group = "control")


save(m.out, matches_ID, data_labeled, data_unlabeled, file = paste0(path_save, "psm_data_nearest_demMatched.Rdata") )

