################################################################################################
#        Baseline for the different models                  
#
#        - mean with lm()
#
################################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()
graphics.off()

# set up folders and directories
dir.create(file.path("MB\\"), recursive = TRUE) 
dir.create(file.path("MB\\data\\"), recursive = TRUE) 
path_data <- "MB\\"
path_save <- "MB\\data\\"

# set seed
set.seed(22)

# iterations of mice
imp.num = 20

# predefine vars
cafpa = c("c.c_a1", "c.c_a2", "c.c_a3", "c.c_a4", "c.c_u1", "c.c_u2", "c.c_b", "c.c_n", "c.c_c", "c.c_e")

# load output var
load("MB\\data\\preprocess\\preprocessed_data_final.Rdata")


# build null model 
ca1.df <- cbind(output.var$c.c_a1, input.var)
ca2.df <- cbind(output.var$c.c_a2, input.var)
ca3.df <- cbind(output.var$c.c_a3, input.var)
ca4.df <- cbind(output.var$c.c_a4, input.var)
cu1.df <- cbind(output.var$c.c_u1, input.var)
cu2.df <- cbind(output.var$c.c_u2, input.var)
cb.df <- cbind(output.var$c.c_b, input.var)
cn.df <- cbind(output.var$c.c_n, input.var)
cc.df <- cbind(output.var$c.c_c, input.var)
ce.df <- cbind(output.var$c.c_e, input.var)

# linear null model 
null.ca1 <- lm(ca1.df$`output.var$c.c_a1`~1, data = ca1.df)
null.ca2 <- lm(ca2.df$`output.var$c.c_a2`~1, data = ca2.df)
null.ca3 <- lm(ca3.df$`output.var$c.c_a3`~1, data = ca3.df)
null.ca4 <- lm(ca4.df$`output.var$c.c_a4`~1, data = ca4.df)
null.cu1 <- lm(cu1.df$`output.var$c.c_u1`~1, data = cu1.df)
null.cu2 <- lm(cu2.df$`output.var$c.c_u2`~1, data = cu2.df)
null.cb <- lm(cb.df$`output.var$c.c_b`~1, data = cb.df)
null.cn <- lm(cn.df$`output.var$c.c_n`~1, data = cn.df)
null.cc <- lm(cc.df$`output.var$c.c_c`~1, data = cc.df)
null.ce <- lm(ce.df$`output.var$c.c_e`~1, data = ce.df)


# baseline model with mean 
base_ca1 <- mean(output.var$c.c_a1, na.rm = TRUE)
base_ca2 <-  mean(output.var$c.c_a2, na.rm = TRUE)
base_ca3 <-  mean(output.var$c.c_a3, na.rm = TRUE)
base_ca4 <-  mean(output.var$c.c_a4, na.rm = TRUE)
base_cu1 <-  mean(output.var$c.c_u1, na.rm = TRUE)
base_cu2 <-  mean(output.var$c.c_u2, na.rm = TRUE)
base_cb <-  mean(output.var$c.c_b, na.rm = TRUE)
base_cn <-  mean(output.var$c.c_n, na.rm = TRUE)
base_cc <-  mean(output.var$c.c_c, na.rm = TRUE)
base_ce <-  mean(output.var$c.c_e, na.rm = TRUE)


base <- list(base_ca1, base_ca2, base_ca3, base_ca4, base_cu1, base_cu2, base_cb, base_cn, base_cc, base_ce)



save(base, null.ca1, null.ca2, null.ca3, null.ca4, null.cb, null.cc, null.ce, null.cn, null.cu1, null.cu2, 
     file = (paste0(path_save, "mean_baseline.Rdata")))

