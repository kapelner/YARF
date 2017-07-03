options(java.parameters = c("-Xmx4000m")) #this means 4GB of RAM for YARF
library(YARF)
set_YARF_num_cores(7)
library(MASS); data(Pima.tr)

#recode response so it's more interpretable
Pima.tr$type = factor(Pima.tr$type, levels = c("Yes", "No"))

res = YARFROC(Pima.tr[, 1 : 7], Pima.tr$type, 
              desired_interval = 0.1, 
              y_axis_fine_resolution = TRUE)

calcAUC(res)

res = YARFROC(Pima.tr[, 1 : 7], Pima.tr$type, 
              desired_interval = 0.05, 
              y_axis_fine_resolution = TRUE,
              x_axis = "fdr", y_axis = "fnr")
res = YARFROC(Pima.tr[, 1 : 7], Pima.tr$type, 
              x_axis = "fomr", y_axis = "fdr", 
              desired_interval = 0.05, 
              y_axis_fine_resolution = TRUE)
