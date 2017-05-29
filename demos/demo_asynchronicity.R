options(java.parameters = c("-Xmx4000m")) #this means 4GB of RAM for YARF
library(YARF)
set_YARF_num_cores(3)

n = 8000
p = 10
X = matrix(rnorm(n * p), nrow = n)
beta = as.matrix(rnorm(p))
y_expe = as.numeric(X %*% beta)
y = y_expe + rnorm(n)
X = data.frame(X)

#build a model asynchronously by setting wait to FALSE
yarf_mod = YARF(X, y, num_trees = 5000, wait = FALSE)
#the model initializes and upon the beginning of tree construction,
#returns you to the R prompt while it works as a background process
yarf_mod
#shows that it is in the process of being built
#you can also check to make sure the regression is correct
head(yarf_mod$model_matrix_training_data) # among other things...

#at any time, you can see the performance by computing the oob
#on the subset of trees that have been built. Here, you can see
#if there are major problems and cut your losses
YARF_update_with_oob_results(yarf_mod)
#If you would like to wait until the model is built, you
#can set up a process to notify you about the progress which
#will return to the prompt when the model is completed
#and you can also see a illustration of the convergence
YARF_progress_reports(yarf_mod, time_delay_in_seconds = 4, plot_oob_error = TRUE, trail = 30)
#you can "stop" this function at any time without stopping the
#model construction if you would like to return to the console

#if you really want to halt construction and free the CPU, you can
#use the stop function
YARF_stop(yarf_mod)
#at this point you can still query the model but if you want
#to remove it and return the memory, you must remove the object 
rm(yarf_mod)
#and then you MUST garbage collect in R to force Java to release 
#its memory hold (R only sees the Java object as a single pointer)
gc()