options(java.parameters = c("-Xmx4000m"))
library(YARF)



###basic test... stump
n = 100
p = 1
X = data.frame(1:n, matrix(runif(n * (p-1)), nrow = n))
y = X[, 1]

yarf_mod = YARF(X, y, num_trees = 1, mtry = 1)
yarf_mod
YARF_update_with_oob_results(yarf_mod)
illustrate_trees(yarf_mod, open_file = TRUE)


#test 1a - linear model
n = 100
X = data.frame(x1 = 0 : (n - 1))
y = 0 + 1 * X[,1] #+ rnorm(n, 0, 0.1)

## split choice ##
set.seed(123)
n = 100
p = 20
X = data.frame(matrix(rnorm(n*p), n))
names(X) = paste('x', 1:20, sep='')
y = 50*X[,1]
yarf_mod = YARF(X, y, num_trees = 1, mtry = ncol(X))

yarf_mod
YARF_update_with_oob_results(yarf_mod)

print_at_split_node_script = "function printAtSplitNode(node){
  return '' + 
    Java.type('YARF.StatToolbox').sample_minimum(node.node_ys()).toFixed(2)
    + '-' +
    Java.type('YARF.StatToolbox').sample_average(node.node_ys()).toFixed(2)
    + '-' +
    Java.type('YARF.StatToolbox').sample_maximum(node.node_ys()).toFixed(2);
}";
print_at_leaf_script = "function printAtLeaf(node){
  return '' + 
    Java.type('YARF.StatToolbox').sample_minimum(node.node_ys()).toFixed(2)
    + '\\n' +
    Java.type('YARF.StatToolbox').sample_average(node.node_ys()).toFixed(2)
    + '\\n' +
    Java.type('YARF.StatToolbox').sample_maximum(node.node_ys()).toFixed(2);
}";

illustrate_trees(yarf_mod, trees = c(1), max_depth = 2,
                 length_in_px_per_half_split = 40,
                 print_at_leaf_script = print_at_leaf_script,
                 print_at_split_node_script = print_at_split_node_script,
                 open_file = TRUE)


# yarf_mod = YARF_update_with_oob_results(yarf_mod)

library(randomForest)
rf_mod = randomForest(X, y, ntree = 50)
rf_mod

noos = 500
Xoos = data.frame(x1 = runif(noos, 0, 100))
yoos_expe = Xoos[, 1]
yoos_yarf = predict(yarf_mod, Xoos)
yoos_rf = predict(rf_mod, Xoos)
sum((yoos_yarf - yoos_expe)^2) / noos
sum((yoos_rf - yoos_expe)^2) / noos

plot(yoos_expe, yoos_yarf, pch = ".")
abline(a = 0, b = 1, col = "blue")
plot(yoos_expe, yoos_rf, pch = ".")
abline(a = 0, b = 1, col = "blue")
plot(yoos_yarf, yoos_rf)
abline(a = 0, b = 1, col = "blue")


#test #1 --- ensure the algorthim predicts as well as randomForest
library(BlandAltmanLeh)
bland.altman.plot(yoos_yarf, yoos_rf, 
  main = "", xlab = "Means", ylab = "Differences (YARF - RF)")
bland.altman.stats(yoos_yarf, yoos_rf)






xstar = seq(-20,120, by = 0.01)
xstar[1] = NA
y_hat = predict(yarf_mod, data.frame(x1 = xstar))

plot(xstar, y_hat)
y_hat[1]

options(java.parameters = c("-Xmx4000m"))
library(YARF)

n = 100
X = data.frame(x1 = 0 : (n - 1))
y = factor(ifelse(X[,1] >= 50, "A", "B"))
yarf_mod = YARF(X, y, num_trees = 500, use_missing_data = TRUE)
yarf_mod
yarf_mod = YARF_update_with_oob_results(yarf_mod)
yarf_mod


options(java.parameters = c("-Xmx4000m"))
library(YARF)
set_YARF_num_cores(2)
library(MASS)
data(Boston)

X = Boston[, 1 : 13]
y = Boston[, 14]

yarf_mod = YARF(X, y, num_trees = 500)
yarf_mod
yarf_mod = YARF_update_with_oob_results(yarf_mod)
yarf_mod
y_hat_yarf = predict(yarf_mod, X)

plot(y, y_hat_yarf)

library(randomForest)
rf_mod = randomForest(X, y, num_trees = 500)
y_hat_rf = predict(rf_mod, X)

plot(y, y_hat_rf)
rf_mod

plot(y_hat_rf, y_hat_yarf)

##verify on classification it works
options(java.parameters = c("-Xmx4000m"))
library(YARF)
set_YARF_num_cores(2)

data(iris)
X = iris[, 1 : 4]
y = iris[, 5]

yarf_mod = YARF(X, y, num_trees = 500)
yarf_mod
yarf_mod = YARF_update_with_oob_results(yarf_mod)
yarf_mod
y_hat = predict(yarf_mod, X)
table(y, y_hat)

library(randomForest)
rf_mod = randomForest(X, y, num_trees = 500)
rf_mod
y_hat = predict(rf_mod, X)
table(y, y_hat)



#show off the asynchronicity
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
#to remove it and return the memory,
rm(yarf_mod)
gc()

##and a logit
y = factor(rbinom(rep(1, n), rep(1, n), prob = exp(y_expe) / (1 + exp(y_expe))), labels = c("Yes", "No"))
yarf_mod = YARF(X, y, num_trees = 5000, wait = FALSE)
YARF_progress_reports(yarf_mod, time_delay_in_seconds = 4, plot_oob_error = TRUE, trail = 30)



##now we will test custom node assignment functions. We first begin
#with the regular node assignment for regression: the average


options(java.parameters = c("-Xmx4000m")); library(YARF); library(MASS); data(Boston)
X = Boston[, 1 : 13]; y = Boston[, 14]
library(readr)

#load the node assign as the median value
node_assign_script = read_file("scr02.js")
shared_scripts = read_file("scr04.js")

yarf_mod = YARF(X, y, num_trees = 500, 
                node_assign_script = node_assign_script,
                shared_scripts = shared_scripts)
YARF_update_with_oob_results(yarf_mod)
#vanilla RF
yarf_mod = YARF(X, y, num_trees = 500)
YARF_update_with_oob_results(yarf_mod)
#note same results


#predict(yarf_mod, X)

#now let's go even crazier... let's make the cost function sum of absolute errors (instead of squared errors)
cost_single_node_calc_script = read_file("scr03.js")


yarf_mod = YARF(X, y, num_trees = 1,
                node_assign_script = node_assign_script,
                cost_single_node_calc_script = cost_single_node_calc_script)
yarf_mod
YARF_update_with_oob_results(yarf_mod)

#now let's get funky and do median

node_assign_script = read_file("scr02.js")
node_assign_script = gsub("\\s", "", node_assign_script) 
node_assign_script

yarf_mod = YARF(X, y, num_trees = 500, node_assign_script = node_assign_script)
yarf_mod
YARF_update_with_oob_results(yarf_mod)
#same results

#now let's go even crazier... let's aggregate across the tree predictions by median
aggregation_script = " 
	function aggregateYhatsIntoOneYhat(y_hat_trees){
		y_hat_trees.sort(function(a,b) {return a - b;});
		var half = Math.floor(y_hat_trees / 2);
		if (y_hat_trees % 2)
			return y_hat_trees[half];
		else
			return (y_hat_trees[half - 1] + y_hat_trees[half]) / 2.0;
	}"


yarf_mod = YARF(X, y, num_trees = 500, 
                node_assign_script = node_assign_script, aggregation_script = aggregation_script)
yarf_mod
YARF_update_with_oob_results(yarf_mod)



options(java.parameters = c("-Xmx4000m")); library(YARF)
n = 2000
p = 1

errors = rcauchy(n)
X = matrix(rnorm(n * p), nrow = n)
beta = as.matrix(100)
y = as.numeric(X %*% beta + errors)
X = data.frame(X)
plot(X[,1], y)

yarf_mod = YARF(X, y, num_trees = 500, wait = FALSE)
yarf_mod
YARF_update_with_oob_results(yarf_mod)
YARF_progress_reports(yarf_mod)


library(readr)
cost_single_node_calc_script = read_file("scr03.js")
shared_scripts = read_file("scr04.js")
node_assign_script = read_file("scr02.js")
yarf_mod2 = YARF(X, y, num_trees = 500, 
                node_assign_script = node_assign_script,
                cost_single_node_calc_script = cost_single_node_calc_script, 
                shared_scripts = shared_scripts,
                wait = FALSE)
yarf_mod2
YARF_update_with_oob_results(yarf_mod2)
YARF_progress_reports(yarf_mod)


#test construction time
options(java.parameters = c("-Xmx4000m")); library(YARF)
n = 2000
p = 1

errors = rcauchy(n)
X = matrix(rnorm(n * p), nrow = n)
beta = as.matrix(100)
y = as.numeric(X %*% beta + errors)
X = data.frame(X)

yarf_mod = YARF(X, y, num_trees = 500)
yarf_mod
YARF_update_with_oob_results(yarf_mod)

library(readr)
cost_single_node_calc_script = read_file("scr05.js")
shared_scripts = read_file("scr04.js")
yarf_mod2 = YARF(X, y, num_trees = 500,
                 cost_single_node_calc_script = cost_single_node_calc_script, 
                 shared_scripts = shared_scripts)
yarf_mod2
YARF_update_with_oob_results(yarf_mod2)
