options(java.parameters = c("-Xmx4000m"))
library(YARF)
library(mlbench)
library(randomForest)


set_YARF_num_cores(2)
shared_scripts = read_file("scr04.js") #the median function and sample_avg function
cost_single_node_calc_script = read_file("scr03.js")
node_assign_script = read_file("scr02.js") #assign node median

set.seed(1984)
n = 500; p = 1
errors = rt(n, 1.8)
X = matrix(rnorm(n * p), nrow = n)
beta = as.matrix(10)
y = as.numeric(X %*% beta + errors)
X = data.frame(X)
summary(lm(y~.,X))
plot(X[,1], y)

yarf_mod = YARF(X, y, num_trees = 500, 
		node_assign_script = node_assign_script,
		cost_single_node_calc_script = cost_single_node_calc_script,
		shared_scripts = shared_scripts,
		wait = TRUE)
YARF_update_with_oob_results(yarf_mod)
yarf_mod = YARF_update_with_oob_results(yarf_mod)

mae_validation_results[i] = yarf_mod$mae_oob
yarf_mod = YARF_update_with_oob_test_results(yarf_mod)
mae_test_results[i] = yarf_mod$mae_oob


powers = seq(0.25, 2, by = 0.25)

mae_validation_results = array(NA, length(powers))
mae_test_results = array(NA, length(powers))

for (i in 1 : length(powers)){
  cost_single_node_calc_script = paste("
    function nodeCost(node){
       var ys = Java.from(node.node_ys());
       var y_avg = sample_avg(ys);
       var sum_err = 0.0;
       for (i = 0; i < ys.length; i++){
        sum_err += Math.pow(Math.abs(ys[i] - y_avg), ", powers[i], ");
       }
       return sum_err;
    }
  ", sep = "") 
  yarf_mod = YARF(X, y, num_trees = 500, 
                   node_assign_script = node_assign_script,
                   cost_single_node_calc_script = cost_single_node_calc_script, 
                   shared_scripts = shared_scripts,
                   wait = TRUE)
  yarf_mod = YARF_update_with_oob_validation_results(yarf_mod)
  mae_validation_results[i] = yarf_mod$mae_oob
  yarf_mod = YARF_update_with_oob_test_results(yarf_mod)
  mae_test_results[i] = yarf_mod$mae_oob
  cat("power: ", powers[i], "validation mae: ", mae_validation_results[i])
}

round(rbind(powers, mae_validation_results), 3)
#now let's get the best cost function results as measured by mae
i_power_min_error = which(mae_validation_results == min(mae_validation_results), arr.ind = TRUE)
#finally test MAE
mae_test_results[i_power_min_error]

round(rbind(powers, mae_test_results), 3)
