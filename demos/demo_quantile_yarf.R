options(java.parameters = c("-Xmx4000m"))
library(YARF)
set_YARF_num_cores(7)

##### the case of the exponential errors
#### true mean is x + 1/gamma

n = 500
gamma = 7
quantile = "0.90"
X = data.frame(x1 = (sort(runif(n, 0, 1))))
y = 0 + 1 * X[,1] + rexp(n, gamma)
plot(X$x1, y, xlab = "x")
abline(a = 1 / gamma, b = 1, col = "gray", lwd = 3)
true_quantile = X$x1 + qexp(as.numeric(quantile), gamma)
points(X$x1, true_quantile, type = "l", col = "red")

yarf_mod_vanilla = YARF(X, y, num_trees = 500)
yarf_mod_vanilla
points(X$x1, predict(yarf_mod_vanilla, X), type = "l", col = "darkgreen", lwd = 1)
YARF_update_with_oob_results(yarf_mod_vanilla, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("js/quantile_cost_oob.js")))

yarf_mod = YARF(X, y, 
              num_trees = 2000, 
              nodesize = 100,
              node_assign_script = read_file("js/null_assignment.js"),
              cost_single_node_calc_script = gsub("quantile_from_R", quantile,
                                                  read_file("js/quantile_cost.js"))
            )
YARF_update_with_oob_results(yarf_mod, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("js/quantile_cost_oob.js")))


plot(X$x1, y, xlab = "x")
points(X$x1, true_quantile, type = "l", col = "red")
points(X$x1, predict(yarf_mod, X), type = "l", col = "blue", lwd = 3)

library(quantregForest)
qrf <- quantregForest(X, y, ntree = 2000, nthread = 7)
conditionalQuantiles <- predict(qrf, X, what = as.numeric(quantile))
points(X$x1, conditionalQuantiles, type = "l", col = "orange", lwd = 1)
#find error for quantreg
error = 0
for (i in 1 : nrow(X)){
  diff = true_quantile[i] - conditionalQuantiles[i];
  if (diff > 0){
    error = error + as.numeric(quantile) * diff;
  }
  else {
    error = error + (as.numeric(quantile) - 1) * diff;			
  }
}
error / nrow(X)
#we beat quantregForest!

