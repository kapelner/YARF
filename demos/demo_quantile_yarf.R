options(java.parameters = c("-Xmx4000m"))
library(YARF)
set_YARF_num_cores(7)
library(MASS)
library(quantregForest)

quantile_loss = function(quantile, y, yhat){
  error = 0
  for (i in 1 : length(y)){
    diff = y[i] - yhat[i];
    if (diff > 0){
      error = error + quantile * diff;
    }
    else {
      error = error + (quantile - 1) * diff;			
    }
  }
  error / length(y)
}

##### the case of the exponential errors
#### true mean is x + 1/gamma

p = 5
ntrain = 500
gamma = 2
quantile = 0.25
Nsim = 1000  
num_trees = 500
nodesize = 50
ntest = 1000

res = matrix(NA, nrow = Nsim, ncol = 5)

cef = function(X){
  X[, 1] + X[, 2] + X[, 1] * X[, 2]
}

for (nsim in 1 : Nsim){
  X = data.frame(mvrnorm(ntrain + ntest, rep(0, p), Sigma = matrix(rep(1, p * p), nrow = p)))

  Xtrain = X[1 : ntrain, ]
  Xtrain = Xtrain[order(Xtrain[,1]), ]
  Xtest = X[(ntrain + 1) : (ntrain + ntest), ]
  Xtest = Xtest[order(Xtest[,1]), ]
  ytrain = 0 + 2 * cef(Xtrain) + rexp(ntrain, gamma)
  ytest_true = 2 * cef(Xtest) + qexp(quantile, gamma) 
  ytest = 2 * cef(Xtest) + rexp(ntest, gamma)
  
  
  # plot(X$x1, y, xlab = "x")
  # abline(a = 1 / gamma, b = 1, col = "gray", lwd = 3)
  # true_quantile = X$x1 + qexp(as.numeric(quantile), gamma)
  # points(X$x1, true_quantile, type = "l", col = "red")
  # 
  # yarf_mod_vanilla = YARF(X, y, num_trees = 500)
  # yarf_mod_vanilla
  # points(X$x1, predict(yarf_mod_vanilla, X), type = "l", col = "darkgreen", lwd = 1)
  # YARF_update_with_oob_results(yarf_mod_vanilla, oob_cost_calculation_script = 
  #                                gsub("quantile_from_R", quantile,
  #                                     read_file("js/quantile_cost_oob.js")))
  

  
  yarf_mod = YARF(Xtrain, ytrain, 
                  num_trees = num_trees, 
                  nodesize = nodesize,
                  node_assign_script = read_file("js/null_assignment.js"),
                  cost_single_node_calc_script = gsub("quantile_from_R", as.character(quantile),
                                                      read_file("js/quantile_cost.js"))
  )
  YARF_update_with_oob_results(yarf_mod, oob_cost_calculation_script = 
                                 gsub("quantile_from_R", as.character(quantile),
                                      read_file("js/quantile_cost_oob.js")))
  
  quantile = as.numeric(quantile)
  
  
  
  qrf <- quantregForest(Xtrain, ytrain, ntree = num_trees, nodesize = nodesize, nthread = 7, importance = TRUE)
  
  #see who's better
  yhat_yarf <- predict(yarf_mod, Xtest)
  yhat_qrf <- predict(qrf, Xtest, what = quantile)
  
  #plot it for pretty pics
  plot(Xtrain[, 1], ytrain, xlab = "x")
  points(Xtest[, 1], ytest_true, type = "l", col = "red")
  points(Xtest[, 1], ytest, col = "yellow")
  points(Xtest[, 1], yhat_yarf, type = "l", col = "blue", lwd = 3)
  points(Xtest[, 1], yhat_qrf, type = "l", col = "brown", lwd = 2)
  
  #find error
  ql_yarf = quantile_loss(quantile, ytest, yhat_yarf)
  ql_qrf = quantile_loss(quantile, ytest, yhat_qrf)
  ql_t_yarf = quantile_loss(quantile, ytest_true, yhat_yarf)
  ql_t_qrf = quantile_loss(quantile, ytest_true, yhat_qrf) 
  mse_yarf = sum((ytest_true - yhat_yarf)^2)
  mse_qrf = sum((ytest_true - yhat_qrf)^2)
  res[nsim, 1] = ql_yarf
  res[nsim, 2] = ql_qrf
  res[nsim, 3] = (ql_qrf - ql_yarf) / ql_qrf * 100
  res[nsim, 4] = mse_yarf
  res[nsim, 5] = mse_qrf
  print(sort(res[1 : nsim, 3]))
  print(mean(res[1 : nsim, 3]))
  save(res, file = "quantile_res.RData")
}
res[1 : nsim, ]

