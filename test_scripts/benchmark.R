options(java.parameters = c("-Xmx4000m"))
library(YARF)
library(mlbench)
library(randomForest)

# ------------------------------------------------------------------------------
#                               Helper Funcs
# ------------------------------------------------------------------------------

pure_signal_train = function(n, n_noise){
    x = cbind(0:(n-1), matrix(runif(n_noise*n, 0, 1), n))
    colnames(x) = paste0('x', 1:dim(x)[2])
    y = x[,1]
    list(x=x,y=y)
}

pure_signal_test = function(n, n_noise){
    x1 = runif(n)*100
    x = cbind(x1, matrix(runif(n_noise*n, 0, 1), n))
    colnames(x) = paste0('x', 1:dim(x)[2])
    y = x[,1]
    list(x=x,y=y)
}


linear_regression = function(n, n_signal, n_noise, sigma){
    p = n_signal + n_noise
    x = matrix(rnorm(p*n), n)
    colnames(x) = paste0('x', 1:dim(x)[2])
    y = rowSums(x[,1:n_signal]) + sigma*rnorm(n)
    list(x=x,y=y)
}

simulation_run = function(x_train, y_train, x_test){

    n = length(y_train)
    p = ncol(x_train)
    boot_ind = list()
    boot_ind[[1]] = 1:n
    out = list()
    
    # no boot, 1 tree: mtry = ncol
    # yarf_mod = YARF(as.data.frame(x_train), y_train, num_trees = 1, mtry = p,
    #     verbose=F, bootstrap_indices=boot_ind)
    # yarf_pred = predict(yarf_mod, as.data.frame(x_test))
    # 
    # rf = randomForest(x_train, y_train, ntree=1, mtry=p, replace=F, sampsize=n)
    # rf_pred = predict(rf, x_test)
    # out[[1]] = cbind(yarf_pred, rf_pred)
    # 
    # # 500 trees: mtry = 1
    # yarf_mod = YARF(as.data.frame(x_train), y_train, mtry = 1, num_trees = 500,
    #         verbose=F)
    # yarf_pred = predict(yarf_mod, as.data.frame(x_test))
    # 
    # rf = randomForest(x_train, y_train, ntree=500, mtry = 1)
    # rf_pred = predict(rf, x_test)
    # out[[2]] = cbind(yarf_pred, rf_pred)
    # 
    # # 500 trees: mtry = ncol
    # yarf_mod = YARF(as.data.frame(x_train), y_train, mtry = p, num_trees = 500,
    #         verbose=F)
    # yarf_pred = predict(yarf_mod, as.data.frame(x_test))
    # 
    # rf = randomForest(x_train, y_train, ntree=500, mtry = p)
    # rf_pred = predict(rf, x_test)
    # out[[3]] = cbind(yarf_pred, rf_pred)

    # 500 tree: mtry = default
    yarf_mod = YARF(as.data.frame(x_train), y_train, num_trees = 500,
        verbose=F)
    yarf_pred = predict(yarf_mod, as.data.frame(x_test))
    
    rf = randomForest(x_train, y_train, ntree=500)
    rf_pred = predict(rf, x_test)
    out[[4]] = cbind(yarf_pred, rf_pred)

    out
    
}

rmse_list = function(out_list, y_test){
    rmse = function(x) sqrt(mean((x-y_test)^2))
    sapply(out_list, function(x) c(rmse(x[,1]), rmse(x[,2])))
}

train_test_split = function(df, train_frac = 0.8){
    n = dim(df)[1]
    n_train = floor(n*train_frac)
    train_ix = sample(1:n, n_train, replace=F)
    list(train=df[train_ix, ], test=df[-train_ix, ])
}

boston = function(){
    data(BostonHousing)
    tmp = train_test_split(BostonHousing)
    train = tmp$train
    test = tmp$test

    train = list(y=train$medv, x = model.matrix(medv~.+0, train))
    test = list(y=test$medv, x = model.matrix(medv~.+0, test))
    list(train=train, test=test)
    
}

# ------------------------------------------------------------------------------
#                                   Bakeoff
# ------------------------------------------------------------------------------
 
n_reps = 1000

results = list()
rmse = list()
t1 = Sys.time()
i=1

for(i in 1:n_reps){

    # --------------------- Regression -------------------- #
    
    # linear regression
    train = linear_regression(500, 5, 3, 1)
    test = linear_regression(5000, 5, 3, 1)
    results[['linreg']][[i]] = simulation_run(train$x, train$y, test$x)
    rmse[['linreg']][[i]] = rmse_list(results[['linreg']][[i]], test$y)
    rm(train, test)

    # friedman 1
    train = mlbench.friedman1(500)
    test = mlbench.friedman1(5000)
    results[['friedman1']][[i]] = simulation_run(train$x, train$y, test$x)
    rmse[['friedman1']][[i]] = rmse_list(results[['friedman1']][[i]], test$y)
    rm(train, test)

    # friedman 2
    train = mlbench.friedman2(500)
    test = mlbench.friedman2(5000)
    results[['friedman2']][[i]] = simulation_run(train$x, train$y, test$x)
    rmse[['friedman2']][[i]] = rmse_list(results[['friedman2']][[i]], test$y)
    rm(train, test)

    # friedman 3
    train = mlbench.friedman3(500)
    test = mlbench.friedman3(5000)
    results[['friedman3']][[i]] = simulation_run(train$x, train$y, test$x)
    rmse[['friedman3']][[i]] = rmse_list(results[['friedman3']][[i]], test$y)
    rm(train, test)

    # peaks
    train = mlbench.peak(500)
    test = mlbench.peak(500)
    results[['peaks']][[i]] = simulation_run(train$x, train$y, test$x)
    rmse[['peaks']][[i]] = rmse_list(results[['peaks']][[i]], test$y)
    rm(train, test)

    # boston housing
    tmp = boston()
    train = tmp$train
    test = tmp$test
    results[['boston']][[i]] = simulation_run(train$x, train$y, test$x)
    rmse[['boston']][[i]] = rmse_list(results[['boston']][[i]], test$y)
    rm(train, test)

    # --------------------- Classification -------------------- #

    #train = mlbench.ringnorm(500)
    #test = mlbench.ringnorm(500)
    #results[['ringnorm']][[i]] = simulation_run(train$x, train$classes, test$x)
    #rm(train, test)
    cat('-------------------- Iteration ', i, ' -------------------\n')
    print(Sys.time()-t1)

    
}

save(rmse, results, file='bench.RData')
lapply(rmse, function(M) M[[4]][,4])

## classification
#sonar
#ionosphere
#2dnormals
#mlbench.cassini(n, relsize=c(2,2,1))
#mlbench.circle(n, d=2)
#mlbench.ringnorm(n, d=20)

