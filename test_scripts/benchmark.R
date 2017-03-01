options(java.parameters = c("-Xmx4000m"))
library(YARF)
library(randomForest)
library(mlbench)
library(reprtree) 

# ------------------------------------------------------------------------------
#                              Pure Signal (Debug)
# ------------------------------------------------------------------------------


set.seed(1234)
#set.seed(12345)
n = 100
p = 1
X = data.frame(0:(n-1), matrix(runif(n * (p-1)), nrow = n))
y = X[, 1]
names(X) = paste0('x', 1:ncol(X))

# yarf
boot_ind = list()
boot_ind[[1]] = 1:n
yarf_mod = YARF(X, y, num_trees = 1, mtry = ncol(X), bootstrap_indices=boot_ind)
illustrate_trees(yarf_mod, open_file = TRUE)

# rf
rf = randomForest(X, y, ntree = 1, mtry=ncol(X), replace=FALSE,
    sampsize=nrow(X), nodesize=5)
randomForest::getTree(rf, 1, labelVar=TRUE)
# reprtree:::plot.getTree(rf, k=1)

# rf_vis
dat = data.frame(x=X, y=y)
rf = randomForest(y ~ ., data=dat, ntree = 1, mtry=ncol(X), replace=FALSE,
    sampsize=nrow(X), nodesize=5)
randomForest::getTree(rf, 1, labelVar=TRUE)
reprtree:::plot.getTree(rf, k=1)


# test
n_test = 5000
X_test = as.data.frame(matrix(100*runif(n_test*p), nrow=n_test))
names(X_test) = paste0('x', 1:ncol(X))
y_test = X_test[, 1]

rf_pred = predict(rf, X_test)
yarf_pred = predict(yarf_mod, X_test)
cbind(y_test, rf_pred, yarf_pred, ifelse(rf_pred == yarf_pred, 0, 1))
mean((rf_pred - y_test))^2
mean((yarf_pred - y_test))^2

# ------------------------------------------------------------------------------
#                              Pure Signal
# ------------------------------------------------------------------------------

set.seed(123)

nreps = 100
mse_rf = rep(NA, nreps)
mse_yarf = rep(NA, nreps)

#get_tree_num_nodes_leaves_max_depths(yarf_mod)
.jcall(yarf_mod$java_YARF, "[I", "getNumNodes")
randomForest::treesize(rf, terminal=FALSE)

for(i in 1:nreps){
    n = 100
    p = 5
    X = data.frame(0:(n-1), matrix(runif(n * (p-1)), nrow = n))
    y = as.numeric(X[, 1])
    names(X) = paste0('x', 1:ncol(X))


    # yarf
    boot_ind = list()
    for(j in 1:500) boot_ind[[j]] = 1:n
    yarf_mod = YARF(X, y, num_trees = 50, bootstrap_indices=boot_ind,
        verbose=FALSE)

    # rf
    rf = randomForest(X, y, ntree = 50, replace=FALSE,
        sampsize=nrow(X), nodesize=5)
    
    # test
    n_test = 5000
    X_test = as.data.frame(matrix(100*runif(n_test*p), nrow=n_test))
    names(X_test) = paste0('x', 1:ncol(X))
    y_test = X_test[, 1]

    rf_pred = predict(rf, X_test)
    yarf_pred = predict(yarf_mod, X_test)
    mse_rf[i] = mean((rf_pred - y_test)^2)
    mse_yarf[i] = mean((yarf_pred - y_test)^2)

    if(i %% 5 == 0) cat(i, '\n')
}

mean(mse_rf - mse_yarf)
t.test(mse_rf, mse_yarf)

# ------------------------------------------------------------------------------
#                              Friedman1
# ------------------------------------------------------------------------------

set.seed(123)

nreps = 1e2
n_train = 500
n_test = 1e3

mse_rf = rep(NA, nreps)
mse_yarf = rep(NA, nreps)

for(i in 1:nreps){

    train = mlbench.friedman1(n=n_train)
    #    mtry = ncol(train$x)
    mtry = 1

    # yarf
    boot_ind = list()
    for(j in 1:500) boot_ind[[j]] = 1:n_train
    yarf_mod = YARF(as.data.frame(train$x), train$y, num_trees = 500,
        mtry = mtry, bootstrap_indices=boot_ind, verbose=F)

    # rf
    rf = randomForest(train$x, train$y, ntree = 500, mtry=mtry,
            replace=FALSE,sampsize=nrow(train$x), nodesize=5)

    # test
    test = mlbench.friedman1(n=n_test)

    rf_pred = predict(rf, test$x)
    yarf_pred = predict(yarf_mod, as.data.frame(test$x))
    mse_rf[i] = mean((rf_pred - test$y)^2)
    mse_yarf[i] = mean((yarf_pred - test$y)^2)

    if(i %% 5 == 0) cat(i, '\n')
}

mean(mse_rf - mse_yarf)

# ------------------------------------------------------------------------------
#                              Friedman2
# ------------------------------------------------------------------------------

set.seed(123)

nreps = 1e2
n_train = 500
n_test = 1e3

mse_rf = rep(NA, nreps)
mse_yarf = rep(NA, nreps)

for(i in 1:nreps){

    train = mlbench.friedman2(n=n_train)
    #    mtry = ncol(train$x)
    mtry = 1

    # yarf
    boot_ind = list()
    boot_ind[[1]] = 1:n_train
    yarf_mod = YARF(as.data.frame(train$x), train$y, num_trees = 1,
        mtry = mtry, bootstrap_indices=boot_ind)

    # rf
    rf = randomForest(train$x, train$y, ntree = 1, mtry=mtry,
        replace=FALSE,sampsize=nrow(train$x), nodesize=5)

    # test
    test = mlbench.friedman2(n=n_test)

    rf_pred = predict(rf, test$x)
    yarf_pred = predict(yarf_mod, as.data.frame(test$x))
    mse_rf[i] = mean((rf_pred - test$y))^2
    mse_yarf[i] = mean((yarf_pred - test$y))^2

    if(i %% 5 == 0) cat(i, '\n')
}

mean(mse_rf - mse_yarf)

# ------------------------------------------------------------------------------
#                              Friedman3
# ------------------------------------------------------------------------------

set.seed(123)

nreps = 1e2
n_train = 500
n_test = 1e3

mse_rf = rep(NA, nreps)
mse_yarf = rep(NA, nreps)

for(i in 1:nreps){

    train = mlbench.friedman3(n=n_train)
    #    mtry = ncol(train$x)
    mtry = 1

    # yarf
    boot_ind = list()
    boot_ind[[1]] = 1:n_train
    yarf_mod = YARF(as.data.frame(train$x), train$y, num_trees = 1,
        mtry = mtry, bootstrap_indices=boot_ind)

    # rf
    rf = randomForest(train$x, train$y, ntree = 1, mtry=mtry,
        replace=FALSE,sampsize=nrow(train$x), nodesize=5)

    # test
    test = mlbench.friedman3(n=n_test)

    rf_pred = predict(rf, test$x)
    yarf_pred = predict(yarf_mod, as.data.frame(test$x))
    mse_rf[i] = mean((rf_pred - test$y))^2
    mse_yarf[i] = mean((yarf_pred - test$y))^2

    if(i %% 5 == 0) cat(i, '\n')
}

mean(mse_rf - mse_yarf)
t.test(mse_rf, mse_yarf)

# ------------------------------------------------------------------------------
#                              Boston Housing
# ------------------------------------------------------------------------------

set.seed(123)

nreps = 1e2

data(BostonHousing)
X = BostonHousing[,1:13]
X = model.matrix(~., X)[,-1]
y = BostonHousing[,14]
train_frac = 0.8
n = length(y)

mse_rf = rep(NA, nreps)
mse_yarf = rep(NA, nreps)

for(i in 1:nreps){

    mtry = ncol(X)
    
    train_ix = sample(1:n, floor(train_frac*n), replace=F)
    train = list(y=y[train_ix], x=X[train_ix,])
    test = list(y=y[-train_ix], x=X[-train_ix,])

    # yarf
    boot_ind = list()
    for(j in 1:500) boot_ind[[j]] = 1:length(train$y)
    yarf_mod = YARF(as.data.frame(train$x), train$y, num_trees = 10,
        mtry = mtry, bootstrap_indices=boot_ind, verbose=F)

    # rf
    rf = randomForest(train$x, train$y, ntree = 10, mtry=mtry,
        replace=FALSE,sampsize=nrow(train$x), nodesize=5)

    # test
    rf_pred = predict(rf, test$x)
    yarf_pred = predict(yarf_mod, as.data.frame(test$x))
    mse_rf[i] = mean((rf_pred - test$y)^2)
    mse_yarf[i] = mean((yarf_pred - test$y)^2)

    if(i %% 5 == 0) cat(i, '\n')
}

mean(mse_rf - mse_yarf)
t.test(mse_rf, mse_yarf)
