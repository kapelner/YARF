
boston_raw = MASS::Boston
boston_raw$chas = factor(boston_raw$chas)
boston_raw$rad = factor(boston_raw$rad)

Xtrain = boston_raw[1 : 400, 1 : 13]
ytrain = boston_raw[1 : 400, 14]
Xtest = boston_raw[401 : 500, 1 : 13]

#punch out some holes
Xtrain[30 : 80, 4] = NA
Xtrain[100 : 110, 7] = NA
Xtrain[40 : 70, 9] = NA
Xtrain[300 : 310, 10] = NA

#a few different ways to run the algorithm
res = YARFMissForest(Xtrain, ytrain)
res = YARFMissForest(Xtrain, ytrain, Xtest)
res = YARFMissForest(Xtrain, ytrain, converge_after_increasing = FALSE)
res = YARFMissForest(Xtrain, ytrain, Xtest, converge_after_increasing = FALSE)
res = YARFMissForest(Xtrain, ytrain, verbose_missForest = FALSE)

#how did it do?
rbind(boston_raw[30 : 80, 4], res$Xtrain_imp[30 : 80, 4])
rbind(boston_raw[100 : 110, 7], res$Xtrain_imp[100 : 110, 7])
rbind(boston_raw[40 : 70, 9], res$Xtrain_imp[40 : 70, 9])
rbind(boston_raw[300 : 310, 10], res$Xtrain_imp[300 : 310, 10])