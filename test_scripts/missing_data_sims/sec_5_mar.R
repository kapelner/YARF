options(java.parameters = "-Xmx5000m")
library(YARF)
library(MASS)
library(missForest)
set_YARF_num_cores(2)

###constants for simulation
Nsim = 1000
pct_test_data = 0.2


#get the Boston housing data
data(Boston)
X = Boston
#X = cbind(X, rnorm(nrow(X)))
y = X$medv
X$medv = NULL

#unitize the covs
X = data.frame(apply(X, 2, function(xj){(xj - min(xj)) / (max(xj) - min(xj))}))

n_test = round(pct_test_data * nrow(X))


calc_rmse = function(y, yhat){
	sqrt(sum((y - yhat)^2) / length(y))
}

################ MAR

create_mar_model_of_bhd = function(X, beta_0, beta){
	for (i in 1 : nrow(X)){
		prob_M_rm = beta_0 + beta * X$indus[i] + beta * X$lstat[i] + beta * X$age[i]
		if (runif(1) < prob_M_rm){
			X$rm[i] = NA
		}
		
		prob_M_crim = beta_0 + beta * X$nox[i] + beta * X$rad[i] + beta * X$tax[i]
		if (runif(1) < prob_M_crim){
			X$crim[i] = NA
		}
	}
	X
}



beta_0 = -3
betas = c(0, 1.33, 1.47, 1.67, 2.1, 2.6, 3.1, 3.8)
approx_prop_missing = seq(from = 0, to = 0.7, by = 0.1)

##test to see if betas are appropriate
#for (i in 1 : length(betas)){
#	Xmar = create_mar_model_of_bhd(X, beta_0, betas[i])
#	actual_prop_missing = 1 - nrow(na.omit(Xmar)) / nrow(Xmar)
#	cat("purported prop missing:", approx_prop_missing[i], "actual prop missing", actual_prop_missing, "\n")
#}

results_yarf_mar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_yarf_w_rfi_and_mf_mar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_rf_mar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
rownames(results_yarf_mar) = approx_prop_missing
rownames(results_yarf_w_rfi_and_mf_mar) = approx_prop_missing
rownames(results_rf_mar) = approx_prop_missing

for (nsim in 1 : Nsim){
	cat("nsim = ", nsim, "\n")
	for (g in 1 : length(betas)){
		test_indices = sample(1 : nrow(X), n_test)
		
		#create the missing matrix and subset the training and test
		Xm = create_mar_model_of_bhd(X, beta_0, betas[g])
		Xtrain = Xm[-test_indices, ]
		ytrain = y[-test_indices]
		Xtest = Xm[test_indices, ]
		ytest = y[test_indices]	
		
		#now start training models and predicting on them
		#impute both training and test data with MissForest
		Xtrain_MF_imputed = missForest(cbind(ytrain, Xtrain))$ximp[, -1]
		
		yarf_mod = YARF(Xtrain, ytrain, verbose = FALSE)
		yarf_mod_rf_imp = YARF(Xtrain_MF_imputed, ytrain, verbose = FALSE)
		rf_mod = randomForest(x = Xtrain_MF_imputed, y = ytrain)
		
		#impute to create an Xtest without missingness for rf --- give it everything but ytest
		imputed = missForest(rbind(Xtest, Xtrain), verbose = FALSE)$ximp				
		Xtest_miss_rf = imputed[1 : n_test, ]
		
		results_yarf_mar[g, nsim] = calc_rmse(predict(yarf_mod, Xtest), ytest)
		results_yarf_w_rfi_and_mf_mar[g, nsim] = calc_rmse(predict(yarf_mod_rf_imp, Xtest_miss_rf), ytest)
		results_rf_mar[g, nsim] = calc_rmse(predict(rf_mod, Xtest_miss_rf), ytest)
		
		cat("yarf oosrmse:", results_yarf_mar[g, nsim], "rf oosrmse:", results_rf_mar[g, nsim], "yarf_with_rf_imp oosrmse:", results_yarf_w_rfi_and_mf_mar[g, nsim], "\n")
		
		avgs_mar_yarf = apply(results_yarf_mar, 1, mean, na.rm = TRUE)		
		rel_mar_avgs_yarf = avgs_mar_yarf / avgs_mar_yarf[1]
		sd_mar_yarf = apply(results_yarf_mar / avgs_mar_yarf[1], 1, sd, na.rm = TRUE)
		
		avgs_mar_yarf_w_rfi_and_mf = apply(results_yarf_w_rfi_and_mf_mar, 1, mean, na.rm = TRUE)		
		rel_mar_avgs_yarf_w_rfi_and_mf = avgs_mar_yarf_w_rfi_and_mf / avgs_mar_yarf[1]
		sd_mar_yarf_w_rfi_and_mf = apply(results_yarf_w_rfi_and_mf_mar / avgs_mar_yarf[1], 1, sd, na.rm = TRUE)
		
		avgs_mar_rf = apply(results_rf_mar, 1, mean, na.rm = TRUE)		
		rel_mar_avgs_rf = avgs_mar_rf / avgs_mar_yarf[1]
		sd_mar_rf = apply(results_rf_mar / avgs_mar_yarf[1], 1, sd, na.rm = TRUE)
		
		par(mar = c(4.2,4,0.2,0.2))
		plot(approx_prop_missing, 
				rel_mar_avgs_yarf, 
				col = "green", 
				type = "o", 
				xlab = "Proportion Missing",
				ylab = "Multiple of Baseline Error",
				ylim = c(1, 1.75)) 
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y_plot = rel_mar_avgs_yarf[i]
			moe = 1.96 * sd_mar_yarf[i] / sqrt(nsim)
			segments(x, y_plot - moe, x, y_plot + moe, col = "green")
		}		
		points(approx_prop_missing, rel_mar_avgs_yarf_w_rfi_and_mf, col = "blue", type = "o")
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y_plot = rel_mar_avgs_yarf_w_rfi_and_mf[i]
			moe = 1.96 * sd_mar_yarf_w_rfi_and_mf[i] / sqrt(nsim)
			segments(x, y_plot - moe, x, y_plot + moe, col = "blue")
		}
		points(approx_prop_missing, rel_mar_avgs_rf, col = "red", type = "o")
		for (i in 1 : length(approx_prop_missing)){
			x = approx_prop_missing[i]
			y_plot = rel_mar_avgs_rf[i]
			moe = 1.96 * sd_mar_rf[i] / sqrt(nsim)
			segments(x, y_plot - moe, x, y_plot + moe, col = "red")
		}
		
		
	}
	
	save.image("sec_5_mar_MF_only.RData")
}

