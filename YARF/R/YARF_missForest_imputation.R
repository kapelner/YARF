#' MissForest Imputation
#' 
#' This function imputes missing data using the missForest algorithm which essentially iteratively runs the RF
#' algorithm on each column with missingness until the predictions do not change significantly. We provide a means
#' to run this imputation on both training data (with responses) and test data (without responses). The user can
#' also enter algorithm hyperparameters like convergence settings.
#' 
#' After each iteration the difference between the previous and the new imputed data matrix is assessed for the 
#' continuous and categorical parts. The stopping criterion is defined such that the imputation process is stopped 
#' as soon as both differences have become larger once. In case of only one type of variable the computation stops 
#' as soon as the corresponding difference goes up for the first time. However, the imputation last performed where 
#' both differences went up is generally less accurate than the previous one. Therefore, whenever the computation 
#' stops due to the stopping criterion (and not due to 'maxiter') the before last imputation matrix is returned.
#' 
#' @param Xtrain					The training data as a data frame.
#' @param ytrain					The training responses as numeric / integer / factor vector.
#' @param Xtest						The test data as a data frame (optional, default \code{NULL} for not specified).
#' @param maxiter					The maximum number of iterations before stopping (default is 10, as in package \code{missForest}).
#' @param converge_after_increasing	The algorithm quits after imputation errors increase once. Default is \code{TRUE} but if not,
#' 									the algorithm continues all \code{maxiter} runs and then returns the final imputations.
#' @param verbose_missForest		Prints out messages by iteration. Default is \code{TRUE}.
#' @param ...						Settings to pass to YARF during the iterations.
#' 
#' @return 							A list with objects: Xtrain_imp (and Xtest_imp if Xtest was specified) as well as nrmses which
#' 									is a record of the sum of the normalized rmse's over all numeric features by iteration and npfcs
#' 									which is a record of the sum of the misclassification rates over all categorical features by iteration.
#' 
#' @author Adam Kapelner
#' @export
YARFMissForest = function(Xtrain, ytrain, Xtest = NULL, maxiter = 10, converge_after_increasing = TRUE, verbose_missForest = TRUE, ...){
	assertDataFrame(Xtrain)
	assertChoice(class(ytrain), c("numeric", "integer", "factor"))
	assertDataFrame(Xtest, null.ok = TRUE)
	assertCount(maxiter, positive = TRUE)
	assertLogical(verbose_missForest)
	assertTRUE(nrow(Xtrain) == length(ytrain))
	
	Xy_names = c(colnames(Xtrain), "y")
	p = ncol(Xtrain)
	
	Xy = cbind(Xtrain, ytrain)
	if (!is.null(Xtest)){
		assertTRUE(ncol(Xtrain) == ncol(Xtest))
		n_test = nrow(Xtest)
		
		#rbind(df1,setnames(df2,names(df1))))
		Xy = rbind(Xy, setnames(cbind(Xtest, NA), names(Xy))) #Xtest doesn't have y's
	}
	
	n_imp = nrow(Xy)
	
	#which variables are factor?
	class_j = array(NA, p + 1)
	for (j in 1 : (p + 1)){
		class_j[j] = class(Xy[, j])
	}
	at_least_one_factor = any(class_j == "factor")
	
	#we need these to calculate normalized RMSEs during the iterations
	sd_js = apply(Xy, 2, function(xj){sd(xj, na.rm = TRUE)})
	
	variables_with_missingness = which(apply(Xy, 2, function(xj){sum(is.na(xj))}) > 0)
	
	#now we get lists of indices for later
	idx_list = list()
	for (j in variables_with_missingness){
		idx_list[[j]] = list(
				missings =      which(is.na(Xy[, j])),
				nonmissings =   which(!is.na(Xy[, j]))
		)
	}
	
	Xy_hat = Xy_imp = Xy #make a copy for imputing and a diff to understand performance
	
	nrmses = array(NA, maxiter)
	npfcs = array(NA, maxiter)
	
	for (iter in 1 : maxiter){
		if (verbose_missForest){cat("----- missForest iteration #", iter, "/", maxiter, "-----\n")}
		
		for (j in variables_with_missingness){
			if (verbose_missForest){cat("------- imputing ", Xy_names[j], "...\n")}
			missings = idx_list[[j]]$missings
			nonmissings = idx_list[[j]]$nonmissings
			
			y_iter_train = Xy_imp[nonmissings, j]
			X_iter_train = Xy_imp[nonmissings, -j]
			
			#now we have to split this up betweeen test and train
			X_iter_test = Xy_imp[missings, -j]
			
			yarf_mod = YARF(X_iter_train, y_iter_train, verbose = FALSE, ...)
			if (verbose_missForest){print(yarf_mod)}
			#record the imputations
			y_hat = predict(yarf_mod, X_iter_test)
			Xy_imp[missings, j] = y_hat
			Xy_hat[missings, j] = y_hat
			#record oob's as well
			Xy_hat[nonmissings, j] = yarf_mod$y_oob[nonmissings]
			#gotta cleanup otherwise we'll run out of RAM
			rm(yarf_mod); gc()
		}
		if (iter > 1){ #sum(apply(Xy_imp, 2, function(xj){sum(is.na(xj))})) == 0 #once there is no more missing data in the imputed data frame, we can begin to gauge if we've converged
			nrmse = 0
			npfc = 0
			for (j in 1 : ncol(Xy)){
				if (class(Xy[, j]) %in% c("numeric", "integer")){
					nrmse = nrmse + 
							sqrt(mean((Xy_hat[, j] - Xy[, j])^2, na.rm = TRUE)) / sd_js[j]
				} else if (class(Xy[, j]) == "factor"){
					npfc = npfc +
							sum(Xy_hat[, j] != Xy[, j], na.rm = TRUE) / n_imp
				}
			}
			nrmses[iter] = nrmse
			npfcs[iter] = npfc
			
			#should we break?
			if (converge_after_increasing & iter > 2){
				if (at_least_one_factor){
					if (nrmses[iter] > nrmses[iter - 1] & npfcs[iter] > npfcs[iter - 1]){
						break
					}
				} else {
					if (nrmses[iter] > nrmses[iter - 1]){
						break
					}
				}
			}
		}
		#save previous imputations because they are more accurate
		Xy_imp_prev = Xy_imp
	}
	
	#now we return
	colnames(Xy_imp_prev) = Xy_names
	ret = list()
	
	if (!is.null(Xtest)){
		ret$Xtrain_imp = Xy_imp_prev[1 : (n_imp - n_test), 1 : p]
		ret$Xtest_imp =  Xy_imp_prev[(n_imp - n_test + 1) : n_imp, 1 : p]
	} else {
		ret$Xtrain_imp = Xy_imp_prev[, 1 : p]
	}
	ret$nrmses = nrmses
	ret$npfcs = npfcs
	ret	
}

