options(java.parameters = "-Xmx5000m")
library(YARF)
library(MASS)
set_YARF_num_cores(8)

seed = 1105

set.seed(seed)

generate_response_model = function(n, sigma_e = 1, Sigma = NULL, mu_vec = NULL){
	p = 3
	
	if (is.null(Sigma)){
		Sigma = 0.8 * diag(p) + 0.2
		Sigma[1, p] = 2 * Sigma[1, p]
		Sigma[p, 1] = 2 * Sigma[p, 1]
	}
	if (is.null(mu_vec)){
		mu_vec = rep(0, p)
	}
	
	Xs = mvrnorm(n, mu_vec, Sigma)
	error = rnorm(n, 0, sigma_e)
	X1 = Xs[, 1]
	X2 = Xs[, 2]
	X3 = Xs[, 3]
	
	y_crazy = X1 + X2 + 2 * X3 - X1^2 + X2^2 + X1 * X2 + error
	
	data.frame(Xs, y_crazy)
}

Nsim = 500
n = 250


###NMAR

beta_0 = -3
betas = c(0, 0.8, 1.4, 2, 2.7, 4, 7, 30)


generate_nmar_model = function(Xy, beta_0, beta){
	for (i in 1 : nrow(Xy)){
		prob = pnorm(beta_0 + beta * Xy[i, 1]^2 + beta * Xy[i, 2])
		if (Xy[i, 2] > 0 && runif(1) < prob){
			Xy[i, 2] = NA #missingness in X2 is determined by a linear probit model of X1 and X2
		}
	}
	Xy	
}

calc_rmse = function(y, yhat){
	sqrt(sum((y - yhat)^2) / length(y))
}

#Xy = generate_response_model(n)
#approx_prop_missing = seq(from = 0, to = 0.7, by = 0.1)
#for (i in 1 : length(approx_prop_missing)){
#	Xmar = generate_nmar_model(Xy[, 1 : 3], beta_0, betas[i])
#	actual_prop_missing = 1 - nrow(na.omit(Xmar)) / nrow(Xmar)
#	cat("purported prop missing:", approx_prop_missing[i], "actual prop missing", actual_prop_missing, "\n")
#}

results_yarf_all_all_nmar = matrix(NA, nrow = length(betas), ncol = Nsim)
results_yarf_all_cc_nmar = matrix(NA, nrow = length(betas), ncol = Nsim)
results_yarf_cc_all_nmar = matrix(NA, nrow = length(betas), ncol = Nsim)
results_yarf_cc_cc_nmar = matrix(NA, nrow = length(betas), ncol = Nsim)
rownames(results_yarf_all_all_nmar) = betas
rownames(results_yarf_all_cc_nmar) = betas
rownames(results_yarf_cc_all_nmar) = betas
rownames(results_yarf_cc_cc_nmar) = betas

for (nsim in 1 : Nsim){
	cat("nsim = ", nsim, "\n")
	for (g in 1 : length(betas)){
		beta = betas[g]
		cat("beta = ", beta, "\n")
		
		#generate data 
		Xy_train_all = generate_nmar_model(generate_response_model(n), beta_0, beta)
		Xy_train_cc = na.omit(Xy_train_all)
		Xy_test_cc = generate_response_model(n)
		Xy_test_all = generate_nmar_model(Xy_test_cc, beta_0, beta)
		
		#train models
		#yarf_all = YARF(Xy = Xy_train_all, verbose = FALSE, seed = seed)
		yarf_all = YARF(Xy = Xy_train_all, verbose = FALSE)
		
		#jet if we have exceedingly few rows in Xycc
		if (nrow(Xy_train_cc) > 5){
			#yarf_cc = YARF(Xy = Xy_train_cc, verbose = FALSE, seed = seed)
			yarf_cc = YARF(Xy = Xy_train_cc, verbose = FALSE)
		}
		
		#test models		
		results_yarf_all_all_nmar[g, nsim] = calc_rmse(predict(yarf_all, Xy_test_all[, 1 : 3]), Xy_test_all[, 4])
		results_yarf_all_cc_nmar[g, nsim] = calc_rmse(predict(yarf_all, Xy_test_cc[, 1 : 3]), Xy_test_cc[, 4])
		if (nrow(Xy_train_cc) > 5){ #jet if we have exceedingly few rows in Xycc
			results_yarf_cc_all_nmar[g, nsim] = calc_rmse(predict(yarf_cc, Xy_test_all[, 1 : 3]), Xy_test_all[, 4])
			results_yarf_cc_cc_nmar[g, nsim] = calc_rmse(predict(yarf_cc, Xy_test_cc[, 1 : 3]), Xy_test_cc[, 4])
		}
	}
}

avgs_nmar_all_all = apply(results_yarf_all_all_nmar, 1, mean, na.rm = TRUE)	
rel_nmar_avgs_all_all = avgs_nmar_all_all / avgs_nmar_all_all[1]
sd_nmar_all_all = apply(results_yarf_all_all_nmar / avgs_nmar_all_all[1], 1, sd, na.rm = TRUE)

avgs_nmar_all_cc = apply(results_yarf_all_cc_nmar, 1, mean, na.rm = TRUE)	
rel_nmar_avgs_all_cc = avgs_nmar_all_cc / avgs_nmar_all_all[1]
sd_nmar_all_cc = apply(results_yarf_all_cc_nmar / avgs_nmar_all_all[1], 1, sd, na.rm = TRUE)

avgs_nmar_cc_all = apply(results_yarf_cc_all_nmar, 1, mean, na.rm = TRUE)	
rel_nmar_avgs_cc_all = avgs_nmar_cc_all / avgs_nmar_all_all[1]
sd_nmar_cc_all = apply(results_yarf_cc_all_nmar / avgs_nmar_all_all[1], 1, sd, na.rm = TRUE)

avgs_nmar_cc_cc = apply(results_yarf_cc_cc_nmar, 1, mean, na.rm = TRUE)	
rel_nmar_avgs_cc_cc = avgs_nmar_cc_cc / avgs_nmar_all_all[1]
sd_nmar_cc_cc = apply(results_yarf_cc_cc_nmar / avgs_nmar_all_all[1], 1, sd, na.rm = TRUE)
	
approx_prop_missing = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7) #this was figured out during simulation to be approximately accurate (the plots don't change that much anyway)

save.image("sec_4.2_nmar.RData")

#Figure 2c
par(mar = c(4.2,2,0.3,0.2))
plot(approx_prop_missing, 
		rel_nmar_avgs_all_all,
		col = "blue", 
		type = "o", 
		ylim = c(1, max(rel_nmar_avgs_all_all, rel_nmar_avgs_all_cc, rel_nmar_avgs_cc_all, rel_nmar_avgs_cc_cc, na.rm = TRUE)),
		xlab = "Proportion Missing",
		ylab = "")
for (i in 2 : length(approx_prop_missing)){
	x = approx_prop_missing[i]
	y = rel_nmar_avgs_all_all[i]
	moe = 1.96 * sd_nmar_all_all[i] / sqrt(nsim)
	segments(x, y - moe, x, y + moe, col = "blue")
}
points(approx_prop_missing, rel_nmar_avgs_all_cc, col = "blue", type = "o", lty = 3)
for (i in 2 : length(approx_prop_missing)){
	x = approx_prop_missing[i]
	y = rel_nmar_avgs_all_cc[i]
	moe = 1.96 * sd_nmar_all_cc[i] / sqrt(nsim)
	segments(x, y - moe, x, y + moe, col = "blue")
}
points(approx_prop_missing, rel_nmar_avgs_cc_all, col = "red", type = "o")
for (i in 2 : length(approx_prop_missing)){
	x = approx_prop_missing[i]
	y = rel_nmar_avgs_cc_all[i]
	moe = 1.96 * sd_nmar_cc_all[i] / sqrt(nsim)
	segments(x, y - moe, x, y + moe, col = "red")
}
points(approx_prop_missing, rel_nmar_avgs_cc_cc, col = "red", type = "o", lty = 3)
for (i in 2 : length(approx_prop_missing)){
	x = approx_prop_missing[i]
	y = rel_nmar_avgs_cc_cc[i]
	moe = 1.96 * sd_nmar_cc_cc[i] / sqrt(nsim)
	segments(x, y - moe, x, y + moe, col = "red")
}

len_prop_missing = length(approx_prop_missing)
df = data.frame(approx_prop_missing=rep(approx_prop_missing, 4),
                rel_nmar_avgs = 
                  c(rel_nmar_avgs_all_all, rel_nmar_avgs_all_cc, rel_nmar_avgs_cc_all, rel_nmar_avgs_cc_cc),
                train_group = c(rep('all', len_prop_missing*2), rep('cc', len_prop_missing*2)),
                test_group = rep(c(rep('all', len_prop_missing), rep('cc', len_prop_missing)), 2)
)

df$moe = NA
df[df$train_group=='all' & df$test_group=='all','moe'] = 1.96 * sd_nmar_all_all / sqrt(Nsim)
df[df$train_group=='all' & df$test_group=='cc','moe'] = 1.96 * sd_nmar_all_cc / sqrt(Nsim)
df[df$train_group=='cc' & df$test_group=='all','moe'] = 1.96 * sd_nmar_cc_all / sqrt(Nsim)
df[df$train_group=='cc' & df$test_group=='cc','moe'] = 1.96 * sd_nmar_cc_cc / sqrt(Nsim)

ggplot(df, aes(x=approx_prop_missing, y = rel_nmar_avgs, color = train_group, linetype = test_group)) +
  geom_point() +
  geom_line() +
  geom_linerange(aes(ymin=rel_nmar_avgs - moe, ymax=rel_nmar_avgs + moe)) +
  scale_color_manual(name = "Trained with...",
                     labels = c("missings", "no missings"),
                     values = c("#8ce5ff", "#ff6c6c")) +
  scale_linetype_manual(name = "Tested with...",
                        labels = c("missings", "no missings"),
                        values = c("solid", "dashed")) +
  guides(col = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ggtitle("NMAR") +
  xlab("Approx. Proportion Missing") +
  ylab("Multiple of Baseline Error")
