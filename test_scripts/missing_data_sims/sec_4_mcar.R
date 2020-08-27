options(java.parameters = "-Xmx5000m")
library(YARF)
library(MASS)
library(ggplot2)
set_YARF_num_cores(8)

seed = 1105

set.seed(seed)

p = 3

generate_response_model = function(n, sigma_e = 1, Sigma = NULL, mu_vec = NULL){
	
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

###MCAR


generate_mcar_model = function(Xy, gamma){
	for (i in 1 : nrow(Xy)){
		for (j in 1 : (ncol(Xy) - 1)){
			if (runif(1) < gamma){
				Xy[i, j] = NA
			}
		}
	}
	Xy
}

approx_prop_missing = seq(from = 0, to = 0.7, by = 0.1)
gammas = 1 - (1 - approx_prop_missing)^(1/3)

calc_rmse = function(y, yhat){
	sqrt(sum((y - yhat)^2) / length(y))
}

results_yarf_all_all_mcar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_yarf_all_cc_mcar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_yarf_cc_all_mcar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
results_yarf_cc_cc_mcar = matrix(NA, nrow = length(approx_prop_missing), ncol = Nsim)
rownames(results_yarf_all_all_mcar) = approx_prop_missing
rownames(results_yarf_all_cc_mcar) = approx_prop_missing
rownames(results_yarf_cc_all_mcar) = approx_prop_missing
rownames(results_yarf_cc_cc_mcar) = approx_prop_missing

# useful debugging objects- RStudio debugger is better though
# Xy_cc_list = list()
# yarf_all_list = list()
# yarf_cc_list = list()
# Xy_train_all_list = list()
# Xy_test_all_list = list()

for (nsim in 1 : Nsim){
	cat("nsim = ", nsim, "\n")
  # yarf_all_list[[nsim]] = list()
  # yarf_cc_list[[nsim]] = list()
  # Xy_cc_list[[nsim]] = list()
  # Xy_train_all_list[[nsim]] = list()
  # Xy_test_all_list[[nsim]] = list()
	for (g in 1 : length(gammas)){
		gamma = gammas[g]
		cat("g = ", g, "\n")
		
		Xy_train_all = generate_mcar_model(generate_response_model(n), gamma)
		Xy_train_cc = na.omit(Xy_train_all)
		Xy_test_cc = generate_response_model(n)
		Xy_test_all = generate_mcar_model(Xy_test_cc, gamma)
		
		# Xy_cc_list[[nsim]][[g]] = Xy_train_cc
		# Xy_train_all_list[[nsim]][[g]] = Xy_train_all
		# Xy_test_all_list[[nsim]][[g]] = Xy_test_all
		
		#train models
		#yarf_all = YARF(X = Xy_train_all[,1:3], y = Xy_train_all[,4], verbose = FALSE, seed = seed)
		yarf_all = YARF(X = Xy_train_all[,1:3], y = Xy_train_all[,4], verbose = FALSE)
		
		
		#jet if we have exceedingly few rows in Xycc
		if (nrow(Xy_train_cc) > 5){
		  #yarf_cc = YARF(X = Xy_train_cc[,1:3], y = Xy_train_cc[,4], verbose = FALSE, seed = seed)
		  yarf_cc = YARF(X = Xy_train_cc[,1:3], y = Xy_train_cc[,4], verbose = FALSE)
		}
		
		# yarf_all_list[[nsim]][[g]] = yarf_all
		# yarf_cc_list[[nsim]][[g]] = yarf_cc
		
		#test models
		results_yarf_all_all_mcar[g, nsim] = calc_rmse(predict(yarf_all, Xy_test_all[, 1 : 3]), Xy_test_all[, 4])
		results_yarf_all_cc_mcar[g, nsim] = calc_rmse(predict(yarf_all, Xy_test_cc[, 1 : 3]), Xy_test_cc[, 4])
		if (nrow(Xy_train_cc) > 5){ #jet if we have exceedingly few rows in Xycc
			results_yarf_cc_all_mcar[g, nsim] = calc_rmse(predict(yarf_cc, Xy_test_all[, 1 : 3]), Xy_test_all[, 4])
			results_yarf_cc_cc_mcar[g, nsim] = calc_rmse(predict(yarf_cc, Xy_test_cc[, 1 : 3]), Xy_test_cc[, 4])
		}
		
		# print(results_yarf_all_all_mcar[g, nsim])
		# print(results_yarf_all_cc_mcar[g, nsim])
	}
}

avgs_mcar_all_all = apply(results_yarf_all_all_mcar, 1, mean, na.rm = TRUE)	
rel_mcar_avgs_all_all = avgs_mcar_all_all / avgs_mcar_all_all[1]	
sd_mcar_all_all = apply(results_yarf_all_all_mcar / avgs_mcar_all_all[1], 1, sd, na.rm = TRUE)

avgs_mcar_all_cc = apply(results_yarf_all_cc_mcar, 1, mean, na.rm = TRUE)	
rel_mcar_avgs_all_cc = avgs_mcar_all_cc / avgs_mcar_all_all[1]
sd_mcar_all_cc = apply(results_yarf_all_cc_mcar / avgs_mcar_all_all[1], 1, sd, na.rm = TRUE)

avgs_mcar_cc_all = apply(results_yarf_cc_all_mcar, 1, mean, na.rm = TRUE)
rel_mcar_avgs_cc_all = avgs_mcar_cc_all / avgs_mcar_all_all[1]
sd_mcar_cc_all = apply(results_yarf_cc_all_mcar / avgs_mcar_all_all[1], 1, sd, na.rm = TRUE)

avgs_mcar_cc_cc = apply(results_yarf_cc_cc_mcar, 1, mean, na.rm = TRUE)
rel_mcar_avgs_cc_cc = avgs_mcar_cc_cc / avgs_mcar_all_all[1]
sd_mcar_cc_cc = apply(results_yarf_cc_cc_mcar / avgs_mcar_all_all[1], 1, sd, na.rm = TRUE)


save.image("sec_4.2_mcar.RData")

#Figure 2a
par(mar = c(4.2,4,0.2,0.2))
plot(approx_prop_missing,
		rel_mcar_avgs_all_all,
		col = "blue",
		type = "o",
		ylim = c(1, max(rel_mcar_avgs_all_all, rel_mcar_avgs_all_cc, rel_mcar_avgs_cc_all, rel_mcar_avgs_cc_cc, na.rm = TRUE)),
		xlab = "Proportion Missing",
		ylab = "Multiple of Baseline Error")
for (i in 2 : length(approx_prop_missing)){
	x = approx_prop_missing[i]
	y = rel_mcar_avgs_all_all[i]
	moe = 1.96 * sd_mcar_all_all[i] / sqrt(Nsim)
	segments(x, y - moe, x, y + moe, col = "blue")
}
points(approx_prop_missing, rel_mcar_avgs_all_cc, col = "blue", type = "o", lty = 3)
for (i in 2 : length(approx_prop_missing)){
	x = approx_prop_missing[i]
	y = rel_mcar_avgs_all_cc[i]
	moe = 1.96 * sd_mcar_all_cc[i] / sqrt(nsim)
	segments(x, y - moe, x, y + moe, col = "blue")
}
points(approx_prop_missing, rel_mcar_avgs_cc_all, col = "red", type = "o")
for (i in 2 : length(approx_prop_missing)){
	x = approx_prop_missing[i]
	y = rel_mcar_avgs_cc_all[i]
	moe = 1.96 * sd_mcar_cc_all[i] / sqrt(nsim)
	segments(x, y - moe, x, y + moe, col = "red")
}
points(approx_prop_missing, rel_mcar_avgs_cc_cc, col = "red", type = "o", lty = 3)
for (i in 2 : length(approx_prop_missing)){
	x = approx_prop_missing[i]
	y = rel_mcar_avgs_cc_cc[i]
	moe = 1.96 * sd_mcar_cc_cc[i] / sqrt(nsim)
	segments(x, y - moe, x, y + moe, col = "red")
}

len_prop_missing = length(approx_prop_missing)
df = data.frame(approx_prop_missing=rep(approx_prop_missing, 4),
                rel_mcar_avgs =
                  c(rel_mcar_avgs_all_all, rel_mcar_avgs_all_cc, rel_mcar_avgs_cc_all, rel_mcar_avgs_cc_cc),
                train_group = c(rep('all', len_prop_missing*2), rep('cc', len_prop_missing*2)),
                test_group = rep(c(rep('all', len_prop_missing), rep('cc', len_prop_missing)), 2)
                )

df$moe = NA
df[df$train_group=='all' & df$test_group=='all','moe'] = 1.96 * sd_mcar_all_all / sqrt(Nsim)
df[df$train_group=='all' & df$test_group=='cc','moe'] = 1.96 * sd_mcar_all_cc / sqrt(Nsim)
df[df$train_group=='cc' & df$test_group=='all','moe'] = 1.96 * sd_mcar_cc_all / sqrt(Nsim)
df[df$train_group=='cc' & df$test_group=='cc','moe'] = 1.96 * sd_mcar_cc_cc / sqrt(Nsim)

ggplot(df, aes(x=approx_prop_missing, y = rel_mcar_avgs, color = train_group, linetype = test_group)) +
  geom_point() +
  geom_line() +
  geom_linerange(aes(ymin=rel_mcar_avgs - moe, ymax=rel_mcar_avgs + moe)) +
  scale_color_manual(name = "Trained with...",
                     labels = c("missings", "no missings"),
                     values = c("#8ce5ff", "#ff6c6c")) +
  scale_linetype_manual(name = "Tested with...",
                        labels = c("missings", "no missings"),
                        values = c("solid", "dashed")) +
  guides(col = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  ggtitle("MCAR") +
  xlab("Approx. Proportion Missing") +
  ylab("Multiple of Baseline Error")

illustrate_trees(yarf_all, max_depth = 6, trees=1, open_file = TRUE)


#moe = 1.96 * sd_mcar_cc_cc[i] / sqrt(nsim)

# df = data.frame(approx_prop_missing=approx_prop_missing,
#                 rel_mcar_avgs_all_all=rel_mcar_avgs_all_all,
#                 rel_mcar_avgs_all_cc=rel_mcar_avgs_all_cc,
#                 rel_mcar_avgs_cc_all=rel_mcar_avgs_cc_all,
#                 rel_mcar_avgs_cc_cc=rel_mcar_avgs_cc_cc,
#                 moe_all_all = 1.96 * sd_mcar_all_all / sqrt(Nsim),
#                 moe_all_cc = 1.96 * sd_mcar_all_cc / sqrt(Nsim),
#                 moe_cc_all = 1.96 * sd_mcar_cc_all / sqrt(Nsim),
#                 moe_cc_cc = 1.96 * sd_mcar_cc_cc / sqrt(Nsim)
# )


#moe = 1.96*c(rep(sd_mcar_all_all, len_prop_missing), rep(sd_mcar_all_cc, len_prop_missing), rep(sd_mcar_cc_all, len_prop_missing), rep(sd_mcar_cc_cc, len_prop_missing) / sqrt(Nsim))

# ggplot(df, aes(x=approx_prop_missing)) +
#   geom_point(aes(y=rel_mcar_avgs_all_all)) +
#   geom_point(aes(y=rel_mcar_avgs_all_cc)) +
#   geom_point(aes(y=rel_mcar_avgs_cc_all)) +
#   geom_point(aes(y=rel_mcar_avgs_cc_cc)) +
#   geom_path(aes(y=rel_mcar_avgs_all_all), color="blue") +
#   geom_path(aes(y=rel_mcar_avgs_all_cc), color="blue", linetype = "dashed") +
#   geom_path(aes(y=rel_mcar_avgs_cc_all), color="red") +
#   geom_path(aes(y=rel_mcar_avgs_cc_cc), color="red", linetype = "dashed") +
#   geom_errorbar(aes(ymin=rel_mcar_avgs_all_all - moe_all_all, ymax=rel_mcar_avgs_all_all + moe_all_all)) +
#   geom_errorbar(aes(ymin=rel_mcar_avgs_all_cc - moe_all_cc, ymax=rel_mcar_avgs_all_cc + moe_all_cc)) +
#   geom_errorbar(aes(ymin=rel_mcar_avgs_cc_all - moe_cc_all, ymax=rel_mcar_avgs_cc_all + moe_cc_all)) +
#   geom_errorbar(aes(ymin=rel_mcar_avgs_cc_cc - moe_all_cc, ymax=rel_mcar_avgs_cc_cc + moe_all_cc))

# groups = interaction(df$train_group, df$test_group, lex.order=TRUE)
# ggplot(df, aes(x=approx_prop_missing, y = rel_mcar_avgs, color = groups, linetype = groups)) +
#   geom_point() +
#   geom_line() +
#   geom_linerange(aes(ymin=rel_mcar_avgs - moe, ymax=rel_mcar_avgs + moe)) +
#   scale_color_manual(name = "Training Group", labels = c("All cases", "Complete cases"), values = c("#8ce5ff", "#8ce5ff", "#ff6c6c", "#ff6c6c")) +
#   scale_linetype_manual(name = "Test Group", labels = rep(c("All cases", "Complete cases"), 2), values = rep(c("solid", "dashed"), 2))
