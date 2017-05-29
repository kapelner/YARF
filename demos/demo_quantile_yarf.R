options(java.parameters = c("-Xmx4000m"))
library(YARF)
set_YARF_num_cores(3)

##### the case of the exponential errors
#### true mean is x + 1/gamma

n = 500
gamma = 7
quantile = "0.9"
X = data.frame(x1 = (sort(runif(n, 0, 1))))
y = 0 + 1*X[,1] + rexp(n, gamma)
plot(X$x1, y)
# abline(a = 0, b = 1 + 1 / gamma, col = "gray", lwd = 3)

yarf_mod_vanilla = YARF(X, y, num_trees = 500)
YARF_update_with_oob_results(yarf_mod_vanilla)
points(X$x1, predict(yarf_mod_vanilla, X), type = "l", col = "darkgreen", lwd = 1)

yarf_mod = YARF(X, y, num_trees = 500,
                node_assign_script = read_file("quantile_assignment.js"),
                cost_single_node_calc_script =
                  gsub("quantile_from_R", quantile,
                       read_file("quantile_cost.js"))
                )
YARF_update_with_oob_results(yarf_mod)
points(X$x1, predict(yarf_mod, X), type = "l", col = "blue", lwd = 1)

YARF_update_with_oob_results(yarf_mod, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))
YARF_update_with_oob_results(yarf_mod_vanilla, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))
### our QR RF works better than vanilla of course

YARF_set_aggregation_method(yarf_mod_vanilla, aggregation_script = 
                              gsub("quantile_from_R", quantile,
                                   read_file("quantile_aggregation.js")))
YARF_update_with_oob_results(yarf_mod_vanilla, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))
### our QR RF works better than the naive quantile aggregation of the yhats

YARF_set_aggregation_method(yarf_mod, aggregation_script = 
                              gsub("quantile_from_R", quantile,
                                   read_file("quantile_aggregation.js")))


YARF_update_with_oob_results(yarf_mod, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))
### and if we do quantile aggregation it doesn't work better


##how far off are we?
y_true = 0 + 1*X[,1] + qexp(as.numeric(quantile), gamma)
plot(X$x1, y, col = "gray")
points(X$x1, y_true, type = "l")
points(X$x1, predict(yarf_mod_vanilla, X), type = "l", col = "blue", lwd = 1)
points(X$x1, predict(yarf_mod, X), type = "l", col = "green", lwd = 1)


#let's see how we do playing with nodesize
yarf_mod = YARF(X, y, num_trees = 1250,
                node_assign_script = read_file("quantile_assignment.js"),
                cost_single_node_calc_script =
                  gsub("quantile_from_R", quantile,
                       read_file("quantile_cost.js")), 
                nodesize = 100
)
YARF_update_with_oob_results(yarf_mod, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))
yarf_mod_vanilla = YARF(X, y, num_trees = 1250, nodesize = 100)
YARF_update_with_oob_results(yarf_mod_vanilla, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))

y_true = 0 + 1*X[,1] + qexp(as.numeric(quantile), gamma)
plot(X$x1, y, col = "gray")
points(X$x1, y_true, type = "l")
points(X$x1, predict(yarf_mod_vanilla, X), type = "l", col = "blue", lwd = 1)
points(X$x1, predict(yarf_mod, X), type = "l", col = "green", lwd = 1)



###################BHD


options(java.parameters = c("-Xmx4000m")); 
library(YARF); set_YARF_num_cores(1)
library(MASS); data(Boston)
X = Boston[, 1 : 13]; y = Boston[, 14]

yarf_mod_vanilla = YARF(X, y, num_trees = 500)
YARF_update_with_oob_results(yarf_mod_vanilla)


yarf_mod = YARF(X, y, num_trees = 500,
                node_assign_script = read_file("quantile_assignment.js"),
                cost_single_node_calc_script =
                  gsub("quantile_from_R", quantile,
                       read_file("quantile_cost.js"))
)
YARF_update_with_oob_results(yarf_mod)

all_yhats = YARF_predict_all_trees(yarf_mod, X)
all_yhats_vanilla = YARF_predict_all_trees(yarf_mod_vanilla, X)
par(mfrow = c(2,1))
hist(all_yhats[1,], br = 80)
hist(all_yhats_vanilla[1,], br = 80)

YARF_update_with_oob_results(yarf_mod, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))
YARF_update_with_oob_results(yarf_mod_vanilla, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))

YARF_set_aggregation_method(yarf_mod, aggregation_script = 
                              gsub("quantile_from_R", quantile,
                                   read_file("quantile_aggregation.js")))
YARF_set_aggregation_method(yarf_mod_vanilla, aggregation_script = 
                              gsub("quantile_from_R", quantile,
                                   read_file("quantile_aggregation.js")))

YARF_update_with_oob_results(yarf_mod, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))
YARF_update_with_oob_results(yarf_mod_vanilla, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))

YARF_set_aggregation_method(yarf_mod, aggregation_script = NULL)
YARF_set_aggregation_method(yarf_mod_vanilla, aggregation_script = NULL)



YARF_update_with_oob_results(yarf_mod, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))
YARF_update_with_oob_results(yarf_mod_vanilla, oob_cost_calculation_script = 
                               gsub("quantile_from_R", quantile,
                                    read_file("quantile_cost_oob.js")))

