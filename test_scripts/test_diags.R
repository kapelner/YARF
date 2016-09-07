# install.packages("rJava")

# Sys.getenv("JAVA_HOME")

# library(rJava)
# .jinit()
# .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
# 
# Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_102")
# options(java.home="/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/")
# Sys.setenv(DYLD_FALLBACK_LIBRARY_PATH="/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/jre/lib/server/")

#-Xrunjdwp:server=y,transport=dt_socket,address=8000, suspend=n
# options(java.parameters = c("-Xmx3000m", "-Xdebug", "-Xrunjdwp:server=y,transport=dt_socket,address=8000"))

options(java.parameters = c("-Xmx4000m"))
library(YARF)

n = 100
X = data.frame(x1 = 0 : (n - 1))
y = 0 + 1 * X[,1] + rnorm(n, 0, 0.1)
plot(X[,1], y)

yarf_mod = YARF(X, y, num_trees = 100, use_missing_data = TRUE)
yarf_mod
yarf_mod = YARF_update_with_oob_results(yarf_mod)
yarf_mod
xstar = seq(-20,120, by = 0.01)
xstar[1] = NA
y_hat = predict(yarf_mod, data.frame(x1 = xstar))

plot(xstar, y_hat)
y_hat[1]

options(java.parameters = c("-Xmx4000m"))
library(YARF)

n = 100
X = data.frame(x1 = 0 : (n - 1))
y = factor(ifelse(X[,1] >= 50, "A", "B"))
yarf_mod = YARF(X, y, num_trees = 500, use_missing_data = TRUE)
yarf_mod
yarf_mod = YARF_update_with_oob_results(yarf_mod)
yarf_mod


options(java.parameters = c("-Xmx4000m"))
library(YARF)
set_YARF_num_cores(2)
library(MASS)
data(Boston)

X = Boston[, 1 : 13]
y = Boston[, 14]

yarf_mod = YARF(X, y, num_trees = 500)
yarf_mod
yarf_mod = YARF_update_with_oob_results(yarf_mod)
yarf_mod
y_hat = predict(yarf_mod, X)

plot(y, y_hat)

library(randomForest)
rf_mod = randomForest(X, y, num_trees = 500)
y_hat = predict(rf_mod, X)

plot(y, y_hat)
rf_mod

##verify on classification it works
options(java.parameters = c("-Xmx4000m"))
library(YARF)
set_YARF_num_cores(2)

data(iris)
X = iris[, 1 : 4]
y = iris[, 5]

yarf_mod = YARF(X, y, num_trees = 500)
yarf_mod
yarf_mod = YARF_update_with_oob_results(yarf_mod)
yarf_mod
y_hat = predict(yarf_mod, X)
table(y, y_hat)

library(randomForest)
rf_mod = randomForest(X, y, num_trees = 500)
rf_mod
y_hat = predict(rf_mod, X)
table(y, y_hat)



#show off the asynchronicity
data(Boston)
for (i in 1 : 6){
  Boston = rbind(Boston, Boston)
}
dim(Boston)

X = Boston[, 1 : 13]
y = Boston[, 14]

yarf_mod = YARF(X, y, num_trees = 500, wait = FALSE)
yarf_mod
YARF_progress_reports(yarf_mod, time_delay_in_seconds = 3)
YARF_update_with_oob_results(yarf_mod)
