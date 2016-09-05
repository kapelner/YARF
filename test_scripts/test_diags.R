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

options(java.parameters = c("-Xmx3000m"))
library(YARF)

n = 100
X = data.frame(x1 = rnorm(n))
y = 3 + 6 * X[,1] + rnorm(n, 0, 0.1)
plot(X[,1], y)

yarf_mod = YARF(X, y, num_trees = 1)


library(MASS)
data(Boston)

X = Boston[, 1 : 13]
y = Boston[, 14]

yarf_mod = YARF(X, y, debug_log = TRUE)

