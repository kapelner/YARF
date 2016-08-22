# install.packages("rJava")

# Sys.getenv("JAVA_HOME")

# library(rJava)
# .jinit()
# .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")

Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_102")
options(java.parameters = "-Xmx3000m")
library(YARF)
library(MASS)
data(Boston)

X = Boston[, 1 : 13]
y = Boston[, 14]

yarf_mod = YARF(X, y)

