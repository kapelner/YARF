install.packages("rJava")
Sys.setenv(JAVA_HOME="C:\Program Files\Java\jre1.8.0_102")
Sys.getenv("JAVA_HOME")

options(java.parameters = "-Xmx3000m")
library(YARF)
library(MASS)
data(Boston)

X = Boston[, 1 : 13]
y = Boston[, 14]

yarf_mod = YARF(X, y)

