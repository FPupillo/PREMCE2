dotR <- file.path(Sys.getenv("HOME"), ".R")

if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")

if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC",
    "CXX14=g++", # or clang++ but you may need a version postfix
    file = M, sep = "\n", append = TRUE)

Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)
install.packages("V8")

install.packages("dplyr",dependencies=TRUE)
install.packages("R.matlab",dependencies=TRUE)
install.packages("ggpubr",dependencies=TRUE)
install.packages("ggplot2",dependencies=TRUE)
install.packages("gridExtra",dependencies=TRUE) 
install.packages('BH')

