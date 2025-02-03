rm(list = ls())

install.packages('foreign')

library(foreign)

mydata = read.dta("https://www.princeton.edu/~otorres/Panel101.dta")