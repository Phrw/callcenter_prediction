#install.packages(c("glmnet","quantreg","glmnetUtils"))

library(plumber)
library(jsonlite)

pr <- plumber::plumb("API_tidsserie.R")
pr$run(host = "0.0.0.0", port = 3830)


