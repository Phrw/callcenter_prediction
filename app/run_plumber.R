#install.packages(c("glmnet","quantreg","glmnetUtils"))

library(plumber)
library(jsonlite)

pr <- plumber::plumb("API.R")
pr$run(host = "0.0.0.0", port = 5050)


