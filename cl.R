library(parallel)
cl.cores <- 12
cl <- makeCluster(cl.cores)
clusterEvalQ(cl,source(file="dmrs.R"))