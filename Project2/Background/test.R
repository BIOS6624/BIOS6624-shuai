
cl <- makeCluster(detectCores())
clusterExport(cl, "simulate_power")
clusterExport(cl, "b0")
clusterExport(cl, "b1")
clusterExport(cl, "b2")
power <- parSapply(cl, seq(1450,1550, by = 10), simulate_power, b0,b1,b2)

stopCluster(cl)
