set.seed(2026)
n_cluster <- 30
n_per_cluster <- 40
cluster_id <- rep(sprintf("C%02d", 1:n_cluster), each=n_per_cluster)
u0 <- rep(rnorm(n_cluster,0,4), each=n_per_cluster)
x1 <- rnorm(n_cluster*n_per_cluster)
x2_cluster <- rep(rnorm(n_cluster), each=n_per_cluster)
y <- 70 + 2.5*x1 + 1.8*x2_cluster + u0 + rnorm(n_cluster*n_per_cluster,0,6)
demo <- data.frame(y=y,x1=x1,x2_cluster=x2_cluster,cluster_id=cluster_id)
saveRDS(demo,"data/demo/demo_hlm_ipd.rds")
