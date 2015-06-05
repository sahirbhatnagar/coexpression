## ---- sim-data-fun ----




## ---- univariate-g-h ----

uni.gh <- function(Z, g, h){
  X <- exp((h*Z^2)/2)*ifelse(g==0, Z , (exp(g*Z)-1)/g) 
  return(X)
}



## ---- multivariate-g-h-sim ----

mv.gh <- function(n, h, sigma, rho){
  #n=1000;h=0;sigma=1;rho=1
  Sigma <- matrix(c(1,0,0,1),2,2)  
  Z <- MASS::mvrnorm(n = n,rep(0,2),Sigma = Sigma)
  R <- mapply(function(z1,z2) sqrt(z1^2+z2^2), z1 = Z[,1], z2 = Z[,2])
  theta <- atan(Z[,2]/Z[,1]) + ifelse(Z[,1]<0, pi, ifelse(Z[,1]>0 & Z[,2]<0,2*pi,0))
  R.star <- uni.gh(Z=R, g=0, h=h)
  Z1.star <- R.star * cos(theta)
  Z2.star <- R.star * sin(theta)
  X <- sigma * matrix(c(1,rho,0, sqrt(1-rho^2)),2,2) %*% mapply(function(z1,z2) matrix(c(z1,z2),2,1), z1=Z1.star, z2=Z2.star)
  return(t(X))
}

dev.off()
mv.gh(n = 100, h = 1, sigma = 1, rho = 0.5) %>% plot
mv.gh(n = 100, h = 1, sigma = 5, rho = -0.5) %>% points(col = "red")

