install.packages("microbenchmark")
library(microbenchmark)
mbm_dp <- microbenchmark(
  dp_0 = (sapply(1,rtruncnorm,a=(1-0.000001), b=(20000+0.000001), mean=rep(20000,100),sd=0))/(20000+0.000001),
  dp_0.1 = (sapply(1,rtruncnorm,a=(1-0.000001), b=(20000+0.000001), mean=rep(20000,100),sd=0.1))/(20000+0.000001),
  dp_10 = (sapply(1,rtruncnorm,a=(1-0.000001), b=(20000+0.000001), mean=rep(20000,100),sd=10))/(20000+0.000001),
  dp_100 = (sapply(1,rtruncnorm,a=(1-0.000001), b=(20000+0.000001), mean=rep(20000,100),sd=100))/(20000+0.000001),
  dp_500 = (sapply(1,rtruncnorm,a=(1-0.000001), b=(20000+0.000001), mean=rep(20000,100),sd=500))/(20000+0.000001),
  times=1000
)
library(ggplot2)
autoplot(mbm_dp)