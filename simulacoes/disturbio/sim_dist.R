### Simulacoes variando frequencia dos disturbios ###

# xi0 inicial = valor medio entre minimo (1) e maximo (X=20000)
# S = 1

source("simula.neutra.trade_LEVE.R")
require(truncnorm)

sim_dist_0 <- simula.neutra.trade(S=1,j=5000,xi0=10000.5,X=20000,dp=500,dist.pos=NULL,dist.int=NULL,ciclo=3e5,step=100)
save(sim_dist_0,file="sim_dist_0.RData")
tab_dist_0 <- fert.t1(sim_dist_0$sp.list,sim_dist_0$sementes)
tab_dist_0[3001]
plot(tab_dist_0,type="l")
plot(tab_dist_0[1:20],type="l")

sim_dist_1 <- simula.neutra.trade(S=1,j=5000,xi0=10000.5,X=20000,dp=500,dist.pos=seq(10,3e5,10),dist.int=0.5,ciclo=3e5,step=100)
save(sim_dist_1,file="sim_dist_1.RData")
tab_dist_1 <- fert.t1(sim_dist_1$sp.list,sim_dist_1$sementes)
tab_dist_1[3001]
plot(tab_dist_1,type="l")
plot(tab_dist_1[1:20],type="l")

# derivacoes do sim_dist_1
## com xi0 de 1000 (abaixo do valor em que estabiliza em sim_dist_1)
sim_dist_2 <- simula.neutra.trade(S=1,j=5000,xi0=1000.5,X=20000,dp=500,dist.pos=seq(10,3e5,10),dist.int=0.5,ciclo=3e5,step=100)
tab_dist_2 <- fert.t1(sim_dist_2$sp.list,sim_dist_2$sementes)
tab_dist_2[3001]
plot(tab_dist_2,type="l")
plot(tab_dist_2[1:20],type="l")
## com maior intensidade de disturbio
sim_dist_3 <- simula.neutra.trade(S=1,j=5000,xi0=10000.5,X=20000,dp=500,dist.pos=seq(10,3e5,10),dist.int=0.9,ciclo=3e5,step=100)
tab_dist_3 <- fert.t1(sim_dist_3$sp.list,sim_dist_3$sementes)
tab_dist_3[3001]
plot(tab_dist_3,type="l")
plot(tab_dist_3[1:20],type="l")
## com maior frequencia de disturbio
sim_dist_4 <- simula.neutra.trade(S=1,j=5000,xi0=10000.5,X=20000,dp=500,dist.pos=seq(5,3e5,5),dist.int=0.5,ciclo=3e5,step=100)
save(sim_dist_4,file="sim_dist_4.RData")
tab_dist_4 <- fert.t1(sim_dist_4$sp.list,sim_dist_4$sementes)
tab_dist_4[3001]
plot(tab_dist_4,type="l")
plot(tab_dist_4[1:20],type="l")
## com xi0 de 50 (abaixo do valor em que estabiliza em sim_dist_0)
sim_dist_5 <- simula.neutra.trade(S=1,j=5000,xi0=50,X=20000,dp=500,dist.pos=seq(10,3e5,10),dist.int=0.5,ciclo=3e5,step=100)
save(sim_dist_5,file="sim_dist_5.RData")
tab_dist_5 <- fert.t1(sim_dist_5$sp.list,sim_dist_5$sementes)
tab_dist_5[3001]
plot(tab_dist_5,type="l")
plot(tab_dist_5[1:20],type="l")