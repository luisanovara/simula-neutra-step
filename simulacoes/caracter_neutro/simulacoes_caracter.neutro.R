# SIMULACOES COM simula.neutra.step.cn (CARACTER NEUTRO)

dados<-data.frame(c(rep(10,3)),c(rep(200,3)),c(rep(10,3)),c(rep(1,3)))

sim_cn_1 <- simula.neutra.step.cn(S= 10, j=200, xi0=rep(seq(10,10,length.out = 10),each=200), dp=1, dist.pos=NULL, dist.int=NULL, ciclo=10000, step=100)
save(sim_cn_1,file="sim_cn_1.RData")
tab_cn_1<-fert.t1(sim_cn_1$sp.list,sim_cn_1$sementes,mean)
matplot(t(tab_cn_1),type="l",xlab="Ciclo",ylab="",axes=T,main="Variação no número de propágulos (cn - rep1)")

sim_cn_2 <- simula.neutra.step.cn(S= 10, j=200, xi0=rep(seq(10,10,length.out = 10),each=200), dp=1, dist.pos=NULL, dist.int=NULL, ciclo=100000, step=100)
save(sim_cn_2,file="sim_cn_2.RData")
tab_cn_2<-fert.t1(sim_cn_2$sp.list,sim_cn_2$sementes,mean)
matplot(t(tab_cn_2),type="l",xlab="Ciclo",ylab="",axes=T,main="Variação no número de propágulos (cn - rep2)")

sim_cn_3 <- simula.neutra.step.cn(S= 10, j=200, xi0=rep(seq(10,10,length.out = 10),each=200), dp=1, dist.pos=NULL, dist.int=NULL, ciclo=1000000, step=100)
save(sim_cn_3,file="sim_cn_3.RData")
tab_cn_3<-fert.t1(sim_cn_3$sp.list,sim_cn_3$sementes,mean)
matplot(t(tab_cn_3),type="l",xlab="Ciclo",ylab="",axes=T,main="Variação no número de propágulos (cn - rep3)")

sim_cn_4 <- simula.neutra.cn(S= 10, j=400, xi0=rep(seq(10,10,length.out = 10),each=200), dp=1, dist.pos=NULL, dist.int=NULL, ciclo=2000000, step=1000) #duracao: 600 min
save(sim_cn_4,file="sim_cn_4.RData")
tab_cn_4<-fert.t1(sim_cn_4$sp.list,sim_cn_4$sementes,mean)
matplot(t(tab_cn_4),type="l",xlab="Ciclo",ylab="",axes=T,main="Variação no número de propágulos (cn - rep4)")
tab_cn_4_sd<-fert.t1(sim_cn_4$sp.list,sim_cn_4$sementes,sd)

lista<-list(sim_cn_1,sim_cn_2,sim_cn_3)
simulacoes_output<-simula_output(lista)

##

sim_pad_1<-simula.neutra.step(S= 10, j=200, xi0=rep(seq(10,10,length.out = 10),each=200), dp=1, dist.pos=NULL, dist.int=NULL, ciclo=10000, step=100)
tab_pad_1<-fert.t1(sim_pad_1$sp.list,sim_pad_1$sementes,mean)
matplot(t(tab_pad_1),type="l",xlab="Ciclo",ylab="",axes=T,main="Variação no número de propágulos (rep1)")

sim_pad_2 <- simula.neutra.step(S= 10, j=200, xi0=rep(seq(10,10,length.out = 10),each=200), dp=1, dist.pos=NULL, dist.int=NULL, ciclo=100000, step=100)
tab_pad_2<-fert.t1(sim_pad_2$sp.list,sim_pad_2$sementes,mean)
matplot(t(tab_pad_2),type="l",xlab="Ciclo",ylab="",axes=T,main="Variação no número de propágulos (rep2)")

sim_pad_3 <- simula.neutra.step(S= 10, j=200, xi0=rep(seq(10,10,length.out = 10),each=200), dp=1, dist.pos=NULL, dist.int=NULL, ciclo=1000000, step=100)
tab_pad_3<-fert.t1(sim_pad_3$sp.list,sim_pad_3$sementes,mean)
matplot(t(tab_pad_3),type="l",xlab="Ciclo",ylab="",axes=T,main="Variação no número de propágulos (rep3)")
