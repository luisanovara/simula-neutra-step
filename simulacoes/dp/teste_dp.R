dp500<-simula.neutra.trade(100,50,rep(seq(1,20000,length.out=100),each=50),20000,500,NULL,NULL,5000,100)
save(dp500,file="teste_dp500.RData")
tab500<-fert.t1(dp500$sp.list,dp500$sementes,fun=mean)
tab500[,51]
graf_tab500<-matplot(t(tab500),type="l",xlab="Ciclo",ylab="")
graf_tab500_zoom<-matplot(t(tab500[,1:20]),type="l",xlab="Ciclo",ylab="")

dp0<-simula.neutra.trade(100,50,rep(seq(1,20000,length.out=100),each=50),20000,0,NULL,NULL,5000,100)
save(dp0,file="teste_dp0.RData")
tab0<-fert.t1(dp0$sp.list,dp0$sementes,fun=mean)
tab0[,51]
graf_tab0<-matplot(t(tab0),type="l",xlab="Ciclo",ylab="")
graf_tab0_zoom<-matplot(t(tab0[,1:20]),type="l",xlab="Ciclo",ylab="")
