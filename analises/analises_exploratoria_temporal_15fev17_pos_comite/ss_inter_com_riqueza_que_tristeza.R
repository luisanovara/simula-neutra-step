## lembrando que: quanto maior a riqueza, maior o ss inter (tanto no especifica_ordem quanto no especifica)

# dp 500
plot(dp500_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,1e8))
plot(dp500_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,1e7))
plot(dp500_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,1e6))
plot(dp500_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,1e4))

table(dp500_riqueza_nger5000)
plot(dp500_riqueza_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,10))

plot(I(dp500_ss_inter_nger5000/(dp500_riqueza_nger5000-1))~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,1e8))
plot(dp500_ss_inter_nger5000[which(dp500_riqueza_nger5000==2)]~bat3_indice_dist[which(dp500_riqueza_nger5000==2)],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,1e8))
plot(dp500_ss_inter_nger5000[which(dp500_riqueza_nger5000==2)]~bat3_indice_dist[which(dp500_riqueza_nger5000==2)],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",xlim=c(150000,300000))

matplot(t(pos_comite_dp500_riqueza_temporal)[1:3001,],type="l",col=alpha(colors()[dist_classes2],0.5),lty=1,ylim=c(1,500),ylab="Riqueza",xlab="Ciclo",bty="l",main="Taxa de mutação = 500")
matplot(t(pos_comite_dp500_riqueza_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes2],0.5),lty=1,ylim=c(1,500),ylab="Riqueza",xlab="Ciclo",bty="l",main="Taxa de mutação = 500")
matplot(t(pos_comite_dp500_riqueza_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes2],0.5),lty=1,ylim=c(1,10),ylab="Riqueza",xlab="Ciclo",bty="l",main="Taxa de mutação = 500")

summary(dp500_ciclos_nger5000)
plot(dp500_ciclos_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Ciclo referente à geração 5 mil",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500")
plot(dp500_ciclos_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Ciclo referente à geração 5 mil",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,500))
plot(dp500_ciclos_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Ciclo referente à geração 5 mil",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,100))

# dp 0
plot(dp0_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,1e8))
plot(dp0_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,1e7))
plot(dp0_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,1e6))
plot(dp0_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,1e4))
abline(h=50)

table(dp0_riqueza_nger5000)
plot(dp0_riqueza_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,10))

plot(I(dp0_ss_inter_nger5000/(dp0_riqueza_nger5000-1))~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,1e8))
plot(dp0_ss_inter_nger5000[which(dp0_riqueza_nger5000==2)]~bat3_indice_dist[which(dp0_riqueza_nger5000==2)],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,1e8))
plot(dp0_ss_inter_nger5000[which(dp0_riqueza_nger5000==2)]~bat3_indice_dist[which(dp0_riqueza_nger5000==2)],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",xlim=c(150000,300000))

matplot(t(pos_comite_dp0_riqueza_temporal)[1:3001,],type="l",col=alpha(colors()[dist_classes2],0.5),lty=1,ylim=c(1,500),ylab="Riqueza",xlab="Ciclo",bty="l",main="Taxa de mutação = 0")
matplot(t(pos_comite_dp0_riqueza_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes2],0.5),lty=1,ylim=c(1,500),ylab="Riqueza",xlab="Ciclo",bty="l",main="Taxa de mutação = 0")
matplot(t(pos_comite_dp0_riqueza_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes2],0.5),lty=1,ylim=c(1,10),ylab="Riqueza",xlab="Ciclo",bty="l",main="Taxa de mutação = 0")

summary(dp0_ciclos_nger5000)
plot(dp0_ciclos_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Ciclo referente à geração 5 mil",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0")
plot(dp0_ciclos_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Ciclo referente à geração 5 mil",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,3001))
