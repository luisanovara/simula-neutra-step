# taxa de mutacao 500, >1sp, ntotal 5000
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,5000))
par(new=T)
matplot(y=t(pos_comite_dp500_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes3],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,5000))

matplot(y=t(pos_comite_dp500_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes3],0.5),lty=1,ylim=c(1,500),ylab="Riqueza",xlab="Geração",bty="l",main="",xlim=c(0,5000))
matplot(y=t(pos_comite_dp500_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes3],0.5),lty=1,ylim=c(1,50),ylab="Riqueza",xlab="Geração",bty="l",main="",xlim=c(0,5000))

# se usar as sim com 20 mil individuos...
matplot(y=t(pos_comite_dp500_mais_ind_riqueza_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes3_mais],0.5),lty=1,ylim=c(1,50),ylab="Riqueza",xlab="Geração",bty="l",main="",xlim=c(0,5000))
matplot(y=t(pos_comite_dp500_mais_ind_media_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 20 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,5000))

# dp 500, mas com uma especie
matplot(y=t(pos_comite_dp500_1sp_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza = 1",cex.axis=0.8,xlim=c(0,5000))

# e dp 0
matplot(y=t(pos_comite_dp0_media_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 0, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,5000))


#### GRAFICOS COM A VARIAVEL X DISTURBIO
## media
# dp 500
plot(dp500_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",xlab="Índice de distúrbio")
plot(dp500_media_nger2000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",xlab="Índice de distúrbio",ylim=c(1,20000))
# dp 0
plot(dp0_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",xlab="Índice de distúrbio")
plot(dp0_media_nger2000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",xlab="Índice de distúrbio",ylim=c(1,20000))

## riqueza
# dp 500
plot(dp500_riqueza_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Riqueza após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,500))
plot(dp500_riqueza_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Riqueza após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,10))
plot(dp500_riqueza_nger2000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Riqueza após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,500))
plot(dp500_riqueza_nger2000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Riqueza após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,10))
# dp 0
plot(dp0_riqueza_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Riqueza após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,500))
plot(dp0_riqueza_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Riqueza após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,10))
plot(dp0_riqueza_nger2000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Riqueza após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,500))
plot(dp0_riqueza_nger2000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Riqueza após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,10))

## ss total e ss inter
# dp 500
#plot(dp500_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,30e8))
plot(dp500_ss_total_nger2000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,30e8))
#plot(dp500_ss_inter_nger5000[which(dp500_riqueza_nger5000==2)]~bat3_indice_dist[which(dp500_riqueza_nger5000==2)],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,30e8))
plot(dp500_ss_inter_nger2000[which(dp500_riqueza_nger2000==2)]~bat3_indice_dist[which(dp500_riqueza_nger2000==2)],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,30e8))
plot(I(dp500_ss_inter_nger2000[which(dp500_riqueza_nger2000==2)]/dp500_ss_total_nger2000[which(dp500_riqueza_nger2000==2)])~bat3_indice_dist[which(dp500_riqueza_nger2000==2)],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico relativo após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,1))

# dp 0
#plot(dp0_ss_total_nger5000[dp0_riqueza_nger5000==2]~bat3_indice_dist[dp0_riqueza_nger5000==2],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,2e8))
plot(dp0_ss_total_nger2000[dp0_riqueza_nger2000==2]~bat3_indice_dist[dp0_riqueza_nger2000==2],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,30e8))
#plot(dp0_ss_inter_nger5000[dp0_riqueza_nger5000==2]~bat3_indice_dist[dp0_riqueza_nger5000==2],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,30e8))
plot(dp0_ss_inter_nger2000[dp0_riqueza_nger2000==2]~bat3_indice_dist[dp0_riqueza_nger2000==2],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,30e8))
plot(I(dp0_ss_inter_nger2000[which(dp0_riqueza_nger2000==2)]/dp0_ss_total_nger2000[which(dp0_riqueza_nger2000==2)])~bat3_indice_dist[which(dp0_riqueza_nger2000==2)],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico relativo após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,1))

# graficos sugerido pelo pi
plot(I(dp500_ss_inter_nger2000[which(dp500_riqueza_nger2000==2)]/dp500_ss_total_nger2000[which(dp500_riqueza_nger2000==2)])~bat3_indice_dist[which(dp500_riqueza_nger2000==2)],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico relativo após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500")





plot(dp500_mais_ind_ss_inter_nger2000[which(dp500_mais_ind_riqueza_nger2000==2)]~bat_indice_dist[which(dp500_mais_ind_riqueza_nger2000==2)],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500, n = 20 mil")
plot(I(dp500_mais_ind_ss_inter_nger2000[which(dp500_mais_ind_riqueza_nger2000==2)]/dp500_mais_ind_ss_total_nger2000[which(dp500_mais_ind_riqueza_nger2000==2)])~bat_indice_dist[which(dp500_mais_ind_riqueza_nger2000==2)],pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico relativo após 2 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500, n = 20 mil")
