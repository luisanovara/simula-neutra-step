# precisa de objetos criados no script "graficos_temporal"
##### GRAFICOS DE ANALISE NO TEMPO DE GERACAO 5000

## mais de uma especie

#dp 0
dp0_ciclos_nger5000 <- apply(pos_comite_dp0_mortes_cumulativas_temporal-25000000,1,function(x){which(abs(x)==min(abs(x)))})
dp0_media_nger5000 <- c()
dp0_ss_total_nger5000 <- c()
dp0_var_total_nger5000 <- c()
dp0_ss_inter_nger5000 <- c()
dp0_var_inter_absoluta_nger5000 <- c()
dp0_var_inter_relativa_nger5000 <- c()
for(i in 1:1000){
  dp0_media_nger5000[i] <- pos_comite_dp0_media_temporal[i,dp0_ciclos_nger5000[i]]
  dp0_ss_total_nger5000[i] <- pos_comite_dp0_ss_total_temporal[i,dp0_ciclos_nger5000[i]]
  dp0_var_total_nger5000[i] <- pos_comite_dp0_var_total_temporal[i,dp0_ciclos_nger5000[i]]
  dp0_ss_inter_nger5000[i] <- pos_comite_dp0_ss_inter_temporal[i,dp0_ciclos_nger5000[i]]
  dp0_var_inter_absoluta_nger5000[i] <- pos_comite_dp0_var_inter_absoluta_temporal[i,dp0_ciclos_nger5000[i]]
  dp0_var_inter_relativa_nger5000[i] <- pos_comite_dp0_var_inter_temporal[i,dp0_ciclos_nger5000[i]]
}

dp0_riqueza_nger5000 <- c()
for(i in 1:1000){
  dp0_riqueza_nger5000[i] <- pos_comite_dp0_riqueza_temporal[i,dp0_ciclos_nger5000[i]]
}


#par(mfrow=c(2,2))
plot(dp0_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",xlab="Índice de distúrbio")
plot(dp0_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(1,20000),main="Taxa de mutação = 0",xlab="Índice de distúrbio")
plot(dp0_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(18000,20000),main="Taxa de mutação = 0",xlab="Índice de distúrbio")
plot(dp0_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(19500,20000),main="Taxa de mutação = 0",xlab="Índice de distúrbio")



plot(dp0_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,4e11),main="Taxa de mutação = 0")
plot(dp0_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,5e8),main="Taxa de mutação = 0")
plot(dp0_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,1e8),main="Taxa de mutação = 0")
plot(dp0_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,1e7),main="Taxa de mutação = 0")
plot(dp0_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,1e4),main="Taxa de mutação = 0")
plot(dp0_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,10),main="Taxa de mutação = 0")

dp0_ss_inter_nger5000[is.nan(dp0_ss_inter_nger5000)]<- 0
plot(dp0_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,4e11))
plot(dp0_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,5e8))
plot(dp0_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,1e8))
plot(dp0_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,1e7))
plot(dp0_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,1e4))
plot(dp0_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 0",ylim=c(0,10))

#plot(dp0_var_total_nger5000~bat3_indice_dist,pch=20,xlab="",ylab="Variância da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)
#plot(dp0_var_inter_absoluta_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Variância interespecífica da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)
#plot(dp0_var_inter_relativa_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Variância interespecífica relativa da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)

#dp 1
dp1_ciclos_nger5000 <- apply(pos_comite_dp1_mortes_cumulativas_temporal-25000000,1,function(x){which(abs(x)==min(abs(x)))})
dp1_media_nger5000 <- c()
dp1_ss_total_nger5000 <- c()
dp1_var_total_nger5000 <- c()
dp1_ss_inter_nger5000 <- c()
dp1_var_inter_absoluta_nger5000 <- c()
dp1_var_inter_relativa_nger5000 <- c()
for(i in 1:1000){
  dp1_media_nger5000[i] <- pos_comite_dp1_media_temporal[i,dp1_ciclos_nger5000[i]]
  dp1_ss_total_nger5000[i] <- pos_comite_dp1_ss_total_temporal[i,dp1_ciclos_nger5000[i]]
  dp1_var_total_nger5000[i] <- pos_comite_dp1_var_total_temporal[i,dp1_ciclos_nger5000[i]]
  dp1_ss_inter_nger5000[i] <- pos_comite_dp1_ss_inter_temporal[i,dp1_ciclos_nger5000[i]]
  dp1_var_inter_absoluta_nger5000[i] <- pos_comite_dp1_var_inter_absoluta_temporal[i,dp1_ciclos_nger5000[i]]
  dp1_var_inter_relativa_nger5000[i] <- pos_comite_dp1_var_inter_temporal[i,dp1_ciclos_nger5000[i]]
}

#par(mfrow=c(2,2))
plot(dp1_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 1",xlab="Índice de distúrbio")
plot(dp1_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(1,20000),main="Taxa de mutação = 1",xlab="Índice de distúrbio")
plot(dp1_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(18000,20000),main="Taxa de mutação = 1",xlab="Índice de distúrbio")
plot(dp1_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(19500,20000),main="Taxa de mutação = 1",xlab="Índice de distúrbio")

plot(dp1_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,4e11),main="Taxa de mutação = 1")
plot(dp1_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,5e8),main="Taxa de mutação = 1")
plot(dp1_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,1e8),main="Taxa de mutação = 1")
plot(dp1_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,1e7),main="Taxa de mutação = 1")
plot(dp1_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,1e4),main="Taxa de mutação = 1")
plot(dp1_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0.8e4,1.2e4),main="Taxa de mutação = 1")

plot(dp1_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 1",ylim=c(0,4e11))
plot(dp1_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 1",ylim=c(0,5e8))
plot(dp1_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 1",ylim=c(0,1e8))
plot(dp1_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 1",ylim=c(0,1e7))
plot(dp1_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 1",ylim=c(0,1e4))
plot(dp1_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 1",ylim=c(0,10))
#plot(dp1_var_total_nger5000~bat3_indice_dist,pch=20,xlab="",ylab="Variância da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)
#plot(dp1_var_inter_absoluta_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Variância interespecífica da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)
#plot(dp1_var_inter_relativa_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Variância interespecífica relativa da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)


#dp 10
dp10_ciclos_nger5000 <- apply(pos_comite_dp10_mortes_cumulativas_temporal-25000000,1,function(x){which(abs(x)==min(abs(x)))})
dp10_media_nger5000 <- c()
dp10_ss_total_nger5000 <- c()
dp10_var_total_nger5000 <- c()
dp10_ss_inter_nger5000 <- c()
dp10_var_inter_absoluta_nger5000 <- c()
dp10_var_inter_relativa_nger5000 <- c()
for(i in 1:1000){
  dp10_media_nger5000[i] <- pos_comite_dp10_media_temporal[i,dp10_ciclos_nger5000[i]]
  dp10_ss_total_nger5000[i] <- pos_comite_dp10_ss_total_temporal[i,dp10_ciclos_nger5000[i]]
  dp10_var_total_nger5000[i] <- pos_comite_dp10_var_total_temporal[i,dp10_ciclos_nger5000[i]]
  dp10_ss_inter_nger5000[i] <- pos_comite_dp10_ss_inter_temporal[i,dp10_ciclos_nger5000[i]]
  dp10_var_inter_absoluta_nger5000[i] <- pos_comite_dp10_var_inter_absoluta_temporal[i,dp10_ciclos_nger5000[i]]
  dp10_var_inter_relativa_nger5000[i] <- pos_comite_dp10_var_inter_temporal[i,dp10_ciclos_nger5000[i]]
}

#par(mfrow=c(2,2))
plot(dp10_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 10",xlab="Índice de distúrbio")
plot(dp10_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(1,20000),main="Taxa de mutação = 10",xlab="Índice de distúrbio")
plot(dp10_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(18000,20000),main="Taxa de mutação = 10",xlab="Índice de distúrbio")
plot(dp10_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(19500,20000),main="Taxa de mutação = 10",xlab="Índice de distúrbio")

plot(dp10_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,4e11),main="Taxa de mutação = 10")
plot(dp10_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,5e8),main="Taxa de mutação = 10")
plot(dp10_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,1e8),main="Taxa de mutação = 10")
plot(dp10_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,1e7),main="Taxa de mutação = 10")
plot(dp10_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,2e6),main="Taxa de mutação = 10")


plot(dp10_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 10",ylim=c(0,4e11))
plot(dp10_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 10",ylim=c(0,5e8))
plot(dp10_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 10",ylim=c(0,1e8))
plot(dp10_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 10",ylim=c(0,1e7))
plot(dp10_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 10",ylim=c(0,1e4))
plot(dp10_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 10",ylim=c(0,10))
#plot(dp10_var_total_nger5000~bat3_indice_dist,pch=20,xlab="",ylab="Variância da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)
#plot(dp10_var_inter_absoluta_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Variância interespecífica da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)
#plot(dp10_var_inter_relativa_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Variância interespecífica relativa da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)


#dp 100
dp100_ciclos_nger5000 <- apply(pos_comite_dp100_mortes_cumulativas_temporal-25000000,1,function(x){which(abs(x)==min(abs(x)))})
dp100_ciclos_nger5000 <- as.integer(dp100_ciclos_nger5000)
dp100_media_nger5000 <- c()
dp100_ss_total_nger5000 <- c()
dp100_var_total_nger5000 <- c()
dp100_ss_inter_nger5000 <- c()
dp100_var_inter_absoluta_nger5000 <- c()
dp100_var_inter_relativa_nger5000 <- c()
for(i in 1:1000){
  dp100_media_nger5000[i] <- pos_comite_dp100_media_temporal[i,dp100_ciclos_nger5000[i]]
  dp100_ss_total_nger5000[i] <- pos_comite_dp100_ss_total_temporal[i,dp100_ciclos_nger5000[i]]
  dp100_var_total_nger5000[i] <- pos_comite_dp100_var_total_temporal[i,dp100_ciclos_nger5000[i]]
  dp100_ss_inter_nger5000[i] <- pos_comite_dp100_ss_inter_temporal[i,dp100_ciclos_nger5000[i]]
  dp100_var_inter_absoluta_nger5000[i] <- pos_comite_dp100_var_inter_absoluta_temporal[i,dp100_ciclos_nger5000[i]]
  dp100_var_inter_relativa_nger5000[i] <- pos_comite_dp100_var_inter_temporal[i,dp100_ciclos_nger5000[i]]
}

#par(mfrow=c(2,2))
plot(dp100_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 100",xlab="Índice de distúrbio")
plot(dp100_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(1,20000),main="Taxa de mutação = 100",xlab="Índice de distúrbio")
plot(dp100_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(15000,20000),main="Taxa de mutação = 100",xlab="Índice de distúrbio")

plot(dp100_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,4e11),main="Taxa de mutação = 100")
plot(dp100_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,5e8),main="Taxa de mutação = 100")
plot(dp100_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,1e8),main="Taxa de mutação = 100")
plot(dp100_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0.5e8,2.5e8),main="Taxa de mutação = 100")


plot(dp100_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 100",ylim=c(0,4e11))
plot(dp100_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 100",ylim=c(0,5e8))
plot(dp100_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 100",ylim=c(0,1e8))
plot(dp100_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 100",ylim=c(0,1e7))
plot(dp100_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 100",ylim=c(0,1e4))
plot(dp100_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 100",ylim=c(0,10))
#plot(dp100_var_total_nger5000~bat3_indice_dist,pch=20,xlab="",ylab="Variância da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)
#plot(dp100_var_inter_absoluta_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Variância interespecífica da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)
#plot(dp100_var_inter_relativa_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Variância interespecífica relativa da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)

#dp 500
dp500_ciclos_nger5000 <- apply(pos_comite_dp500_mortes_cumulativas_temporal-25000000,1,function(x){which(abs(x)==min(abs(x)))})
dp500_media_nger5000 <- c()
dp500_ss_total_nger5000 <- c()
dp500_var_total_nger5000 <- c()
dp500_ss_inter_nger5000 <- c()
dp500_var_inter_absoluta_nger5000 <- c()
dp500_var_inter_relativa_nger5000 <- c()
for(i in 1:1000){
  dp500_media_nger5000[i] <- pos_comite_dp500_media_temporal[i,dp500_ciclos_nger5000[i]]
  dp500_ss_total_nger5000[i] <- pos_comite_dp500_ss_total_temporal[i,dp500_ciclos_nger5000[i]]
  dp500_var_total_nger5000[i] <- pos_comite_dp500_var_total_temporal[i,dp500_ciclos_nger5000[i]]
  dp500_ss_inter_nger5000[i] <- pos_comite_dp500_ss_inter_temporal[i,dp500_ciclos_nger5000[i]]
  dp500_var_inter_absoluta_nger5000[i] <- pos_comite_dp500_var_inter_absoluta_temporal[i,dp500_ciclos_nger5000[i]]
  dp500_var_inter_relativa_nger5000[i] <- pos_comite_dp500_var_inter_temporal[i,dp500_ciclos_nger5000[i]]
}

dp500_riqueza_nger5000 <- c()
for(i in 1:1000){
  dp500_riqueza_nger5000[i] <- pos_comite_dp500_riqueza_temporal[i,dp500_ciclos_nger5000[i]]
}

#par(mfrow=c(2,2))
plot(dp500_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",xlab="Índice de distúrbio")
plot(dp500_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(1,20000),main="Taxa de mutação = 500",xlab="Índice de distúrbio")

plot(dp500_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,4e11),main="Taxa de mutação = 500")
plot(dp500_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,5e8),main="Taxa de mutação = 500")
plot(dp500_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,30e8),main="Taxa de mutação = 500")
plot(dp500_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(2.5e8,2.7e9),main="Taxa de mutação = 500")


plot(dp500_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,4e11))
plot(dp500_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,5e8))
plot(dp500_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,1e8))
plot(dp500_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,1e7))
plot(dp500_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,1e4))
plot(dp500_ss_inter_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático interespecífico da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500",ylim=c(0,10))
#plot(dp500_var_total_nger5000~bat3_indice_dist,pch=20,xlab="",ylab="Variância da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)
#plot(dp500_var_inter_absoluta_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Variância interespecífica da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)
#plot(dp500_var_inter_relativa_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Variância interespecífica relativa da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8)