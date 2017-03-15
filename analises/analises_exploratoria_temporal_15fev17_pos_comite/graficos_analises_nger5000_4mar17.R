# precisa de objetos criados no script "graficos_temporal"
##### GRAFICOS DE ANALISE NO TEMPO DE GERACAO 5000

## uma especie

#dp 500
dp500_1sp_ciclos_nger5000 <- apply(pos_comite_dp500_1sp_mortes_cumulativas_temporal-25000000,1,function(x){which(abs(x)==min(abs(x)))})
dp500_1sp_media_nger5000 <- c()
dp500_1sp_ss_total_nger5000 <- c()
dp500_1sp_var_total_nger5000 <- c()
dp500_1sp_ss_inter_nger5000 <- c()
dp500_1sp_var_inter_nger5000 <- c()
dp500_1sp_riqueza_nger5000 <- c()
for(i in 1:1000){
  dp500_1sp_media_nger5000[i] <- pos_comite_dp500_1sp_media_temporal[i,dp500_1sp_ciclos_nger5000[i]]
  dp500_1sp_ss_total_nger5000[i] <- pos_comite_dp500_1sp_ss_total_temporal[i,dp500_1sp_ciclos_nger5000[i]]
  dp500_1sp_var_total_nger5000[i] <- pos_comite_dp500_1sp_var_total_temporal[i,dp500_1sp_ciclos_nger5000[i]]
  dp500_1sp_ss_inter_nger5000[i] <- pos_comite_dp500_1sp_ss_inter_temporal[i,dp500_1sp_ciclos_nger5000[i]]
  dp500_1sp_var_inter_nger5000[i] <- pos_comite_dp500_1sp_var_inter_temporal[i,dp500_1sp_ciclos_nger5000[i]]
  dp500_1sp_riqueza_nger5000[i] <- pos_comite_dp500_1sp_riqueza_temporal[i,dp500_1sp_ciclos_nger5000[i]]
}

#par(mfrow=c(2,2))
plot(dp500_1sp_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,main="Taxa de mutação = 500 e riqueza = 1",xlab="Índice de distúrbio")
plot(dp500_1sp_media_nger5000~bat3_indice_dist,pch=20,ylab="Estratégia de vida média após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(1,20000),main="Taxa de mutação = 500 e riqueza = 1",xlab="Índice de distúrbio")

plot(dp500_1sp_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,4e11),main="Taxa de mutação = 500 e riqueza = 1")
plot(dp500_1sp_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,5e8),main="Taxa de mutação = 500 e riqueza = 1")
plot(dp500_1sp_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(0,30e8),main="Taxa de mutação = 500 e riqueza = 1")
plot(dp500_1sp_ss_total_nger5000~bat3_indice_dist,pch=20,xlab="Índice de distúrbio",ylab="Desvio quadrático total da estratégia de vida após 5 mil gerações",bty="l",las=1,cex.axis=0.8,ylim=c(2.5e8,2.7e9),main="Taxa de mutação = 500 e riqueza = 1")

# ntotal 20 mil