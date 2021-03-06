##############################################################
###################### Analise temporal ######################
##############################################################

# Simulacoes pos comite

### variaveis resposta 
pos_comite_dp500_1sp_media_temporal <- matrix(ncol=3001,nrow=1000)
pos_comite_dp500_1sp_ss_total_temporal <- matrix(ncol=3001,nrow=1000)
pos_comite_dp500_1sp_mortes_cumulativas_temporal <- matrix(ncol=3001,nrow=1000)
for(i in c(seq(1,392,8),993)){ 
  load(paste("resultados18mar17_dp500_1sp_",i,"-",i+7,".RData",sep=""))
  x<-resultados
  media_8 <- matrix(ncol=3001,nrow=8)
  ss_total_8 <- matrix(ncol=3001,nrow=8)
  mortes_8 <- matrix(ncol=3001,nrow=8)
  for(j in 1:length(x)){
    prop_ind <- x[[j]]$sementes
    # media
    media <- apply(prop_ind,2,mean,na.rm=T)
    # ss total
    ss_total <- matrix(ncol=3001,nrow=1)
    for(k in 1:3001){
      ss_total[1,k] <- sum((prop_ind[,k]-media[k])^2)
    }
    ss_total <- as.vector(ss_total)
    # salvando
    media_8[j,] <- media
    ss_total_8[j,] <- ss_total
    mortes_8[j,] <- x[[j]]$n.mortes.cumulativo
  }
  pos_comite_dp500_1sp_media_temporal[i:(i+7),] <- media_8
  pos_comite_dp500_1sp_ss_total_temporal[i:(i+7),] <- ss_total_8
  pos_comite_dp500_1sp_mortes_cumulativas_temporal[i:(i+7),] <- mortes_8
}
save(pos_comite_dp500_1sp_media_temporal,file="pos_comite_18mar17_dp500_1sp_media_temporal_lahr.RData")
save(pos_comite_dp500_1sp_ss_total_temporal,file="pos_comite_18mar17_dp500_1sp_ss_total_temporal_lahr.RData")
save(pos_comite_dp500_1sp_mortes_cumulativas_temporal,file="pos_comite_18mar17_dp500_1sp_mortes_cumulativas_temporal_lahr.RData")