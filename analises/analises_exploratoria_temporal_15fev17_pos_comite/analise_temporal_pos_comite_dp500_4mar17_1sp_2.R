##############################################################
###################### Analise temporal ######################
##############################################################

# Simulacoes pos comite

load("pos_comite_18mar17_dp500_1sp_media_temporal_lahr.RData")
load("pos_comite_18mar17_dp500_1sp_ss_total_temporal_lahr.RData")
load("pos_comite_18mar17_dp500_1sp_mortes_cumulativas_temporal_lahr.RData")
for(i in seq(393,992,12)){ 
  load(paste("resultados18mar17_dp500_1sp_",i,"-",i+11,".RData",sep=""))
  x<-resultados
  media_12 <- matrix(ncol=3001,nrow=12)
  ss_total_12 <- matrix(ncol=3001,nrow=12)
  mortes_12 <- matrix(ncol=3001,nrow=12)
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
    media_12[j,] <- media
    ss_total_12[j,] <- ss_total
    mortes_12[j,] <- x[[j]]$n.mortes.cumulativo
  }
  pos_comite_dp500_1sp_media_temporal[i:(i+11),] <- media_12
  pos_comite_dp500_1sp_ss_total_temporal[i:(i+11),] <- ss_total_12
  pos_comite_dp500_1sp_mortes_cumulativas_temporal[i:(i+11),] <- mortes_12
}
save(pos_comite_dp500_1sp_media_temporal,file="pos_comite_18mar17_dp500_1sp_media_temporal_lahr.RData")
save(pos_comite_dp500_1sp_ss_total_temporal,file="pos_comite_18mar17_dp500_1sp_ss_total_temporal_lahr.RData")
save(pos_comite_dp500_1sp_mortes_cumulativas_temporal,file="pos_comite_18mar17_dp500_1sp_mortes_cumulativas_temporal_lahr.RData")