##############################################################
###################### Analise temporal ######################
##############################################################

# Simulacoes pos comite

### variaveis resposta 
pos_comite_dp500_1sp_media_temporal <- matrix(ncol=3001,nrow=1000)
pos_comite_dp500_1sp_ss_total_temporal <- matrix(ncol=3001,nrow=1000)
pos_comite_dp500_1sp_var_total_temporal <- matrix(ncol=3001,nrow=1000)
pos_comite_dp500_1sp_ss_inter_temporal <- matrix(ncol=3001,nrow=1000)
pos_comite_dp500_1sp_var_inter_temporal <- matrix(ncol=3001,nrow=1000)
pos_comite_dp500_1sp_riqueza_temporal <- matrix(ncol=3001,nrow=1000)
pos_comite_dp500_1sp_mortes_cumulativas_temporal <- matrix(ncol=3001,nrow=1000)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados4mar17_dp500_1sp_",i,"-",i+7,".RData",sep=""))
  x<-resultados
  media_8 <- matrix(ncol=3001,nrow=8)
  ss_total_8 <- matrix(ncol=3001,nrow=8)
  var_total_8 <- matrix(ncol=3001,nrow=8)
  ss_inter_8 <- matrix(ncol=3001,nrow=8)
  var_inter_8 <- matrix(ncol=3001,nrow=8)
  riqueza_8 <- matrix(ncol=3001,nrow=8)
  mortes_8 <- matrix(ncol=3001,nrow=8)
  for(j in 1:length(x)){
    prop_ind <- x[[j]]$sementes
    sp_ind <- x[[j]]$sp.list
    # media
    media <- apply(prop_ind,2,mean,na.rm=T)
    # variancia total
    var_total <- apply(prop_ind,2,var,na.rm=T)
    # ss total e ss inter
    media_sp <- matrix(ncol=3001,nrow=length(unique(sp_ind[,1])))
    tam_sp <- matrix(ncol=3001,nrow=length(unique(sp_ind[,1])))
    ss_inter <- matrix(ncol=3001,nrow=1)
    ss_total <- matrix(ncol=3001,nrow=1)
    for(k in 1:3001){
      temporario <- tapply(prop_ind[,k],sp_ind[,k],mean)
      media_sp[as.numeric(names(temporario)),k] <- temporario
      temporario2 <- tapply(prop_ind[,k],sp_ind[,k],length)
      tam_sp[as.numeric(names(temporario2)),k] <- temporario2
      ss_inter[1,k] <- sum(((temporario-media[k])^2)*temporario2)
      ss_total[1,k] <- sum((prop_ind[,k]-media[k])^2)
    }
    ss_total <- as.vector(ss_total)
    ss_inter <- as.vector(ss_inter)
    # riqueza
    riqueza <- apply(sp_ind,2,function(y){length(unique(y))})
    # variancia inter
    var_inter <- ss_inter/(riqueza-1)
    var_inter <- as.vector(var_inter)
    var_inter[which(is.nan(var_inter)==T)]<-NA
    # salvando
    media_8[j,] <- media
    ss_total_8[j,] <- ss_total
    var_total_8[j,] <- var_total
    ss_inter_8[j,] <- ss_inter
    var_inter_8[j,] <- var_inter
    riqueza_8[j,] <- riqueza
    mortes_8[j,] <- x[[j]]$n.mortes.cumulativo
  }
  pos_comite_dp500_1sp_media_temporal[i:(i+7),] <- media_8
  pos_comite_dp500_1sp_ss_total_temporal[i:(i+7),] <- ss_total_8
  pos_comite_dp500_1sp_var_total_temporal[i:(i+7),] <- var_total_8
  pos_comite_dp500_1sp_ss_inter_temporal[i:(i+7),] <- ss_inter_8
  pos_comite_dp500_1sp_var_inter_temporal[i:(i+7),] <- var_inter_8
  pos_comite_dp500_1sp_riqueza_temporal[i:(i+7),] <- riqueza_8
  pos_comite_dp500_1sp_mortes_cumulativas_temporal[i:(i+7),] <- mortes_8
}
save(pos_comite_dp500_1sp_media_temporal,file="pos_comite_4mar17_dp500_1sp_media_temporal.RData")
save(pos_comite_dp500_1sp_ss_total_temporal,file="pos_comite_4mar17_dp500_1sp_ss_total_temporal.RData")
save(pos_comite_dp500_1sp_var_total_temporal,file="pos_comite_4mar17_dp500_1sp_var_total_temporal.RData")
save(pos_comite_dp500_1sp0_ss_inter_temporal,file="pos_comite_4mar17_dp500_1sp_ss_inter_temporal.RData")
save(pos_comite_dp500_1sp_var_inter_temporal,file="pos_comite_4mar17_dp500_1sp_var_inter_temporal.RData")
save(pos_comite_dp500_1sp_riqueza_temporal,file="pos_comite_4mar17_dp500_1sp_riqueza_temporal.RData")
save(pos_comite_dp500_1sp_mortes_cumulativas_temporal,file="pos_comite_4mar17_dp500_1sp_mortes_cumulativas_temporal.RData")
