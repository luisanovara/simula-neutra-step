##############################################################
###################### Analise temporal ######################
##############################################################

# Simulacoes pos comite

### variaveis resposta 
#pos_comite_dp0_media_temporal <- matrix(ncol=3001,nrow=1000)
#pos_comite_dp0_var_total_temporal <- matrix(ncol=3001,nrow=1000)
#pos_comite_dp0_var_inter_temporal <- matrix(ncol=3001,nrow=1000)
#pos_comite_dp100_riqueza_temporal <- matrix(ncol=3001,nrow=1000)
#pos_comite_dp100_mortes_cumulativas_temporal <- matrix(ncol=3001,nrow=1000)
#load("pos_comite_15fev17_dp10_media_temporal.RData")
#load("pos_comite_15fev17_dp10_var_total_temporal.RData")
#load("pos_comite_15fev17_dp10_var_inter_temporal.RData")
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados15fev17_dp100_-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  #media_indice_8 <- matrix(ncol=3001,nrow=8)
  #var_total_indice_8 <- matrix(ncol=3001,nrow=8)
  #var_inter_indice_8 <- matrix(ncol=3001,nrow=8)
  riqueza_8 <- matrix(ncol=3001,nrow=8)
  mortes_8 <- matrix(ncol=3001,nrow=8)
  for(j in 1:length(x)){
    # media total
    #prop_ind <- x[[j]]$sementes
    #prop_com <- apply(prop_ind,2,mean,na.rm=T)
    # variancia total
    #var_com <- apply(prop_ind,2,var,na.rm=T)
    # variancia inter relativa
    sp_ind <- x[[j]]$sp.list
    #prop_sp <- matrix(ncol=3001,nrow=length(unique(sp_ind[,1])))
    #tam_sp <- matrix(ncol=3001,nrow=length(unique(sp_ind[,1])))
    #desv_entre <- matrix(ncol=3001,nrow=1)
    #for(k in 1:3001){
      #temporario <- tapply(prop_ind[,k],sp_ind[,k],mean)
      #prop_sp[as.numeric(names(temporario)),k] <- temporario
      #temporario2 <- tapply(prop_ind[,k],sp_ind[,k],length)
      #tam_sp[as.numeric(names(temporario2)),k] <- temporario2
      #desv_entre[1,k] <- sum(((temporario-prop_com[k])^2)*temporario2)/(length(unique(sp_ind[,k]))-1)
    #}
    #desv_entre[which(is.nan(desv_entre)==T)]<-0
    # riqueza
    riqueza <- apply(sp_ind,2,function(y){length(unique(y))})
    #media_indice_8[j,] <- prop_com
    #var_total_indice_8[j,] <- var_com
    #var_inter_indice_8[j,] <- desv_entre/var_com
    riqueza_8[j,] <- riqueza
    mortes_8[j,] <- x[[j]]$n.mortes.cumulativo
  }
  #pos_comite_dp0_media_temporal[i:(i+7),] <- media_indice_8
  #pos_comite_dp0_var_total_temporal[i:(i+7),] <- var_total_indice_8
  #pos_comite_dp0_var_inter_temporal[i:(i+7),] <- var_inter_indice_8
  pos_comite_dp100_riqueza_temporal[i:(i+7),] <- riqueza_8
  pos_comite_dp100_mortes_cumulativas_temporal[i:(i+7),] <- mortes_8
}
#save(pos_comite_dp0_media_temporal,file="pos_comite_15fev17_dp0_media_temporal.RData")
#save(pos_comite_dp0_var_total_temporal,file="pos_comite_15fev17_dp0_var_total_temporal.RData")
#save(pos_comite_dp0_var_inter_temporal,file="pos_comite_15fev17_dp0_var_inter_temporal.RData")
save(pos_comite_dp100_riqueza_temporal,file="pos_comite_15fev17_dp100_riqueza_temporal.RData")
save(pos_comite_dp100_mortes_cumulativas_temporal,file="pos_comite_15fev17_dp100_mortes_cumulativas_temporal.RData")
