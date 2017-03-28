# calculando 1) o ss inter (com ponderacao pelas abundancias das especies), 2) o ss inter (sem ponderacao pelas abundancias das especies) e 3) o var inter (q seria o ss inter sem ponderacao dividido pela riqueza-1)

pos_comite_dp500_ss_inter_ponderada_temporal <- matrix(ncol=3001,nrow=1000)
pos_comite_dp500_ss_inter_nao_ponderada_temporal <- matrix(ncol=3001,nrow=1000)
pos_comite_dp500_var_inter_temporal <- matrix(ncol=3001,nrow=1000)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados15fev17-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  ss_inter_ponderada_8 <- matrix(ncol=3001,nrow=8)
  ss_inter_nao_ponderada_8 <- matrix(ncol=3001,nrow=8)
  var_inter_8 <- matrix(ncol=3001,nrow=8)
  for(j in 1:length(x)){
    prop_ind <- x[[j]]$sementes
    sp_ind <- x[[j]]$sp.list
    prop_com <- apply(prop_ind,2,mean,na.rm=T)
    ss_inter_ponderada <- matrix(ncol=3001,nrow=1)
    ss_inter_nao_ponderada <- matrix(ncol=3001,nrow=1)
    var_inter <- matrix(ncol=3001,nrow=1)
    for(k in 1:3001){
    prop_sp <- tapply(prop_ind[,k],sp_ind[,k],mean)
    tam_sp <- tapply(prop_ind[,k],sp_ind[,k],length)
    ss_inter_ponderada[1,k] <- sum(((prop_sp-prop_com[k])^2)*tam_sp)
    ss_inter_nao_ponderada[1,k] <- sum((prop_sp-prop_com[k])^2)
    var_inter[1,k] <- sum((prop_sp-prop_com[k])^2)/(length(unique(sp_ind[,k]))-1)
    }
    ss_inter_ponderada_8[j,] <- ss_inter_ponderada
    ss_inter_nao_ponderada_8[j,] <- ss_inter_nao_ponderada
    var_inter_8[j,] <- var_inter
  }
  pos_comite_dp500_ss_inter_ponderada_temporal[i:(i+7),]<-ss_inter_ponderada_8
  pos_comite_dp500_ss_inter_nao_ponderada_temporal[i:(i+7),]<-ss_inter_nao_ponderada_8
  pos_comite_dp500_var_inter_temporal[i:(i+7),]<-var_inter_8
}
save(pos_comite_dp500_ss_inter_ponderada_temporal,file="pos_comite_15fev17_dp500_ss_inter_ponderada_temporal.RData")
save(pos_comite_dp500_ss_inter_nao_ponderada_temporal,file="pos_comite_15fev17_dp500_ss_inter_nao_ponderada_temporal.RData")
save(pos_comite_dp500_var_inter_temporal,file="pos_comite_15fev17_dp500_var_inter_temporal.RData")