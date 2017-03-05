#########################################################
#########################################################
############### Outputs brutos para tempo ###############
###################### de meia vida #####################
#########################################################
#########################################################

####################### Bateria 2 #######################

bat2_prop_meiavida_geral <- matrix(ncol=1000,nrow=5500)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_meiavida_8 <- matrix(ncol=8,nrow=5500)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    meia_riq <- riq_temp[1]/2
    meia_vida <- max(which(abs(riq_temp-meia_riq)==min(abs(riq_temp-meia_riq))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    prop_meiavida <- prop[,meia_vida]
    prop_meiavida_8[,j] <- c(prop_meiavida,rep(NA,5500-tam_com))
  }
  bat2_prop_meiavida_geral[,i:(i+7)] <- prop_meiavida_8
}
save(bat2_prop_meiavida_geral,file="bat2_prop_meiavida_geral.RData")


bat2_sp_meiavida_geral <- matrix(ncol=1000,nrow=5500)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  sp_meiavida_8 <- matrix(ncol=8,nrow=5500)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    meia_riq <- riq_temp[1]/2
    meia_vida <- max(which(abs(riq_temp-meia_riq)==min(abs(riq_temp-meia_riq))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    sp <- as.matrix(x[[j]]$sp.list)
    sp_meiavida <- sp[,meia_vida]
    sp_meiavida_8[,j] <- c(sp_meiavida,rep(NA,5500-tam_com))
  }
  bat2_sp_meiavida_geral[,i:(i+7)] <- sp_meiavida_8
}
save(bat2_sp_meiavida_geral,file="bat2_sp_meiavida_geral.RData")

bat2_meia_vida <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  meia_vida_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    meia_riq <- riq_temp[1]/2
    meia_vida <- max(which(abs(riq_temp-meia_riq)==min(abs(riq_temp-meia_riq))))
    meia_vida_8[,j] <- meia_vida
  }
  bat2_meia_vida[,i:(i+7)] <- meia_vida_8
}
save(bat2_meia_vida,file="bat2_meia_vida.RData")

####################### Bateria 3 #######################

bat3_prop_meiavida_geral <- matrix(ncol=1000,nrow=5500)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_meiavida_8 <- matrix(ncol=8,nrow=5500)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    meia_riq <- riq_temp[1]/2
    meia_vida <- max(which(abs(riq_temp-meia_riq)==min(abs(riq_temp-meia_riq))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    prop_meiavida <- prop[,meia_vida]
    prop_meiavida_8[,j] <- c(prop_meiavida,rep(NA,5500-tam_com))
  }
  bat3_prop_meiavida_geral[,i:(i+7)] <- prop_meiavida_8
}
save(bat3_prop_meiavida_geral,file="bat3_prop_meiavida_geral.RData")

bat3_sp_meiavida_geral <- matrix(ncol=1000,nrow=5500)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  sp_meiavida_8 <- matrix(ncol=8,nrow=5500)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    meia_riq <- riq_temp[1]/2
    meia_vida <- max(which(abs(riq_temp-meia_riq)==min(abs(riq_temp-meia_riq))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    sp <- as.matrix(x[[j]]$sp.list)
    sp_meiavida <- sp[,meia_vida]
    sp_meiavida_8[,j] <- c(sp_meiavida,rep(NA,5500-tam_com))
  }
  bat3_sp_meiavida_geral[,i:(i+7)] <- sp_meiavida_8
}
save(bat3_sp_meiavida_geral,file="bat3_sp_meiavida_geral.RData")

bat3_meia_vida <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  meia_vida_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    meia_riq <- riq_temp[1]/2
    meia_vida <- max(which(abs(riq_temp-meia_riq)==min(abs(riq_temp-meia_riq))))
    meia_vida_8[,j] <- meia_vida
  }
  bat3_meia_vida[,i:(i+7)] <- meia_vida_8
}
save(bat3_meia_vida,file="bat3_meia_vida.RData")


#########################################################
#########################################################
############### Outputs brutos para tempo ###############
#################### de geracao 5000 ####################
#########################################################
#########################################################

####################### Bateria 2 #######################

bat2_nger <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  nger_8<- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    mortes_acumuladas <- x[[j]]$n.mortes.cumulativo
    n.mortes_prox_nger <- mortes_acumuladas - (nger*tam_com)
    ngeracao <- which(abs(n.mortes_prox_nger)==min(abs(n.mortes_prox_nger)))
    nger_8[,j] <- ngeracao
  }
  bat2_nger[,i:(i+7)] <- nger_8
}

####################### Bateria 3 #######################

bat3_nger <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  nger_8<- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    mortes_acumuladas <- x[[j]]$n.mortes.cumulativo
    n.mortes_prox_nger <- mortes_acumuladas - (nger*tam_com)
    ngeracao <- which(abs(n.mortes_prox_nger)==min(abs(n.mortes_prox_nger)))
    nger_8[,j] <- ngeracao
  }
  bat3_nger[,i:(i+7)] <- nger_8
}

#########################################################
#########################################################
########### Outputs brutos para outros tempos ###########
#########################################################
#########################################################

bat2_prop_90_geral <- matrix(ncol=1000,nrow=5500)
bat2_sp_90_geral <- matrix(ncol=1000,nrow=5500)
bat2_temp_90 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_90_8 <- matrix(ncol=8,nrow=5500)
  sp_90_8 <- matrix(ncol=8,nrow=5500)
  temp_90_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_90 <- riq_temp[1]*0.9
    temp_90 <- max(which(abs(riq_temp-riq_90)==min(abs(riq_temp-riq_90))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_90 <- prop[,temp_90]
    sp_90 <- sp[,temp_90]
    prop_90_8[,j] <- c(prop_90,rep(NA,5500-tam_com))
    sp_90_8[,j] <- c(sp_90,rep(NA,5500-tam_com))
    temp_90_8[,j] <- temp_90
  }
  bat2_prop_90_geral[,i:(i+7)] <- prop_90_8
  bat2_sp_90_geral[,i:(i+7)] <- sp_90_8
  bat2_temp_90[,i:(i+7)] <- temp_90_8
}
save(c(bat2_prop_90_geral,bat2_sp_90_geral,bat2_temp_90),file="bat2_prop_sp_temp_90_geral.RData")

bat3_prop_90_geral <- matrix(ncol=1000,nrow=5500)
bat3_sp_90_geral <- matrix(ncol=1000,nrow=5500)
bat3_temp_90 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_90_8 <- matrix(ncol=8,nrow=5500)
  sp_90_8 <- matrix(ncol=8,nrow=5500)
  temp_90_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_90 <- riq_temp[1]*0.9
    temp_90 <- max(which(abs(riq_temp-riq_90)==min(abs(riq_temp-riq_90))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_90 <- prop[,temp_90]
    sp_90 <- sp[,temp_90]
    prop_90_8[,j] <- c(prop_90,rep(NA,5500-tam_com))
    sp_90_8[,j] <- c(sp_90,rep(NA,5500-tam_com))
    temp_90_8[,j] <- temp_90
  }
  bat3_prop_90_geral[,i:(i+7)] <- prop_90_8
  bat3_sp_90_geral[,i:(i+7)] <- sp_90_8
  bat3_temp_90[,i:(i+7)] <- temp_90_8
}
save(c(bat3_prop_90_geral,bat3_sp_90_geral,bat3_temp_90),file="bat3_prop_sp_temp_90_geral.RData")

bat2_prop_80_geral <- matrix(ncol=1000,nrow=5500)
bat2_sp_80_geral <- matrix(ncol=1000,nrow=5500)
bat2_temp_80 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_80_8 <- matrix(ncol=8,nrow=5500)
  sp_80_8 <- matrix(ncol=8,nrow=5500)
  temp_80_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_80 <- riq_temp[1]*0.8
    temp_80 <- max(which(abs(riq_temp-riq_80)==min(abs(riq_temp-riq_80))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_80 <- prop[,temp_80]
    sp_80 <- sp[,temp_80]
    prop_80_8[,j] <- c(prop_80,rep(NA,5500-tam_com))
    sp_80_8[,j] <- c(sp_80,rep(NA,5500-tam_com))
    temp_80_8[,j] <- temp_80
  }
  bat2_prop_80_geral[,i:(i+7)] <- prop_80_8
  bat2_sp_80_geral[,i:(i+7)] <- sp_80_8
  bat2_temp_80[,i:(i+7)] <- temp_80_8
}
save(c(bat2_prop_80_geral,bat2_sp_80_geral,bat2_temp_80),file="bat2_prop_sp_temp_80_geral.RData")

bat3_prop_80_geral <- matrix(ncol=1000,nrow=5500)
bat3_sp_80_geral <- matrix(ncol=1000,nrow=5500)
bat3_temp_80 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_80_8 <- matrix(ncol=8,nrow=5500)
  sp_80_8 <- matrix(ncol=8,nrow=5500)
  temp_80_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_80 <- riq_temp[1]*0.8
    temp_80 <- max(which(abs(riq_temp-riq_80)==min(abs(riq_temp-riq_80))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_80 <- prop[,temp_80]
    sp_80 <- sp[,temp_80]
    prop_80_8[,j] <- c(prop_80,rep(NA,5500-tam_com))
    sp_80_8[,j] <- c(sp_80,rep(NA,5500-tam_com))
    temp_80_8[,j] <- temp_80
  }
  bat3_prop_80_geral[,i:(i+7)] <- prop_80_8
  bat3_sp_80_geral[,i:(i+7)] <- sp_80_8
  bat3_temp_80[,i:(i+7)] <- temp_80_8
}
save(c(bat3_prop_80_geral,bat3_sp_80_geral,bat3_temp_80),file="bat3_prop_sp_temp_80_geral.RData")

bat2_prop_70_geral <- matrix(ncol=1000,nrow=5500)
bat2_sp_70_geral <- matrix(ncol=1000,nrow=5500)
bat2_temp_70 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_70_8 <- matrix(ncol=8,nrow=5500)
  sp_70_8 <- matrix(ncol=8,nrow=5500)
  temp_70_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_70 <- riq_temp[1]*0.7
    temp_70 <- max(which(abs(riq_temp-riq_70)==min(abs(riq_temp-riq_70))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_70 <- prop[,temp_70]
    sp_70 <- sp[,temp_70]
    prop_70_8[,j] <- c(prop_70,rep(NA,5500-tam_com))
    sp_70_8[,j] <- c(sp_70,rep(NA,5500-tam_com))
    temp_70_8[,j] <- temp_70
  }
  bat2_prop_70_geral[,i:(i+7)] <- prop_70_8
  bat2_sp_70_geral[,i:(i+7)] <- sp_70_8
  bat2_temp_70[,i:(i+7)] <- temp_70_8
}
save(c(bat2_prop_70_geral,bat2_sp_70_geral,bat2_temp_70),file="bat2_prop_sp_temp_70_geral.RData")

bat3_prop_70_geral <- matrix(ncol=1000,nrow=5500)
bat3_sp_70_geral <- matrix(ncol=1000,nrow=5500)
bat3_temp_70 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_70_8 <- matrix(ncol=8,nrow=5500)
  sp_70_8 <- matrix(ncol=8,nrow=5500)
  temp_70_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_70 <- riq_temp[1]*0.7
    temp_70 <- max(which(abs(riq_temp-riq_70)==min(abs(riq_temp-riq_70))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_70 <- prop[,temp_70]
    sp_70 <- sp[,temp_70]
    prop_70_8[,j] <- c(prop_70,rep(NA,5500-tam_com))
    sp_70_8[,j] <- c(sp_70,rep(NA,5500-tam_com))
    temp_70_8[,j] <- temp_70
  }
  bat3_prop_70_geral[,i:(i+7)] <- prop_70_8
  bat3_sp_70_geral[,i:(i+7)] <- sp_70_8
  bat3_temp_70[,i:(i+7)] <- temp_70_8
}
save(c(bat3_prop_70_geral,bat3_sp_70_geral,bat3_temp_70),file="bat3_prop_sp_temp_70_geral.RData")


bat2_prop_60_geral <- matrix(ncol=1000,nrow=5500)
bat2_sp_60_geral <- matrix(ncol=1000,nrow=5500)
bat2_temp_60 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_60_8 <- matrix(ncol=8,nrow=5500)
  sp_60_8 <- matrix(ncol=8,nrow=5500)
  temp_60_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_60 <- riq_temp[1]*0.6
    temp_60 <- max(which(abs(riq_temp-riq_60)==min(abs(riq_temp-riq_60))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_60 <- prop[,temp_60]
    sp_60 <- sp[,temp_60]
    prop_60_8[,j] <- c(prop_60,rep(NA,5500-tam_com))
    sp_60_8[,j] <- c(sp_60,rep(NA,5500-tam_com))
    temp_60_8[,j] <- temp_60
  }
  bat2_prop_60_geral[,i:(i+7)] <- prop_60_8
  bat2_sp_60_geral[,i:(i+7)] <- sp_60_8
  bat2_temp_60[,i:(i+7)] <- temp_60_8
}
save(c(bat2_prop_60_geral,bat2_sp_60_geral,bat2_temp_60),file="bat2_prop_sp_temp_60_geral.RData")

bat3_prop_60_geral <- matrix(ncol=1000,nrow=5500)
bat3_sp_60_geral <- matrix(ncol=1000,nrow=5500)
bat3_temp_60 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_60_8 <- matrix(ncol=8,nrow=5500)
  sp_60_8 <- matrix(ncol=8,nrow=5500)
  temp_60_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_60 <- riq_temp[1]*0.6
    temp_60 <- max(which(abs(riq_temp-riq_60)==min(abs(riq_temp-riq_60))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_60 <- prop[,temp_60]
    sp_60 <- sp[,temp_60]
    prop_60_8[,j] <- c(prop_60,rep(NA,5500-tam_com))
    sp_60_8[,j] <- c(sp_60,rep(NA,5500-tam_com))
    temp_60_8[,j] <- temp_60
  }
  bat3_prop_60_geral[,i:(i+7)] <- prop_60_8
  bat3_sp_60_geral[,i:(i+7)] <- sp_60_8
  bat3_temp_60[,i:(i+7)] <- temp_60_8
}
save(c(bat3_prop_60_geral,bat3_sp_60_geral,bat3_temp_60),file="bat3_prop_sp_temp_60_geral.RData")


bat2_prop_40_geral <- matrix(ncol=1000,nrow=5500)
bat2_sp_40_geral <- matrix(ncol=1000,nrow=5500)
bat2_temp_40 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_40_8 <- matrix(ncol=8,nrow=5500)
  sp_40_8 <- matrix(ncol=8,nrow=5500)
  temp_40_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_40 <- riq_temp[1]*0.4
    temp_40 <- max(which(abs(riq_temp-riq_40)==min(abs(riq_temp-riq_40))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_40 <- prop[,temp_40]
    sp_40 <- sp[,temp_40]
    prop_40_8[,j] <- c(prop_40,rep(NA,5500-tam_com))
    sp_40_8[,j] <- c(sp_40,rep(NA,5500-tam_com))
    temp_40_8[,j] <- temp_40
  }
  bat2_prop_40_geral[,i:(i+7)] <- prop_40_8
  bat2_sp_40_geral[,i:(i+7)] <- sp_40_8
  bat2_temp_40[,i:(i+7)] <- temp_40_8
}
save(c(bat2_prop_40_geral,bat2_sp_40_geral,bat2_temp_40),file="bat2_prop_sp_temp_40_geral.RData")

bat3_prop_40_geral <- matrix(ncol=1000,nrow=5500)
bat3_sp_40_geral <- matrix(ncol=1000,nrow=5500)
bat3_temp_40 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_40_8 <- matrix(ncol=8,nrow=5500)
  sp_40_8 <- matrix(ncol=8,nrow=5500)
  temp_40_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_40 <- riq_temp[1]*0.4
    temp_40 <- max(which(abs(riq_temp-riq_40)==min(abs(riq_temp-riq_40))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_40 <- prop[,temp_40]
    sp_40 <- sp[,temp_40]
    prop_40_8[,j] <- c(prop_40,rep(NA,5500-tam_com))
    sp_40_8[,j] <- c(sp_40,rep(NA,5500-tam_com))
    temp_40_8[,j] <- temp_40
  }
  bat3_prop_40_geral[,i:(i+7)] <- prop_40_8
  bat3_sp_40_geral[,i:(i+7)] <- sp_40_8
  bat3_temp_40[,i:(i+7)] <- temp_40_8
}
save(c(bat3_prop_40_geral,bat3_sp_40_geral,bat3_temp_40),file="bat3_prop_sp_temp_40_geral.RData")

bat2_prop_30_geral <- matrix(ncol=1000,nrow=5500)
bat2_sp_30_geral <- matrix(ncol=1000,nrow=5500)
bat2_temp_30 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_30_8 <- matrix(ncol=8,nrow=5500)
  sp_30_8 <- matrix(ncol=8,nrow=5500)
  temp_30_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_30 <- riq_temp[1]*0.3
    temp_30 <- max(which(abs(riq_temp-riq_30)==min(abs(riq_temp-riq_30))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_30 <- prop[,temp_30]
    sp_30 <- sp[,temp_30]
    prop_30_8[,j] <- c(prop_30,rep(NA,5500-tam_com))
    sp_30_8[,j] <- c(sp_30,rep(NA,5500-tam_com))
    temp_30_8[,j] <- temp_30
  }
  bat2_prop_30_geral[,i:(i+7)] <- prop_30_8
  bat2_sp_30_geral[,i:(i+7)] <- sp_30_8
  bat2_temp_30[,i:(i+7)] <- temp_30_8
}
save(c(bat2_prop_30_geral,bat2_sp_30_geral,bat2_temp_30),file="bat2_prop_sp_temp_30_geral.RData")

bat3_prop_30_geral <- matrix(ncol=1000,nrow=5500)
bat3_sp_30_geral <- matrix(ncol=1000,nrow=5500)
bat3_temp_30 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_30_8 <- matrix(ncol=8,nrow=5500)
  sp_30_8 <- matrix(ncol=8,nrow=5500)
  temp_30_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_30 <- riq_temp[1]*0.3
    temp_30 <- max(which(abs(riq_temp-riq_30)==min(abs(riq_temp-riq_30))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_30 <- prop[,temp_30]
    sp_30 <- sp[,temp_30]
    prop_30_8[,j] <- c(prop_30,rep(NA,5500-tam_com))
    sp_30_8[,j] <- c(sp_30,rep(NA,5500-tam_com))
    temp_30_8[,j] <- temp_30
  }
  bat3_prop_30_geral[,i:(i+7)] <- prop_30_8
  bat3_sp_30_geral[,i:(i+7)] <- sp_30_8
  bat3_temp_30[,i:(i+7)] <- temp_30_8
}
save(c(bat3_prop_30_geral,bat3_sp_30_geral,bat3_temp_30),file="bat3_prop_sp_temp_30_geral.RData")


bat2_prop_20_geral <- matrix(ncol=1000,nrow=5500)
bat2_sp_20_geral <- matrix(ncol=1000,nrow=5500)
bat2_temp_20 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_20_8 <- matrix(ncol=8,nrow=5500)
  sp_20_8 <- matrix(ncol=8,nrow=5500)
  temp_20_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_20 <- riq_temp[1]*0.2
    temp_20 <- max(which(abs(riq_temp-riq_20)==min(abs(riq_temp-riq_20))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_20 <- prop[,temp_20]
    sp_20 <- sp[,temp_20]
    prop_20_8[,j] <- c(prop_20,rep(NA,5500-tam_com))
    sp_20_8[,j] <- c(sp_20,rep(NA,5500-tam_com))
    temp_20_8[,j] <- temp_20
  }
  bat2_prop_20_geral[,i:(i+7)] <- prop_20_8
  bat2_sp_20_geral[,i:(i+7)] <- sp_20_8
  bat2_temp_20[,i:(i+7)] <- temp_20_8
}
save(c(bat2_prop_20_geral,bat2_sp_20_geral,bat2_temp_20),file="bat2_prop_sp_temp_20_geral.RData")

bat3_prop_20_geral <- matrix(ncol=1000,nrow=5500)
bat3_sp_20_geral <- matrix(ncol=1000,nrow=5500)
bat3_temp_20 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_20_8 <- matrix(ncol=8,nrow=5500)
  sp_20_8 <- matrix(ncol=8,nrow=5500)
  temp_20_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_20 <- riq_temp[1]*0.2
    temp_20 <- max(which(abs(riq_temp-riq_20)==min(abs(riq_temp-riq_20))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_20 <- prop[,temp_20]
    sp_20 <- sp[,temp_20]
    prop_20_8[,j] <- c(prop_20,rep(NA,5500-tam_com))
    sp_20_8[,j] <- c(sp_20,rep(NA,5500-tam_com))
    temp_20_8[,j] <- temp_20
  }
  bat3_prop_20_geral[,i:(i+7)] <- prop_20_8
  bat3_sp_20_geral[,i:(i+7)] <- sp_20_8
  bat3_temp_20[,i:(i+7)] <- temp_20_8
}
save(c(bat3_prop_20_geral,bat3_sp_20_geral,bat3_temp_20),file="bat3_prop_sp_temp_20_geral.RData")


bat2_prop_10_geral <- matrix(ncol=1000,nrow=5500)
bat2_sp_10_geral <- matrix(ncol=1000,nrow=5500)
bat2_temp_10 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_10_8 <- matrix(ncol=8,nrow=5500)
  sp_10_8 <- matrix(ncol=8,nrow=5500)
  temp_10_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_10 <- riq_temp[1]*0.1
    temp_10 <- max(which(abs(riq_temp-riq_10)==min(abs(riq_temp-riq_10))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_10 <- prop[,temp_10]
    sp_10 <- sp[,temp_10]
    prop_10_8[,j] <- c(prop_10,rep(NA,5500-tam_com))
    sp_10_8[,j] <- c(sp_10,rep(NA,5500-tam_com))
    temp_10_8[,j] <- temp_10
  }
  bat2_prop_10_geral[,i:(i+7)] <- prop_10_8
  bat2_sp_10_geral[,i:(i+7)] <- sp_10_8
  bat2_temp_10[,i:(i+7)] <- temp_10_8
}
save(c(bat2_prop_10_geral,bat2_sp_10_geral,bat2_temp_10),file="bat2_prop_sp_temp_10_geral.RData")

bat3_prop_10_geral <- matrix(ncol=1000,nrow=5500)
bat3_sp_10_geral <- matrix(ncol=1000,nrow=5500)
bat3_temp_10 <- matrix(ncol=1000,nrow=1)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_10_8 <- matrix(ncol=8,nrow=5500)
  sp_10_8 <- matrix(ncol=8,nrow=5500)
  temp_10_8 <- matrix(ncol=8,nrow=1)
  for(j in 1:length(x)){
    riq_temp <- apply(x[[j]]$sp.list,2,function(x){length(unique(x))}) 
    riq_10 <- riq_temp[1]*0.1
    temp_10 <- max(which(abs(riq_temp-riq_10)==min(abs(riq_temp-riq_10))))
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    prop <- as.matrix(x[[j]]$sementes)
    sp <- as.matrix(x[[j]]$sp.list)
    prop_10 <- prop[,temp_10]
    sp_10 <- sp[,temp_10]
    prop_10_8[,j] <- c(prop_10,rep(NA,5500-tam_com))
    sp_10_8[,j] <- c(sp_10,rep(NA,5500-tam_com))
    temp_10_8[,j] <- temp_10
  }
  bat3_prop_10_geral[,i:(i+7)] <- prop_10_8
  bat3_sp_10_geral[,i:(i+7)] <- sp_10_8
  bat3_temp_10[,i:(i+7)] <- temp_10_8
}
save(list=c("bat3_prop_10_geral","bat3_sp_10_geral","bat3_temp_10"),file="bat3_prop_sp_temp_10_geral.RData")

