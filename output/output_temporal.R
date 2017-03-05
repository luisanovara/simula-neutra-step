##############################################################
###################### Analise temporal ######################
##############################################################

################## Variancia entre especies ##################
##################### em todos os tempos #####################

fert.t1 <- function(cod.sp,n.propag,fun=mean)
{ 
  especie <- unique(cod.sp[,1])
  sp.level<-factor(cod.sp,levels=cod.sp)
  t.a<-function(x){tapply(n.propag[,x],factor(cod.sp[,x],levels=especie),fun)}
  res<-sapply(1:ncol(n.propag), t.a)
  colnames(res) <- colnames(n.propag)
  rownames(res) <- paste("sp",especie, sep="")
  return(res)
}

######################### Bateria 2 #########################

bat2_var_indice_media_temporal <- matrix(ncol=3001,nrow=1000)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  var_indice_8 <- matrix(ncol=3001,nrow=8)
  for(j in 1:length(x)){
    prop <- x[[j]]$sementes
    sp <- x[[j]]$sp.list
    mat <- fert.t1(sp,prop)
    mat_indice <- (mat-1)/(20000-1)
    var_indice <- apply(mat_indice,2,var,na.rm=T)
    var_indice_8[j,] <- var_indice
  }
  bat2_var_indice_media_temporal[i:(i+7),] <- var_indice_8
}

save(bat2_var_indice_media_temporal,file="sim25jul16_var_indice_media.RData")


bat2_dist_classes <- cut(bat2_indice_dist,breaks=3,labels=1:3)
matplot(t(bat2_var_indice_media_temporal),type="l",col=alpha(bat2_dist_classes,0.5),lty=2) #preto: dist baixo, vermelho: dist intermediario, verde: dist alto
matplot(t(bat2_var_indice_media_temporal[,1:100]),type="l",col=alpha(bat2_dist_classes,0.5),lty=2) #preto: dist baixo, vermelho: dist intermediario, verde: dist alto
matplot(t(bat2_var_indice_media_temporal[,1:60]),type="l",col=alpha(bat2_dist_classes,0.5),ylim=c(0,0.02),lty=2) #preto: dist baixo, vermelho: dist intermediario, verde: dist alto

######################### Bateria 3 #########################

bat3_var_indice_media_temporal <- matrix(ncol=3001,nrow=1000)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  var_indice_8 <- matrix(ncol=3001,nrow=8)
  for(j in 1:length(x)){
    prop <- x[[j]]$sementes
    sp <- x[[j]]$sp.list
    mat <- fert.t1(sp,prop)
    mat_indice <- (mat-1)/(20000-1)
    var_indice <- apply(mat_indice,2,var,na.rm=T)
    var_indice_8[j,] <- var_indice
  }
  bat3_var_indice_media_temporal[i:(i+7),] <- var_indice_8
}

save(bat3_var_indice_media_temporal,file="sim27jul16_var_indice_media.RData")

bat3_dist_classes <- cut(bat3_indice_dist,breaks=3,labels=1:3)
matplot(t(bat3_var_indice_media_temporal),type="l",col=alpha(bat3_dist_classes,0.5),lty=1) #preto: dist baixo, vermelho: dist intermediario, verde: dist alto
matplot(t(bat3_var_indice_media_temporal[,1:100]),type="l",col=alpha(bat3_dist_classes,0.5),lty=1) #preto: dist baixo, vermelho: dist intermediario, verde: dist alto
matplot(t(bat3_var_indice_media_temporal[,1:60]),type="l",col=alpha(bat3_dist_classes,0.5),ylim=c(0,0.02),lty=1) #preto: dist baixo, vermelho: dist intermediario, verde: dist alto
matplot(t(bat3_var_indice_media_temporal[,100:400]),type="l",col=alpha(bat3_dist_classes,0.5),ylim=c(0,0.02),lty=1) #preto: dist baixo, vermelho: dist intermediario, verde: dist alto

########################### FST 2 ############################
########## em 90%, 80% ... 10% de especies restantes #########

bat3_90_fst_2 <- matrix(NA,ncol=1000,nrow=1)
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_90_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_90_geral[,k])
  riqueza <- length(unique(vetor_especies_original))
  vetor_especies_modificado_crescente <- rep(1:riqueza,times=table(vetor_especies_original))
  vetor_especies_modificado <- vetor_especies_modificado_crescente[order(order(vetor_especies_original),vetor_especies_original)]
  abund_especies <- table(vetor_especies_modificado)
  freq_presentes <- tapply(vetor_valores,vetor_especies_modificado,FUN=table)
  freq_todas <- rep(list(rep(0,20000)),times=riqueza)
  freq_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  freq_relat_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  for(i in 1:riqueza){
    freq_todas[[i]][as.numeric(names(freq_presentes[[i]]))] <- freq_presentes[[i]]
    for(j in 1:200){
      freq_todas_classes[[i]][j] <- sum(freq_todas[[i]][(100*j-99):(100*j)])
    }
    freq_relat_todas_classes[[i]] <- freq_todas_classes[[i]]/abund_especies[[i]]
  }
  p <- rep(list(rep(0,riqueza)),times=200)
  p_media <- c()
  p_var <- c()
  for(l in 1:200){
    for(m in 1:riqueza){
      p[[l]][m] <- freq_relat_todas_classes[[m]][l]
    }
    p_media[l] <- mean(p[[l]])
    p_var[l] <- var(p[[l]])
  }
  bat3_90_fst_2[,k] <- sum(p_var)/sum(p_media*(1-p_media))
}
save(bat3_90_fst_2,file="bat3_90_fst_2.RData")

bat3_80_fst_2 <- matrix(NA,ncol=1000,nrow=1)
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_80_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_80_geral[,k])
  riqueza <- length(unique(vetor_especies_original))
  vetor_especies_modificado_crescente <- rep(1:riqueza,times=table(vetor_especies_original))
  vetor_especies_modificado <- vetor_especies_modificado_crescente[order(order(vetor_especies_original),vetor_especies_original)]
  abund_especies <- table(vetor_especies_modificado)
  freq_presentes <- tapply(vetor_valores,vetor_especies_modificado,FUN=table)
  freq_todas <- rep(list(rep(0,20000)),times=riqueza)
  freq_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  freq_relat_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  for(i in 1:riqueza){
    freq_todas[[i]][as.numeric(names(freq_presentes[[i]]))] <- freq_presentes[[i]]
    for(j in 1:200){
      freq_todas_classes[[i]][j] <- sum(freq_todas[[i]][(100*j-99):(100*j)])
    }
    freq_relat_todas_classes[[i]] <- freq_todas_classes[[i]]/abund_especies[[i]]
  }
  p <- rep(list(rep(0,riqueza)),times=200)
  p_media <- c()
  p_var <- c()
  for(l in 1:200){
    for(m in 1:riqueza){
      p[[l]][m] <- freq_relat_todas_classes[[m]][l]
    }
    p_media[l] <- mean(p[[l]])
    p_var[l] <- var(p[[l]])
  }
  bat3_80_fst_2[,k] <- sum(p_var)/sum(p_media*(1-p_media))
}
save(bat3_80_fst_2,file="bat3_80_fst_2.RData")

bat3_70_fst_2 <- matrix(NA,ncol=1000,nrow=1)
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_70_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_70_geral[,k])
  riqueza <- length(unique(vetor_especies_original))
  vetor_especies_modificado_crescente <- rep(1:riqueza,times=table(vetor_especies_original))
  vetor_especies_modificado <- vetor_especies_modificado_crescente[order(order(vetor_especies_original),vetor_especies_original)]
  abund_especies <- table(vetor_especies_modificado)
  freq_presentes <- tapply(vetor_valores,vetor_especies_modificado,FUN=table)
  freq_todas <- rep(list(rep(0,20000)),times=riqueza)
  freq_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  freq_relat_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  for(i in 1:riqueza){
    freq_todas[[i]][as.numeric(names(freq_presentes[[i]]))] <- freq_presentes[[i]]
    for(j in 1:200){
      freq_todas_classes[[i]][j] <- sum(freq_todas[[i]][(100*j-99):(100*j)])
    }
    freq_relat_todas_classes[[i]] <- freq_todas_classes[[i]]/abund_especies[[i]]
  }
  p <- rep(list(rep(0,riqueza)),times=200)
  p_media <- c()
  p_var <- c()
  for(l in 1:200){
    for(m in 1:riqueza){
      p[[l]][m] <- freq_relat_todas_classes[[m]][l]
    }
    p_media[l] <- mean(p[[l]])
    p_var[l] <- var(p[[l]])
  }
  bat3_70_fst_2[,k] <- sum(p_var)/sum(p_media*(1-p_media))
}
save(bat3_70_fst_2,file="bat3_70_fst_2.RData")

bat3_60_fst_2 <- matrix(NA,ncol=1000,nrow=1)
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_60_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_60_geral[,k])
  riqueza <- length(unique(vetor_especies_original))
  vetor_especies_modificado_crescente <- rep(1:riqueza,times=table(vetor_especies_original))
  vetor_especies_modificado <- vetor_especies_modificado_crescente[order(order(vetor_especies_original),vetor_especies_original)]
  abund_especies <- table(vetor_especies_modificado)
  freq_presentes <- tapply(vetor_valores,vetor_especies_modificado,FUN=table)
  freq_todas <- rep(list(rep(0,20000)),times=riqueza)
  freq_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  freq_relat_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  for(i in 1:riqueza){
    freq_todas[[i]][as.numeric(names(freq_presentes[[i]]))] <- freq_presentes[[i]]
    for(j in 1:200){
      freq_todas_classes[[i]][j] <- sum(freq_todas[[i]][(100*j-99):(100*j)])
    }
    freq_relat_todas_classes[[i]] <- freq_todas_classes[[i]]/abund_especies[[i]]
  }
  p <- rep(list(rep(0,riqueza)),times=200)
  p_media <- c()
  p_var <- c()
  for(l in 1:200){
    for(m in 1:riqueza){
      p[[l]][m] <- freq_relat_todas_classes[[m]][l]
    }
    p_media[l] <- mean(p[[l]])
    p_var[l] <- var(p[[l]])
  }
  bat3_60_fst_2[,k] <- sum(p_var)/sum(p_media*(1-p_media))
}
save(bat3_60_fst_2,file="bat3_60_fst_2.RData")

bat3_40_fst_2 <- matrix(NA,ncol=1000,nrow=1)
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_40_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_40_geral[,k])
  riqueza <- length(unique(vetor_especies_original))
  vetor_especies_modificado_crescente <- rep(1:riqueza,times=table(vetor_especies_original))
  vetor_especies_modificado <- vetor_especies_modificado_crescente[order(order(vetor_especies_original),vetor_especies_original)]
  abund_especies <- table(vetor_especies_modificado)
  freq_presentes <- tapply(vetor_valores,vetor_especies_modificado,FUN=table)
  freq_todas <- rep(list(rep(0,20000)),times=riqueza)
  freq_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  freq_relat_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  for(i in 1:riqueza){
    freq_todas[[i]][as.numeric(names(freq_presentes[[i]]))] <- freq_presentes[[i]]
    for(j in 1:200){
      freq_todas_classes[[i]][j] <- sum(freq_todas[[i]][(100*j-99):(100*j)])
    }
    freq_relat_todas_classes[[i]] <- freq_todas_classes[[i]]/abund_especies[[i]]
  }
  p <- rep(list(rep(0,riqueza)),times=200)
  p_media <- c()
  p_var <- c()
  for(l in 1:200){
    for(m in 1:riqueza){
      p[[l]][m] <- freq_relat_todas_classes[[m]][l]
    }
    p_media[l] <- mean(p[[l]])
    p_var[l] <- var(p[[l]])
  }
  bat3_40_fst_2[,k] <- sum(p_var)/sum(p_media*(1-p_media))
}
save(bat3_40_fst_2,file="bat3_40_fst_2.RData")

bat3_30_fst_2 <- matrix(NA,ncol=1000,nrow=1)
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_30_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_30_geral[,k])
  riqueza <- length(unique(vetor_especies_original))
  vetor_especies_modificado_crescente <- rep(1:riqueza,times=table(vetor_especies_original))
  vetor_especies_modificado <- vetor_especies_modificado_crescente[order(order(vetor_especies_original),vetor_especies_original)]
  abund_especies <- table(vetor_especies_modificado)
  freq_presentes <- tapply(vetor_valores,vetor_especies_modificado,FUN=table)
  freq_todas <- rep(list(rep(0,20000)),times=riqueza)
  freq_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  freq_relat_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  for(i in 1:riqueza){
    freq_todas[[i]][as.numeric(names(freq_presentes[[i]]))] <- freq_presentes[[i]]
    for(j in 1:200){
      freq_todas_classes[[i]][j] <- sum(freq_todas[[i]][(100*j-99):(100*j)])
    }
    freq_relat_todas_classes[[i]] <- freq_todas_classes[[i]]/abund_especies[[i]]
  }
  p <- rep(list(rep(0,riqueza)),times=200)
  p_media <- c()
  p_var <- c()
  for(l in 1:200){
    for(m in 1:riqueza){
      p[[l]][m] <- freq_relat_todas_classes[[m]][l]
    }
    p_media[l] <- mean(p[[l]])
    p_var[l] <- var(p[[l]])
  }
  bat3_30_fst_2[,k] <- sum(p_var)/sum(p_media*(1-p_media))
}
save(bat3_30_fst_2,file="bat3_30_fst_2.RData")

bat3_20_fst_2 <- matrix(NA,ncol=1000,nrow=1)
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_20_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_20_geral[,k])
  riqueza <- length(unique(vetor_especies_original))
  vetor_especies_modificado_crescente <- rep(1:riqueza,times=table(vetor_especies_original))
  vetor_especies_modificado <- vetor_especies_modificado_crescente[order(order(vetor_especies_original),vetor_especies_original)]
  abund_especies <- table(vetor_especies_modificado)
  freq_presentes <- tapply(vetor_valores,vetor_especies_modificado,FUN=table)
  freq_todas <- rep(list(rep(0,20000)),times=riqueza)
  freq_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  freq_relat_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  for(i in 1:riqueza){
    freq_todas[[i]][as.numeric(names(freq_presentes[[i]]))] <- freq_presentes[[i]]
    for(j in 1:200){
      freq_todas_classes[[i]][j] <- sum(freq_todas[[i]][(100*j-99):(100*j)])
    }
    freq_relat_todas_classes[[i]] <- freq_todas_classes[[i]]/abund_especies[[i]]
  }
  p <- rep(list(rep(0,riqueza)),times=200)
  p_media <- c()
  p_var <- c()
  for(l in 1:200){
    for(m in 1:riqueza){
      p[[l]][m] <- freq_relat_todas_classes[[m]][l]
    }
    p_media[l] <- mean(p[[l]])
    p_var[l] <- var(p[[l]])
  }
  bat3_20_fst_2[,k] <- sum(p_var)/sum(p_media*(1-p_media))
}
save(bat3_20_fst_2,file="bat3_20_fst_2.RData")

bat3_10_fst_2 <- matrix(NA,ncol=1000,nrow=1)
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_10_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_10_geral[,k])
  riqueza <- length(unique(vetor_especies_original))
  vetor_especies_modificado_crescente <- rep(1:riqueza,times=table(vetor_especies_original))
  vetor_especies_modificado <- vetor_especies_modificado_crescente[order(order(vetor_especies_original),vetor_especies_original)]
  abund_especies <- table(vetor_especies_modificado)
  freq_presentes <- tapply(vetor_valores,vetor_especies_modificado,FUN=table)
  freq_todas <- rep(list(rep(0,20000)),times=riqueza)
  freq_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  freq_relat_todas_classes <- rep(list(rep(0,200)),times=riqueza)
  for(i in 1:riqueza){
    freq_todas[[i]][as.numeric(names(freq_presentes[[i]]))] <- freq_presentes[[i]]
    for(j in 1:200){
      freq_todas_classes[[i]][j] <- sum(freq_todas[[i]][(100*j-99):(100*j)])
    }
    freq_relat_todas_classes[[i]] <- freq_todas_classes[[i]]/abund_especies[[i]]
  }
  p <- rep(list(rep(0,riqueza)),times=200)
  p_media <- c()
  p_var <- c()
  for(l in 1:200){
    for(m in 1:riqueza){
      p[[l]][m] <- freq_relat_todas_classes[[m]][l]
    }
    p_media[l] <- mean(p[[l]])
    p_var[l] <- var(p[[l]])
  }
  bat3_10_fst_2[,k] <- sum(p_var)/sum(p_media*(1-p_media))
}
save(bat3_10_fst_2,file="bat3_10_fst_2.RData")

oi <- matrix(data=c(bat3_90_fst_2,bat3_80_fst_2,bat3_70_fst_2,bat3_60_fst_2,bat3_fst_2,bat3_40_fst_2,bat3_30_fst_2,bat3_20_fst_2,bat3_10_fst_2),byrow=F,nrow=1000)
bat3_dist_classes <- cut(bat3_indice_dist,breaks=3,labels=1:3)
matplot(t(oi),type="l",col=alpha(bat3_dist_classes,0.3),lty=1,xlim=c(1,9),ylim=c(0,0.8))

matplot(t(oi[which(bat3_dist_classes==1),]),type="l",col=alpha("black",0.2),lty=1,xlim=c(1,9),ylim=c(0,0.8))
matplot(t(oi[which(bat3_dist_classes==2),]),type="l",col=alpha("red",0.2),lty=1,xlim=c(1,9),ylim=c(0,0.8))
matplot(t(oi[which(bat3_dist_classes==3),]),type="l",col=alpha("green",0.2),lty=1,xlim=c(1,9),ylim=c(0,0.8))
#preto: dist baixo, vermelho: dist intermediario, verde: dist alto