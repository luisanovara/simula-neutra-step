########################################################
############### coexistencia de especies ###############
########################################################

load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_sp_meiavida_geral.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_prop_meiavida_geral.RData")

load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_25jul16/bat2_sp_meiavida_geral.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_25jul16/bat2_prop_meiavida_geral.RData")

load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_var_intersp_indice_media.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_meia_vida.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_indice_sobreposicao_dnorm.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_indice_sobreposicao_freq.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_indice_sobreposicao_freq_classes.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_fst.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_fst_2.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_fst_3.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_prop_nger_5000.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_sp_nger.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_25jul16/bat2_prop_nger_5000.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_25jul16/bat2_sp_nger.RData")

###################### indice de sobreposicao com dnorm no tempo de meia vida
bat3_indice_sobreposicao_dnorm <- c()
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_meiavida_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_meiavida_geral[,k])
  riqueza <- length(unique(vetor_especies_original))
  vetor_especies_modificado_crescente <- rep(1:riqueza,times=table(vetor_especies_original))
  vetor_especies_modificado <- vetor_especies_modificado_crescente[order(order(vetor_especies_original),vetor_especies_original)]
  d_especie_i_total <- c()
  for(i in seq(1:riqueza)){
    d_especie_i <- c()
    for(j in seq(1:riqueza)[-i]){
      d_especie_i[j] <- sum(dnorm(vetor_valores[vetor_especies_modificado==j],mean=mean(vetor_valores[vetor_especies_modificado==i]),sd=sd(vetor_valores[vetor_especies_modificado==i])))
    }
    d_especie_i_total[i] <- mean(d_especie_i,na.rm=T)
  }
  bat3_indice_sobreposicao_dnorm[k] <- mean(d_especie_i_total,na.rm=T)
}
save(bat3_indice_sobreposicao_dnorm,file="bat3_indice_sobreposicao_dnorm.RData")
plot(bat3_indice_sobreposicao_dnorm~bat3_indice_dist)
plot(bat3_indice_sobreposicao_dnorm~bat3_indice_dist,ylim=c(0,0.2))
plot(bat3_indice_sobreposicao_dnorm~bat3_indice_dist,ylim=c(0,0.1))

mod_efeito_dnorm <- lm(bat3_indice_sobreposicao_dnorm[-which(is.infinite(bat3_indice_sobreposicao_dnorm))]~bat3_indice_dist[-which(is.infinite(bat3_indice_sobreposicao_dnorm))])
mod_nulo_dnorm <- lm(bat3_indice_sobreposicao_dnorm[-which(is.infinite(bat3_indice_sobreposicao_dnorm))]~1)
AICtab(mod_nulo_dnorm,mod_efeito_dnorm,weights=T) #mod nulo (mod efeito negativo)
abline(mod_nulo_dnorm)
abline(mod_efeito_dnorm,col="red")

######## indice de sobreposicao com frequencias no tempo de meia vida (Ale)

# individual
media_comunidade <- c()
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_meiavida_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_meiavida_geral[,k])
  riqueza <- length(unique(vetor_especies_original))
  vetor_especies_modificado_crescente <- rep(1:riqueza,times=table(vetor_especies_original))
  vetor_especies_modificado <- vetor_especies_modificado_crescente[order(order(vetor_especies_original),vetor_especies_original)]
  d_especie_i_total <- c()
  for(i in seq(1:riqueza)){
    d_especie_i <- c()
    for(j in seq(1:riqueza)[-i]){
      d_especie_i[j] <- sum(table(vetor_valores[vetor_especies_modificado==j][vetor_valores[vetor_especies_modificado==j] %in% vetor_valores[vetor_especies_modificado==i]])/sum(table(vetor_valores[vetor_especies_modificado==j]))+table(vetor_valores[vetor_especies_modificado==i][vetor_valores[vetor_especies_modificado==i] %in% vetor_valores[vetor_especies_modificado==j]])/sum(table(vetor_valores[vetor_especies_modificado==i])))
    }
    d_especie_i_total[i] <- mean(d_especie_i,na.rm=T)
  }
  media_comunidade[k] <- mean(d_especie_i_total)
}
save(media_comunidade,file="bat3_indice_sobreposicao_freq.RData")
plot(media_comunidade~bat3_indice_dist)
plot(media_comunidade~bat3_indice_dist,ylim=c(0,0.2))

mod_efeito_freq <- lm(media_comunidade~bat3_indice_dist)
mod_nulo_freq <- lm(media_comunidade~1)
AICtab(mod_nulo_freq,mod_efeito_freq,weights=T) #mod nulo (mod efeito negativo)
abline(mod_efeito_freq,col="red")
abline(mod_nulo_freq)

# por classes
media_comunidade_classes <- c()
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_meiavida_geral[,k])
  for(l in 1:200){
    vetor_valores[vetor_valores>=(100*l-99) & vetor_valores<=(100*l)] <- l
  }
  vetor_especies_original <- na.omit(bat3_sp_meiavida_geral[,k])
  riqueza <- length(unique(vetor_especies_original))
  vetor_especies_modificado_crescente <- rep(1:riqueza,times=table(vetor_especies_original))
  vetor_especies_modificado <- vetor_especies_modificado_crescente[order(order(vetor_especies_original),vetor_especies_original)]
  d_especie_i_total <- c()
  for(i in seq(1:riqueza)){
    d_especie_i <- c()
    for(j in seq(1:riqueza)[-i]){
      d_especie_i[j] <- sum(table(vetor_valores[vetor_especies_modificado==j][vetor_valores[vetor_especies_modificado==j] %in% vetor_valores[vetor_especies_modificado==i]])/sum(table(vetor_valores[vetor_especies_modificado==j]))+table(vetor_valores[vetor_especies_modificado==i][vetor_valores[vetor_especies_modificado==i] %in% vetor_valores[vetor_especies_modificado==j]])/sum(table(vetor_valores[vetor_especies_modificado==i])))
    }
    d_especie_i_total[i] <- mean(d_especie_i,na.rm=T)
  }
  media_comunidade_classes[k] <- mean(d_especie_i_total)
}
save(media_comunidade_classes,file="bat3_indice_sobreposicao_freq_classes.RData")
plot(media_comunidade_classes~bat3_indice_dist)

plot(media_comunidade_classes~bat3_indice_dist,xlim=c(0,3e5),ylim=c(0,1.5))
plot(media_comunidade_classes[which(dados3_27jul16[,1]>=100)]~bat3_indice_dist[which(dados3_27jul16[,1]>=100)],xlim=c(0,3e5),ylim=c(0,1.5))

mod_efeito_freq_classes <- lm(media_comunidade_classes~bat3_indice_dist)
mod_nulo_freq_classes <- lm(media_comunidade_classes~1)
AICtab(mod_nulo_freq_classes,mod_efeito_freq_classes) #mod efeito! (neg)
abline(mod_efeito_freq_classes,col="red")
abline(mod_nulo_freq_classes)

################ fst no tempo de meia vida

bat3_fst_2 <- matrix(NA,ncol=1000,nrow=1)
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_meiavida_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_meiavida_geral[,k])
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
  bat3_fst_2[,k] <- sum(p_var)/sum(p_media*(1-p_media))
}
save(bat3_fst_2,file="bat3_fst_2.RData")
plot(as.vector(bat3_fst_2)~bat3_indice_dist,ylim=c(0,0.3))

mod_efeito_fst_2 <- lm(as.vector(bat3_fst_2)~bat3_indice_dist)
mod_nulo_fst_2 <- lm(as.vector(bat3_fst_2)~1)
AICtab(mod_nulo_fst_2,mod_efeito_fst_2,weights=T) #mod efeito! (positivo)
abline(mod_efeito_fst_2,col="red")
abline(mod_nulo_fst_2)

bat3_fst_3 <- matrix(NA,ncol=1000,nrow=1)
for(k in 1:1000){
  vetor_valores <- na.omit(bat3_prop_meiavida_geral[,k])
  vetor_especies_original <- na.omit(bat3_sp_meiavida_geral[,k])
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
    freq_relat_todas_classes[[i]] <- freq_todas_classes[[i]]/sum(abund_especies)
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
  bat3_fst_3[,k] <- sum(p_var)/sum(p_media*(1-p_media))
}
save(bat3_fst_3,file="bat3_fst_3.RData")
plot(as.vector(bat3_fst_3)~bat3_indice_dist,ylim=c(0,0.3))
plot(as.vector(bat3_fst_3)~bat3_indice_dist,ylim=c(0,0.05))
plot(as.vector(bat3_fst_3)~bat3_indice_dist,ylim=c(0,0.03))

mod_efeito_fst_3 <- lm(as.vector(bat3_fst_3)~bat3_indice_dist)
mod_nulo_fst_3 <- lm(as.vector(bat3_fst_3)~1)
AICtab(mod_nulo_fst_3,mod_efeito_fst_3,weights=T) #mod nulo (mod efeito negativo... contrario do resto ate entao)
abline(mod_nulo_fst_3)
abline(mod_efeito_fst_3,col="red")

################ variancia inter no tempo de meia vida x tempo de meia vida (indice de coexistencia)

bat3_var_indice_meia_vida <- c()
for(i in 1:1000){
  bat3_var_indice_meia_vida[i] <- bat3_var_indice_media_temporal[i,bat3_meia_vida[i]]
}
bat3_var_indice_meia_vida[which(is.na(bat3_var_indice_meia_vida))] <- 0
bat3_coex <- bat3_var_indice_meia_vida*as.integer(bat3_meia_vida)

plot(bat3_var_indice_meia_vida~bat3_indice_dist,xlim=c(0,3e5),ylim=c(0,0.04))
mod_efeito_var_inter<- lm(bat3_var_indice_meia_vida~bat3_indice_dist)
mod_nulo_var_inter<- lm(bat3_var_indice_meia_vida~1)
AICtab(mod_nulo_var_inter,mod_efeito_var_inter,weights=T) #mod nulo (mod efeito positivo)

plot(bat3_var_indice_meia_vida[which(dados3_27jul16[,1]>=100)]~bat3_indice_dist[which(dados3_27jul16[,1]>=100)],xlim=c(0,3e5),ylim=c(0,0.04))
plot(as.integer(bat3_meia_vida)~bat3_indice_dist)
plot(bat3_coex~bat3_indice_dist)


################ media das medias (no tempo de meia-vida)

media_medias <- c()
for(i in 1:1000){
  media_medias[i] <- mean(tapply(bat3_prop_meiavida_geral[,i],bat3_sp_meiavida_geral[,i],mean))
}
plot(media_medias~bat3_indice_dist,xlim=c(0,3e5),ylim=c(0,20000))
plot(media_medias[which(dados3_27jul16[,1]>=100)]~bat3_indice_dist[which(dados3_27jul16[,1]>=100)],xlim=c(0,3e5),ylim=c(0,20000))

plot(apply(bat3_prop_meiavida_geral,2,mean,na.rm=T)~bat3_indice_dist,xlim=c(0,3e5),ylim=c(0,20000))

## os graficos da media geral e da media das medias/sp sao muito semelhantes. isso pode indicar que mesmo as especies que tem abundancia menor, e que provavelmente serao extintas, tem estrategia de vida semelhantes a especie de maior abundancia. o que mela de vez o estudo da coexistencia nesse tempo...







load("/Volumes/NOVARA_8G/bat3_prop_sp_temp_90_geral.RData")
load("/Volumes/NOVARA_8G/bat3_prop_sp_temp_10_geral.RData")
# Usando a medida 2 de fst em todos os tempos
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
plot(as.vector(bat3_90_fst_2)~bat3_indice_dist,ylim=c(0,0.3))



#### componentes da variancia

# desv entre

bat3_prop_meiavida_geral <- (bat3_prop_meiavida_geral-1)/(20000-1)

vetor_med <- apply(bat3_prop_meiavida_geral,2,mean,na.rm=T)

mat_med_sp <- matrix(data=0,ncol=1000,nrow=500)
mat_length_sp <- matrix(data=0,ncol=1000,nrow=500)
mat_nomes_sp <- matrix(data=0,ncol=1000,nrow=500)
for(i in 1:1000){
   medias <- tapply(bat3_prop_meiavida_geral[,i],bat3_sp_meiavida_geral[,i],mean,na.rm=T)
   mat_med_sp[,i] <- c(medias,rep(NA,500-length(medias)))
   tamanhos <- tapply(bat3_prop_meiavida_geral[,i],bat3_sp_meiavida_geral[,i],length)
   mat_length_sp[,i] <- c(tamanhos,rep(NA,500-length(tamanhos)))
   nomes <- as.numeric(names(medias))
   mat_nomes_sp[,i] <- c(nomes,rep(NA,500-length(nomes)))
}

ss.entre <- c()
for(i in 1:1000){
  ss.entre[i] <- sum(((na.omit(mat_med_sp[,i]) - vetor_med[i])^2) * na.omit(mat_length_sp[,i]))
}
riqueza <- apply(bat3_sp_meiavida_geral,2,function(x){length(unique(na.omit(x)))})
gl.entre <- riqueza-1
desv.entre <- ss.entre/gl.entre

# desv intra

ss.intra <- c()
for(i in 1:1000){
  media <- c()
  for(j in 1:riqueza[i]){
    media[j] <- sum((bat3_prop_meiavida_geral[which(bat3_sp_meiavida_geral[,i]==mat_nomes_sp[j,i]),i]-mat_med_sp[j,i])^2)
  }
  ss.intra[i] <- sum(media)
}
abundancia <- apply(mat_length_sp,2,sum,na.rm=T)
gl.intra <- abundancia - gl.entre - 1
desv.intra <- ss.intra/gl.intra

bat3_estatistica_F_meia_vida <- desv.entre/desv.intra
summary(estatistica_F)
save(bat3_estatistica_F_meia_vida,file="bat3_estatistica_F_meia_vida.RData")

plot(bat3_estatistica_F_meia_vida~bat3_indice_dist)
plot(I(bat3_estatistica_F_meia_vida*as.integer(bat3_meia_vida))~bat3_indice_dist)
plot(estatistica_F~bat3_indice_dist,ylim=c(0,4000))
plot(estatistica_F~bat3_indice_dist,ylim=c(0,2000))
plot(estatistica_F~bat3_indice_dist,ylim=c(0,1000))
mod_efeito_f<-lm(estatistica_F~bat3_indice_dist)
mod_nulo_f<-lm(estatistica_F~1)
AICtab(mod_nulo_f,mod_efeito_f) #mod efeito! (negativo... igual ao fst_3!)
abline(mod_efeito_f,col="red")
abline(mod_nulo_f)

ss.total <- c()
for(i in 1:1000){
  ss.total[i] <- sum((na.omit(bat3_prop_meiavida_geral[,i]) - na.omit(vetor_med[i]))^2)
}
gl.total <- abundancia - 1
desv.total <- ss.total/gl.total
porc_entre <- desv.entre/desv.total

plot(porc_entre~bat3_indice_dist)

# no tempo de nger5000
#### componentes da variancia

# desv entre

bat3_prop_nger_geral <- (prop_nger_geral-1)/(20000-1)

vetor_med <- apply(bat3_prop_nger_geral,2,mean,na.rm=T)

mat_med_sp <- matrix(data=0,ncol=1000,nrow=500)
mat_length_sp <- matrix(data=0,ncol=1000,nrow=500)
mat_nomes_sp <- matrix(data=0,ncol=1000,nrow=500)
for(i in 1:1000){
  medias <- tapply(bat3_prop_nger_geral[,i],bat3_sp_nger[,i],mean,na.rm=T)
  mat_med_sp[,i] <- c(medias,rep(NA,500-length(medias)))
  tamanhos <- tapply(bat3_prop_nger_geral[,i],bat3_sp_nger[,i],length)
  mat_length_sp[,i] <- c(tamanhos,rep(NA,500-length(tamanhos)))
  nomes <- as.numeric(names(medias))
  mat_nomes_sp[,i] <- c(nomes,rep(NA,500-length(nomes)))
}

ss.entre <- c()
for(i in 1:1000){
  ss.entre[i] <- sum(((na.omit(mat_med_sp[,i]) - vetor_med[i])^2) * na.omit(mat_length_sp[,i]))
}
riqueza <- apply(bat3_sp_nger,2,function(x){length(unique(na.omit(x)))})
gl.entre <- riqueza-1
desv.entre <- ss.entre/gl.entre


# desv intra

ss.intra <- c()
for(i in 1:1000){
  media <- c()
  for(j in 1:riqueza[i]){
    media[j] <- sum((bat3_prop_nger_geral[which(bat3_sp_nger[,i]==mat_nomes_sp[j,i]),i]-mat_med_sp[j,i])^2)
  }
  ss.intra[i] <- sum(media)
}
abundancia <- apply(mat_length_sp,2,sum,na.rm=T)
gl.intra <- abundancia - gl.entre - 1
desv.intra <- ss.intra/gl.intra

bat3_estatistica_F_nger <- desv.entre/desv.intra
summary(bat3_estatistica_F_nger)
save(bat3_estatistica_F_nger,file="bat3_estatistica_F_nger.RData")

plot(bat3_estatistica_F_nger~bat3_indice_dist)
plot(bat3_estatistica_F_nger[which(riqueza!=1)]~bat3_indice_dist[which(riqueza!=1)])
plot(bat3_estatistica_F_nger[which(riqueza==2)]~bat3_indice_dist[which(riqueza==2)])
plot(bat3_estatistica_F_nger[which(riqueza==3)]~bat3_indice_dist[which(riqueza==3)])
plot(bat3_estatistica_F_nger[which(riqueza==5)]~bat3_indice_dist[which(riqueza==5)])
plot(bat3_estatistica_F_nger[which(riqueza==10)]~bat3_indice_dist[which(riqueza==10)])
plot(bat3_estatistica_F_nger[which(riqueza>=10)]~bat3_indice_dist[which(riqueza>=10)])

plot(estatistica_F~bat3_indice_dist,ylim=c(0,4000))
plot(estatistica_F~bat3_indice_dist,ylim=c(0,2000))
plot(estatistica_F~bat3_indice_dist,ylim=c(0,1000))
mod_efeito_f<-lm(estatistica_F~bat3_indice_dist)
mod_nulo_f<-lm(estatistica_F~1)
AICtab(mod_nulo_f,mod_efeito_f) #mod efeito! (negativo... igual ao fst_3!)
abline(mod_efeito_f,col="red")
abline(mod_nulo_f)


ss.total <- c()
for(i in 1:1000){
  ss.total[i] <- sum((na.omit(bat3_prop_nger_geral[,i]) - na.omit(vetor_med[i]))^2)
}
gl.total <- abundancia - 1
desv.total <- ss.total/gl.total
porc_entre <- desv.entre/desv.total
bat3_porc_entre <- porc_entre

plot(bat3_porc_entre~bat3_indice_dist)

plot(bat3_porc_entre[which(riqueza==2)]~bat3_indice_dist[which(riqueza==2)])
plot(bat3_porc_entre[which(riqueza==3)]~bat3_indice_dist[which(riqueza==3)])

save(bat3_porc_entre,file="bat3_porc_entre_nger5000.RData")
bat3_porc_entre_nger5000_riq2 <- bat3_porc_entre[which(riqueza==2)]
save(bat3_porc_entre_nger5000_riq2,file="bat3_porc_entre_nger5000_riq2.RData")
bat3_indice_dist_riq2 <- bat3_indice_dist[which(riqueza==2)]
save(bat3_indice_dist_riq2,file="bat3_indice_dist_riq2.RData")

# no tempo de nger5000
#### componentes da variancia

# desv entre

bat2_prop_nger_geral <- (prop_nger_geral-1)/(20000-1)

vetor_med <- apply(bat2_prop_nger_geral,2,mean,na.rm=T)

mat_med_sp <- matrix(data=0,ncol=1000,nrow=500)
mat_length_sp <- matrix(data=0,ncol=1000,nrow=500)
mat_nomes_sp <- matrix(data=0,ncol=1000,nrow=500)
for(i in 1:1000){
  medias <- tapply(bat2_prop_nger_geral[,i],bat2_sp_nger[,i],mean,na.rm=T)
  mat_med_sp[,i] <- c(medias,rep(NA,500-length(medias)))
  tamanhos <- tapply(bat2_prop_nger_geral[,i],bat2_sp_nger[,i],length)
  mat_length_sp[,i] <- c(tamanhos,rep(NA,500-length(tamanhos)))
  nomes <- as.numeric(names(medias))
  mat_nomes_sp[,i] <- c(nomes,rep(NA,500-length(nomes)))
}

ss.entre <- c()
for(i in 1:1000){
  ss.entre[i] <- sum(((na.omit(mat_med_sp[,i]) - vetor_med[i])^2) * na.omit(mat_length_sp[,i]))
}
riqueza <- apply(bat2_sp_nger,2,function(x){length(unique(na.omit(x)))})
gl.entre <- riqueza-1
desv.entre <- ss.entre/gl.entre

# desv intra

ss.intra <- c()
for(i in 1:1000){
  media <- c()
  for(j in 1:riqueza[i]){
    media[j] <- sum((bat2_prop_nger_geral[which(bat2_sp_nger[,i]==mat_nomes_sp[j,i]),i]-mat_med_sp[j,i])^2)
  }
  ss.intra[i] <- sum(media)
}
abundancia <- apply(mat_length_sp,2,sum,na.rm=T)
gl.intra <- abundancia - gl.entre - 1
desv.intra <- ss.intra/gl.intra

bat2_estatistica_F_nger <- desv.entre/desv.intra
summary(bat2_estatistica_F_nger)
save(bat2_estatistica_F_nger,file="bat2_estatistica_F_nger.RData")

plot(bat2_estatistica_F_nger~bat2_indice_dist)
plot(bat2_estatistica_F_nger[which(riqueza!=1)]~bat2_indice_dist[which(riqueza!=1)])
plot(bat2_estatistica_F_nger[which(riqueza==2)]~bat2_indice_dist[which(riqueza==2)])
plot(bat2_estatistica_F_nger[which(riqueza==3)]~bat2_indice_dist[which(riqueza==3)])
plot(bat2_estatistica_F_nger[which(riqueza==5)]~bat2_indice_dist[which(riqueza==5)])
plot(bat2_estatistica_F_nger[which(riqueza==10)]~bat2_indice_dist[which(riqueza==10)])
plot(bat2_estatistica_F_nger[which(riqueza>=10)]~bat2_indice_dist[which(riqueza>=10)])

plot(estatistica_F~bat2_indice_dist,ylim=c(0,4000))
plot(estatistica_F~bat2_indice_dist,ylim=c(0,2000))
plot(estatistica_F~bat2_indice_dist,ylim=c(0,1000))
mod_efeito_f<-lm(estatistica_F~bat2_indice_dist)
mod_nulo_f<-lm(estatistica_F~1)
AICtab(mod_nulo_f,mod_efeito_f) #mod efeito! (negativo... igual ao fst_3!)
abline(mod_efeito_f,col="red")
abline(mod_nulo_f)


ss.total <- c()
for(i in 1:1000){
  ss.total[i] <- sum((na.omit(bat2_prop_nger_geral[,i]) - na.omit(vetor_med[i]))^2)
}
gl.total <- abundancia - 1
desv.total <- ss.total/gl.total
porc_entre <- desv.entre/desv.total

bat2_porc_entre <- porc_entre

plot(porc_entre~bat2_indice_dist)
plot(bat2_porc_entre[which(riqueza==2)]~bat2_indice_dist[which(riqueza==2)])
plot(bat2_porc_entre[which(riqueza==3)]~bat2_indice_dist[which(riqueza==3)])
plot(porc_entre[which(riqueza==4)]~bat2_indice_dist[which(riqueza==4)])
plot(porc_entre[which(riqueza==5)]~bat2_indice_dist[which(riqueza==5)])
plot(porc_entre[which(riqueza==6)]~bat2_indice_dist[which(riqueza==6)])
plot(porc_entre[which(riqueza==7)]~bat2_indice_dist[which(riqueza==7)])

save(bat2_porc_entre,file="bat2_porc_entre_nger5000.RData")

bat2_porc_entre_nger5000_riq2 <- bat2_porc_entre[which(riqueza==2)]
save(bat2_porc_entre_nger5000_riq2,file="bat2_porc_entre_nger5000_riq2.RData")
bat2_indice_dist_riq2 <- bat2_indice_dist[which(riqueza==2)]
save(bat2_indice_dist_riq2,file="bat2_indice_dist_riq2.RData")
