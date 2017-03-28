# sortear valores de uma distribuicao normal, e separa-los entre diferentes numeros de especies. 
especifica <- function(vetor_riqueza,n_total,media,dp) {
  mat_final <- matrix(ncol=6,nrow=length(vetor_riqueza))
  for (i in 1:length(vetor_riqueza)){
    abund_sp <- round(n_total/vetor_riqueza[i])
    abund_total <- abund_sp*vetor_riqueza[i]
    sp_ind <- rep(1:vetor_riqueza[i], each=abund_sp)
    prop_ind <- rnorm(abund_total,mean=media,sd=dp)
    prop_sp <- tapply(prop_ind,sp_ind,mean)
    prop_sp_vetor <- rep(prop_sp,each=abund_sp)
    prop_com <- mean(prop_ind)
    ss_inter <- abund_sp*sum((prop_sp-prop_com)^2)
    gl_inter <- vetor_riqueza[i] - 1
    var_inter <- ss_inter/gl_inter
    ss_total <- sum((prop_ind-prop_com)^2)
    gl_total <- abund_total - 1
    var_total <- ss_total/gl_total
    ss_intra <- sum((prop_ind-prop_sp_vetor)^2)
    gl_intra <- gl_total - gl_inter
    var_intra <- ss_intra/gl_intra
    mat_final[i,1] <- ss_intra
    mat_final[i,2] <- var_intra
    mat_final[i,3] <- ss_inter
    mat_final[i,4] <- var_inter
    mat_final[i,5] <- ss_total
    mat_final[i,6] <- var_total
  }
  colnames(mat_final) <- c("ss_intra","var_intra","ss_inter","var_inter","ss_total","var_total")
  rownames(mat_final) <- vetor_riqueza
  return(mat_final)
}

vetor_riqueza2 <- c(1,seq(0,500,5)[-1])

opa<-especifica(vetor_riqueza2,20000,10000,500)
opa <- cbind(opa,var_inter_relativa=opa[,4]/opa[,6])
plot(opa[,1]~vetor_riqueza2,pch=20)
plot(opa[,2]~vetor_riqueza2,pch=20)
plot(opa[,3]~vetor_riqueza2,pch=20)
plot(opa[,4]~vetor_riqueza2,pch=20)
plot(opa[,5]~vetor_riqueza2,pch=20)
plot(opa[,6]~vetor_riqueza2,pch=20)
plot(opa[,7]~vetor_riqueza2,pch=20)

opa2<-especifica(vetor_riqueza2,5000,10000,500)
plot(opa2[,1]~vetor_riqueza2,pch=20)
plot(opa2[,2]~vetor_riqueza2,pch=20)
plot(opa2[,3]~vetor_riqueza2,pch=20)
plot(opa2[,4]~vetor_riqueza2,pch=20)
plot(opa2[,5]~vetor_riqueza2,pch=20)
plot(opa2[,6]~vetor_riqueza2,pch=20)

opa3<-especifica(vetor_riqueza2,2000,10000,500)
plot(opa3[,1]~vetor_riqueza2,pch=20)
plot(opa3[,2]~vetor_riqueza2,pch=20)
plot(opa3[,3]~vetor_riqueza2,pch=20,ylim=c(0,1.5e8))
plot(opa3[,4]~vetor_riqueza2,pch=20)
plot(opa3[,4]~vetor_riqueza2,pch=20,xlim=c(0,100))
plot(opa3[,5]~vetor_riqueza2,pch=20)
plot(opa3[,6]~vetor_riqueza2,pch=20)

opa4<-especifica(vetor_riqueza2,2000,10000,50)
plot(opa4[,1]~vetor_riqueza2,pch=20)
plot(sqrt(opa4[,1])~vetor_riqueza2,pch=20)
plot(opa4[,2]~vetor_riqueza2,pch=20)
plot(sqrt(opa4[,2])~vetor_riqueza2,pch=20)
plot(opa4[,3]~vetor_riqueza2,pch=20)
plot(sqrt(opa4[,3])~vetor_riqueza2,pch=20)
plot(opa4[,4]~vetor_riqueza2,pch=20)
plot(sqrt(opa4[,4])~vetor_riqueza2,pch=20)
plot(opa4[,5]~vetor_riqueza2,pch=20)
plot(sqrt(opa4[,5])~vetor_riqueza2,pch=20)
plot(opa4[,6]~vetor_riqueza2,pch=20)
plot(sqrt(opa4[,6])~vetor_riqueza2,pch=20)


especifica_ordem <- function(vetor_riqueza,n_total,media,dp) {
  mat_final <- matrix(ncol=6,nrow=length(vetor_riqueza))
  for (i in 1:length(vetor_riqueza)){
    abund_sp <- round(n_total/vetor_riqueza[i])
    abund_total <- abund_sp*vetor_riqueza[i]
    sp_ind <- rep(1:vetor_riqueza[i], each=abund_sp)
    prop_ind <- rnorm(abund_total,mean=media,sd=dp)
    prop_ind <- sort(prop_ind)
    prop_sp <- tapply(prop_ind,sp_ind,mean)
    prop_sp_vetor <- rep(prop_sp,each=abund_sp)
    prop_com <- mean(prop_ind)
    ss_inter <- abund_sp*sum((prop_sp-prop_com)^2)
    gl_inter <- vetor_riqueza[i] - 1
    var_inter <- ss_inter/gl_inter
    ss_total <- sum((prop_ind-prop_com)^2)
    gl_total <- abund_total - 1
    var_total <- ss_total/gl_total
    ss_intra <- sum((prop_ind-prop_sp_vetor)^2)
    gl_intra <- gl_total - gl_inter
    var_intra <- ss_intra/gl_intra
    mat_final[i,1] <- ss_intra
    mat_final[i,2] <- var_intra
    mat_final[i,3] <- ss_inter
    mat_final[i,4] <- var_inter
    mat_final[i,5] <- ss_total
    mat_final[i,6] <- var_total
  }
  colnames(mat_final) <- c("ss_intra","var_intra","ss_inter","var_inter","ss_total","var_total")
  rownames(mat_final) <- vetor_riqueza
  return(mat_final)
}

opa5<-especifica_ordem(vetor_riqueza2,20000,10000,500)
opa5 <- cbind(opa5,var_inter_relativa=opa5[,4]/opa5[,6])
plot(opa5[,1]~vetor_riqueza2,pch=20)
plot(opa5[,2]~vetor_riqueza2,pch=20)
plot(opa5[,3]~vetor_riqueza2,pch=20)
plot(opa5[,3][-1]~vetor_riqueza2[-1],pch=20)
plot(opa5[,4]~vetor_riqueza2,pch=20)
plot(opa5[,5]~vetor_riqueza2,pch=20)
plot(opa5[,6]~vetor_riqueza2,pch=20)
plot(opa5[,7]~vetor_riqueza2,pch=20)

opa6<-especifica_ordem(vetor_riqueza2,20000,10000,50)
plot(opa6[,3]~vetor_riqueza2,pch=20)

especifica_ordem_unif <- function(vetor_riqueza,n_total,min,max) {
  mat_final <- matrix(ncol=6,nrow=length(vetor_riqueza))
  for (i in 1:length(vetor_riqueza)){
    abund_sp <- round(n_total/vetor_riqueza[i])
    abund_total <- abund_sp*vetor_riqueza[i]
    sp_ind <- rep(1:vetor_riqueza[i], each=abund_sp)
    prop_ind <- runif(abund_total,min=min,max=max)
    prop_ind <- sort(prop_ind)
    prop_sp <- tapply(prop_ind,sp_ind,mean)
    prop_sp_vetor <- rep(prop_sp,each=abund_sp)
    prop_com <- mean(prop_ind)
    ss_inter <- abund_sp*sum((prop_sp-prop_com)^2)
    gl_inter <- vetor_riqueza[i] - 1
    var_inter <- ss_inter/gl_inter
    ss_total <- sum((prop_ind-prop_com)^2)
    gl_total <- abund_total - 1
    var_total <- ss_total/gl_total
    ss_intra <- sum((prop_ind-prop_sp_vetor)^2)
    gl_intra <- gl_total - gl_inter
    var_intra <- ss_intra/gl_intra
    mat_final[i,1] <- ss_intra
    mat_final[i,2] <- var_intra
    mat_final[i,3] <- ss_inter
    mat_final[i,4] <- var_inter
    mat_final[i,5] <- ss_total
    mat_final[i,6] <- var_total
  }
  colnames(mat_final) <- c("ss_intra","var_intra","ss_inter","var_inter","ss_total","var_total")
  rownames(mat_final) <- vetor_riqueza
  return(mat_final)
}

opa7<-especifica_ordem(vetor_riqueza2,20000,1,20000)
opa7 <- cbind(opa7,var_inter_relativa=opa7[,4]/opa7[,6])
plot(opa7[,1]~vetor_riqueza2,pch=20)
plot(opa7[,2]~vetor_riqueza2,pch=20)
plot(opa7[,3]~vetor_riqueza2,pch=20)
plot(opa7[,3][-1]~vetor_riqueza2[-1],pch=20)
plot(opa7[,4]~vetor_riqueza2,pch=20)
plot(opa7[,5]~vetor_riqueza2,pch=20)
plot(opa7[,6]~vetor_riqueza2,pch=20)
plot(opa7[,7]~vetor_riqueza2,pch=20)

# nao houve diferenca entre var inter e var inter relativa, entao acho melhor eliminar a var inter relativa, por causa do motivo apontado no outro script (a var inter relativa eh insensivel a distancia entre especies, porque ela eh sempre igual para comunidades com a mesma riqueza, independetemente dos valores das especies)
# entre var inter e ss inter, acho melhor ficar com a ss inter, porque se ganha a informacao sobre a ss intra tambem. a var inter tb eh correlacionada com a riqueza (soh q em vez de positivamente, eh negativamente), entao nao vejo vantagens.


load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")
# ss total
plot(dados$ss_total~dados$dist_indice_scale,pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$ss_total[dados$bateria==1]~dados$dist_indice_scale[dados$bateria==1],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$ss_total[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$ss_total[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l",col=colors()[dados$dist_classes])
# ss inter
plot(dados$ss_inter~dados$dist_indice_scale,pch=20,bty="l",col=colors()[dados$dist_classes])
#plot(dados$ss_inter[dados$bateria==1]~dados$dist_indice_scale[dados$bateria==1],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$ss_inter[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$ss_inter[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l",col=colors()[dados$dist_classes])
# ss inter relativo
plot(dados$ss_inter_relativo~dados$dist_indice_scale,pch=20,bty="l",col=colors()[dados$dist_classes])
#plot(dados$ss_inter_relativo[dados$bateria==1]~dados$dist_indice_scale[dados$bateria==1],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$ss_inter_relativo[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$ss_inter_relativo[dados$bateria==2&dados$riqueza_final==2]~dados$dist_indice_scale[dados$bateria==2&dados$riqueza_final==2],pch=20,bty="l",col=colors()[dados$dist_classes[dados$riqueza_final==2]])
plot(dados$ss_inter_relativo[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l",xlim=c(-1,3))
plot(dados$ss_inter_relativo[dados$bateria==3&dados$riqueza_final==2]~dados$dist_indice_scale[dados$bateria==3&dados$riqueza_final==2],pch=20,bty="l",xlim=c(-1,3))
# var total
plot(dados$var_total~dados$dist_indice_scale,pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$var_total[dados$bateria==1]~dados$dist_indice_scale[dados$bateria==1],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$var_total[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$var_total[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l",col=colors()[dados$dist_classes])
# var inter
plot(dados$var_inter~dados$dist_indice_scale,pch=20,bty="l",col=colors()[dados$dist_classes])
#plot(dados$var_inter[dados$bateria==1]~dados$dist_indice_scale[dados$bateria==1],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$var_inter[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$var_inter[dados$bateria==2&dados$riqueza_final==2]~dados$dist_indice_scale[dados$bateria==2&dados$riqueza_final==2],pch=20,bty="l")
plot(dados$var_inter[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$var_inter[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l")
plot(dados$var_inter[dados$bateria==3&dados$riqueza_final==2]~dados$dist_indice_scale[dados$bateria==3&dados$riqueza_final==2],pch=20,bty="l")
plot(dados$var_inter_corrigida[dados$bateria==3&dados$riqueza_final==2]~dados$dist_indice_scale[dados$bateria==3&dados$riqueza_final==2],pch=20,bty="l")
# var inter relativa
plot(dados$var_inter_relativa~dados$dist_indice_scale,pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$var_inter_corrigida_relativa~dados$dist_indice_scale,pch=20,bty="l",col=colors()[dados$dist_classes])
#plot(dados$var_inter[dados$bateria==1]~dados$dist_indice_scale[dados$bateria==1],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$var_inter_relativa[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=colors()[dados$dist_classes])
plot(dados$var_inter_relativa[dados$bateria==2&dados$riqueza_final==2]~dados$dist_indice_scale[dados$bateria==2&dados$riqueza_final==2],pch=20,bty="l",col=colors()[dados$dist_classes[dados$riqueza_final==2]])
plot(dados$var_inter_relativa[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l",xlim=c(-1,3))
plot(dados$var_inter_relativa[dados$bateria==3&dados$riqueza_final==2]~dados$dist_indice_scale[dados$bateria==3&dados$riqueza_final==2],pch=20,bty="l",xlim=c(-1,3))
plot(dados$var_inter_corrigida_relativa[dados$bateria==3&dados$riqueza_final==2]~dados$dist_indice_scale[dados$bateria==3&dados$riqueza_final==2],pch=20,bty="l",xlim=c(-1,3))
# utilizarei o ss inter (e ss inter relativo). nao houve diferenca entre o ss inter e a var inter na relacao com o disturbio, e nem houve "interferencia" da riqueza final nesta relacao (i.e., analisar todas as comunidades ou apenas aquelas com riqueza final 2, por exemplo, nao mudou o padrao de ss inter com o disturbio - e nem de var inter). porem, o ss inter tem a vantagem de ser partitivo (somado ao ss intra, dah o ss total), o que facilita a interpretacao do ss inter relativo, que eh interessante de analisar.
# POREM, para nao ter que incluir a riqueza final como uma covariavel (porque ignorar tb eh osso), eu pode fazer COM AS DUAS: SS INTER E VAR INTER (NAO CORRIGIDA)
# MELHOR USAR SOH A VAR INTER E QUE SE FODA