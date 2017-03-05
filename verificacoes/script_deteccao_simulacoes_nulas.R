require(PerformanceAnalytics)

simula_output<-function(lista_simulacoes,nger=5000)
{
  x<-lista_simulacoes
  resultado<-matrix()
  resultado_lista<-list()
  for (i in 1:length(x))
  {
    tam_com <- attributes(x[[i]])$start[[1]]*attributes(x[[i]])$start[[2]]
    mortes_acumuladas <- x[[i]]$n.mortes.cumulativo
    n.mortes_prox_nger <- mortes_acumuladas - (nger*tam_com)
    ngeracao <- which(abs(n.mortes_prox_nger)==min(abs(n.mortes_prox_nger)))
    prop <- as.matrix(x[[i]]$sementes)
    sp <- as.matrix(x[[i]]$sp.list)
    abund<-as.vector(table(sp[,ngeracao]))
    riq.fin <- length(unique(sp[,ngeracao]))
    riq.inic <- length(unique(sp[,1]))
    med_geral <- mean(prop[,ngeracao])
    med_sp <- tapply(X=prop[,ngeracao],INDEX=sp[,ngeracao],FUN=mean)
    median_geral <- median(prop[,ngeracao])
    median_sp <- tapply(X=prop[,ngeracao],INDEX=sp[,ngeracao],FUN=median)
    moda_geral <- prop[,ngeracao][max(table(prop[,ngeracao]))]
    moda_sp <- tapply(X=prop[,ngeracao],INDEX=sp[,ngeracao],FUN=function(coisa){return(coisa[max(table(coisa))])})
    vari_geral <- var(prop[,ngeracao])
    vari_sp <- tapply(X=prop[,ngeracao],INDEX=sp[,ngeracao],FUN=var)
    assim_geral <- skewness(prop[,ngeracao])
    assim_sp <- tapply(X=prop[,ngeracao],INDEX=sp[,ngeracao],FUN=skewness)
    exc_curt_geral <- kurtosis(prop[,ngeracao])
    exc_curt_sp <- tapply(X=prop[,ngeracao],INDEX=sp[,ngeracao],FUN=kurtosis)
    resultado <- matrix(data=c(riq.inic,rep(NA,riq.fin),riq.fin,rep(NA,riq.fin),tam_com,abund,med_geral,med_sp,median_geral,median_sp,moda_geral,moda_sp,vari_geral,vari_sp,assim_geral,assim_sp,exc_curt_geral,exc_curt_sp),ncol=9,dimnames=list(c("geral",paste("sp",(unique(sp[,ngeracao])[order(unique(sp[,ngeracao]))]),sep="")),c("riq_inic","riq_fin","abundancia","media","mediana","moda","variancia","assimetria","excesso_curtose")))
    resultado_lista[[i]]<-resultado
  }
  return(resultado_lista)
}

simulacoes_pos_comite_dp100_output<-list()

for(i in seq(from=1,to=1000,by=8)){ 

load(paste("resultados15fev17_dp100_-",i,"_",i+7,".RData",sep=""))

tf<-c(NA)
lista_corretas<-list(NA)

if(length(resultados)==0) {
  simulacoes_pos_comite_dp100_output[[i]] <- 0
  simulacoes_pos_comite_dp100_output[[i+1]] <- 0
  simulacoes_pos_comite_dp100_output[[i+2]] <- 0
  simulacoes_pos_comite_dp100_output[[i+3]] <- 0
  simulacoes_pos_comite_dp100_output[[i+4]] <- 0
  simulacoes_pos_comite_dp100_output[[i+5]] <- 0
  simulacoes_pos_comite_dp100_output[[i+6]] <- 0
  simulacoes_pos_comite_dp100_output[[i+7]] <- 0
}

else{

for(j in 1:8){
  tf[j]<-is.null(resultados[[j]])
  if(tf[j]==T) {simulacoes_pos_comite_dp100_output[[(i+j-1)]]<-0}
  else
    (lista_corretas[[length(tf)-sum(tf)]]<-resultados[[j]])
}

for(k in 1:length(lista_corretas)){  
  simulacoes_pos_comite_dp100_output[[i+ which(tf==F)[k]-1]]<-simula_output(lista_corretas)[[k]]
    }
}

}

sucesso_sim<-rep(NA,length(simulacoes_pos_comite_dp100_output))

for(i in 1:length(simulacoes_pos_comite_dp100_output)) {
  sucesso_sim[i]<- length(simulacoes_pos_comite_dp100_output[[i]])
}
sum(sucesso_sim==1)
