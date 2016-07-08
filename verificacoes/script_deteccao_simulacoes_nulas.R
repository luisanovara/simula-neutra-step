simulacoes_27jun16_output<-list()

for(i in seq(from=1,to=1000,by=8)[64:65]){ 

load(paste("resultados27jun16-",i,"_",i+7,".RData",sep=""))

tf<-c(NA)
lista_corretas<-list(NA)

if(length(resultados)==0) {
    simulacoes_27jun16_output[[i]] <- 0
    simulacoes_27jun16_output[[i+1]] <- 0
    simulacoes_27jun16_output[[i+2]] <- 0
    simulacoes_27jun16_output[[i+3]] <- 0
    simulacoes_27jun16_output[[i+4]] <- 0
    simulacoes_27jun16_output[[i+5]] <- 0
    simulacoes_27jun16_output[[i+6]] <- 0
    simulacoes_27jun16_output[[i+7]] <- 0
}

else{

for(j in 1:8){
  tf[j]<-is.null(resultados[[j]])
  if(tf[j]==T) {simulacoes_27jun16_output[[(i+j-1)]]<-0}
  else
    (lista_corretas[[length(tf)-sum(tf)]]<-resultados[[j]])
}

for(k in 1:length(lista_corretas)){  
    simulacoes_27jun16_output[[i+ which(tf==F)[k]-1]]<-simula_output(lista_corretas)[[k]]
    }
}

}

sucesso_sim<-rep(NA,length(simulacoes_27jun16_output))

for(i in 1:length(simulacoes_27jun16_output)) {
  sucesso_sim[i]<- length(simulacoes_27jun16_output[[i]])
}
sum(sucesso_sim==1)