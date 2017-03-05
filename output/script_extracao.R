nger <- 5000
sempapi_prop_nger_5000_geral <- matrix(ncol=1000,nrow=5500)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados15fev17-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  sempapi_8_prop_nger_5000_geral <- matrix(ncol=8,nrow=5500)
  for(j in 1:length(x)){
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    mortes_acumuladas <- x[[j]]$n.mortes.cumulativo
    n.mortes_prox_nger <- mortes_acumuladas - (nger*tam_com)
    ngeracao <- which(abs(n.mortes_prox_nger)==min(abs(n.mortes_prox_nger)))
    prop <- as.matrix(x[[j]]$sementes)
    prop_nger <- prop[,ngeracao]
    sempapi_8_prop_nger_5000_geral[,j] <- c(prop_nger,rep(NA,5500-tam_com))
  }
  sempapi_prop_nger_5000_geral[,i:(i+7)] <- sempapi_8_prop_nger_5000_geral
}
save(sempapi_prop_nger_5000_geral,file="resultados15fev17_1-.RData")