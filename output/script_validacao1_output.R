#load("resultados08jan17.RData")
x<-resultados
nger <- 5000
prop_nger_5000_geral_validacao <- matrix(ncol=10,nrow=5000)
for(j in 1:length(x)){
  tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
  mortes_acumuladas <- x[[j]]$n.mortes.cumulativo
  n.mortes_prox_nger <- mortes_acumuladas - (nger*tam_com)
  ngeracao <- which(abs(n.mortes_prox_nger)==min(abs(n.mortes_prox_nger)))
  prop <- as.matrix(x[[j]]$sementes)
  prop_nger <- prop[,ngeracao]
  prop_nger_5000_geral_validacao[,j] <- prop_nger
}
save(prop_nger_5000_geral_validacao,file="prop_nger_5000_geral_validacao_14fev16_dp0.RData")