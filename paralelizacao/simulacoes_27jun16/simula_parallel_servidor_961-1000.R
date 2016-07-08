### Script para paralelização no cluster Abacus (PI) - modificado por Renato Coutinho

# Pacotes para paralelizacao
require(plyr)
require(doMC)
# Pacote para rodar simula.neutra.trade
require(truncnorm)
# Script com codigo de simula.neutra.trade
source("simula.neutra.trade_LEVE.R")
# Dados do hipercubo
load("dados_arredond_hipercubo_27jun16.RData")

simula.parallel <- function(replica) {
  res <- simula.neutra.trade(S = dados3_27jun16[replica,1],
                             j = round(5000/dados3_27jun16[replica,1]),
                             xi0 = rep(seq(1,20000,length.out = dados3_27jun16[replica,1]),each=round(5000/dados3_27jun16[replica,1])),
                             X = 20000,
                             dp = 500,
                             dist.pos = seq(dados3_27jun16[replica,2],3e5,dados3_27jun16[replica,2]),
                             dist.int = dados3_27jun16[replica,3],
                             ciclo = 3e5,
                             step = 100)
  return(res)
}

######## doMC e plyr
registerDoMC(8)

for (i in (seq(1,1000,8)[121:125]))
{
  replica.sim <- as.list(i:(i+7))
  resultados <- llply(.data = replica.sim, .fun = simula.parallel, .parallel = TRUE)
  save(resultados,file=paste("resultados27jun16-",i,"_",i+7,".RData",sep=""))
}