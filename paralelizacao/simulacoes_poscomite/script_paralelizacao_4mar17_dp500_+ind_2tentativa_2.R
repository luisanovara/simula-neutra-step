### Script para paralelização no servidor Ubuntu (Ale)

# Pacotes para paralelizacao
require(plyr)
require(doMC)
# Pacote para rodar codigo
require(truncnorm)
# Script com codigo de simula.neutra.trade
source("simula.neutra.trade_LEVE_sem_banco_sem_papi_rapido.R")
# Dados do hipercubo
load("dados_arredond_hipercubo_04mar17.RData")
dados <- dados3_04mar17

simula.parallel <- function(replica) {
  res <- simula.neutra.trade(S = dados[replica,1],
                             j = round(20000/dados[replica,1]),
                             xi0 = rep(seq(1,20000,length.out = dados[replica,1]),each=round(20000/dados[replica,1])),
                             X = 20000,
                             dp = 500,
                             dist.pos = if(dados[replica,2]>0 & dados[replica,2]<75e3) round(seq(from = 75e3/(dados[replica,2]+1), to = 75e3-(75e3/(dados[replica,2]+1)), length.out = dados[replica,2])) else if(dados[replica,2]==0) NULL else seq(1,75e3,1),
                             dist.int = dados[replica,3],
                             ciclo = 75e3,
                             step = 100
  )
  return(res)
}

######## doMC e plyr
registerDoMC(8)

for (i in (seq(1,1000,8)[43:50][1]))
{
  replica.sim <- as.list(i:(i+7))
  resultados <- llply(.data = replica.sim, .fun = simula.parallel, .parallel = TRUE)
  save(resultados,file=paste("resultados4mar17_dp500_+ind_2tentativa_",i,"-",i+7,".RData",sep=""))
}