### Script para paralelização no servidor Ubuntu (Ale)

# Pacotes para paralelizacao
require(plyr)
require(doMC)
# Script com codigo de simula.neutra.trade
source("simula.neutra.trade_LEVE_dp0_sem_banco_sem_papi_rapido.R")
# Dados do hipercubo
load("dados_arredond_hipercubo_25jul16.RData")

simula.parallel <- function(replica) {
  res <- simula.neutra.trade(S = dados3_25jul16[replica,1],
                             j = round(5000/dados3_25jul16[replica,1]),
                             xi0 = rep(seq(1,20000,length.out = dados3_25jul16[replica,1]),each=round(5000/dados3_25jul16[replica,1])),
                             X = 20000,
                             dp = 0,
                             dist.pos = if(dados3_25jul16[replica,2]>0 & dados3_25jul16[replica,2]<3e5) round(seq(from = 3e5/(dados3_25jul16[replica,2]+1), to = 3e5-(3e5/(dados3_25jul16[replica,2]+1)), length.out = dados3_25jul16[replica,2])) else if(dados3_25jul16[replica,2]==0) NULL else seq(1,3e5,1),
                             dist.int = dados3_25jul16[replica,3],
                             ciclo = 3e5,
                             step = 100
  )
  return(res)
}

######## doMC e plyr
registerDoMC(4)

for (i in (seq(1,1000,8)[31:35]))
{
  replica.sim <- as.list(i:(i+7))
  resultados <- llply(.data = replica.sim, .fun = simula.parallel, .parallel = TRUE)
  save(resultados,file=paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
}