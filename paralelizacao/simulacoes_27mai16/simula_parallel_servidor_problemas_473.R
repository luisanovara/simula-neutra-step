### Script para paralelização no servidor Ubuntu (Ale)

# Pacotes para paralelizacao
require(plyr)
require(doMC)
# Pacote para rodar simula.neutra.trade
require(truncnorm)
# Script com codigo de simula.neutra.trade
source("simula.neutra.trade_LEVE.R")
# Dados do hipercubo
load("dados_arredond_hipercubo_27mai16.RData")

simula.parallel <- function(replica) {
  res <- simula.neutra.trade(S = 1,
                             j = 5000,
                             xi0 = dados3_27mai16[replica,1],
                             X = 20000,
                             dp = 500,
                             dist.pos = seq(dados3_27mai16[replica,2],3e5,dados3_27mai16[replica,2]),
                             dist.int = dados3_27mai16[replica,3],
                             ciclo = 3e5,
                             step = 100
  )
  return(res)
}

######## doMC e plyr
registerDoMC(5)

for (i in 473)
{
  replica.sim <- as.list(i:(i+4))
  resultados <- llply(.data = replica.sim, .fun = simula.parallel, .parallel = TRUE)
  save(resultados,file=paste("resultados27mai16-",i,"_",i+7,".RData",sep=""))
}