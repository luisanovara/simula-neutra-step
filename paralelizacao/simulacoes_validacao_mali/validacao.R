##### Validacao do modelo (sugerida por Mali)

### Script para paralelização no servidor Ubuntu (Ale)

# Pacotes para paralelizacao
require(plyr)
require(doMC)
# Pacote para rodar simula.neutra.trade
require(truncnorm)
# Script com codigo de simula.neutra.trade
source("simula.neutra.trade_LEVE_sem_banco_sem_papi_rapido.R")
# Dados para dp
vetor_dp <- rep(0,10)

simula.parallel <- function(replica) {
  res <- simula.neutra.trade(S = 10,
                             j = 500,
                             xi0 = rep(seq(1,20000,length.out = 10),each=500),
                             X = 20000,
                             dp = vetor_dp[replica],
                             dist.pos = seq(1,3e5),
                             dist.int = 1,
                             ciclo = 3e5,
                             step = 100
  )
  return(res)
}

######## doMC e plyr
registerDoMC(8)
replica.sim <- as.list(1:10)
resultados <- llply(.data = replica.sim, .fun = simula.parallel, .parallel = TRUE)
save(resultados,file="resultados14fev17_poscomite_dp0.RData")