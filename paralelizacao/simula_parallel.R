### Script para paralelização

# Pacotes para paralelizacao
#require(plyr)
#require(doMC)
require(parallel)
# Pacote para rodar simula.neutra.trade
require(truncnorm)
# Script com codigo de simula.neutra.trade
source("simula.neutra.trade.R")
# Dados do hipercubo
load("dados_arredond_hipercubo_27mai16.RData")

#pars.sim <- list()
#for(i in 1:dim(dados3_09mai16)[1]) {
    #pars.sim[[i]] <- dados3_09mai16[i,]
#}

replica.sim <- as.list(1:dim(dados3_27mai16)[1])

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

#registerDoMC(8)

#results <- llply(.data = replica.sim, .fun = simula.parallel, .parallel = TRUE)

######## parallel

cl_luisa_1 <- makePSOCKcluster(names=rep(c("abacus0002","abacus0003","abacus0004","abacus0005","abacus0006"),each=8))
clusterExport(cl_luisa_1, c("simula.neutra.trade","dados3_27mai16","rtruncnorm"))
results_27mai16 <- parLapply(cl_luisa_1, replica.sim, simula.parallel)
stopCluster(cl_luisa_1)
save(results_27mai16,file="resultados_27mai16.RData")
