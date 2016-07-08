### Script para paralelização no cluster Abacus (PI) - modificado por Renato Coutinho

# Pacotes para paralelizacao
require(parallel)
# Pacote para rodar simula.neutra.trade
require(truncnorm)
# Script com codigo de simula.neutra.trade
source("simula.neutra.trade_LEVE.R")
# Dados do hipercubo
load("dados_arredond_hipercubo_08jun16.RData")

replica.sim <- as.matrix(dados3_08jun16)

simula.parallel <- function(replica) {
    res <- tryCatch({
        simula.neutra.trade(S = replica[1],
                              j = round(5000/replica[1]),
                              xi0 = rep(seq(1,20000,length.out = replica[1]),each=round(5000/replica[1])),
                              X = 20000,
                              dp = 500,
                              dist.pos = seq(replica[2],3e5,replica[2]),
                              dist.int = replica[3],
                              ciclo = 3e5,
                              step = 100
                              )
    }, error = function(e) return(paste(e, "Problem parameters were",
                                        as.character(replica))))
    return(res)
}

######## parallel

cl_luisa_1 <- makePSOCKcluster(names=rep(c("abacus0002","abacus0003","abacus0004","abacus0005","abacus0006"),each=4))
clusterExport(cl_luisa_1, c("simula.neutra.trade"))
clusterEvalQ(cl_luisa_1,library(truncnorm))
results_08jun16 <- parApply(cl_luisa_1, replica.sim, 1, simula.parallel)
stopCluster(cl_luisa_1)
save(results_08jun16,file="resultados_08jun16.RData")
