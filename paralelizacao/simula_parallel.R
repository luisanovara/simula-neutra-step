### Script para paralelização

library("plyr")
library("doMC")

pars.sim <- list()
for(i in 1:dim(dados3_09mai16)[1]) {
    pars.sim[[i]] <- dados3_09mai16[i,]
}

replica.sim <- as.list(1:dim(dados3_09mai16)[1])

simula.parallel <- function(replica) {
    res <- simula.neutra.step(S = 1,
                              j = 5000,
                              xi0 = dados3_09mai16[replica,1],
                              dp = 500,
                              dist.pos = seq(dados3_09mai16[replica,2],3e5,dados3_09mai16[replica,2]),
                              dist.int = dados3_09mai16[replica,3],
                              ciclo = 3e5,
                              step = 100
                              )
    return(res)
}

registerDoMC(8)

results <- llply(.data = replica.sim, .fun = simula.parallel, .parallel = TRUE)
