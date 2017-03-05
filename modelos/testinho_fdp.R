dados3_21jul16<-data.frame(xi0=c(100,100,100),dist.pos=c(50,0,100),dist.int=c(0.5,0.5,0.5))
registerDoMC(1)
replica.sim<-list(1,2,3)
resultados <- llply(.data = replica.sim, .fun = simula.parallel, .parallel = TRUE)

simula.parallel <- function(replica) {
  res <- simula.neutra.trade(S = 1,
                             j = 50,
                             xi0 = dados3_21jul16[replica,1],
                             X = 1000,
                             dp = 5,
                             dist.pos = if(dados3_21jul16[replica,2]>0 & dados3_21jul16[replica,2]<100) round(seq(from = 100/(dados3_21jul16[replica,2]+1), to = 100-(100/(dados3_21jul16[replica,2]+1)), length.out = dados3_21jul16[replica,2])) else if(dados3_21jul16[replica,2]==0) NULL else seq(1,100,1),
                             dist.int = dados3_21jul16[replica,3],
                             ciclo = 100,
                             step = 10
  )
  return(res)
}