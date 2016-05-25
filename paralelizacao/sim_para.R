oi<-data.frame(rep(100,8),rep(10000,8),rep(100,8))

replica.sim <- as.list(1:8)

simula.parallel <- function(replica) {
  res <- simula.neutra.step(S = 100,
                            j = 10,
                            xi0 = 100,
                            dp = 5,
                            dist.pos = 100,
                            dist.int = 0.5,
                            ciclo = 10000,
                            step = 100
  )
  return(res)
}

registerDoMC(8)

results <- llply(.data = replica.sim, .fun = simula.parallel, .parallel = TRUE)
