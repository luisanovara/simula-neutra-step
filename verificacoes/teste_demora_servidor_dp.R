### Testes: porque as simulacoes dp 0 (?) demoram tanto?
## Variando dp e mantendo outros parametros constantes

# Pacote para rodar simula.neutra.trade
require(truncnorm)
# Script com codigo de simula.neutra.trade
#source("simula.neutra.trade_LEVE.R")
#source("simula.neutra.trade_LEVE_sem_banco_sem.R")

### Com banco de sementes 

teste0 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 0,dist.pos = seq(100,10000,100),dist.int = dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 195 min

teste0.0001 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 0.0001,dist.pos = seq(100,10000,100),dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 176.85 min

teste0.1 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 0.1,dist.pos = seq(100,10000,100),dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 170.34 min

teste0.5 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 0.5,dist.pos = seq(100,10000,100),dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 182.78 min

teste1 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 1,dist.pos = seq(100,10000,100),dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 185.08 min

teste10 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 10,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 166.94 min

teste100 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 100,dist.pos = seq(100,10000,100),dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 156.81 min

teste200 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 200,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 67.03 min

teste300 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 300,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 30.62 min

teste400 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 400,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 17.09 min

teste500 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 500,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 13.61 min

teste600 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 600,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 7.82 min

### Sem banco de sementes (simula.neutra.trade modificada em 07/07/16 apos sugestao do Ale)

teste0 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 0,dist.pos = seq(100,10000,100),dist.int = dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 6.08 min

teste0.0001 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 0.0001,dist.pos = seq(100,10000,100),dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 6.28 min

teste0.1 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 0.1,dist.pos = seq(100,10000,100),dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 6.96 min

teste0.5 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 0.5,dist.pos = seq(100,10000,100),dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 7.13 min

teste1 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 1,dist.pos = seq(100,10000,100),dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 6 min

teste10 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 10,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 6.71 min

teste100 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 100,dist.pos = seq(100,10000,100),dist.int = 0.3585,ciclo = 10000,step = 100) #duracao: 3 min

teste200 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 200,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 2.5 min

teste300 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 300,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 2.27 min

teste400 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 400,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 2.42 min

teste500 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 500,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 2.54 min

teste600 <- simula.neutra.trade(S = 428,j = 12, xi0 = rep(seq(1,20000,length.out = 428),each=12),X = 20000,dp = 600,dist.pos = seq(100,10000,100),dist.int =0.3585,ciclo = 10000,step = 100) #duracao: 2.66 min