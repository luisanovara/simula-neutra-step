######################################
######### Simulando cenario neutro Hubbel
######################################
# versao inicial A. A de Oliveira, outubro 2009, modificado por Paulo Inacio, outubro 2009
########################################
# funcao principal
###################
#      S =  número de especies da comunidade : inteiro positivo
#   *
#      J =  número de individuos da comunidade (tamanho da comunidade): inteiro positivo
#      j= número inicial de individuos por especie
#   *
#      xi =  número de propagulos que o individuo i produz por intervalo
#    *
#      cv =  proporcao de variacao na producao de propaculos em relacao a sua esperanca: real positivo
#    *
#      X =  total de propagulos que um individuo produz (esforco reprodutivo total): inteiro positivo
########### Deduzidos ###########  
#      Dado que o esforco reprodutivo total e o mesmo para todos os individuos (X), o tempo medio de vida de cada individuo e E[ti] = Xxi
#   *
#      A cada intervalo cada individuo tem uma probabilidade fixa pi de morrer. Portanto, a probabilidade de sobreviver t intervalos e dada por uma distribuicao geometrica1) com suporte 0,8. A esperanca desta distribuicao e E[ti]=pi-1, portanto:
#            pi = xiX
#    *
#      Dado s igual para todas as especies, e uma distribuicao gaussiana de caracteres continuos, o número de sementes por intervalo xi da prole de um individuo pode ser definido como uma variavel normal, com parâmetros
#            µ = xi
#            s = cv·µ 
#



simula.neutra.step=function(S= 100, j=10, X=1000, cv=0.1, ciclo=1e6, step=100)
{
t0=proc.time()[[3]]
  ## Tamanho da comunidade
  J <- S*j
  ##Verifica se X e multiplo de J
  if(abs(X/J - round(X/J)) > .Machine$double.eps^0.5) ## aqui verifica se o X/J e inteiro ou quase!
    {
    stop("\n\tO potencial reprodutivo (X) precisa ser multiplo do tamanho da comunidade (J). Tente novamente!\n\n")
    } 
  ##Matrizes para guardar os resultados
  ## matriz da especie de cada individuo por ciclo
  ind.mat=matrix(nrow=J,ncol=1+ciclo/step) 
  ##Matriz de propagulos produzidos por individuo em cada ciclo
  prop.mat=matrix(nrow=J,ncol=1+ciclo/step)
  ## Matriz de probabilidade de morte de cada individuo, por ciclo
  dead.mat=matrix(nrow=J,ncol=1+ciclo/step)
  ##Vetor com numero de mortos por ciclo
  n.dead <- c()
  n.dead[1] <- 0
  ##CONDICOES INICIAIS##
  ##Deduzidas de acordo com o modelo de Hubbell:
  ## Todas as especies comecam com o mesmo numero de individuos (j=J/S)
  ind.mat[,1] <- rep(1:S,each=j)
  cod.sp <- ind.mat[,1]
  ##A probabilidade inicial de morte, tomada de uma geometrica,
  ##dado que o numero de mortes esperado por ciclo eh a mesma para todos (p=1/J)
  dead.mat[,1] <- 1/J
  p.death <- dead.mat[,1]
  ##O esforco reprodutivo inicial e deduzido da esperanca de tempo de vida da geometrica E[t]=J
  ## o que portanto resulta em X/J propagulos produzidos a cada ciclo, por todos
  prop.mat[,1] <- X/J
  n.propag <- prop.mat[,1]
  ##Aqui comecam as simulacoes
  for(i in 2:(1+ciclo/step))
  {
    n.mortes <- 0
    for(j in 1:step) 
    {
      ## Sorteio dos que morrerao
      morte=rbinom(J, 1, prob=p.death)
      ##Total de mortos, que e armazenado em n.dead
      D=sum(morte)
      n.mortes <- n.mortes+D
      ## Se ha mortos comeca aqui a rotina de substituicao dos valores
      if(D>0) 
       {
           ##vetor de propagulos: cada propagulo tem o codigo numerico do individuo
          seed.bank <- rep(1:J,n.propag) 
          ##Indices dos individuos que morreram
          nascer= which(morte==1)
          ##Sorteio dos propagulos que irao repor os mortos
          mami=sample(seed.bank,D)
          ##Um vetor para armazenar o fenotipo do pai
          papi <- c()
          ##Um loop para sortear o pai entre os individuos da especie de cada
          ## propagulo-mae sorteado
          for(w in 1:D)
          	{
            papi[w] <- sample(n.propag[ cod.sp==cod.sp[mami[w]] ],1)
          	}
          ##O valor esperado de propagulos dos filhotes eh
          ## a media do numero medio de propagulos produzidos pelo pai e pela mae
          medias.prop=(n.propag[mami]+papi)/2
          ##Codigos das especies dos mortos sao substituidos pelos nascidos
          ##dos propagulos sorteados
          cod.sp[nascer]<-cod.sp[mami]
          ## Numero medio de propagulos dos novos individuos nascidos e sorteada
          ## de uma normal discretizada e truncada entre 1 e J,
          ## com o coeficiente de variacao estabelecido
          n.propag[nascer] <- sapply(medias.prop,rnormt,cv=cv,min=1,max=X)
          ##A matriz de probabilidades de morrer eh atualizada para os novos individuos
          p.death[nascer] <- n.propag[nascer]/X
       }
    }
    ## A cada step ciclos os resultados sao gravados
    ind.mat[,i] <- cod.sp
    dead.mat[,i] <- p.death
    prop.mat[,i] <- n.propag
    n.dead[i] <- n.mortes
    ##cat(format(Sys.time(), "%d%b%Y_%H:%M"), "\t ciclo = ", i, "\n") # para avisar a cada ciclo! desligar se estiver usando Rcloud
  }
  tempo <- seq(0,ciclo,by=step)
  colnames(ind.mat) <- tempo
  colnames(dead.mat) <- tempo
  colnames(prop.mat) <- tempo
  names(n.dead) <- tempo
  resulta=list(tempo=tempo,sp.list=ind.mat,sementes=prop.mat,prob.morte=dead.mat,n.mortes=n.dead)
  t1=proc.time()[[3]]
  cat("\n\t tempo de processamento: ", round((t1-t0)/60,2),"\n") 
  ## incluindo atributos no arquivo resulta
  attributes(resulta)$start=list(especies=S, individuos=j, nprop=X, cv=cv, ciclos=ciclo, passos=step)
  return(resulta)
}

## Pi esteve aqui
################################################
### funcăo modelo totalmente neutro - Hubell
#Esta e a funca simula.neutra.step editada para retirara o tradeoff reprodutivo e a variacao neste 
#caracter. O resulatdo e o modelo neutro do Hubbell, com a diferenca que o número esperado de mortes
# a cada ciclo e um, mas ha variacao estocastica nisto. no modelo original, ha sempre uma morte por ciclo
################################################
simula.neutra.hub=function(S= 100, j=10, ciclo=1e4, step=100){ 
  ## Tamanho da comunidade
  J <- S*j
  ##Matrizes para guardar os resultados
  ## matriz da especie de cada individuo por ciclo
  ind.mat=matrix(nrow=J,ncol=1+ciclo/step) 
  ##Vetor com numero de mortos por ciclo
  n.dead <- c()
  n.dead[1] <- 0
  ##CONDICOES INICIAIS##
  ##Deduzidas de acordo com o modelo de Hubbell:
  ## Todas as especies comecam com o mesmo numero de individuos (j=J/S)
  ind.mat[,1] <- rep(1:S,each=j)
  cod.sp <- ind.mat[,1]
  ##A probabilidade inicial de morte, tomada de uma geometrica,
  ##dado que o numero de mortes esperado por ciclo eh a mesma para todos (p=1/J)
  p.death <- 1/J
  ##Aqui comecam as simulacoes
  for(i in 2:(1+ciclo/step)){
    n.mortes <- 0
    for(j in 1:step){
      ## Sorteio dos que morrerao
      morte=rbinom(J, 1, prob=p.death)
      ##Total de mortos, que e armazenado em n.dead
      D=sum(morte)
      n.mortes <- n.mortes+D
      ## Se ha mortos comeca aqui a rotina de substituicao dos valores
      if(D>0) 
        {
          ##Indices dos individuos que morreram
          nascer= which(morte==1)
          ##Quem substitui os mortos
          novos <- sample(1:J,D,replace=T)
          ##Substituindo
          cod.sp[nascer]<-cod.sp[novos]
        }
    }
    ## A cada step ciclos os resultados sao gravados
    ind.mat[,i] <- cod.sp
    n.dead[i] <- n.mortes
  }
  tempo <- seq(0,ciclo,by=step)
  colnames(ind.mat) <- tempo
  names(n.dead) <- tempo
  resulta=list(tempo=tempo,sp.list=ind.mat,n.mortes=n.dead)
  return(resulta)
}

############################################################
#########################################################
# Normal Discretizada Truncada

#Sorteia n numeros inteiros de uma aproximacao discreta da normal truncada em seus limites inferiores 
#e superiores dados sua media e coeficiente de variacao. 

##Funcao de sorteio de uma normal discretizada e truncada
rnormt <- function(mean,n=1,cv,min,max)
{
  vals <- (min-0.5):(max+0.5)
  p <- pnorm(vals,mean=mean,sd=mean*cv)
  p2 <- diff(p)
  sample(min:max,n,prob=p2,replace=T)
}

##########################################

