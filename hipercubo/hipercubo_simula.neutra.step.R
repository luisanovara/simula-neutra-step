##########################################################
#################### HIPERCUBO LATINO ####################
##########################################################
require(pse)

##########################################################
################# SIMULACOES 24/01/2016 ##################
##########################################################

################# LISTANDO OS PARÂMETROS #################
##################### DA MINHA FUNÇÃO ####################
factors <- c("S","X","dp","dist.pos","dist.int")
############### DEFININDO AS DISTRIBUIÇÕES ###############
############ DE PROBABILIDADE DOS PARÂMETROS #############
q <- c("qunif", "qunif", "qunif","qunif","qunif")
################ DEFININDO OS PARÂMETROS #################
#################### DAS DISTRIBUIÇÕES ###################
q.arg <- list( list(min=1,max=50), list(min=5000,max=500000), list(min=0, max=1),
               list(min=1,max=1000), #PROVISORIO...
               list(min=0,max=1))
#######################################
#modelRun <- function (dados) {
  #mapply(simula.neutra.step.externa, dados[,1], dados[,2], dados[,3], dados[,4]) }
################## RODANDO O HIPERCUBO ###################
#res.name <- c("propagulos/ciclo")
#hipersuperincrivelcubolatino <- LHS(modelRun, factors, N=10, q, q.arg, res.name, nboot=50)
uncoupled_hipercubo <- LHS(NULL, factors, N=1000, q, q.arg, nboot=50)
######### ACESSANDO OS VALORES USADOS COMO INPUT #########
#get.data(hipersuperincrivelcubolatino)
dados<-get.data(uncoupled_hipercubo)
dados_24jan2016<-cbind(round(dados[,1]),round(dados[,2]),dados[,3],round(dados[,4]),dados[,5]) #arredondando valores de parametros que precisam ser inteiros
dados_24jan2016<-as.data.frame(dados_24jan2016)
################ ACESSANDO OS RESULTADOS #################hip
#get.results(hipersuperincrivelcubolatino)
################# ANÁLISE DE INCERTEZA ###################
#plotecdf(hipersuperincrivelcubolatino, stack=F)
############### ANÁLISE DE SENSIBILIDADE #################
#plotscatter(hipersuperincrivelcubolatino, add.lm=FALSE)
#plotprcc(hipersuperincrivelcubolatino)
############### SBMA: A AMOSTRA ESTÁ OK? #################
#targetLHS <- target.sbma (target=0.7, modelRun, factors, q, q.arg, res.names, FUN=min)

##########################################################
################# SIMULACOES 25/03/2016 ##################
##########################################################

################# LISTANDO OS PARÂMETROS #################
##################### DA MINHA FUNÇÃO ####################
factors <- c("S","xi0","dp","dist.pos","dist.int")
############### DEFININDO AS DISTRIBUIÇÕES ###############
############ DE PROBABILIDADE DOS PARÂMETROS #############
q <- c("qunif","qunif","qunif","qunif","qunif")
################ DEFININDO OS PARÂMETROS #################
#################### DAS DISTRIBUIÇÕES ###################
q.arg <- list(list(min=5,max=500),
              list(min=1,max=20000),
              list(min=0, max=5),
              list(min=1,max=100000),
              list(min=0,max=1))
################## RODANDO O HIPERCUBO ###################
uncoupled_hipercubo_25mar16 <- LHS(NULL, factors, N=10000, q, q.arg, nboot=50)
######### ACESSANDO OS VALORES DE INPUT #########
dados_25mar16<-get.data(uncoupled_hipercubo)
dados2_25mar16<-cbind(round(dados[,1]),dados[,2],dados[,3],round(dados[,4]),dados[,5]) #arredondando valores de parametros que precisam ser inteiros: S (numero de especies) e dist.pos (intervalo entre eventos de disturbio)
dados3_25mar16<-as.data.frame(dados2)

