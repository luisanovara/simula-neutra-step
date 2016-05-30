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
              list(min=1,max=1000), #para reduzir o tempo de proc., mudei de 20 000 para 1000
              list(min=0, max=5),
              list(min=1,max=100000),
              list(min=0,max=1))
################## RODANDO O HIPERCUBO ###################
uncoupled_hipercubo_25mar16 <- LHS(NULL, factors, N=100, q, q.arg) #para reduzir o tempo de proc., mudei de 10 000 simulacoes para 1000, e depois para 100
######### ACESSANDO OS VALORES DE INPUT #########
dados_25mar16<-get.data(uncoupled_hipercubo_25mar16)
dados2_25mar16<-cbind(round(dados_25mar16[,1]),dados_25mar16[,2],dados_25mar16[,3],round(dados_25mar16[,4]),dados_25mar16[,5]) #arredondando valores de parametros que precisam ser inteiros: S (numero de especies) e dist.pos (intervalo entre eventos de disturbio)
dados3_25mar16<-as.data.frame(dados2_25mar16)
####
save(dados3_25mar16,file="dados_hipercubo_25mar16.RData")

##########################################################
################# SIMULACOES 27/04/2016 ##################
##########################################################

################# LISTANDO OS PARÂMETROS #################
##################### DA MINHA FUNÇÃO ####################
factors <- c("S","xi0","dist.pos","dist.int")
############### DEFININDO AS DISTRIBUIÇÕES ###############
############ DE PROBABILIDADE DOS PARÂMETROS #############
q <- c("qunif","qunif","qunif","qunif")
################ DEFININDO OS PARÂMETROS #################
#################### DAS DISTRIBUIÇÕES ###################
q.arg <- list(list(min=5,max=500),
              list(min=1,max=6000),
              list(min=1,max=1000000),
              list(min=0,max=1))
################## RODANDO O HIPERCUBO ###################
uncoupled_hipercubo_27abr16 <- LHS(NULL, factors, N=1000, q, q.arg) 
######### ACESSANDO OS VALORES DE INPUT #########
dados_27abr16<-get.data(uncoupled_hipercubo_27abr16)
dados2_27abr16<-cbind(round(dados_27abr16[,1]),dados_27abr16[,2],round(dados_27abr16[,3]),dados_27abr16[,4]) #arredondando valores de parametros que precisam ser inteiros: S (numero de especies) e dist.pos (intervalo entre eventos de disturbio)
dados3_27abr16<-as.data.frame(dados2_27abr16)
####
save(uncoupled_hipercubo_27abr16,file="hipercubo_27abr16.RData")
save(dados_27abr16,file="dados_hipercubo_27abr16.RData")
save(dados3_27abr16,file="dados_arredond_hipercubo_27abr16.RData")

##########################################################
################# SIMULACOES 09/05/2016 ##################
##########################################################

##########################################################
############### EXTREMO 1 SP C/ EVOLUCAO #################
##########################################################

################# LISTANDO OS PARÂMETROS #################
##################### DA MINHA FUNÇÃO ####################
factors <- c("xi0","dist.pos","dist.int")
############### DEFININDO AS DISTRIBUIÇÕES ###############
############ DE PROBABILIDADE DOS PARÂMETROS #############
q <- c("qunif","qunif","qunif")
################ DEFININDO OS PARÂMETROS #################
#################### DAS DISTRIBUIÇÕES ###################
q.arg <- list(list(min=1,max=5000), #eu ia colocar de 1 a 6000, mas defini que, na simulacao com várias spp e sem evolucao, calcularei o X a partir do xi0 mínimo (1) e do J (5000), totalizando 5000. Como nao pode haver xi0 inicial maior do que o X, decidi abaixar o valor de xi0 máximo de todas as simulacoes
              list(min=1,max=3e5),
              list(min=0,max=1))
################## RODANDO O HIPERCUBO ###################
uncoupled_hipercubo_09mai16 <- LHS(NULL, factors, N=1000, q, q.arg) 
######### ACESSANDO OS VALORES DE INPUT #########
dados_09mai16<-get.data(uncoupled_hipercubo_09mai16)
dados2_09mai16<-cbind(dados_09mai16[,1],round(dados_09mai16[,2]),dados_09mai16[,3]) #arredondando valores do parametro que precisa ser inteiros: dist.pos (intervalo entre eventos de disturbio)
dados3_09mai16<-as.data.frame(dados2_09mai16)
####
save(uncoupled_hipercubo_09mai16,file="hipercubo_09mai16.RData")
save(dados_09mai16,file="dados_hipercubo_09mai16.RData")
save(dados3_09mai16,file="dados_arredond_hipercubo_09mai16.RData")

##########################################################
################# SIMULACOES 27/05/2016 ##################
##########################################################

##########################################################
############### EXTREMO 1 SP C/ EVOLUCAO #################
##########################################################

################# LISTANDO OS PARÂMETROS #################
##################### DA MINHA FUNÇÃO ####################
factors <- c("xi0","dist.pos","dist.int")
############### DEFININDO AS DISTRIBUIÇÕES ###############
############ DE PROBABILIDADE DOS PARÂMETROS #############
q <- c("qunif","qunif","qunif")
################ DEFININDO OS PARÂMETROS #################
#################### DAS DISTRIBUIÇÕES ###################
q.arg <- list(list(min=1,max=20000),
              list(min=1,max=3e5),
              list(min=0,max=1))
################## RODANDO O HIPERCUBO ###################
uncoupled_hipercubo_27mai16 <- LHS(NULL, factors, N=1000, q, q.arg) 
######### ACESSANDO OS VALORES DE INPUT #########
dados_27mai16<-get.data(uncoupled_hipercubo_27mai16)
dados2_27mai16<-cbind(dados_27mai16[,1],round(dados_27mai16[,2]),dados_27mai16[,3]) #arredondando valores do parametro que precisa ser inteiros: dist.pos (intervalo entre eventos de disturbio)
dados3_27mai16<-as.data.frame(dados2_27mai16)
####
save(uncoupled_hipercubo_27mai16,file="hipercubo_27mai16.RData")
save(dados_27mai16,file="dados_hipercubo_27mai16.RData")
save(dados3_27mai16,file="dados_arredond_hipercubo_27mai16.RData")
