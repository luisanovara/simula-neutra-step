# MODELOS REAL OFICIAL nao linear
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

######################### MEDIA #########################

# CENARIO EXCLUSAO

media_excl <- dados$media[dados$bateria==2]/20000.0001
dist_excl <- dados$dist_indice[dados$bateria==2]

# MODELOS

## NORMAL

### nulo
media_nulo_excl<-lm(media_excl~1)
funcao_nulo_mle_normal <- function(mean,sd){
  -sum(dnorm(media_excl, mean=mean, sd=sd, log=TRUE))
}
media_nulo_excl_mle_normal <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(media_excl),sd=sd(media_excl)))
media_nulo_excl_mle_normal_nm <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(media_excl),sd=sd(media_excl)),method = "Nelder-Mead")

### linear
media_linear_excl<-lm(media_excl~dist_excl)
funcao_linear_mle_normal <- function(a,b,sd){
  mean <- a*dist_excl+b
  -sum(dnorm(media_excl, mean=mean, sd=sd, log=TRUE))
}
media_linear_excl_mle_normal <- mle2(funcao_linear_mle_normal, start=list(a=coef(media_linear_excl)[[2]],b=coef(media_linear_excl)[[1]],sd=sd(media_excl)))
media_linear_excl_mle_normal_nm <- mle2(funcao_linear_mle_normal, start=list(a=coef(media_linear_excl)[[2]],b=coef(media_linear_excl)[[1]],sd=sd(media_excl)),method = "Nelder-Mead")

### mm
media_coef_mm_linearizada_excl <- coef(lm(I(dist_excl/media_excl)~dist_excl))
media_mm_excl <- nls(media_excl~I(I(a*dist_excl)/I(b+dist_excl)+c),start=list(a=1/media_coef_mm_linearizada_excl[[2]],b=media_coef_mm_linearizada_excl[[1]]*(1/media_coef_mm_linearizada_excl[[2]]),c=0))
funcao_mm_mle_normal <- function(a,b,c,sd){
  mean <- ((a*dist_excl)/(b+dist_excl))+c
  -sum(dnorm(media_excl, mean=mean, sd=sd, log=TRUE))
}
media_mm_excl_mle_normal <- mle2(funcao_mm_mle_normal, start=list(a=1/media_coef_mm_linearizada_excl[[2]],b=media_coef_mm_linearizada_excl[[1]]*(1/media_coef_mm_linearizada_excl[[2]]),c=0,sd=sd(media_excl)))
media_mm_excl_mle_normal_nm <- mle2(funcao_mm_mle_normal, start=list(a=1/media_coef_mm_linearizada_excl[[2]],b=media_coef_mm_linearizada_excl[[1]]*(1/media_coef_mm_linearizada_excl[[2]]),c=0,sd=sd(media_excl)),method = "Nelder-Mead")

### potencia
#plot(media_excl~dist_excl)
#curve(0.0009*(x^0.55)+0,add=T)
media_pot_excl <- nls(media_excl~I(a*I(dist_excl^b)+c),start=list(a=0.0009,b=0.55,c=0))
funcao_pot_mle_normal <- function(a,b,c,sd){
  mean <- (a*(dist_excl^b))+c
  -sum(dnorm(media_excl, mean=mean, sd=sd, log=TRUE))
}
start<-list(a=0.0009,b=0.55,c=0,sd=sd(media_excl))
media_pot_excl_mle_normal<-mle2(funcao_pot_mle_normal, start=start)
media_pot_excl_mle_normal_nm<-mle2(funcao_pot_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
# media_coef_logit_linearizada_excl <- coef(lm(log(media_excl/(1-media_excl))~dist_excl))
# media_logit_excl <- nls(media_excl~(1/(1+exp(-a*dist_excl-b)))+c,start=list(a=media_coef_logit_linearizada_excl[[2]],b=media_coef_logit_linearizada_excl[[1]],c=0))
#plot(media_excl~dist_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio")
#curve((0.921/(1+exp(-(0.000035*(x-90000))))),add=T,col="red")
media_logit_excl<- nls(media_excl~(a/(1+exp(-(b*(dist_excl-c))))+d),start=list(a=1,b=0.0005,c=60,d=0))
funcao_logit_mle_normal <- function(a,b,c,d,sd){
  mean <- a/(1+exp(-(b*(dist_excl-c))))+d
  -sum(dnorm(media_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=1,b=0.0005,c=60,d=0,sd=sd(media_excl))
media_logit_excl_mle_normal <- mle2(funcao_logit_mle_normal, start=start)
media_logit_excl_mle_normal_nm <- mle2(funcao_logit_mle_normal, start=start,method = "Nelder-Mead")

### exponencial
media_coef_exp_linearizada_excl <- coef(lm(log(media_excl)~dist_excl))
media_exp_excl <- nls(media_excl~(exp(a*dist_excl+b)),start=list(a=media_coef_exp_linearizada_excl[[2]],b=media_coef_exp_linearizada_excl[[1]]))
funcao_exp_mle_normal <- function(a,b,sd){
  mean <- (exp(a*dist_excl+b))
  -sum(dnorm(media_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=media_coef_exp_linearizada_excl[[2]],b=media_coef_exp_linearizada_excl[[1]],sd=sd(media_excl))
media_exp_excl_mle_normal <- mle2(funcao_exp_mle_normal, start=start)
media_exp_excl_mle_normal_nm <- mle2(funcao_exp_mle_normal, start=start,method = "Nelder-Mead")
# 
# ## GAMA
# 
# ### nulo
# funcao_nulo_mle_gama <- function(scale,shape){
#   -sum(dgamma(media_excl, scale=scale, shape=shape, log=TRUE))
# }
# media_nulo_excl_mle_gama <- mle2(funcao_nulo_mle_gama, start=list(scale=var(media_excl)/mean(media_excl),shape=mean(media_excl)^2/var(media_excl)))
# media_nulo_excl_mle_gama_nm <- mle2(funcao_nulo_mle_gama, start=list(scale=var(media_excl)/mean(media_excl),shape=mean(media_excl)^2/var(media_excl)),method = "Nelder-Mead")
# 
# ### linear
# funcao_linear_mle_gama <- function(a,b,shape){
#   scale <- ((a*dist_excl+b))/shape
#   -sum(dgamma(media_excl, scale=scale, shape=shape, log=TRUE))
# }
# #media_linear_excl_mle_gama <- mle2(funcao_linear_mle_gama, start=list(a=coef(media_linear_excl)[[2]],b=coef(media_linear_excl)[[1]],shape=mean(media_excl)^2/var(media_excl)))
# media_linear_excl_mle_gama_nm <- mle2(funcao_linear_mle_gama, start=list(a=coef(media_linear_excl)[[2]],b=coef(media_linear_excl)[[1]],shape=mean(media_excl)^2/var(media_excl)),method = "Nelder-Mead")
# 
# ### mm
# funcao_mm_mle_gama <- function(a,b,c,shape){
#   scale <- (((a*dist_excl)/(b+dist_excl))+c)/shape
#   -sum(dgamma(media_excl, scale=scale, shape=shape, log=TRUE))
# }
# #media_mm_excl_mle_gama <- mle2(funcao_mm_mle_gama, start=list(a=1/media_coef_mm_linearizada_excl[[2]],b=media_coef_mm_linearizada_excl[[1]]*(1/media_coef_mm_linearizada_excl[[2]]),c=0,shape=mean(media_excl)^2/var(media_excl)),control=list(maxit=1000))
# media_mm_excl_mle_gama_nm <- mle2(funcao_mm_mle_gama, start=list(a=1/media_coef_mm_linearizada_excl[[2]],b=media_coef_mm_linearizada_excl[[1]]*(1/media_coef_mm_linearizada_excl[[2]]),c=0,shape=mean(media_excl)^2/var(media_excl)),method = "Nelder-Mead",control=list(maxit=2000))
# 
# ### potencia
# funcao_pot_mle_gama <- function(a,b,c,shape){
#   scale <- ((a*(dist_excl^b))+c)/shape
#   -sum(dgamma(media_excl, scale=scale, shape=shape, log=TRUE))
# }
# start<-list(a=0.0009,b=0.55,c=0,shape=mean(media_excl)^2/var(media_excl))
# #media_pot_excl_mle_gama <- mle2(funcao_pot_mle_gama, start=start)
# media_pot_excl_mle_gama_nm <- mle2(funcao_pot_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=10000))
# 
# ### logistica
# funcao_logit_mle_gama <- function(a,b,c,d,shape){
#   scale <- (a/(1+exp(-(b*(dist_excl-c))))+d)/shape
#   -sum(dgamma(media_excl, scale=scale, shape=shape, log=TRUE))
# }
# start=list(a=0.921,b=0.000035,c=90000,d=0,shape=mean(media_excl)^2/var(media_excl))
# #media_logit_excl_mle_gama <- mle2(funcao_logit_mle_gama, start=start)
# media_logit_excl_mle_gama_nm <- mle2(funcao_logit_mle_gama, start=start,method = "Nelder-Mead")
# 
# ### exponencial
# funcao_exp_mle_gama <- function(a,b,shape){
#   scale <- (exp(a*dist_excl+b))/shape
#   -sum(dgamma(media_excl, shape=shape, scale=scale, log=TRUE))
# }
# start=list(a=media_coef_exp_linearizada_excl[[2]],b=media_coef_exp_linearizada_excl[[1]],shape=mean(media_excl)^2/var(media_excl))
# #media_exp_excl_mle_gama <- mle2(funcao_exp_mle_gama, start=start)
# media_exp_excl_mle_gama_nm <- mle2(funcao_exp_mle_gama, start=start,method = "Nelder-Mead")
# 

#### selecao
AICtab(#media_nulo_excl,
       #media_nulo_excl_mle_normal,
       media_nulo_excl_mle_normal_nm,
       #media_nulo_excl_mle_gama,
       #media_nulo_excl_mle_gama_nm,
       #media_linear_excl,
       #media_linear_excl_mle_normal,
       media_linear_excl_mle_normal_nm,
       #media_linear_excl_mle_gama,
       #media_linear_excl_mle_gama_nm,
       #media_mm_excl,
       #media_mm_excl_mle_normal,
       media_mm_excl_mle_normal_nm,
       #media_mm_excl_mle_gama,
       #media_mm_excl_mle_gama_nm,
       #media_pot_excl,
       #media_pot_excl_mle_normal,
       media_pot_excl_mle_normal_nm,
       #media_pot_excl_mle_gama,
       #media_pot_excl_mle_gama_nm,
       #media_logit_excl,
       #media_logit_excl_mle_normal,
       media_logit_excl_mle_normal_nm,
       #media_logit_excl_mle_gama,
       #media_logit_excl_mle_gama_nm,
       #media_exp_excl,
       #media_exp_excl_mle_normal,
       media_exp_excl_mle_normal_nm
       #media_exp_excl_mle_gama,
       #media_exp_excl_mle_gama_nm
       )


#### plotando
par(mar=c(5,5,4,3))
par(mgp=c(3.5,0.7,0))
par(tck=-0.02)
plot(media_excl~dist_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio (mortes acumuladas / tamanho da comunidade)",axes=F)
axis(1,las=1,at=c(-50000,0,50000,100000,150000,200000,250000,300000,350000),labels=c("","0",expression("50"%.%10^"3"),expression("100"%.%10^"3"),expression("150"%.%10^"3"),expression("200"%.%10^"3"),expression("250"%.%10^"3"),expression("300"%.%10^"3"),""),cex.axis=0.9)
axis(2,las=1,at=c(-1,0,0.25,0.5,0.75,1,2),labels=c("","0","0,25","0,5","0,75","1",""),cex.axis=0.9)
curve(((coef(media_mm_excl_mle_normal_nm)[[1]]*x)/(coef(media_mm_excl_mle_normal_nm)[[2]]+x)+coef(media_mm_excl_mle_normal_nm)[[3]]),add=T,col="black")
