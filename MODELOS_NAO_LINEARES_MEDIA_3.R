# MODELOS REAL OFICIAL nao linear
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

######################### MEDIA #########################

# CENARIO ADAPTACAO + EXCLUSAO

media_adapt_excl <- dados$media[dados$bateria==3]/20000.0001
dist_adapt_excl <- dados$dist_indice[dados$bateria==3]

# MODELOS

## NORMAL

### nulo
media_nulo_adapt_excl<-lm(media_adapt_excl~1)
funcao_nulo_mle_normal <- function(mean,sd){
  -sum(dnorm(media_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
media_nulo_adapt_excl_mle_normal <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(media_adapt_excl),sd=sd(media_adapt_excl)))
media_nulo_adapt_excl_mle_normal_nm <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(media_adapt_excl),sd=sd(media_adapt_excl)),method = "Nelder-Mead")

### linear
media_linear_adapt_excl<-lm(media_adapt_excl~dist_adapt_excl)
funcao_linear_mle_normal <- function(a,b,sd){
  mean <- a*dist_adapt_excl+b
  -sum(dnorm(media_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
media_linear_adapt_excl_mle_normal <- mle2(funcao_linear_mle_normal, start=list(a=coef(media_linear_adapt_excl)[[2]],b=coef(media_linear_adapt_excl)[[1]],sd=sd(media_adapt_excl)))
media_linear_adapt_excl_mle_normal_nm <- mle2(funcao_linear_mle_normal, start=list(a=coef(media_linear_adapt_excl)[[2]],b=coef(media_linear_adapt_excl)[[1]],sd=sd(media_adapt_excl)),method = "Nelder-Mead")

### mm
media_coef_mm_linearizada_adapt_excl <- coef(lm(I(dist_adapt_excl/media_adapt_excl)~dist_adapt_excl))
media_mm_adapt_excl <- nls(media_adapt_excl~I(I(a*dist_adapt_excl)/I(b+dist_adapt_excl)+c),start=list(a=1/media_coef_mm_linearizada_adapt_excl[[2]],b=media_coef_mm_linearizada_adapt_excl[[1]]*(1/media_coef_mm_linearizada_adapt_excl[[2]]),c=0))
funcao_mm_mle_normal <- function(a,b,c,sd){
  mean <- ((a*dist_adapt_excl)/(b+dist_adapt_excl))+c
  -sum(dnorm(media_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
media_mm_adapt_excl_mle_normal <- mle2(funcao_mm_mle_normal, start=list(a=1/media_coef_mm_linearizada_adapt_excl[[2]],b=media_coef_mm_linearizada_adapt_excl[[1]]*(1/media_coef_mm_linearizada_adapt_excl[[2]]),c=0,sd=sd(media_adapt_excl)))
media_mm_adapt_excl_mle_normal_nm <- mle2(funcao_mm_mle_normal, start=list(a=1/media_coef_mm_linearizada_adapt_excl[[2]],b=media_coef_mm_linearizada_adapt_excl[[1]]*(1/media_coef_mm_linearizada_adapt_excl[[2]]),c=0,sd=sd(media_adapt_excl)),method = "Nelder-Mead")

### potencia
#plot(media_adapt_excl~dist_adapt_excl)
#curve(0.0009*(x^0.55)+0,add=T)
media_pot_adapt_excl <- nls(media_adapt_excl~I(a*I(dist_adapt_excl^b)+c),start=list(a=0.0009,b=0.55,c=0))
funcao_pot_mle_normal <- function(a,b,c,sd){
  mean <- (a*(dist_adapt_excl^b))+c
  -sum(dnorm(media_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start<-list(a=0.0009,b=0.55,c=0,sd=sd(media_adapt_excl))
media_pot_adapt_excl_mle_normal<-mle2(funcao_pot_mle_normal, start=start)
media_pot_adapt_excl_mle_normal_nm<-mle2(funcao_pot_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
# media_coef_logit_linearizada_adapt_excl <- coef(lm(log(media_adapt_excl/(1-media_adapt_excl))~dist_adapt_excl))
# media_logit_adapt_excl <- nls(media_adapt_excl~(1/(1+exp(-a*dist_adapt_excl-b)))+c,start=list(a=media_coef_logit_linearizada_adapt_excl[[2]],b=media_coef_logit_linearizada_adapt_excl[[1]],c=0))
#plot(media_adapt_excl~dist_adapt_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio")
#curve((0.921/(1+exp(-(0.000035*(x-90000))))),add=T,col="red")
media_logit_adapt_excl<- nls(media_adapt_excl~(a/(1+exp(-(b*(dist_adapt_excl-c))))+d),start=list(a=0.921,b=0.000035,c=90000,d=0))
funcao_logit_mle_normal <- function(a,b,c,d,sd){
  mean <- a/(1+exp(-(b*(dist_adapt_excl-c))))+d
  -sum(dnorm(media_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=coef(media_logit_adapt_excl)[[1]],b=coef(media_logit_adapt_excl)[[2]],c=coef(media_logit_adapt_excl)[[3]],d=coef(media_logit_adapt_excl)[[4]],sd=sd(media_adapt_excl))
media_logit_adapt_excl_mle_normal <- mle2(funcao_logit_mle_normal, start=start)
media_logit_adapt_excl_mle_normal_nm <- mle2(funcao_logit_mle_normal, start=start,method = "Nelder-Mead")

### exponencial
media_coef_exp_linearizada_adapt_excl <- coef(lm(log(media_adapt_excl)~dist_adapt_excl))
media_exp_adapt_excl <- nls(media_adapt_excl~(exp(a*dist_adapt_excl+b)),start=list(a=media_coef_exp_linearizada_adapt_excl[[2]],b=media_coef_exp_linearizada_adapt_excl[[1]]))
funcao_exp_mle_normal <- function(a,b,sd){
  mean <- (exp(a*dist_adapt_excl+b))
  -sum(dnorm(media_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=media_coef_exp_linearizada_adapt_excl[[2]],b=media_coef_exp_linearizada_adapt_excl[[1]],sd=sd(media_adapt_excl))
media_exp_adapt_excl_mle_normal <- mle2(funcao_exp_mle_normal, start=start)
media_exp_adapt_excl_mle_normal_nm <- mle2(funcao_exp_mle_normal, start=start,method = "Nelder-Mead")

## GAMA

### nulo
funcao_nulo_mle_gama <- function(scale,shape){
  -sum(dgamma(media_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
media_nulo_adapt_excl_mle_gama <- mle2(funcao_nulo_mle_gama, start=list(scale=var(media_adapt_excl)/mean(media_adapt_excl),shape=mean(media_adapt_excl)^2/var(media_adapt_excl)))
media_nulo_adapt_excl_mle_gama_nm <- mle2(funcao_nulo_mle_gama, start=list(scale=var(media_adapt_excl)/mean(media_adapt_excl),shape=mean(media_adapt_excl)^2/var(media_adapt_excl)),method = "Nelder-Mead")

### linear
funcao_linear_mle_gama <- function(a,b,shape){
  scale <- ((a*dist_adapt_excl+b))/shape
  -sum(dgamma(media_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
#media_linear_adapt_excl_mle_gama <- mle2(funcao_linear_mle_gama, start=list(a=coef(media_linear_adapt_excl)[[2]],b=coef(media_linear_adapt_excl)[[1]],shape=mean(media_adapt_excl)^2/var(media_adapt_excl)))
media_linear_adapt_excl_mle_gama_nm <- mle2(funcao_linear_mle_gama, start=list(a=coef(media_linear_adapt_excl)[[2]],b=coef(media_linear_adapt_excl)[[1]],shape=mean(media_adapt_excl)^2/var(media_adapt_excl)),method = "Nelder-Mead")

### mm
funcao_mm_mle_gama <- function(a,b,c,shape){
  scale <- (((a*dist_adapt_excl)/(b+dist_adapt_excl))+c)/shape
  -sum(dgamma(media_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
#media_mm_adapt_excl_mle_gama <- mle2(funcao_mm_mle_gama, start=list(a=1/media_coef_mm_linearizada_adapt_excl[[2]],b=media_coef_mm_linearizada_adapt_excl[[1]]*(1/media_coef_mm_linearizada_adapt_excl[[2]]),c=0,shape=mean(media_adapt_excl)^2/var(media_adapt_excl)),control=list(maxit=1000))
media_mm_adapt_excl_mle_gama_nm <- mle2(funcao_mm_mle_gama, start=list(a=1/media_coef_mm_linearizada_adapt_excl[[2]],b=media_coef_mm_linearizada_adapt_excl[[1]]*(1/media_coef_mm_linearizada_adapt_excl[[2]]),c=0,shape=mean(media_adapt_excl)^2/var(media_adapt_excl)),method = "Nelder-Mead",control=list(maxit=2000))

### potencia
funcao_pot_mle_gama <- function(a,b,c,shape){
  scale <- ((a*(dist_adapt_excl^b))+c)/shape
  -sum(dgamma(media_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
start<-list(a=0.0009,b=0.55,c=0,shape=mean(media_adapt_excl)^2/var(media_adapt_excl))
#media_pot_adapt_excl_mle_gama <- mle2(funcao_pot_mle_gama, start=start)
media_pot_adapt_excl_mle_gama_nm <- mle2(funcao_pot_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
funcao_logit_mle_gama <- function(a,b,c,d,shape){
  scale <- (a/(1+exp(-(b*(dist_adapt_excl-c))))+d)/shape
  -sum(dgamma(media_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
start=list(a=0.921,b=0.000035,c=90000,d=0,shape=mean(media_adapt_excl)^2/var(media_adapt_excl))
#media_logit_adapt_excl_mle_gama <- mle2(funcao_logit_mle_gama, start=start)
media_logit_adapt_excl_mle_gama_nm <- mle2(funcao_logit_mle_gama, start=start,method = "Nelder-Mead")

### exponencial
funcao_exp_mle_gama <- function(a,b,shape){
  scale <- (exp(a*dist_adapt_excl+b))/shape
  -sum(dgamma(media_adapt_excl, shape=shape, scale=scale, log=TRUE))
}
start=list(a=media_coef_exp_linearizada_adapt_excl[[2]],b=media_coef_exp_linearizada_adapt_excl[[1]],shape=mean(media_adapt_excl)^2/var(media_adapt_excl))
#media_exp_adapt_excl_mle_gama <- mle2(funcao_exp_mle_gama, start=start)
media_exp_adapt_excl_mle_gama_nm <- mle2(funcao_exp_mle_gama, start=start,method = "Nelder-Mead")


#### selecao
AICtab(media_nulo_adapt_excl,
       media_nulo_adapt_excl_mle_normal,
       media_nulo_adapt_excl_mle_normal_nm,
       #media_nulo_adapt_excl_mle_gama,
       media_nulo_adapt_excl_mle_gama_nm,
       media_linear_adapt_excl,
       media_linear_adapt_excl_mle_normal,
       media_linear_adapt_excl_mle_normal_nm,
       #media_linear_adapt_excl_mle_gama,
       media_linear_adapt_excl_mle_gama_nm,
       media_mm_adapt_excl,
       media_mm_adapt_excl_mle_normal,
       media_mm_adapt_excl_mle_normal_nm,
       #media_mm_adapt_excl_mle_gama,
       media_mm_adapt_excl_mle_gama_nm,
       media_pot_adapt_excl,
       media_pot_adapt_excl_mle_normal,
       media_pot_adapt_excl_mle_normal_nm,
       #media_pot_adapt_excl_mle_gama,
       media_pot_adapt_excl_mle_gama_nm,
       media_logit_adapt_excl,
       media_logit_adapt_excl_mle_normal,
       media_logit_adapt_excl_mle_normal_nm,
       #media_logit_adapt_excl_mle_gama,
       media_logit_adapt_excl_mle_gama_nm,
       media_exp_adapt_excl,
       media_exp_adapt_excl_mle_normal,
       media_exp_adapt_excl_mle_normal_nm,
       #media_exp_adapt_excl_mle_gama,
       media_exp_adapt_excl_mle_gama_nm)


#### plotando
par(bty="l")
plot(media_adapt_excl~dist_adapt_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio")

abline(media_nulo_adapt_excl,col="gray")
abline(h=coef(media_nulo_adapt_excl_mle_normal)[[1]],col="gray")
abline(h=coef(media_nulo_adapt_excl_mle_normal_nm)[[1]],col="gray")
#abline(h=coef(media_nulo_adapt_excl_mle_gama)[[1]],col="gray")
abline(h=coef(media_nulo_adapt_excl_mle_gama_nm)[[1]],col="gray")

abline(media_linear_adapt_excl)
curve(coef(media_linear_adapt_excl_mle_normal)[[1]]*x+coef(media_linear_adapt_excl_mle_normal)[[2]],add=T)
curve(coef(media_linear_adapt_excl_mle_normal_nm)[[1]]*x+coef(media_linear_adapt_excl_mle_normal_nm)[[2]],add=T)
#curve((coef(media_linear_adapt_excl_mle_gama)[[1]]*x+coef(media_linear_adapt_excl_mle_gama)[[2]]),add=T)
curve((coef(media_linear_adapt_excl_mle_gama_nm)[[1]]*x+coef(media_linear_adapt_excl_mle_gama_nm)[[2]]),add=T)

curve(((coef(media_mm_adapt_excl)[[1]]*x)/(coef(media_mm_adapt_excl)[[2]]+x)+coef(media_mm_adapt_excl)[[3]]),add=T,col="blue")
curve(((coef(media_mm_adapt_excl_mle_normal)[[1]]*x)/(coef(media_mm_adapt_excl_mle_normal)[[2]]+x)+coef(media_mm_adapt_excl_mle_normal)[[3]]),add=T,col="blue")
curve(((coef(media_mm_adapt_excl_mle_normal_nm)[[1]]*x)/(coef(media_mm_adapt_excl_mle_normal_nm)[[2]]+x)+coef(media_mm_adapt_excl_mle_normal_nm)[[3]]),add=T,col="blue")
#curve(((coef(media_mm_adapt_excl_mle_gama)[[1]]*x)/(coef(media_mm_adapt_excl_mle_gama)[[2]]+x)+coef(media_mm_adapt_excl_mle_gama)[[3]]),add=T,col="blue")
curve(((coef(media_mm_adapt_excl_mle_gama_nm)[[1]]*x)/(coef(media_mm_adapt_excl_mle_gama_nm)[[2]]+x)+coef(media_mm_adapt_excl_mle_gama_nm)[[3]]),add=T,col="blue")

curve((coef(media_pot_adapt_excl)[[1]]*(x^coef(media_pot_adapt_excl)[[2]])+coef(media_pot_adapt_excl)[[3]]),add=T,col="red")
curve((coef(media_pot_adapt_excl_mle_normal)[[1]]*(x^coef(media_pot_adapt_excl_mle_normal)[[2]])+coef(media_pot_adapt_excl_mle_normal)[[3]]),add=T,col="red")
curve((coef(media_pot_adapt_excl_mle_normal_nm)[[1]]*(x^coef(media_pot_adapt_excl_mle_normal_nm)[[2]])+coef(media_pot_adapt_excl_mle_normal_nm)[[3]]),add=T,col="red")
#curve((coef(media_pot_adapt_excl_mle_gama)[[1]]*(x^coef(media_pot_adapt_excl_mle_gama)[[2]])+coef(media_pot_adapt_excl_mle_gama)[[3]]),add=T,col="red")
curve((coef(media_pot_adapt_excl_mle_gama_nm)[[1]]*(x^coef(media_pot_adapt_excl_mle_gama_nm)[[2]])+coef(media_pot_adapt_excl_mle_gama_nm)[[3]]),add=T,col="red")

curve((coef(media_logit_adapt_excl)[[1]]/(1+exp(-(coef(media_logit_adapt_excl)[[2]]*(x-coef(media_logit_adapt_excl)[[3]])))))+coef(media_logit_adapt_excl)[[4]],add=T,col="green")
curve((coef(media_logit_adapt_excl_mle_normal)[[1]]/(1+exp(-(coef(media_logit_adapt_excl_mle_normal)[[2]]*(x-coef(media_logit_adapt_excl_mle_normal)[[3]])))))+coef(media_logit_adapt_excl_mle_normal)[[4]],add=T,col="green")
curve((coef(media_logit_adapt_excl_mle_normal_nm)[[1]]/(1+exp(-(coef(media_logit_adapt_excl_mle_normal_nm)[[2]]*(x-coef(media_logit_adapt_excl_mle_normal_nm)[[3]])))))+coef(media_logit_adapt_excl_mle_normal_nm)[[4]],add=T,col="green")
#curve(((coef(media_logit_adapt_excl_mle_gama)[[1]]/(1+exp(-(coef(media_logit_adapt_excl_mle_gama)[[2]]*(x-coef(media_logit_adapt_excl_mle_gama)[[3]])))))+coef(media_logit_adapt_excl_mle_gama)[[4]]),add=T,col="green")
curve(((coef(media_logit_adapt_excl_mle_gama_nm)[[1]]/(1+exp(-(coef(media_logit_adapt_excl_mle_gama_nm)[[2]]*(x-coef(media_logit_adapt_excl_mle_gama_nm)[[3]])))))+coef(media_logit_adapt_excl_mle_gama_nm)[[4]]),add=T,col="green")

curve((coef(media_exp_adapt_excl)[[1]]*x+coef(media_exp_adapt_excl)[[2]]),add=T,col="yellow")
curve((coef(media_exp_adapt_excl_mle_normal)[[1]]*x+coef(media_exp_adapt_excl_mle_normal)[[2]]),add=T,col="yellow")
curve((coef(media_exp_adapt_excl_mle_normal_nm)[[1]]*x+coef(media_exp_adapt_excl_mle_normal_nm)[[2]]),add=T,col="yellow")
#curve((coef(media_exp_adapt_excl_mle_gama)[[1]]*x+coef(media_exp_adapt_excl_mle_gama)[[2]]),add=T,col="yellow")
curve((coef(media_exp_adapt_excl_mle_gama_nm)[[1]]*x+coef(media_exp_adapt_excl_mle_gama_nm)[[2]]),add=T,col="yellow")

