# MODELOS REAL OFICIAL nao linear
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

#################### VARIANCIA INTER ####################

# CENARIO ADAPTACAO + EXCLUSAO
var_inter_adapt_excl1 <- dados$var_inter[dados$bateria==3]/(20000^2)
var_inter_adapt_excl <- var_inter_adapt_excl1[-which(is.na(var_inter_adapt_excl1))]
var_inter_adapt_excl <- var_inter_adapt_excl+0.00001
dist_adapt_excl <- dados$dist_indice[dados$bateria==3]
dist_adapt_excl <- dist_adapt_excl[-which(is.na(var_inter_adapt_excl1))]

# MODELOS

## NORMAL

### nulo
var_inter_nulo_adapt_excl<-lm(var_inter_adapt_excl~1)
funcao_nulo_mle_normal <- function(mean,sd){
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
var_inter_nulo_adapt_excl_mle_normal <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(var_inter_adapt_excl),sd=sd(var_inter_adapt_excl)))
var_inter_nulo_adapt_excl_mle_normal_nm <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(var_inter_adapt_excl),sd=sd(var_inter_adapt_excl)),method = "Nelder-Mead")

### linear
var_inter_linear_adapt_excl<-lm(var_inter_adapt_excl~dist_adapt_excl)
funcao_linear_mle_normal <- function(a,b,sd){
  mean <- a*dist_adapt_excl+b
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
var_inter_linear_adapt_excl_mle_normal <- mle2(funcao_linear_mle_normal, start=list(a=coef(var_inter_linear_adapt_excl)[[2]],b=coef(var_inter_linear_adapt_excl)[[1]],sd=sd(var_inter_adapt_excl)))
var_inter_linear_adapt_excl_mle_normal_nm <- mle2(funcao_linear_mle_normal, start=list(a=coef(var_inter_linear_adapt_excl)[[2]],b=coef(var_inter_linear_adapt_excl)[[1]],sd=sd(var_inter_adapt_excl)),method = "Nelder-Mead")

### mm

var_inter_mm_adapt_excl <- nls(var_inter_adapt_excl~I(I(a*dist_adapt_excl)/I(b+dist_adapt_excl)+c),start=list(a=-55,b=25,c=55))
funcao_mm_mle_normal <- function(a,b,c,sd){
  mean <- ((a*dist_adapt_excl)/(b+dist_adapt_excl))+c
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
var_inter_mm_adapt_excl_mle_normal <- mle2(funcao_mm_mle_normal, start=list(a=coef(var_inter_mm_adapt_excl)[[1]],b=coef(var_inter_mm_adapt_excl)[[2]],c=coef(var_inter_mm_adapt_excl)[[3]],sd=sd(var_inter_adapt_excl)))
var_inter_mm_adapt_excl_mle_normal_nm <- mle2(funcao_mm_mle_normal, start=list(a=coef(var_inter_mm_adapt_excl)[[1]],b=coef(var_inter_mm_adapt_excl)[[2]],c=coef(var_inter_mm_adapt_excl)[[3]],sd=sd(var_inter_adapt_excl)),method = "Nelder-Mead")

### potencia
var_inter_pot_adapt_excl <- nls(var_inter_adapt_excl~(a*(dist_adapt_excl^b)+c),start=list(a=845.51,b=-0.65488,c=-0.92423))
funcao_pot_mle_normal <- function(a,b,c,sd){
  mean <- (a*(dist_adapt_excl^b))+c
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start<-list(a=845.51,b=-0.65488,c=-0.92423,sd=sd(var_inter_adapt_excl))
var_inter_pot_adapt_excl_mle_normal<-mle2(funcao_pot_mle_normal, start=start,control=list(maxit=1000))
var_inter_pot_adapt_excl_mle_normal_nm<-mle2(funcao_pot_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
var_inter_logit_adapt_excl <- nls(var_inter_adapt_excl~(a/(1+exp(-(b*(dist_adapt_excl-c))))+d),start=list(a=0.1,b=-0.003,c=2000,d=0))
funcao_logit_mle_normal <- function(a,b,c,d,sd){
  mean <- (a/(1+exp(-(b*(dist_adapt_excl-c))))+d)
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=0.1,b=-0.003,c=2000,d=0,sd=sd(var_inter_adapt_excl))
var_inter_logit_adapt_excl_mle_normal <- mle2(funcao_logit_mle_normal, start=start)
var_inter_logit_adapt_excl_mle_normal_nm <- mle2(funcao_logit_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=50000))

### exponencial
var_inter_exp_adapt_excl <-  nls(var_inter_adapt_excl~(exp(a*dist_adapt_excl+b)),start=list(a=-0.0015,b=0))
funcao_exp_mle_normal <- function(a,b,sd){
  mean <- (exp(a*dist_adapt_excl+b))
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=-0.0015,b=0,sd=sd(var_inter_adapt_excl))
var_inter_exp_adapt_excl_mle_normal <- mle2(funcao_exp_mle_normal, start=start)
var_inter_exp_adapt_excl_mle_normal_nm <- mle2(funcao_exp_mle_normal, start=start,method = "Nelder-Mead")

### quadratica
var_inter_quadr_adapt_excl <- nls(var_inter_adapt_excl~(a*(dist_adapt_excl^2))+(b*dist_adapt_excl)+c,start=list(a=-3e-10,b=6e-5,c=0))
funcao_quadr_mle_normal <- function(a,b,c,sd){
  mean <- a*(dist_adapt_excl^2) + b*dist_adapt_excl + c
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=-3e-10,b=6e-5,c=0,sd=sd(var_inter_adapt_excl))
var_inter_quadr_adapt_excl_mle_normal <- mle2(funcao_quadr_mle_normal, start=start)
var_inter_quadr_adapt_excl_mle_normal_nm <- mle2(funcao_quadr_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### ricker
var_inter_ricker_adapt_excl <- nls(var_inter_adapt_excl~(a*dist_adapt_excl*exp(b*dist_adapt_excl))+c,start=list(a=1.4e-04,b=-20e-06,c=0))
funcao_ricker_mle_normal <- function(a,b,c,sd){
  mean <- (a*dist_adapt_excl*exp(b*dist_adapt_excl))+c
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=1.4e-04,b=-20e-06,c=0,sd=sd(var_inter_adapt_excl))
var_inter_ricker_adapt_excl_mle_normal <- mle2(funcao_ricker_mle_normal, start=start)
var_inter_ricker_adapt_excl_mle_normal_nm <- mle2(funcao_ricker_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### holling
var_inter_holling_adapt_excl <- nls(var_inter_adapt_excl~((a*(dist_adapt_excl^2))/(b+(c*dist_adapt_excl)+(dist_adapt_excl^2)))+d,start=list(a=0.0011,b=3.5e+08,c=-15000,d=0))
funcao_holling_mle_normal <- function(a,b,c,d,sd){
  mean <- ((a*(dist_adapt_excl^2))/(b+(c*dist_adapt_excl)+(dist_adapt_excl^2)))+d
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=0.0011,b=3.5e+08,c=-15000,d=0,sd=sd(var_inter_adapt_excl))
var_inter_holling_adapt_excl_mle_normal <- mle2(funcao_holling_mle_normal, start=start,control=list(maxit=1000))
var_inter_holling_adapt_excl_mle_normal_nm <- mle2(funcao_holling_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

## GAMA

### nulo
funcao_nulo_mle_gama <- function(scale,shape){
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
var_inter_nulo_adapt_excl_mle_gama <- mle2(funcao_nulo_mle_gama, start=list(scale=var(var_inter_adapt_excl)/mean(var_inter_adapt_excl),shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)))
var_inter_nulo_adapt_excl_mle_gama_nm <- mle2(funcao_nulo_mle_gama, start=list(scale=var(var_inter_adapt_excl)/mean(var_inter_adapt_excl),shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)),method = "Nelder-Mead")

### linear
funcao_linear_mle_gama <- function(a,shape){
  scale <- ((a*dist_adapt_excl))/shape
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
#var_inter_linear_adapt_excl_mle_gama <- mle2(funcao_linear_mle_gama, start=list(a=coef(var_inter_linear_adapt_excl)[[2]]/(mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)),b=coef(var_inter_linear_adapt_excl)[[1]]/(mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)),shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)))
#var_inter_linear_adapt_excl_mle_gama_nm <- mle2(funcao_linear_mle_gama, start=list(a=coef(var_inter_linear_adapt_excl)[[2]],b=coef(var_inter_linear_adapt_excl)[[1]],shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)),method = "Nelder-Mead")

### mm
funcao_mm_mle_gama <- function(a,b,shape){
  scale <- (((a*dist_adapt_excl)/(b+dist_adapt_excl)))/shape
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
var_inter_mm_adapt_excl_mle_gama <- mle2(funcao_mm_mle_gama, start=list(a=-coef(var_inter_mm_adapt_excl)[[1]],b=coef(var_inter_mm_adapt_excl)[[2]],shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)))
var_inter_mm_adapt_excl_mle_gama_nm <- mle2(funcao_mm_mle_gama, start=list(a=-coef(var_inter_mm_adapt_excl)[[1]],b=coef(var_inter_mm_adapt_excl)[[2]],c=coef(var_inter_mm_adapt_excl)[[3]],shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)),method = "Nelder-Mead",control=list(maxit=1000))

### potencia
funcao_pot_mle_gama <- function(a,b,shape){
  scale <- ((a*(dist_adapt_excl^b)))/shape
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
start<-list(a=0.0009,b=0.55,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
start<-list(a=100,b=-0.5,c=-0.5,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
var_inter_pot_adapt_excl_mle_gama <- mle2(funcao_pot_mle_gama, start=start)
var_inter_pot_adapt_excl_mle_gama_nm <- mle2(funcao_pot_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
funcao_logit_mle_gama <- function(a,b,c,shape){
  scale <- (a/(1+exp(-(b*(dist_adapt_excl-c)))))/shape
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
start=list(a=0.921,b=0.000035,c=90000,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
var_inter_logit_adapt_excl_mle_gama <- mle2(funcao_logit_mle_gama, start=start)
var_inter_logit_adapt_excl_mle_gama_nm <- mle2(funcao_logit_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### exponencial
funcao_exp_mle_gama <- function(a,shape){
  scale <- (exp(a*dist_adapt_excl))/shape
  -sum(dgamma(var_inter_adapt_excl, shape=shape, scale=scale, log=TRUE))
}
start=list(a=-0.0015,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
var_inter_exp_adapt_excl_mle_gama <- mle2(funcao_exp_mle_gama, start=start)
var_inter_exp_adapt_excl_mle_gama_nm <- mle2(funcao_exp_mle_gama, start=start,method = "Nelder-Mead")

### quadratica
funcao_quadr_mle_gama <- function(a,b,shape){
  scale <- (a*(dist_adapt_excl^2) + b*dist_adapt_excl)/shape
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
start <- list(a=-3e-10,b=6e-5,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
var_inter_quadr_adapt_excl_mle_gama <- mle2(funcao_quadr_mle_gama, start=start)
var_inter_quadr_adapt_excl_mle_gama_nm <- mle2(funcao_quadr_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### ricker
funcao_ricker_mle_gama <- function(a,b,shape){
  scale <- ((a*dist_adapt_excl*exp(b*dist_adapt_excl)))/shape
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
start <- list(a=1.4e-04,b=-20e-06,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
start <- list(a=3e-05,b=-3e-05,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
var_inter_ricker_adapt_excl_mle_gama <- mle2(funcao_ricker_mle_gama, start=start)
var_inter_ricker_adapt_excl_mle_gama_nm <- mle2(funcao_ricker_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### holling
funcao_holling_mle_gama <- function(a,b,c,shape){
  scale <- (((a*(dist_adapt_excl^2))/(b+(c*dist_adapt_excl)+(dist_adapt_excl^2))))/shape
  -sum(dgamma(var_inter_adapt_excl, shape=shape, scale=scale, log=TRUE))
}
start=list(a=0.0011,b=3.5e+08,c=-15000,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
var_inter_holling_adapt_excl_mle_gama <- mle2(funcao_holling_mle_gama, start=start)
var_inter_holling_adapt_excl_mle_gama_nm <- mle2(funcao_holling_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=2000))


#### selecao
AICtab(var_inter_nulo_adapt_excl,
       var_inter_nulo_adapt_excl_mle_normal,
       var_inter_nulo_adapt_excl_mle_normal_nm,
       var_inter_nulo_adapt_excl_mle_gama,
       var_inter_nulo_adapt_excl_mle_gama_nm,
       var_inter_linear_adapt_excl,
       var_inter_linear_adapt_excl_mle_normal,
       var_inter_linear_adapt_excl_mle_normal_nm,
       #var_inter_linear_adapt_excl_mle_gama,
       #var_inter_linear_adapt_excl_mle_gama_nm,
       #var_inter_mm_adapt_excl,
       #var_inter_mm_adapt_excl_mle_normal,
       #var_inter_mm_adapt_excl_mle_normal_nm,
       #var_inter_mm_adapt_excl_mle_gama,
       #var_inter_mm_adapt_excl_mle_gama_nm,
       #var_inter_pot_adapt_excl,
       var_inter_pot_adapt_excl_mle_normal,
       var_inter_pot_adapt_excl_mle_normal_nm,
       #var_inter_pot_adapt_excl_mle_gama,
       #var_inter_pot_adapt_excl_mle_gama_nm,
       #var_inter_logit_adapt_excl,
       var_inter_logit_adapt_excl_mle_normal,
       var_inter_logit_adapt_excl_mle_normal_nm,
       var_inter_logit_adapt_excl_mle_gama,
       var_inter_logit_adapt_excl_mle_gama_nm,
       var_inter_exp_adapt_excl,
       var_inter_exp_adapt_excl_mle_normal,
       var_inter_exp_adapt_excl_mle_normal_nm,
       var_inter_exp_adapt_excl_mle_gama,
       var_inter_exp_adapt_excl_mle_gama_nm,
       var_inter_quadr_adapt_excl,
       var_inter_quadr_adapt_excl_mle_normal,
       var_inter_quadr_adapt_excl_mle_normal_nm,
       #var_inter_quadr_adapt_excl_mle_gama,
       #var_inter_quadr_adapt_excl_mle_gama_nm,
       var_inter_ricker_adapt_excl,
       var_inter_ricker_adapt_excl_mle_normal,
       var_inter_ricker_adapt_excl_mle_normal_nm,
       #var_inter_ricker_adapt_excl_mle_gama,
       var_inter_ricker_adapt_excl_mle_gama_nm,
       var_inter_holling_adapt_excl,
       var_inter_holling_adapt_excl_mle_normal,
       var_inter_holling_adapt_excl_mle_normal_nm,
       #var_inter_holling_adapt_excl_mle_gama
       var_inter_holling_adapt_excl_mle_gama_nm
)


#### plotando
par(bty="l")
plot(var_inter_adapt_excl~dist_adapt_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio")

#abline(var_inter_nulo_adapt_excl,col="gray")
#abline(h=coef(var_inter_nulo_adapt_excl_mle_normal)[[1]],col="gray")
#abline(h=coef(var_inter_nulo_adapt_excl_mle_normal_nm)[[1]],col="gray")
abline(h=coef(var_inter_nulo_adapt_excl_mle_gama)[[1]],col="gray")
abline(h=var(var_inter_adapt_excl)/coef(var_inter_nulo_adapt_excl_mle_gama_nm)[[1]],col="gray")

abline(var_inter_linear_adapt_excl)
#curve(coef(var_inter_linear_adapt_excl_mle_normal)[[1]]*x+coef(var_inter_linear_adapt_excl_mle_normal)[[2]],add=T)
#curve(coef(var_inter_linear_adapt_excl_mle_normal_nm)[[1]]*x+coef(var_inter_linear_adapt_excl_mle_normal_nm)[[2]],add=T)
#curve((coef(var_inter_linear_adapt_excl_mle_gama)[[1]]*x+coef(var_inter_linear_adapt_excl_mle_gama)[[2]]),add=T)
#curve((coef(var_inter_linear_adapt_excl_mle_gama_nm)[[1]]*x+coef(var_inter_linear_adapt_excl_mle_gama_nm)[[2]]),add=T)

curve(((coef(var_inter_mm_adapt_excl)[[1]]*x)/(coef(var_inter_mm_adapt_excl)[[2]]+x)+coef(var_inter_mm_adapt_excl)[[3]]),add=T,col="blue")
#curve(((coef(var_inter_mm_adapt_excl_mle_normal)[[1]]*x)/(coef(var_inter_mm_adapt_excl_mle_normal)[[2]]+x)+coef(var_inter_mm_adapt_excl_mle_normal)[[3]]),add=T,col="blue")
#curve(((coef(var_inter_mm_adapt_excl_mle_normal_nm)[[1]]*x)/(coef(var_inter_mm_adapt_excl_mle_normal_nm)[[2]]+x)+coef(var_inter_mm_adapt_excl_mle_normal_nm)[[3]]),add=T,col="blue")
#curve(((coef(var_inter_mm_adapt_excl_mle_gama)[[1]]*x)/(coef(var_inter_mm_adapt_excl_mle_gama)[[2]]+x)+coef(var_inter_mm_adapt_excl_mle_gama)[[3]]),add=T,col="blue")
curve(((coef(var_inter_mm_adapt_excl_mle_gama_nm)[[1]]*x)/(coef(var_inter_mm_adapt_excl_mle_gama_nm)[[2]]+x)+coef(var_inter_mm_adapt_excl_mle_gama_nm)[[3]]),add=T,col="blue")

#curve((coef(var_inter_pot_adapt_excl)[[1]]*(x^coef(var_inter_pot_adapt_excl)[[2]])+coef(var_inter_pot_adapt_excl)[[3]]),add=T,col="red")
#curve((coef(var_inter_pot_adapt_excl_mle_normal)[[1]]*(x^coef(var_inter_pot_adapt_excl_mle_normal)[[2]])+coef(var_inter_pot_adapt_excl_mle_normal)[[3]]),add=T,col="red")
#curve((coef(var_inter_pot_adapt_excl_mle_normal_nm)[[1]]*(x^coef(var_inter_pot_adapt_excl_mle_normal_nm)[[2]])+coef(var_inter_pot_adapt_excl_mle_normal_nm)[[3]]),add=T,col="red")
#curve((coef(var_inter_pot_adapt_excl_mle_gama)[[1]]*(x^coef(var_inter_pot_adapt_excl_mle_gama)[[2]])+coef(var_inter_pot_adapt_excl_mle_gama)[[3]]),add=T,col="red")
curve((coef(var_inter_pot_adapt_excl_mle_gama_nm)[[1]]*(x^coef(var_inter_pot_adapt_excl_mle_gama_nm)[[2]])+coef(var_inter_pot_adapt_excl_mle_gama_nm)[[3]]),add=T,col="red")

#curve((coef(var_inter_logit_adapt_excl)[[1]]/(1+exp(-(coef(var_inter_logit_adapt_excl)[[2]]*(x-coef(var_inter_logit_adapt_excl)[[3]])))))+coef(var_inter_logit_adapt_excl)[[4]],add=T,col="green")
#curve((coef(var_inter_logit_adapt_excl_mle_normal)[[1]]/(1+exp(-(coef(var_inter_logit_adapt_excl_mle_normal)[[2]]*(x-coef(var_inter_logit_adapt_excl_mle_normal)[[3]])))))+coef(var_inter_logit_adapt_excl_mle_normal)[[4]],add=T,col="green")
#curve((coef(var_inter_logit_adapt_excl_mle_normal_nm)[[1]]/(1+exp(-(coef(var_inter_logit_adapt_excl_mle_normal_nm)[[2]]*(x-coef(var_inter_logit_adapt_excl_mle_normal_nm)[[3]])))))+coef(var_inter_logit_adapt_excl_mle_normal_nm)[[4]],add=T,col="green")
curve(((coef(var_inter_logit_adapt_excl_mle_gama)[[1]]/(1+exp(-(coef(var_inter_logit_adapt_excl_mle_gama)[[2]]*(x-coef(var_inter_logit_adapt_excl_mle_gama)[[3]])))))+coef(var_inter_logit_adapt_excl_mle_gama)[[4]]),add=T,col="green")
curve(((coef(var_inter_logit_adapt_excl_mle_gama_nm)[[1]]/(1+exp(-(coef(var_inter_logit_adapt_excl_mle_gama_nm)[[2]]*(x-coef(var_inter_logit_adapt_excl_mle_gama_nm)[[3]])))))+coef(var_inter_logit_adapt_excl_mle_gama_nm)[[4]]),add=T,col="green")

curve((coef(var_inter_exp_adapt_excl)[[1]]*x+coef(var_inter_exp_adapt_excl)[[2]]),add=T,col="yellow")
#curve((coef(var_inter_exp_adapt_excl_mle_normal)[[1]]*x+coef(var_inter_exp_adapt_excl_mle_normal)[[2]]),add=T,col="yellow")
#curve((coef(var_inter_exp_adapt_excl_mle_normal_nm)[[1]]*x+coef(var_inter_exp_adapt_excl_mle_normal_nm)[[2]]),add=T,col="yellow")
curve((coef(var_inter_exp_adapt_excl_mle_gama)[[1]]*x+coef(var_inter_exp_adapt_excl_mle_gama)[[2]]),add=T,col="yellow")
curve((coef(var_inter_exp_adapt_excl_mle_gama_nm)[[1]]*x+coef(var_inter_exp_adapt_excl_mle_gama_nm)[[2]]),add=T,col="yellow")

#curve((coef(var_inter_quadr_adapt_excl)[[1]]*(x^2)+coef(var_inter_quadr_adapt_excl)[[2]]*x+coef(var_inter_quadr_adapt_excl)[[3]]),add=T,col="yellow")
curve((-3e-10*(x^2)+6e-5*x),add=T,col="yellow")

#curve(((coef(var_inter_holling_adapt_excl)[[1]]*(x^2))/(coef(var_inter_holling_adapt_excl)[[2]]+(coef(var_inter_holling_adapt_excl)[[3]]*x)+(x^2)))+coef(var_inter_holling_adapt_excl)[[4]],add=T,col="pink")
#curve(((coef(var_inter_holling_adapt_excl_mle_normal_nm)[[1]]*(x^2))/(coef(var_inter_holling_adapt_excl_mle_normal_nm)[[2]]+(coef(var_inter_holling_adapt_excl_mle_normal_nm)[[3]]*x)+(x^2)))+coef(var_inter_holling_adapt_excl_mle_normal_nm)[[4]],add=T,col="black")
curve(((coef(var_inter_holling_adapt_excl_mle_gama_nm)[[1]]*(x^2))/(coef(var_inter_holling_adapt_excl_mle_gama_nm)[[2]]+(coef(var_inter_holling_adapt_excl_mle_gama_nm)[[3]]*x)+(x^2)))+coef(var_inter_holling_adapt_excl_mle_gama_nm)[[4]],add=T,col="pink")

#curve((coef(var_inter_ricker_adapt_excl)[[1]]*x*exp(coef(var_inter_ricker_adapt_excl)[[2]]*x)+coef(var_inter_ricker_adapt_excl)[[3]]),add=T,col="purple")
#curve((coef(var_inter_ricker_adapt_excl_mle_normal_nm)[[1]]*x*exp(coef(var_inter_ricker_adapt_excl_mle_normal_nm)[[2]]*x)+coef(var_inter_ricker_adapt_excl_mle_normal_nm)[[3]]),add=T,col="purple")
#curve((coef(var_inter_ricker_adapt_excl_mle_gama)[[1]]*x*exp(coef(var_inter_ricker_adapt_excl_mle_gama)[[2]]*x)+coef(var_inter_ricker_adapt_excl_mle_gama)[[3]]),add=T,col="purple")
curve((coef(var_inter_ricker_adapt_excl_mle_gama_nm)[[1]]*x*exp(coef(var_inter_ricker_adapt_excl_mle_gama_nm)[[2]]*x)),add=T,col="black")
curve((3e-5*x*exp(-3e-5*x)),add=T,col="red")
