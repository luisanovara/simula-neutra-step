# MODELOS REAL OFICIAL nao linear
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

#################### VARIANCIA INTER ####################

# CENARIO EXCLUSAO
var_inter_excl1 <- dados$var_inter[dados$bateria==2]/(20000^2)
var_inter_excl <- var_inter_excl1[-which(is.na(var_inter_excl1))]
var_inter_excl <- var_inter_excl+0.00001
dist_excl <- dados$dist_indice[dados$bateria==2]
dist_excl <- dist_excl[-which(is.na(var_inter_excl1))]

# MODELOS

# ## NORMAL
# 
# ### nulo
var_inter_nulo_excl<-lm(var_inter_excl~1)
funcao_nulo_mle_normal <- function(mean,sd){
  -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
}
var_inter_nulo_excl_mle_normal <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(var_inter_excl),sd=sd(var_inter_excl)))
var_inter_nulo_excl_mle_normal_nm <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(var_inter_excl),sd=sd(var_inter_excl)),method = "Nelder-Mead")
# 
# ### linear
var_inter_linear_excl<-lm(var_inter_excl~dist_excl)
funcao_linear_mle_normal <- function(a,b,sd){
  mean <- a*dist_excl+b
  -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
}
var_inter_linear_excl_mle_normal <- mle2(funcao_linear_mle_normal, start=list(a=coef(var_inter_linear_excl)[[2]],b=coef(var_inter_linear_excl)[[1]],sd=sd(var_inter_excl)))
var_inter_linear_excl_mle_normal_nm <- mle2(funcao_linear_mle_normal, start=list(a=coef(var_inter_linear_excl)[[2]],b=coef(var_inter_linear_excl)[[1]],sd=sd(var_inter_excl)),method = "Nelder-Mead")

# ### mm
var_inter_mm_excl <- nls(var_inter_excl~I(I(a*dist_excl)/I(b+dist_excl)+c),start=list(a=-55,b=25,c=55))
funcao_mm_mle_normal <- function(a,b,c,sd){
  mean <- ((a*dist_excl)/(b+dist_excl))+c
  -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
}
var_inter_mm_excl_mle_normal <- mle2(funcao_mm_mle_normal, start=list(a=coef(var_inter_mm_excl)[[1]],b=coef(var_inter_mm_excl)[[2]],c=coef(var_inter_mm_excl)[[3]],sd=sd(var_inter_excl)))
var_inter_mm_excl_mle_normal_nm <- mle2(funcao_mm_mle_normal, start=list(a=coef(var_inter_mm_excl)[[1]],b=coef(var_inter_mm_excl)[[2]],c=coef(var_inter_mm_excl)[[3]],sd=sd(var_inter_excl)),method = "Nelder-Mead")

### potencia
var_inter_pot_excl <- nls(var_inter_excl~(a*(dist_excl^b)+c),start=list(a=845.51,b=-0.65488,c=-0.92423))
funcao_pot_mle_normal <- function(a,b,c,sd){
  mean <- (a*(dist_excl^b))+c
  -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
}
start<-list(a=845.51,b=-0.65488,c=-0.92423,sd=sd(var_inter_excl))
var_inter_pot_excl_mle_normal<-mle2(funcao_pot_mle_normal, start=start,control=list(maxit=1000))
var_inter_pot_excl_mle_normal_nm<-mle2(funcao_pot_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
#var_inter_logit_excl <- nls(var_inter_excl~(a/(1+exp(-(b*(dist_excl-c))))+d),start=list(a=0.1,b=-0.003,c=2000,d=0))
funcao_logit_mle_normal <- function(a,b,c,sd){
  mean <- (a/(1+exp(-(b*(dist_excl-c)))))
  -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=0.1,b=-0.003,c=2000,sd=sd(var_inter_excl))
var_inter_logit_excl_mle_normal <- mle2(funcao_logit_mle_normal, start=start)
#var_inter_logit_excl_mle_normal_nm <- mle2(funcao_logit_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=50000))

### exponencial
var_inter_exp_excl <-  nls(var_inter_excl~(exp(a*dist_excl+b)),start=list(a=-0.0015,b=0))
funcao_exp_mle_normal <- function(a,b,sd){
  mean <- (exp(a*dist_excl+b))
  -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=-0.0015,b=0,sd=sd(var_inter_excl))
var_inter_exp_excl_mle_normal <- mle2(funcao_exp_mle_normal, start=start)
var_inter_exp_excl_mle_normal_nm <- mle2(funcao_exp_mle_normal, start=start,method = "Nelder-Mead")

### quadratica
var_inter_quadr_excl <- nls(var_inter_excl~(a*(dist_excl^2))+(b*dist_excl)+c,start=list(a=0.3e-12,b=-4e-08,c=0.0013))
funcao_quadr_mle_normal <- function(a,b,c,sd){
  mean <- a*(dist_excl^2) + b*dist_excl + c
  -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=0.3e-12,b=-4e-08,c=0.0013,sd=sd(var_inter_excl))
var_inter_quadr_excl_mle_normal <- mle2(funcao_quadr_mle_normal, start=start)
var_inter_quadr_excl_mle_normal_nm <- mle2(funcao_quadr_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### ricker
#var_inter_ricker_excl <- nls(var_inter_excl~(a*dist_excl*exp(b*dist_excl))+c,start=list(a=-1.3048e-07,b=-35e-06,c=0.0013))
funcao_ricker_mle_normal <- function(a,b,c,sd){
  mean <- (a*dist_excl*exp(b*dist_excl))+c
  -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=-1.3048e-07,b=-35e-06,c=0.0013,sd=sd(var_inter_excl))
var_inter_ricker_excl_mle_normal <- mle2(funcao_ricker_mle_normal, start=start)
var_inter_ricker_excl_mle_normal_nm <- mle2(funcao_ricker_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### holling
var_inter_holling_excl <- nls(var_inter_excl~((a*(dist_excl^2))/(b+(c*dist_excl)+(dist_excl^2)))+d,start=list(a=-0.0028,b=0.5e7,c=-100,d=0.0028))
funcao_holling_mle_normal <- function(a,b,c,d,sd){
  mean <- ((a*(dist_excl^2))/(b+(c*dist_excl)+(dist_excl^2)))+d
  -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=-0.0028,b=0.5e7,c=-100,d=0.0028,sd=sd(var_inter_excl))
var_inter_holling_excl_mle_normal <- mle2(funcao_holling_mle_normal, start=start,control=list(maxit=1000))
var_inter_holling_excl_mle_normal_nm <- mle2(funcao_holling_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

## GAMA

### nulo
funcao_nulo_mle_gama <- function(scale,shape){
  -sum(dgamma(var_inter_excl, scale=scale, shape=shape, log=TRUE))
}
var_inter_nulo_excl_mle_gama <- mle2(funcao_nulo_mle_gama, start=list(scale=var(var_inter_excl)/mean(var_inter_excl),shape=mean(var_inter_excl)^2/var(var_inter_excl)))
var_inter_nulo_excl_mle_gama_nm <- mle2(funcao_nulo_mle_gama, start=list(scale=var(var_inter_excl)/mean(var_inter_excl),shape=mean(var_inter_excl)^2/var(var_inter_excl)),method = "Nelder-Mead")

### linear
funcao_linear_mle_gama <- function(a,b,shape){
  scale <- ((a*dist_excl+b))/shape
  -sum(dgamma(var_inter_excl, scale=scale, shape=shape, log=TRUE))
}
var_inter_linear_excl_mle_gama <- mle2(funcao_linear_mle_gama, start=list(a=coef(var_inter_linear_excl)[[2]]/(mean(var_inter_excl)^2/var(var_inter_excl)),b=coef(var_inter_linear_excl)[[1]]/(mean(var_inter_excl)^2/var(var_inter_excl)),shape=mean(var_inter_excl)^2/var(var_inter_excl)))
var_inter_linear_excl_mle_gama_nm <- mle2(funcao_linear_mle_gama, start=list(a=coef(var_inter_linear_excl)[[2]],b=coef(var_inter_linear_excl)[[1]],shape=mean(var_inter_excl)^2/var(var_inter_excl)),method = "Nelder-Mead")
var_inter_linear_excl_glm_gama <- glm(var_inter_excl~dist_excl,family=Gamma(link="identity"))

### mm
funcao_mm_mle_gama <- function(a,b,c,shape){
  scale <- (((a*dist_excl)/(b+dist_excl))+c)/shape
  -sum(dgamma(var_inter_excl, scale=scale, shape=shape, log=TRUE))
}
var_inter_mm_excl_mle_gama <- mle2(funcao_mm_mle_gama, start=list(a=-coef(var_inter_mm_excl)[[1]],b=coef(var_inter_mm_excl)[[2]],c=coef(var_inter_mm_excl)[[3]],shape=mean(var_inter_excl)^2/var(var_inter_excl)))
var_inter_mm_excl_mle_gama_nm <- mle2(funcao_mm_mle_gama, start=list(a=-coef(var_inter_mm_excl)[[1]],b=coef(var_inter_mm_excl)[[2]],c=coef(var_inter_mm_excl)[[3]],shape=mean(var_inter_excl)^2/var(var_inter_excl)),method = "Nelder-Mead",control=list(maxit=1000))

### potencia
funcao_pot_mle_gama <- function(a,b,c,shape){
  scale <- ((a*(dist_excl^b))+c)/shape
  -sum(dgamma(var_inter_excl, scale=scale, shape=shape, log=TRUE))
}
start<-list(a=0.0009,b=0.55,c=0,shape=mean(var_inter_excl)^2/var(var_inter_excl))
start<-list(a=100,b=-0.5,c=-0.5,shape=mean(var_inter_excl)^2/var(var_inter_excl))
#var_inter_pot_excl_mle_gama <- mle2(funcao_pot_mle_gama, start=start)
#var_inter_pot_excl_mle_gama_nm <- mle2(funcao_pot_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
funcao_logit_mle_gama <- function(a,b,c,shape){
  scale <- (a/(1+exp(-(b*(dist_excl-c)))))/shape
  -sum(dgamma(var_inter_excl, scale=scale, shape=shape, log=TRUE))
}
start=list(a=0.921,b=0.000035,c=90000,shape=mean(var_inter_excl)^2/var(var_inter_excl))
#var_inter_logit_excl_mle_gama <- mle2(funcao_logit_mle_gama, start=start)
var_inter_logit_excl_mle_gama_nm <- mle2(funcao_logit_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=15000))

### exponencial
funcao_exp_mle_gama <- function(a,b,shape){
  scale <- (exp(a*dist_excl+b))/shape
  -sum(dgamma(var_inter_excl, shape=shape, scale=scale, log=TRUE))
}
start=list(a=-0.0015,b=0,shape=mean(var_inter_excl)^2/var(var_inter_excl))
var_inter_exp_excl_mle_gama <- mle2(funcao_exp_mle_gama, start=start)
var_inter_exp_excl_mle_gama_nm <- mle2(funcao_exp_mle_gama, start=start,method = "Nelder-Mead")

### quadratica
funcao_quadr_mle_gama <- function(a,b,c,shape){
  scale <- (a*(dist_excl^2) + b*dist_excl + c)/shape
  -sum(dgamma(var_inter_excl, scale=scale, shape=shape, log=TRUE))
}
start <- list(a=-4.8e-13,b=4.8e-8,c=0,shape=mean(var_inter_excl)^2/var(var_inter_excl))
#var_inter_quadr_excl_mle_gama <- mle2(funcao_quadr_mle_gama, start=start)
#var_inter_quadr_excl_mle_gama_nm <- mle2(funcao_quadr_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### ricker
funcao_ricker_mle_gama <- function(a,b,c,shape){
  scale <- ((a*dist_excl*exp(b*dist_excl))+c)/shape
  -sum(dgamma(var_inter_excl, scale=scale, shape=shape, log=TRUE))
}
start <- list(a=1.3048e-07,b=-30e-06,c=0,shape=mean(var_inter_excl)^2/var(var_inter_excl))
#var_inter_ricker_excl_mle_gama <- mle2(funcao_ricker_mle_gama, start=start)
var_inter_ricker_excl_mle_gama_nm <- mle2(funcao_ricker_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### holling
funcao_holling_mle_gama <- function(a,b,c,d,shape){
  scale <- (((a*(dist_excl^2))/(b+(c*dist_excl)+(dist_excl^2)))+d)/shape
  -sum(dgamma(var_inter_excl, shape=shape, scale=scale, log=TRUE))
}
start=list(a=0.0011,b=3.5e+08,c=-15000,d=0,shape=mean(var_inter_excl)^2/var(var_inter_excl))
#var_inter_holling_excl_mle_gama <- mle2(funcao_holling_mle_gama, start=start)
var_inter_holling_excl_mle_gama_nm <- mle2(funcao_holling_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=1000))


AICtab(var_inter_nulo_excl,
       #var_inter_nulo_excl_mle_normal,
       #var_inter_nulo_excl_mle_normal_nm,
       var_inter_nulo_excl_mle_gama,
       #var_inter_nulo_excl_mle_gama_nm,
       var_inter_linear_excl,
       #var_inter_linear_excl_mle_normal,
       #var_inter_linear_excl_mle_normal_nm,
       #var_inter_linear_excl_mle_gama,
       #var_inter_linear_excl_mle_gama_nm,
       #var_inter_linear_excl_glm_gama,
       #var_inter_mm_excl,
       var_inter_mm_excl_mle_normal,
       #var_inter_mm_excl_mle_normal_nm,
       #var_inter_mm_excl_mle_gama,
       var_inter_mm_excl_mle_gama_nm,
       var_inter_pot_excl,
       #var_inter_pot_excl_mle_normal,
       #var_inter_pot_excl_mle_normal_nm,
       #var_inter_pot_excl_mle_gama,
       #var_inter_pot_excl_mle_gama_nm,
       #var_inter_logit_excl,
       var_inter_logit_excl_mle_normal,
       #var_inter_logit_excl_mle_normal_nm,
       #var_inter_logit_excl_mle_gama,
       var_inter_logit_excl_mle_gama_nm,
       var_inter_exp_excl,
       #var_inter_exp_excl_mle_normal,
       #var_inter_exp_excl_mle_normal_nm,
       #var_inter_exp_excl_mle_gama,
       var_inter_exp_excl_mle_gama_nm,
       var_inter_quadr_excl,
       var_inter_quadr_excl_mle_normal,
       #var_inter_quadr_excl_mle_normal_nm,
       #var_inter_quadr_excl_mle_gama,
       #var_inter_quadr_excl_mle_gama_nm,
       #var_inter_quadr_excl_glm_gama,
       #var_inter_ricker_excl,
       #var_inter_ricker_excl_mle_normal,
       var_inter_ricker_excl_mle_normal_nm,
       #var_inter_ricker_excl_mle_gama,
       var_inter_ricker_excl_mle_gama_nm,
       var_inter_holling_excl,
       #var_inter_holling_excl_mle_normal,
       #var_inter_holling_excl_mle_normal_nm,
       #var_inter_holling_excl_mle_gama,
       var_inter_holling_excl_mle_gama_nm,
       base=T
)


AICtab(var_inter_nulo_excl_mle_normal_nm,var_inter_nulo_excl_mle_gama_nm)

#### plotando
par(mar=c(5,5,4,3))
par(mgp=c(3.5,0.7,0))
par(tck=-0.02)
plot(var_inter_excl~dist_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),las=1,ylab="Variância interespecífica do índice de estratégia de vida",xlab="Índice de distúrbio (mortes acumuladas / tamanho da comunidade)",axes=F)
axis(1,las=1,at=c(-50000,0,50000,100000,150000,200000,250000,300000,350000),labels=c("","0",expression("50"%.%10^"3"),expression("100"%.%10^"3"),expression("150"%.%10^"3"),expression("200"%.%10^"3"),expression("250"%.%10^"3"),expression("300"%.%10^"3"),""),cex.axis=0.9)
axis(2,las=1,at=c(-10,0,10,20,30,40,50,60),labels=c("","0","10","20","30","40","50",""),cex.axis=0.9)

curve(((coef(var_inter_mm_excl_mle_gama_nm)[[1]]*x)/(coef(var_inter_mm_excl_mle_gama_nm)[[2]]+x)+coef(var_inter_mm_excl_mle_gama_nm)[[3]]),add=T,col="black")
