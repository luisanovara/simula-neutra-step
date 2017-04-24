# MODELOS REAL OFICIAL nao linear
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

#################### VARIANCIA TOTAL ####################

# CENARIO ADAPTACAO
var_total_adapt <- dados$var_total[dados$bateria==1]/(20000^2)
dist_adapt <- dados$dist_indice[dados$bateria==1]

# MODELOS

## NORMAL

### nulo
var_total_nulo_adapt<-lm(var_total_adapt~1)
funcao_nulo_mle_normal <- function(mean,sd){
  -sum(dnorm(var_total_adapt, mean=mean, sd=sd, log=TRUE))
}
#var_total_nulo_adapt_mle_normal <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(var_total_adapt),sd=sd(var_total_adapt)))
var_total_nulo_adapt_mle_normal_nm <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(var_total_adapt),sd=sd(var_total_adapt)),method = "Nelder-Mead")

### linear
var_total_linear_adapt<-lm(var_total_adapt~dist_adapt)
funcao_linear_mle_normal <- function(a,b,sd){
  mean <- a*dist_adapt+b
  -sum(dnorm(var_total_adapt, mean=mean, sd=sd, log=TRUE))
}
#var_total_linear_adapt_mle_normal <- mle2(funcao_linear_mle_normal, start=list(a=coef(var_total_linear_adapt)[[2]],b=coef(var_total_linear_adapt)[[1]],sd=sd(var_total_adapt)))
var_total_linear_adapt_mle_normal_nm <- mle2(funcao_linear_mle_normal, start=list(a=coef(var_total_linear_adapt)[[2]],b=coef(var_total_linear_adapt)[[1]],sd=sd(var_total_adapt)),method = "Nelder-Mead")

### mm
var_total_coef_mm_linearizada_adapt <- coef(lm(I(dist_adapt/var_total_adapt)~dist_adapt))

var_total_mm_adapt <- nls(var_total_adapt~I(I(a*dist_adapt)/I(b+dist_adapt)+c),start=list(a=1/var_total_coef_mm_linearizada_adapt[[2]],b=var_total_coef_mm_linearizada_adapt[[1]]*(1/var_total_coef_mm_linearizada_adapt[[2]]),c=0))
funcao_mm_mle_normal <- function(a,b,c,sd){
  mean <- ((a*dist_adapt)/(b+dist_adapt))+c
  -sum(dnorm(var_total_adapt, mean=mean, sd=sd, log=TRUE))
}
#var_total_mm_adapt_mle_normal <- mle2(funcao_mm_mle_normal, start=list(a=1/var_total_coef_mm_linearizada_adapt[[2]],b=var_total_coef_mm_linearizada_adapt[[1]]*(1/var_total_coef_mm_linearizada_adapt[[2]]),c=0,sd=sd(var_total_adapt)))
var_total_mm_adapt_mle_normal_nm <- mle2(funcao_mm_mle_normal, start=list(a=1/var_total_coef_mm_linearizada_adapt[[2]],b=var_total_coef_mm_linearizada_adapt[[1]]*(1/var_total_coef_mm_linearizada_adapt[[2]]),c=0,sd=sd(var_total_adapt)),method = "Nelder-Mead")

### potencia
#plot(var_total_adapt~dist_adapt)
#curve(0.0009*(x^0.55)+0,add=T)
#var_total_pot_adapt <- nls(var_total_adapt~I(a*I(dist_adapt^b)+c),start=list(a=0.0009,b=0.55,c=0),control=list(maxiter=10000))
funcao_pot_mle_normal <- function(a,b,c,sd){
  mean <- (a*(dist_adapt^b))+c
  -sum(dnorm(var_total_adapt, mean=mean, sd=sd, log=TRUE))
}
start<-list(a=0.0009,b=0.55,c=0,sd=sd(var_total_adapt))
#var_total_pot_adapt_mle_normal<-mle2(funcao_pot_mle_normal, start=start)
var_total_pot_adapt_mle_normal_nm<-mle2(funcao_pot_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
var_total_logit_adapt<- nls(var_total_adapt~(a/(1+exp(-b*(dist_adapt-c)))),start=list(a=0.0016,b=0.0002,c=5000))
funcao_logit_mle_normal <- function(a,b,c,sd){
  mean <- a/(1+exp(-(b*(dist_adapt-c))))
  -sum(dnorm(var_total_adapt, mean=mean, sd=sd, log=TRUE))
}
start=list(a=0.0016,b=0.0002,c=5000,sd=sd(var_total_adapt))
#var_total_logit_adapt_mle_normal <- mle2(funcao_logit_mle_normal, start=start)
var_total_logit_adapt_mle_normal_nm <- mle2(funcao_logit_mle_normal, start=as.list(c(coef(var_total_logit_adapt),sd=sd(var_total_adapt))),method = "Nelder-Mead",control=list(maxit=1000))

### exponencial
var_total_coef_exp_linearizada_adapt <- coef(lm(log(var_total_adapt)~dist_adapt))
var_total_exp_adapt <- nls(var_total_adapt~(exp(a*dist_adapt+b)),start=list(a=var_total_coef_exp_linearizada_adapt[[2]],b=var_total_coef_exp_linearizada_adapt[[1]]))
funcao_exp_mle_normal <- function(a,b,sd){
  mean <- (exp(a*dist_adapt+b))
  -sum(dnorm(var_total_adapt, mean=mean, sd=sd, log=TRUE))
}
start=list(a=var_total_coef_exp_linearizada_adapt[[2]],b=var_total_coef_exp_linearizada_adapt[[1]],sd=sd(var_total_adapt))
#var_total_exp_adapt_mle_normal <- mle2(funcao_exp_mle_normal, start=start)
var_total_exp_adapt_mle_normal_nm <- mle2(funcao_exp_mle_normal, start=start,method = "Nelder-Mead")

### quadratica
var_total_quadr_adapt <- nls(var_total_adapt~(a*(dist_adapt^2))+(b*dist_adapt)+c,start=list(a=-4.8e-13,b=4.8e-8,c=0))
funcao_quadr_mle_normal <- function(a,b,c,sd){
  mean <- a*(dist_adapt^2) + b*dist_adapt + c
  -sum(dnorm(var_total_adapt, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=-4.8e-13,b=4.8e-8,c=0,sd=sd(var_total_adapt))
#var_total_quadr_adapt_mle_normal <- mle2(funcao_quadr_mle_normal, start=start)
var_total_quadr_adapt_mle_normal_nm <- mle2(funcao_quadr_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### ricker
var_total_ricker_adapt <- nls(var_total_adapt~(a*dist_adapt*exp(b*dist_adapt))+c,start=list(a=1.3048e-07,b=-30e-06,c=0))
funcao_ricker_mle_normal <- function(a,b,c,sd){
  mean <- (a*dist_adapt*exp(b*dist_adapt))+c
  -sum(dnorm(var_total_adapt, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=1.3048e-07,b=-30e-06,c=0,sd=sd(var_total_adapt))
#var_total_ricker_adapt_mle_normal <- mle2(funcao_ricker_mle_normal, start=start)
var_total_ricker_adapt_mle_normal_nm <- mle2(funcao_ricker_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### holling
var_total_holling_adapt <- nls(var_total_adapt~((a*(dist_adapt^2))/(b+(c*dist_adapt)+(dist_adapt^2)))+d,start=list(a=0.0011,b=3.5e+08,c=-15000,d=0))
funcao_holling_mle_normal <- function(a,b,c,d,sd){
  mean <- ((a*(dist_adapt^2))/(b+(c*dist_adapt)+(dist_adapt^2)))+d
  -sum(dnorm(var_total_adapt, mean=mean, sd=sd, log=TRUE))
}
#start=list(a=0.0011,b=3.5e+08,c=-15000,d=0,sd=sd(var_total_adapt))
#start=list(a=0.0011,b=4e+08,c=-18000,d=0,sd=sd(var_total_adapt))
start=list(a=0.0011,b=4.5e+08,c=-15000,d=0.00005,sd=sd(var_total_adapt))
start=list(a=0.00121,b=-10e5,c=15000,d=3.26e-5,sd=sd(var_total_adapt))
#var_total_holling_adapt_mle_normal <- mle2(funcao_holling_mle_normal, start=start)
var_total_holling_adapt_mle_normal_nm <- mle2(funcao_holling_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

## GAMA

# ### nulo
funcao_nulo_mle_gama <- function(scale,shape){
  -sum(dgamma(var_total_adapt, scale=scale, shape=shape, log=TRUE))
}
# var_total_nulo_adapt_mle_gama <- mle2(funcao_nulo_mle_gama, start=list(scale=var(var_total_adapt)/mean(var_total_adapt),shape=mean(var_total_adapt)^2/var(var_total_adapt)))
var_total_nulo_adapt_mle_gama_nm <- mle2(funcao_nulo_mle_gama, start=list(scale=var(var_total_adapt)/mean(var_total_adapt),shape=mean(var_total_adapt)^2/var(var_total_adapt)),method = "Nelder-Mead")
# 
# ### linear
funcao_linear_mle_gama <- function(a,b,shape){
   scale <- ((a*dist_adapt+b))/shape
   -sum(dgamma(var_total_adapt, scale=scale, shape=shape, log=TRUE))
 }
#var_total_linear_adapt_mle_gama <- mle2(funcao_linear_mle_gama, start=list(a=coef(var_total_linear_adapt)[[2]],b=coef(var_total_linear_adapt)[[1]],shape=mean(var_total_adapt)^2/var(var_total_adapt)))
var_total_linear_adapt_mle_gama_nm <- mle2(funcao_linear_mle_gama, start=list(a=coef(var_total_linear_adapt)[[2]],b=coef(var_total_linear_adapt)[[1]],shape=mean(var_total_adapt)^2/var(var_total_adapt)),method = "Nelder-Mead")

# ### mm
funcao_mm_mle_gama <- function(a,b,c,shape){
  scale <- (((a*dist_adapt)/(b+dist_adapt))+c)/shape
  -sum(dgamma(var_total_adapt, scale=scale, shape=shape, log=TRUE))
}
#var_total_mm_adapt_mle_gama <- mle2(funcao_mm_mle_gama, start=list(a=1/var_total_coef_mm_linearizada_adapt[[2]],b=var_total_coef_mm_linearizada_adapt[[1]]*(1/var_total_coef_mm_linearizada_adapt[[2]]),c=0,shape=mean(var_total_adapt)^2/var(var_total_adapt)),control=list(maxit=1000))
#var_total_mm_adapt_mle_gama_nm <- mle2(funcao_mm_mle_gama, start=list(a=1/var_total_coef_mm_linearizada_adapt[[2]],b=var_total_coef_mm_linearizada_adapt[[1]]*(1/var_total_coef_mm_linearizada_adapt[[2]]),c=0,shape=mean(var_total_adapt)^2/var(var_total_adapt)),method = "Nelder-Mead",control=list(maxit=2000))
# 
### potencia
funcao_pot_mle_gama <- function(a,b,c,shape){
  scale <- ((a*(dist_adapt^b))+c)/shape
  -sum(dgamma(var_total_adapt, scale=scale, shape=shape, log=TRUE))
}
start<-list(a=0.0009,b=0.55,c=0,shape=mean(var_total_adapt)^2/var(var_total_adapt))
#var_total_pot_adapt_mle_gama <- mle2(funcao_pot_mle_gama, start=start)
var_total_pot_adapt_mle_gama_nm <- mle2(funcao_pot_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
funcao_logit_mle_gama <- function(a,b,c,shape){
  scale <- (a/(1+exp(-(b*(dist_adapt-c)))))/shape
  -sum(dgamma(var_total_adapt, scale=scale, shape=shape, log=TRUE))
}
start=list(a=0.921,b=0.000035,c=90000,shape=mean(var_total_adapt)^2/var(var_total_adapt))
var_total_logit_adapt_mle_gama <- mle2(funcao_logit_mle_gama, start=start)
var_total_logit_adapt_mle_gama_nm <- mle2(funcao_logit_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### exponencial
funcao_exp_mle_gama <- function(a,b,shape){
  scale <- (exp(a*dist_adapt+b))/shape
  -sum(dgamma(var_total_adapt, shape=shape, scale=scale, log=TRUE))
}
start=list(a=var_total_coef_exp_linearizada_adapt[[2]],b=var_total_coef_exp_linearizada_adapt[[1]],shape=mean(var_total_adapt)^2/var(var_total_adapt))
var_total_exp_adapt_mle_gama <- mle2(funcao_exp_mle_gama, start=start)
var_total_exp_adapt_mle_gama_nm <- mle2(funcao_exp_mle_gama, start=start,method = "Nelder-Mead")

### quadratica
funcao_quadr_mle_gama <- function(a,b,c,shape){
  scale <- (a*(dist_adapt^2) + b*dist_adapt + c)/shape
  -sum(dgamma(var_total_adapt, scale=scale, shape=shape, log=TRUE))
}
start <- list(a=-4.8e-13,b=4.8e-8,c=0,shape=mean(var_total_adapt)^2/var(var_total_adapt))
#var_total_quadr_excl_mle_gama <- mle2(funcao_quadr_mle_gama, start=start)
#var_total_quadr_excl_mle_gama_nm <- mle2(funcao_quadr_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### ricker
funcao_ricker_mle_gama <- function(a,b,c,shape){
  scale <- ((a*dist_adapt*exp(b*dist_adapt))+c)/shape
  -sum(dgamma(var_total_adapt, scale=scale, shape=shape, log=TRUE))
}
start <- list(a=1.3048e-07,b=-30e-06,c=0,shape=mean(var_total_adapt)^2/var(var_total_adapt))
#var_total_ricker_adapt_mle_gama <- mle2(funcao_ricker_mle_gama, start=start)
var_total_ricker_adapt_mle_gama_nm <- mle2(funcao_ricker_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### holling
funcao_holling_mle_gama <- function(a,b,c,d,shape){
  scale <- (((a*(dist_adapt^2))/(b+(c*dist_adapt)+(dist_adapt^2)))+d)/shape
  -sum(dgamma(var_total_adapt, shape=shape, scale=scale, log=TRUE))
}
start=list(a=0.0011,b=3.5e+08,c=-15000,d=0,shape=mean(var_total_adapt)^2/var(var_total_adapt))
#var_total_holling_adapt_mle_gama <- mle2(funcao_holling_mle_gama, start=start)
var_total_holling_adapt_mle_gama_nm <- mle2(funcao_holling_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=1000))

#### selecao
AICtab(var_total_nulo_adapt,
       #var_total_nulo_adapt_mle_normal,
       #var_total_nulo_adapt_mle_normal_nm,
       #var_total_nulo_adapt_mle_gama,
       var_total_nulo_adapt_mle_gama_nm,
       var_total_linear_adapt,
       #var_total_linear_adapt_mle_normal,
       #var_total_linear_adapt_mle_normal_nm,
       #var_total_linear_adapt_mle_gama,
       var_total_linear_adapt_mle_gama_nm,
       var_total_mm_adapt,
       #var_total_mm_adapt_mle_normal,
       #var_total_mm_adapt_mle_normal_nm,
       #var_total_mm_adapt_mle_gama,
       #var_total_mm_adapt_mle_gama_nm,
       #var_total_pot_adapt,
       #var_total_pot_adapt_mle_normal,
       var_total_pot_adapt_mle_normal_nm,
       #var_total_pot_adapt_mle_gama,
       var_total_pot_adapt_mle_gama_nm,
       var_total_logit_adapt,
       #var_total_logit_adapt_mle_normal,
       #var_total_logit_adapt_mle_normal_nm,
       #var_total_logit_adapt_mle_gama,
       var_total_logit_adapt_mle_gama_nm,
       var_total_exp_adapt,
       #var_total_exp_adapt_mle_normal,
       #var_total_exp_adapt_mle_normal_nm,
       #var_total_exp_adapt_mle_gama,
       var_total_exp_adapt_mle_gama_nm,
       var_total_quadr_adapt,
       #var_total_quadr_adapt_mle_normal,
       #var_total_quadr_adapt_mle_normal_nm,
       #var_total_quadr_adapt_mle_gama,
       #var_total_quadr_adapt_mle_gama_nm,
       var_total_ricker_adapt,
       #var_total_ricker_adapt_mle_normal,
       #var_total_ricker_adapt_mle_normal_nm,
       #var_total_ricker_adapt_mle_gama,
       var_total_ricker_adapt_mle_gama_nm,
       var_total_holling_adapt,
       #var_total_holling_adapt_mle_normal,
       #var_total_holling_adapt_mle_normal_nm,
       #var_total_holling_adapt_mle_gama,
       var_total_holling_adapt_mle_gama_nm,
       base=T
)

AICtab(var_total_nulo_adapt_mle_normal_nm,var_total_nulo_adapt_mle_gama_nm)


#### plotando
par(mar=c(5,5,4,3))
par(mgp=c(3.5,0.7,0))
par(tck=-0.02)
plot(var_total_adapt~dist_adapt,pch=20,col="gray",bty="l",xlim=c(0,3e5),las=1,ylab="Variância total do índice de estratégia de vida",xlab="Índice de distúrbio (mortes acumuladas / tamanho da comunidade)",axes=F,ylim=c(0,0.0025))
axis(1,las=1,at=c(-50000,0,50000,100000,150000,200000,250000,300000,350000),labels=c("","0",expression("50"%.%10^"3"),expression("100"%.%10^"3"),expression("150"%.%10^"3"),expression("200"%.%10^"3"),expression("250"%.%10^"3"),expression("300"%.%10^"3"),""),cex.axis=0.9)
axis(2,las=1,at=c(-1,0,0.0005,0.0010,0.0015,0.0020,0.0025,1),labels=c("","0",expression(text=paste("0,5"%.%10^"-3",sep="")),expression(text=paste(1%.%10^"-3",sep="")),expression(text=paste("1,5"%.%10^"-3",sep="")),expression(text=paste("2"%.%10^"-3",sep="")),expression(text=paste("2,5"%.%10^"-3",sep="")),""),cex.axis=0.9)
curve((coef(var_total_logit_adapt)[[1]]/(1+exp(-(coef(var_total_logit_adapt)[[2]]*(x-coef(var_total_logit_adapt)[[3]]))))),add=T,col="black")
#curve(((coef(var_total_holling_adapt_mle_normal_nm)[[1]]*(x^2))/(coef(var_total_holling_adapt_mle_normal_nm)[[2]]+(coef(var_total_holling_adapt_mle_normal_nm)[[3]]*x)+(x^2)))+coef(var_total_holling_adapt_mle_normal_nm)[[4]],add=T,col="black")
# curve((coef(var_total_ricker_adapt_mle_normal_nm)[[1]]*x*exp(coef(var_total_ricker_adapt_mle_normal_nm)[[2]]*x))+coef(var_total_ricker_adapt_mle_normal_nm)[[3]],add=T,col="black")
# curve((coef(var_total_logit_adapt_mle_normal_nm)[[1]]/(1+exp(-(coef(var_total_logit_adapt_mle_normal_nm)[[2]]*(x-coef(var_total_logit_adapt_mle_normal_nm)[[3]])))))+coef(var_total_logit_adapt_mle_normal_nm)[[4]],add=T,col="green")
# 
# 
# curve((coef(var_total_logit_adapt)[[1]]/(1+exp(-(coef(var_total_logit_adapt)[[2]]*(x-coef(var_total_logit_adapt)[[3]])))))+coef(var_total_logit_adapt)[[4]],add=T,col="red")
#curve(((coef(var_total_holling_adapt)[[1]]*(x^2))/(coef(var_total_holling_adapt)[[2]]+(coef(var_total_holling_adapt)[[3]]*x)+(x^2)))+coef(var_total_holling_adapt)[[4]],add=T,col="red")
