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

# ### nulo
var_inter_nulo_adapt_excl<-lm(var_inter_adapt_excl~1)
 funcao_nulo_mle_normal <- function(mean,sd){
   -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
 }
 var_inter_nulo_adapt_excl_mle_normal <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(var_inter_adapt_excl),sd=sd(var_inter_adapt_excl)))
 var_inter_nulo_adapt_excl_mle_normal_nm <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(var_inter_adapt_excl),sd=sd(var_inter_adapt_excl)),method = "Nelder-Mead")
# 
# ### linear
var_inter_linear_adapt_excl<-lm(var_inter_adapt_excl~dist_adapt_excl)
funcao_linear_mle_normal <- function(a,sd){
  mean <- a*dist_adapt_excl
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
var_inter_linear_adapt_excl_mle_normal <- mle2(funcao_linear_mle_normal, start=list(a=coef(var_inter_linear_adapt_excl)[[2]],sd=sd(var_inter_adapt_excl)))
var_inter_linear_adapt_excl_mle_normal_nm <- mle2(funcao_linear_mle_normal, start=list(a=coef(var_inter_linear_adapt_excl)[[2]],sd=sd(var_inter_adapt_excl)),method = "Nelder-Mead")

### mm

var_inter_mm_adapt_excl <- nls(var_inter_adapt_excl~I(I(a*dist_adapt_excl)/I(b+dist_adapt_excl)),start=list(a=-1,b=25))
funcao_mm_mle_normal <- function(a,b,sd){
  mean <- ((a*dist_adapt_excl)/(b+dist_adapt_excl))
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
var_inter_mm_adapt_excl_mle_normal <- mle2(funcao_mm_mle_normal, start=list(a=coef(var_inter_mm_adapt_excl)[[1]],b=coef(var_inter_mm_adapt_excl)[[2]],sd=sd(var_inter_adapt_excl)))
var_inter_mm_adapt_excl_mle_normal_nm <- mle2(funcao_mm_mle_normal, start=list(a=coef(var_inter_mm_adapt_excl)[[1]],b=coef(var_inter_mm_adapt_excl)[[2]],sd=sd(var_inter_adapt_excl)),method = "Nelder-Mead")

### potencia
#var_inter_pot_adapt_excl <- nls(var_inter_adapt_excl~(a*(dist_adapt_excl^b)),start=list(a=845.51,b=-0.65488))
funcao_pot_mle_normal <- function(a,b,sd){
  mean <- (a*(dist_adapt_excl^b))
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start<-list(a=845.51,b=-0.65488,sd=sd(var_inter_adapt_excl))
var_inter_pot_adapt_excl_mle_normal<-mle2(funcao_pot_mle_normal, start=start,control=list(maxit=1000))
var_inter_pot_adapt_excl_mle_normal_nm<-mle2(funcao_pot_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
#var_inter_logit_adapt_excl <- nls(var_inter_adapt_excl~(a/(1+exp(-(b*(dist_adapt_excl-c))))),start=list(a=0.1,b=-0.003,c=2000))
funcao_logit_mle_normal <- function(a,b,c,sd){
  mean <- (a/(1+exp(-(b*(dist_adapt_excl-c)))))
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=0.1,b=-0.003,c=2000,sd=sd(var_inter_adapt_excl))
var_inter_logit_adapt_excl_mle_normal <- mle2(funcao_logit_mle_normal, start=start)
var_inter_logit_adapt_excl_mle_normal_nm <- mle2(funcao_logit_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=100000))

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
var_inter_quadr_adapt_excl <- nls(var_inter_adapt_excl~(a*(dist_adapt_excl^2))+(b*dist_adapt_excl),start=list(a=-3e-10,b=6e-5))
funcao_quadr_mle_normal <- function(a,b,sd){
  mean <- a*(dist_adapt_excl^2) + b*dist_adapt_excl
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=-3e-10,b=6e-5,sd=sd(var_inter_adapt_excl))
var_inter_quadr_adapt_excl_mle_normal <- mle2(funcao_quadr_mle_normal, start=start)
var_inter_quadr_adapt_excl_mle_normal_nm <- mle2(funcao_quadr_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### ricker
var_inter_ricker_adapt_excl <- nls(var_inter_adapt_excl~(a*dist_adapt_excl*exp(b*dist_adapt_excl)),start=list(a=1.4e-04,b=-20e-06))
funcao_ricker_mle_normal <- function(a,b,sd){
  mean <- (a*dist_adapt_excl*exp(b*dist_adapt_excl))
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=1.4e-04,b=-20e-06,sd=sd(var_inter_adapt_excl))
var_inter_ricker_adapt_excl_mle_normal <- mle2(funcao_ricker_mle_normal, start=start)
var_inter_ricker_adapt_excl_mle_normal_nm <- mle2(funcao_ricker_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### holling
var_inter_holling_adapt_excl <- nls(var_inter_adapt_excl~((a*(dist_adapt_excl^2))/(b+(c*dist_adapt_excl)+(dist_adapt_excl^2))),start=list(a=0.0011,b=3.5e+08,c=-15000))
funcao_holling_mle_normal <- function(a,b,c,sd){
  mean <- ((a*(dist_adapt_excl^2))/(b+(c*dist_adapt_excl)+(dist_adapt_excl^2)))
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=0.0011,b=3.5e+08,c=-15000,sd=sd(var_inter_adapt_excl))
var_inter_holling_adapt_excl_mle_normal <- mle2(funcao_holling_mle_normal, start=start,control=list(maxit=1000))
var_inter_holling_adapt_excl_mle_normal_nm <- mle2(funcao_holling_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### gaussiana
curve(2.5*exp(-(((x-100e3)^2)/(2*30000^2))),add=T,col="blue")
curve(coef(var_inter_gauss_adapt_excl)[[1]]*exp(-(((x-coef(var_inter_gauss_adapt_excl)[[2]])^2)/(2*coef(var_inter_gauss_adapt_excl)[[3]]^2))),add=T,col="blue")
var_inter_gauss_adapt_excl <- nls(var_inter_adapt_excl~(a*exp(-(((dist_adapt_excl-b)^2)/(2*c^2)))),start=list(a=2.5,b=100e3,c=30000))
funcao_gauss_mle_normal <- function(a,b,c,sd){
  mean <- (a*exp(-(((dist_adapt_excl-b)^2)/(2*c^2))))
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=2.5,b=100e3,c=30000,sd=sd(var_inter_adapt_excl))
var_inter_gauss_adapt_excl_mle_normal <- mle2(funcao_gauss_mle_normal, start=start)
var_inter_gauss_adapt_excl_mle_normal_nm <- mle2(funcao_gauss_mle_normal, start=start,method = "Nelder-Mead")

### a variancia da distribuicao eh modelada

### gaussiana
funcao_gauss_mle_normal_sd <- function(a,b,c,mean){
  sd <- (a*exp(-(((dist_adapt_excl-b)^2)/(2*c^2))))
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=2.5,b=100e3,c=30000,mean=mean(var_inter_adapt_excl))
var_inter_gauss_adapt_excl_mle_normal_sd <- mle2(funcao_gauss_mle_normal_sd, start=start)
var_inter_gauss_adapt_excl_mle_normal_nm_sd <- mle2(funcao_gauss_mle_normal_sd, start=start,method = "Nelder-Mead")



funcao_gauss_mle_normal_sd_mean <- function(a,b,c,d,e,f){
  mean <- (a*exp(-(((dist_adapt_excl-b)^2)/(2*c^2))))  
  sd <- (d*exp(-(((dist_adapt_excl-e)^2)/(2*f^2))))
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=2.5,b=100e3,c=30000,d=2.5,e=100e3,f=30000)
var_inter_gauss_adapt_excl_mle_normal_sd_mean <- mle2(funcao_gauss_mle_normal_sd_mean, start=start,control=list(maxit=10000))
var_inter_gauss_adapt_excl_mle_normal_nm_sd_mean <- mle2(funcao_gauss_mle_normal_sd_mean, start=start,method = "Nelder-Mead")



### ricker
funcao_ricker_mle_normal_sd <- function(a,b,mean){
  sd <- (a*dist_adapt_excl*exp(b*dist_adapt_excl))
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=1.4e-04,b=-20e-06,mean=mean(var_inter_adapt_excl))
var_inter_ricker_adapt_excl_mle_normal_sd <- mle2(funcao_ricker_mle_normal_sd, start=start)
var_inter_ricker_adapt_excl_mle_normal_nm_sd <- mle2(funcao_ricker_mle_normal_sd, start=start,method = "Nelder-Mead",control=list(maxit=1000))


funcao_ricker_mle_normal_sd_mean <- function(a,b,c,d){
  mean <- (a*dist_adapt_excl*exp(b*dist_adapt_excl))
  sd <- (c*dist_adapt_excl*exp(d*dist_adapt_excl))
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=1.4e-04,b=-20e-06,c=1.4e-04,d=-20e-06)
var_inter_ricker_adapt_excl_mle_normal_sd_mean <- mle2(funcao_ricker_mle_normal_sd_mean, start=start)
var_inter_ricker_adapt_excl_mle_normal_nm_sd_mean <- mle2(funcao_ricker_mle_normal_sd_mean, start=start,method = "Nelder-Mead",control=list(maxit=1000))


var_inter_ricker_adapt_excl_mle_normal_nm_sd_mean

## GAMA

### nulo
funcao_nulo_mle_gama <- function(scale,shape){
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
var_inter_nulo_adapt_excl_mle_gama <- mle2(funcao_nulo_mle_gama, start=list(scale=var(var_inter_adapt_excl)/mean(var_inter_adapt_excl),shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)))
var_inter_nulo_adapt_excl_mle_gama_nm <- mle2(funcao_nulo_mle_gama, start=list(scale=var(var_inter_adapt_excl)/mean(var_inter_adapt_excl),shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)),method = "Nelder-Mead")

### linear
funcao_linear_mle_gama <- function(a,b,shape){
  scale <- ((a*dist_adapt_excl+b))/shape
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
#var_inter_linear_adapt_excl_mle_gama <- mle2(funcao_linear_mle_gama, start=list(a=-5e-6,b=0.25,shape=0.071869))
#var_inter_linear_adapt_excl_mle_gama <- mle2(funcao_linear_mle_gama, start=list(a=coef(var_inter_linear_adapt_excl)[[2]]/(mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)),shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl),b=0.25),method = "Nelder-Mead")
var_inter_linear_adapt_excl_glm_gama <- glm(var_inter_adapt_excl~dist_adapt_excl,family=Gamma(link="identity"))


### mm
funcao_mm_mle_gama <- function(a,b,shape){
  scale <- (((a*dist_adapt_excl)/(b+dist_adapt_excl)))/shape
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
#var_inter_mm_adapt_excl_mle_gama <- mle2(funcao_mm_mle_gama, start=list(a=-coef(var_inter_mm_adapt_excl)[[1]],b=coef(var_inter_mm_adapt_excl)[[2]],shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)))
#var_inter_mm_adapt_excl_mle_gama_nm <- mle2(funcao_mm_mle_gama, start=list(a=-coef(var_inter_mm_adapt_excl)[[1]],b=coef(var_inter_mm_adapt_excl)[[2]],shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl)),method = "Nelder-Mead",control=list(maxit=1000))

### potencia
funcao_pot_mle_gama <- function(a,b,shape){
  scale <- ((a*(dist_adapt_excl^b)))/shape
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
start<-list(a=0.0009,b=0.55,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
start<-list(a=100,b=-0.5,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
var_inter_pot_adapt_excl_mle_gama <- mle2(funcao_pot_mle_gama, start=start,control=list(maxit=10000))
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
funcao_exp_mle_gama <- function(a,b,shape){
  scale <- (exp(a*dist_adapt_excl+b))/shape
  -sum(dgamma(var_inter_adapt_excl, shape=shape, scale=scale, log=TRUE))
}
start=list(a=-0.0015,b=0,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
var_inter_exp_adapt_excl_mle_gama <- mle2(funcao_exp_mle_gama, start=start)
var_inter_exp_adapt_excl_mle_gama_nm <- mle2(funcao_exp_mle_gama, start=start,method = "Nelder-Mead")

### quadratica
funcao_quadr_mle_gama <- function(a,b,shape){
  scale <- (a*(dist_adapt_excl^2) + b*dist_adapt_excl)/shape
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
start <- list(a=-3e-10,b=6e-5,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
#var_inter_quadr_adapt_excl_mle_gama <- mle2(funcao_quadr_mle_gama, start=start)
#var_inter_quadr_adapt_excl_mle_gama_nm <- mle2(funcao_quadr_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=1000))
var_inter_quadr_adapt_excl_glm_gama <- glm(var_inter_adapt_excl~I(dist_adapt_excl^2)-1,family=Gamma(link="identity"))

### ricker
funcao_ricker_mle_gama <- function(a,b,shape){
  scale <- ((a*dist_adapt_excl*exp(b*dist_adapt_excl)))/shape
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
start <- list(a=1.4e-04,b=-20e-06,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
start <- list(a=3e-05,b=-3e-05,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
#var_inter_ricker_adapt_excl_mle_gama <- mle2(funcao_ricker_mle_gama, start=start)
var_inter_ricker_adapt_excl_mle_gama_nm <- mle2(funcao_ricker_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=1000))

### holling
funcao_holling_mle_gama <- function(a,b,c,shape){
  scale <- (((a*(dist_adapt_excl^2))/(b+(c*dist_adapt_excl)+(dist_adapt_excl^2))))/shape
  -sum(dgamma(var_inter_adapt_excl, shape=shape, scale=scale, log=TRUE))
}
start=list(a=0.0011,b=3.5e+08,c=-15000,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
var_inter_holling_adapt_excl_mle_gama <- mle2(funcao_holling_mle_gama, start=start)
var_inter_holling_adapt_excl_mle_gama_nm <- mle2(funcao_holling_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=2000))

#### gaussiana
funcao_gauss_mle_gama <- function(a,b,c,shape){
  scale <- (a*exp(-(((dist_adapt_excl-b)^2)/(2*c^2))))/shape
  -sum(dgamma(var_inter_adapt_excl, shape=shape, scale=scale, log=TRUE))
}
start=list(a=2.5,b=100e3,c=30000,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
#var_inter_gauss_adapt_excl_mle_gama <- mle2(funcao_gauss_mle_gama, start=start)
var_inter_gauss_adapt_excl_mle_gama_nm <- mle2(funcao_gauss_mle_gama, start=start,method = "Nelder-Mead")

##### variancia

### ricker
funcao_ricker_mle_gama_var <- function(a,b,shape){
  scale <- sqrt(((a*dist_adapt_excl*exp(b*dist_adapt_excl)))/shape)
  -sum(dgamma(var_inter_adapt_excl, scale=scale, shape=shape, log=TRUE))
}
start <- list(a=1.4e-04,b=-20e-06,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
start <- list(a=3e-05,b=-3e-05,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
var_inter_ricker_adapt_excl_mle_gama_var <- mle2(funcao_ricker_mle_gama_var, start=start)
var_inter_ricker_adapt_excl_mle_gama_nm_var <- mle2(funcao_ricker_mle_gama_var, start=start,method = "Nelder-Mead",control=list(maxit=1000))

#### gaussiana
funcao_gauss_mle_gama_var <- function(a,b,c,shape){
  scale <- sqrt((a*exp(-(((dist_adapt_excl-b)^2)/(2*c^2))))/shape)
  -sum(dgamma(var_inter_adapt_excl, shape=shape, scale=scale, log=TRUE))
}
start=list(a=2.5,b=100e3,c=30000,shape=mean(var_inter_adapt_excl)^2/var(var_inter_adapt_excl))
var_inter_gauss_adapt_excl_mle_gama_var <- mle2(funcao_gauss_mle_gama_var, start=start,control=list(maxit=1000))
var_inter_gauss_adapt_excl_mle_gama_nm_var <- mle2(funcao_gauss_mle_gama_var, start=start,method = "Nelder-Mead")


#### selecao
AICtab(var_inter_nulo_adapt_excl,
       #var_inter_nulo_adapt_excl_mle_normal,
       #var_inter_nulo_adapt_excl_mle_normal_nm,
       var_inter_nulo_adapt_excl_mle_gama,
       #var_inter_nulo_adapt_excl_mle_gama_nm,
       var_inter_linear_adapt_excl,
       #var_inter_linear_adapt_excl_mle_normal,
       #var_inter_linear_adapt_excl_mle_normal_nm,
      #var_inter_linear_adapt_excl_mle_gama,
      #var_inter_linear_adapt_excl_mle_gama_nm,
        var_inter_linear_adapt_excl_glm_gama,
       var_inter_mm_adapt_excl,
       #var_inter_mm_adapt_excl_mle_normal,
       #var_inter_mm_adapt_excl_mle_normal_nm,
      #var_inter_mm_adapt_excl_mle_gama,
      #var_inter_mm_adapt_excl_mle_gama_nm,
       #var_inter_pot_adapt_excl,
       var_inter_pot_adapt_excl_mle_normal,
       #var_inter_pot_adapt_excl_mle_normal_nm,
       #var_inter_pot_adapt_excl_mle_gama,
       var_inter_pot_adapt_excl_mle_gama_nm,
       #var_inter_logit_adapt_excl,
       #var_inter_logit_adapt_excl_mle_normal,
       var_inter_logit_adapt_excl_mle_normal_nm,
       #var_inter_logit_adapt_excl_mle_gama,
       var_inter_logit_adapt_excl_mle_gama_nm,
       var_inter_exp_adapt_excl,
       #var_inter_exp_adapt_excl_mle_normal,
       #var_inter_exp_adapt_excl_mle_normal_nm,
       #var_inter_exp_adapt_excl_mle_gama,
       var_inter_exp_adapt_excl_mle_gama_nm,
       var_inter_quadr_adapt_excl,
       #var_inter_quadr_adapt_excl_mle_normal,
       #var_inter_quadr_adapt_excl_mle_normal_nm,
      #var_inter_quadr_adapt_excl_mle_gama,
      #var_inter_quadr_adapt_excl_mle_gama_nm,
        var_inter_quadr_adapt_excl_glm_gama,
       var_inter_ricker_adapt_excl,
       #var_inter_ricker_adapt_excl_mle_normal,
       #var_inter_ricker_adapt_excl_mle_normal_nm,
      #var_inter_ricker_adapt_excl_mle_gama,
       var_inter_ricker_adapt_excl_mle_gama_nm,
       var_inter_holling_adapt_excl,
       #var_inter_holling_adapt_excl_mle_normal,
      #var_inter_holling_adapt_excl_mle_normal_nm,
       #var_inter_holling_adapt_excl_mle_gama,
       var_inter_holling_adapt_excl_mle_gama_nm,
      var_inter_gauss_adapt_excl,
      #var_inter_gauss_adapt_excl_mle_normal,
      #var_inter_gauss_adapt_excl_mle_normal_nm,
      #var_inter_gauss_adapt_excl_mle_gama,
      var_inter_gauss_adapt_excl_mle_gama_nm,
      var_inter_gauss_adapt_excl_mle_normal_sd,
      #var_inter_gauss_adapt_excl_mle_normal_nm_sd,
      var_inter_gauss_adapt_excl_mle_normal_nm_sd_mean,
      var_inter_ricker_adapt_excl_mle_normal_nm_sd,
      var_inter_ricker_adapt_excl_mle_normal_nm_sd_mean,
      #var_inter_gauss_adapt_excl_mle_gama_var,
      var_inter_gauss_adapt_excl_mle_gama_nm_var,
      #var_inter_ricker_adapt_excl_mle_gama_var,
      var_inter_ricker_adapt_excl_mle_gama_nm_var,
      base=T
)

AICtab(var_inter_nulo_adapt_excl_mle_normal_nm,var_inter_nulo_adapt_excl_mle_gama_nm)

#### plotando
par(mar=c(5,5,4,3))
par(mgp=c(3.5,0.7,0))
par(tck=-0.02)
plot(var_inter_adapt_excl~dist_adapt_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylab="Variância interespecífica do índice de estratégia de vida",xlab="Índice de distúrbio (mortes acumuladas / tamanho da comunidade)",axes=F)
#plot(var_inter_adapt_excl~dist_adapt_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylab="Variância interespecífica do índice de estratégia de vida",xlab=expression(paste("Índice de distúrbio (","10"^"3"," mortes acumuladas / tamanho da comunidade)",sep="")),axes=F)
axis(1,las=1,at=c(-50000,0,50000,100000,150000,200000,250000,300000,350000),labels=c("","0",expression("50"%.%10^"3"),expression("100"%.%10^"3"),expression("150"%.%10^"3"),expression("200"%.%10^"3"),expression("250"%.%10^"3"),expression("300"%.%10^"3"),""),cex.axis=0.9)
axis(2,las=1,at=c(-10,0,0.5,1,1.5,2,2.5,10),labels=c("","0","0,5","1","1,5","2","2,5",""),cex.axis=0.9)
curve((coef(var_inter_ricker_adapt_excl_mle_gama_nm)[[1]]*x*exp(coef(var_inter_ricker_adapt_excl_mle_gama_nm)[[2]]*x)),add=T,col="black")

#curve((3e-5*x*exp(-3e-5*x)),add=T,col="red")

#curve(2.5*exp(-(((x-100e3)^2)/(2*30000^2))),add=T,col="blue")
c#urve(coef(var_inter_gauss_adapt_excl)[[1]]*exp(-(((x-coef(var_inter_gauss_adapt_excl)[[2]])^2)/(2*coef(var_inter_gauss_adapt_excl)[[3]]^2))),add=T,col="blue")
