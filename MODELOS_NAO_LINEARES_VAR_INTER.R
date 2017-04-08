# MODELOS REAL OFICIAL nao linear
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

#################### VARIANCIA INTER ####################

# CENARIO EXCLUSAO
var_inter_excl1 <- dados$var_inter[dados$bateria==2]/(20000^2)
var_inter_excl <- var_inter_excl1[-which(is.na(var_inter_excl1))]
var_inter_excl <- var_inter_excl+0.00001
#var_inter_excl <- var_inter_excl + 0.000001
dist_excl <- dados$dist_indice[dados$bateria==2]
dist_excl <- dist_excl[-which(is.na(var_inter_excl1))]
## modelos
### nulo
var_inter_nulo_excl <-lm(var_inter_excl~1)
### linear
var_inter_linear_excl <-lm(var_inter_excl~dist_excl)
### mm
coef_mm_linearizada <- coef(lm(I(dist_excl/var_inter_excl)~dist_excl))
var_inter_mm_excl <- nls(var_inter_excl~I(I(a*dist_excl)/I(b+dist_excl)+c),start=list(a=1/coef_mm_linearizada[[2]],b=coef_mm_linearizada[[1]]*(1/coef_mm_linearizada[[2]]),c=0))
curve(((-55*x)/(25+x))+55,add=T,col="red")
var_inter_mm_excl2 <- nls(var_inter_excl~I(I(a*dist_excl)/I(b+dist_excl)+c),start=list(a=-55,b=25,c=55))

### potencia
plot(var_inter_excl~dist_excl)
curve(1e-1*(x^(-0.001))+0,add=T,col="blue")
#curve(1e-10*(x^(-1))+0,add=T,col="blue") # esta eh a curva que se usa quando nao somamos 0.1 no y
var_inter_pot_excl <- nls(var_inter_excl~I(a*I(dist_excl^(b))+c),start=list(a=1e-10,b=-1,c=0)) #### QUE POOOOORRRA
##
require(nlmrt)
my_data<-data.frame(my_x=dist_excl,my_y=var_inter_excl)
teste <- nlxb(my_y~(a*(my_x^b)+c),start=list(a=1e-10,b=-1,c=0),data=my_data) # deu certo, agora vou usar os valores pro nls
##
var_inter_pot_excl <- nls(my_y~(a*(my_x^b)+c),start=list(a=teste$coefficients[[1]],b=teste$coefficients[[2]],c=teste$coefficients[[3]]),data=my_data) # rolou
# var_inter_pot_excl <- nls2(var_inter_excl~(a*(dist_excl^b)+c),start=list(a=data.frame(a=c(11e-4,mean(11e-4,teste$coefficients[[1]]),teste$coefficients[[1]])),b=data.frame(b=c(0.01,mean(0.01,teste$coefficients[[2]]),teste$coefficients[[2]])),c=data.frame(c=c(0,mean(0,teste$coefficients[[3]]),teste$coefficients[[3]]))),algorithm = "brute-force")
# ## mle
# funcao_pot_mle <- function(a,b,c,sd){
#   mean <- (a*(dist_excl^b))+c
#   -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
# }
# start <- list(a=11e-4,b=0.01,c=0,sd=sd(var_inter_excl))
# var_inter_pot_excl <- mle2(funcao_pot_mle, start=start)


### logistica
coef_logit_linearizada <- coef(lm(log(var_inter_excl/(1-var_inter_excl))~dist_excl))
var_inter_logit_excl <- nls(var_inter_excl~(1/(1+exp(-a*dist_excl-b)))+c,start=list(a=coef_logit_linearizada[[2]],b=coef_logit_linearizada[[1]],c=0))
curve((0.1/(1+exp(-(-0.003*(x-2000))))),add=T,col="red")
var_inter_logit_excl_2 <- nls(var_inter_excl~(a/(1+exp(-(b*(dist_excl-c))))+d),start=list(a=0.1,b=-0.003,c=2000,d=0))
##
require(nlmrt)
my_data<-data.frame(my_x=dist_excl,my_y=var_inter_excl)
teste2 <- nlxb(my_y~(a/(1+exp(-(b*(dist_excl-c))))+d),start=list(a=0.1,b=-0.003,c=2000,d=0),data=my_data)
require(nls2)
var_inter_logit_excl_2_fb <- nls2(var_inter_excl~(a/(1+exp(-(b*(dist_excl-c))))+d),start=list(a=data.frame(a=c(0.1,0.1,0.15,0.09)),b=data.frame(b=c(-0.005,-0.003,-1,-0.5)),c=data.frame(c=c(2000,2000,2000,2000)),d=data.frame(d=c(0,0,0,0))),algorithm = "brute-force")

funcao_logit_mle <- function(a,b,c,d,sd){
  mean <- (a/(1+exp(-(b*(dist_excl-c))))+d)
  -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=0.1,b=-0.003,c=2000,d=0,sd=sd(var_inter_excl))
var_inter_logit_excl_2_mle <- mle2(funcao_logit_mle, start=start)

### exponencial
coef_exp_linearizada <- coef(lm(log(var_inter_excl)~dist_excl))
var_inter_exp_excl <- nls(var_inter_excl~(exp(a*dist_excl+b)),start=list(a=coef_exp_linearizada[[2]],b=coef_exp_linearizada[[1]]))
curve(exp(-0.0015*x+0),add=T,col="violet")
var_inter_exp_excl <- nls(var_inter_excl~(exp(a*dist_excl+b)),start=list(a=-0.0015,b=0))

### quadratica
curve((0.3e-12*(x^2))+(-4e-08*x)+0.0013,add=T,col="red")
my_data<-data.frame(my_x=dist_excl,my_y=var_inter_excl)
var_inter_quadr_exc <- nls(my_y~(a*(my_x^2))+(b*my_x)+c,start=list(a=0.3e-12,b=-4e-08,c=0.0013),data=my_data)
### ricker
curve(-1.3048e-07*x*exp(-35e-06*x)+0.0013,add=T,col="blue")
my_data<-data.frame(my_x=dist_excl,my_y=var_inter_excl)
var_inter_ricker_excl <- nls(my_y~(a*my_x*exp(b*my_x))+c,start=list(a=-1.3048e-07,b=-35e-06,c=0.0013),data=my_data)
funcao_ricker_mle <- function(a,b,c,sd){
  mean <- (a*dist_excl*exp(b*dist_excl))+c
  -sum(dnorm(var_inter_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=-1.3048e-07,b=-35e-06,c=0.0013,sd=sd(var_inter_excl))
var_inter_ricker_excl_mle <- mle2(funcao_ricker_mle, start=start)

### holling type 4
curve(((-0.0028*(x^2))/(0.5e7+(-100*x)+(x^2)))+0.0028,add=T,col="blue")
my_data<-data.frame(my_x=dist_excl,my_y=var_inter_excl)
var_inter_holling_excl <- nls(my_y~((a*(my_x^2))/(b+(c*my_x)+(my_x^2)))+d,start=list(a=-0.0028,b=0.5e7,c=-100,d=0.0028),data=my_data)


#### selecao
#AICtab(var_inter_nulo_excl,var_inter_linear_excl,var_inter_mm_excl,var_inter_mm_excl2,var_inter_pot_excl,var_inter_logit_excl_2_fb,var_inter_logit_excl_2_mle,var_inter_exp_excl,var_inter_quadr_exc,var_inter_ricker_excl_mle,var_inter_holling_excl)
AICtab(var_inter_nulo_excl,var_inter_linear_excl,var_inter_mm_excl,var_inter_mm_excl2,var_inter_pot_excl,var_inter_logit_excl_2_mle,var_inter_exp_excl,var_inter_quadr_exc,var_inter_ricker_excl_mle,var_inter_holling_excl)

#### plotando
plot(var_inter_excl~dist_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5))
curve((coef(var_inter_logit_excl_2_mle)[[1]]/(1+exp(-(coef(var_inter_logit_excl_2_mle)[[2]]*(x-coef(var_inter_logit_excl_2_mle)[[3]])))))+coef(var_inter_logit_excl_2_mle)[[4]],add=T,col="green4")
curve(((coef(var_inter_mm_excl2)[[1]]*x)/(coef(var_inter_mm_excl2)[[2]]+x))+coef(var_inter_mm_excl2)[[3]],add=T,col="blue")
curve((coef(var_inter_pot_excl)[[1]]*(x^coef(var_inter_pot_excl)[[2]])+coef(var_inter_pot_excl)[[3]]),add=T,col="red")
abline(var_inter_linear_excl)
curve(exp(coef(var_inter_exp_excl)[[1]]*x+coef(var_inter_exp_excl)[[2]]),add=T,col="violet")
curve(((coef(var_inter_holling_excl)[[1]]*(x^2))/(coef(var_inter_holling_excl)[[2]]+(coef(var_inter_holling_excl)[[3]]*x)+(x^2)))+coef(var_inter_holling_excl)[[4]],add=T,col="pink")
curve((coef(var_inter_ricker_excl_mle)[[1]]*x*exp(coef(var_inter_ricker_excl_mle)[[2]]*x)+coef(var_inter_ricker_excl_mle)[[3]]),add=T,col="yellow")
abline(var_inter_nulo_excl,col="gray")
curve((coef(var_inter_quadr_exc)[[1]]*(x^2)+coef(var_inter_quadr_exc)[[2]]*x+coef(var_inter_quadr_exc)[[3]]),add=T,col="yellow")

# CENARIO ADAPTACAO + EXCLUSAO
var_inter_adapt_excl1 <- dados$var_inter[dados$bateria==3]/(20000^2)
var_inter_adapt_excl <- var_inter_adapt_excl1[-which(is.na(var_inter_adapt_excl1))]
var_inter_adapt_excl <- var_inter_adapt_excl+0.00001
dist_adapt_excl <- dados$dist_indice[dados$bateria==3]
dist_adapt_excl <- dist_adapt_excl[-which(is.na(var_inter_adapt_excl1))]
## modelos
### nulo
var_inter_nulo_adapt_excl <-lm(var_inter_adapt_excl~1)
### linear
var_inter_linear_adapt_excl <-lm(var_inter_adapt_excl~dist_adapt_excl)
### mm
coef_mm_linearizada <- coef(lm(I(dist_adapt_excl/var_inter_adapt_excl)~dist_adapt_excl))
var_inter_mm_adapt_excl <- nls(var_inter_adapt_excl~I(I(a*dist_adapt_excl)/I(b+dist_adapt_excl)+c),start=list(a=1/coef_mm_linearizada[[2]],b=coef_mm_linearizada[[1]]*(1/coef_mm_linearizada[[2]]),c=0))
### potencia
plot(var_inter_adapt_excl~dist_adapt_excl)
curve(11e-4*(x^0.01)+0,add=T,col="red")
var_inter_pot_adapt_excl <- nls(var_inter_adapt_excl~I(a*I(dist_adapt_excl^b)+c),start=list(a=11e-4,b=0.01,c=0))
##
require(nlmrt)
my_data<-data.frame(my_x=dist_adapt_excl,my_y=var_inter_adapt_excl)
teste <- nlxb(my_y~(a*(my_x^b)+c),start=list(a=11e-4,b=0.01,c=0),data=my_data)
##
#var_inter_pot_adapt_excl <- nls(my_y~(a*(my_x^b)+c),start=list(a=teste$coefficients[[1]],b=teste$coefficients[[2]],c=teste$coefficients[[3]]),data=my_data) # nao deu certo saporra entao vou usar o nls 2 com forca bruta
#require(nls2)
#var_inter_pot_adapt_excl <- nls2(var_inter_adapt_excl~(a*(dist_adapt_excl^b)+c),start=list(a=data.frame(a=c(11e-4,mean(11e-4,teste$coefficients[[1]]),teste$coefficients[[1]])),b=data.frame(b=c(0.01,mean(0.01,teste$coefficients[[2]]),teste$coefficients[[2]])),c=data.frame(c=c(0,mean(0,teste$coefficients[[3]]),teste$coefficients[[3]]))),algorithm = "brute-force")
## mle
funcao_pot_mle <- function(a,b,c,sd){
  mean <- (a*(dist_adapt_excl^b))+c
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=11e-4,b=0.01,c=0,sd=sd(var_inter_adapt_excl))
var_inter_pot_adapt_excl_mle <- mle2(funcao_pot_mle, start=start)

### logistica
coef_logit_linearizada <- coef(lm(log(var_inter_adapt_excl/(1-var_inter_adapt_excl))~dist_adapt_excl))
var_inter_logit_adapt_excl <- nls(var_inter_adapt_excl~(1/(1+exp(-a*dist_adapt_excl-b)))+c,start=list(a=coef_logit_linearizada[[2]],b=coef_logit_linearizada[[1]],c=0))
curve((0.0016/(1+exp(-(0.0002*(x-5000)))))-0.0004,add=T,col="red")
var_inter_logit_adapt_excl_2 <- nls(var_inter_adapt_excl~(a/(1+exp(-(b*(dist_adapt_excl-c))))+d),start=list(a=0.0016,b=0.0002,c=5000,d=0.0004))
##
require(nlmrt)
my_data<-data.frame(my_x=dist_adapt_excl,my_y=var_inter_adapt_excl)
teste <- nlxb(my_y~(1/(1+exp(-a*my_x-b)))+c,start=list(a=coef_logit_linearizada[[2]],b=coef_logit_linearizada[[1]],c=0),data=my_data) # deu certo, agora vou usar os valores pro nls
##
var_inter_logit_adapt_excl <- nls(my_y~(1/(1+exp(-a*my_x-b)))+c,start=list(a=teste$coefficients[[1]],b=teste$coefficients[[2]],c=teste$coefficients[[3]]),data=my_data) # ROLOU
# require(nls2)
# var_inter_logit_adapt_excl <- nls2(var_inter_adapt_excl~(1/(1+exp(-a*dist_adapt_excl-b)))+c,start=list(a=data.frame(a=c(coef_logit_linearizada[[2]],mean(coef_logit_linearizada[[2]],teste$coefficients[[1]]),teste$coefficients[[1]])),b=data.frame(b=c(coef_logit_linearizada[[1]],mean(coef_logit_linearizada[[1]],teste$coefficients[[2]]),teste$coefficients[[2]])),c=data.frame(c=c(0,mean(0,teste$coefficients[[3]]),teste$coefficients[[3]]))),algorithm = "brute-force")
### exponencial
coef_exp_linearizada <- coef(lm(log(var_inter_adapt_excl)~dist_adapt_excl))
var_inter_exp_adapt_excl <- nls(var_inter_adapt_excl~(exp(a*dist_adapt_excl+b)),start=list(a=coef_exp_linearizada[[2]],b=coef_exp_linearizada[[1]]))
### quadratica
curve((-3e-10*(x^2))+(6e-5*x),add=T,col="red")
my_data<-data.frame(my_x=dist_adapt_excl,my_y=var_inter_adapt_excl)
var_inter_quadr_adapt_excl <- nls(my_y~(a*(my_x^2))+(b*my_x)+c,start=list(a=-3e-10,b=6e-5,c=0),data=my_data)
### ricker
curve(1.4e-04*x*exp(-20e-06*x),add=T,col="blue")
my_data<-data.frame(my_x=dist_adapt_excl,my_y=var_inter_adapt_excl)
var_inter_ricker_adapt_excl <- nls(my_y~(a*my_x*exp(b*my_x))+c,start=list(a=1.4e-04,b=-20e-06,c=0),data=my_data)
### holling type 4
curve(((0.0011*(x^2))/(350000000+(-15000*x)+(x^2)))+0.00002,add=T,col="green")
my_data<-data.frame(my_x=dist_adapt_excl,my_y=var_inter_adapt_excl)
var_inter_holling_adapt_excl <- nls(my_y~((a*(my_x^2))/(b+(c*my_x)+(my_x^2)))+d,start=list(a=0.0011,b=3.5e+08,c=-15000,d=0),data=my_data)
funcao_holling_mle <- function(a,b,c,d,sd){
  mean <- ((a*(dist_adapt_excl^2))/(b+(c*dist_adapt_excl)+(dist_adapt_excl^2)))+d
  -sum(dnorm(var_inter_adapt_excl, mean=mean, sd=sd, log=TRUE))
}
start=list(a=0.0011,b=3.5e+08,c=-15000,d=0,sd=sd(var_inter_adapt_excl))
var_inter_holling_adapt_excl_mle <- mle2(funcao_holling_mle, start=start)

#### selecao
AICtab(var_inter_nulo_adapt_excl,var_inter_linear_adapt_excl,var_inter_mm_adapt_excl,var_inter_pot_adapt_excl_mle,var_inter_logit_adapt_excl,var_inter_exp_adapt_excl,var_inter_quadr_adapt_excl,var_inter_ricker_adapt_excl,var_inter_holling_adapt_excl_mle)
#### plotando
plot(var_inter_adapt_excl~dist_adapt_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5))
curve((1/(1+exp(-(coef(var_inter_logit_adapt_excl)[[1]]*x)-coef(var_inter_logit_adapt_excl)[[2]])))+coef(var_inter_logit_adapt_excl)[[3]],add=T,col="green")
curve((coef(var_inter_logit_adapt_excl_2)[[1]]/(1+exp(-(coef(var_inter_logit_adapt_excl_2)[[2]]*(x-coef(var_inter_logit_adapt_excl_2)[[3]])))))+coef(var_inter_logit_adapt_excl_2)[[4]],add=T,col="green4")
curve(((coef(var_inter_mm_adapt_excl)[[1]]*x)/(coef(var_inter_mm_adapt_excl)[[2]]+x))+coef(var_inter_mm_adapt_excl)[[3]],add=T,col="blue") #CORRIGIR?
curve((coef(var_inter_pot_adapt_excl_mle)[[1]]*(x^coef(var_inter_pot_adapt_excl_mle)[[2]])+coef(var_inter_pot_adapt_excl_mle)[[3]]),add=T,col="red")
abline(var_inter_linear_adapt_excl)
curve(exp(coef(var_inter_exp_adapt_excl)[[1]]*x+coef(var_inter_exp_adapt_excl)[[2]]),add=T,col="violet")
abline(var_inter_nulo_adapt_excl,col="gray")
curve(((coef(var_inter_holling_adapt_excl_mle)[[1]]*(x^2))/(coef(var_inter_holling_adapt_excl_mle)[[2]]+(coef(var_inter_holling_adapt_excl_mle)[[3]]*x)+(x^2)))+coef(var_inter_holling_adapt_excl_mle)[[4]],add=T,col="pink")
curve((coef(var_inter_ricker_adapt_excl)[[1]]*x*exp(coef(var_inter_ricker_adapt_excl)[[2]]*x)+coef(var_inter_ricker_adapt_excl)[[3]]),add=T,col="yellow")
curve((coef(var_inter_quadr_adapt_excl)[[1]]*(x^2)+coef(var_inter_quadr_adapt_excl)[[2]]*x+coef(var_inter_quadr_adapt_excl)[[3]]),add=T,col="yellow")
