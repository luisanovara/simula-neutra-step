# MODELOS REAL OFICIAL nao linear
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

######################### MEDIA #########################

# CENARIO ADAPTACAO

media_adapt <- dados$media[dados$bateria==1]/20000
dist_adapt <- dados$dist_indice[dados$bateria==1]

# MODELOS

## NORMAL

# ### nulo
# media_nulo_adapt<-lm(media_adapt~1)
# funcao_nulo_mle_normal <- function(mean,sd){
#   -sum(dnorm(media_adapt, mean=mean, sd=sd, log=TRUE))
# }
# media_nulo_adapt_mle_normal <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(media_adapt),sd=sd(media_adapt)))
# media_nulo_adapt_mle_normal_nm <- mle2(funcao_nulo_mle_normal, start=list(mean=mean(media_adapt),sd=sd(media_adapt)),method = "Nelder-Mead")
# 
# ### linear
media_linear_adapt<-lm(media_adapt~dist_adapt)
# funcao_linear_mle_normal <- function(a,b,sd){
#   mean <- a*dist_adapt+b
#   -sum(dnorm(media_adapt, mean=mean, sd=sd, log=TRUE))
# }
# media_linear_adapt_mle_normal <- mle2(funcao_linear_mle_normal, start=list(a=coef(media_linear_adapt)[[2]],b=coef(media_linear_adapt)[[1]],sd=sd(media_adapt)))
# media_linear_adapt_mle_normal_nm <- mle2(funcao_linear_mle_normal, start=list(a=coef(media_linear_adapt)[[2]],b=coef(media_linear_adapt)[[1]],sd=sd(media_adapt)),method = "Nelder-Mead")
# 
# ### mm
media_coef_mm_linearizada_adapt <- coef(lm(I(dist_adapt/media_adapt)~dist_adapt))
# media_mm_adapt <- nls(media_adapt~I(I(a*dist_adapt)/I(b+dist_adapt)+c),start=list(a=1/media_coef_mm_linearizada_adapt[[2]],b=media_coef_mm_linearizada_adapt[[1]]*(1/media_coef_mm_linearizada_adapt[[2]]),c=0))
# funcao_mm_mle_normal <- function(a,b,c,sd){
#   mean <- ((a*dist_adapt)/(b+dist_adapt))+c
#   -sum(dnorm(media_adapt, mean=mean, sd=sd, log=TRUE))
# }
# media_mm_adapt_mle_normal <- mle2(funcao_mm_mle_normal, start=list(a=1/media_coef_mm_linearizada_adapt[[2]],b=media_coef_mm_linearizada_adapt[[1]]*(1/media_coef_mm_linearizada_adapt[[2]]),c=0,sd=sd(media_adapt)))
# media_mm_adapt_mle_normal_nm <- mle2(funcao_mm_mle_normal, start=list(a=1/media_coef_mm_linearizada_adapt[[2]],b=media_coef_mm_linearizada_adapt[[1]]*(1/media_coef_mm_linearizada_adapt[[2]]),c=0,sd=sd(media_adapt)),method = "Nelder-Mead")
# 
# ### potencia
# #plot(media_adapt~dist_adapt)
# #curve(0.0009*(x^0.55)+0,add=T)
# media_pot_adapt <- nls(media_adapt~I(a*I(dist_adapt^b)+c),start=list(a=0.0009,b=0.55,c=0))
# funcao_pot_mle_normal <- function(a,b,c,sd){
#   mean <- (a*(dist_adapt^b))+c
#   -sum(dnorm(media_adapt, mean=mean, sd=sd, log=TRUE))
# }
# start<-list(a=0.0009,b=0.55,c=0,sd=sd(media_adapt))
# media_pot_adapt_mle_normal<-mle2(funcao_pot_mle_normal, start=start)
# media_pot_adapt_mle_normal_nm<-mle2(funcao_pot_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=10000))
# 
# ### logistica
# # media_coef_logit_linearizada_adapt <- coef(lm(log(media_adapt/(1-media_adapt))~dist_adapt))
# # media_logit_adapt <- nls(media_adapt~(1/(1+exp(-a*dist_adapt-b)))+c,start=list(a=media_coef_logit_linearizada_adapt[[2]],b=media_coef_logit_linearizada_adapt[[1]],c=0))
# #plot(media_adapt~dist_adapt,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio")
# #curve((0.921/(1+exp(-(0.000035*(x-90000))))),add=T,col="red")
# media_logit_adapt<- nls(media_adapt~(a/(1+exp(-(b*(dist_adapt-c))))+d),start=list(a=0.921,b=0.000035,c=90000,d=0))
# funcao_logit_mle_normal <- function(a,b,c,d,sd){
#   mean <- a/(1+exp(-(b*(dist_adapt-c))))+d
#   -sum(dnorm(media_adapt, mean=mean, sd=sd, log=TRUE))
# }
# start=list(a=0.921,b=0.000035,c=90000,d=0,sd=sd(media_adapt))
# media_logit_adapt_mle_normal <- mle2(funcao_logit_mle_normal, start=start)
# media_logit_adapt_mle_normal_nm <- mle2(funcao_logit_mle_normal, start=start,method = "Nelder-Mead")
# 
# ### exponencial
media_coef_exp_linearizada_adapt <- coef(lm(log(media_adapt)~dist_adapt))
# media_exp_adapt <- nls(media_adapt~(exp(a*dist_adapt+b)),start=list(a=media_coef_exp_linearizada_adapt[[2]],b=media_coef_exp_linearizada_adapt[[1]]))
# funcao_exp_mle_normal <- function(a,b,sd){
#   mean <- (exp(a*dist_adapt+b))
#   -sum(dnorm(media_adapt, mean=mean, sd=sd, log=TRUE))
# }
# start=list(a=media_coef_exp_linearizada_adapt[[2]],b=media_coef_exp_linearizada_adapt[[1]],sd=sd(media_adapt))
# media_exp_adapt_mle_normal <- mle2(funcao_exp_mle_normal, start=start)
# media_exp_adapt_mle_normal_nm <- mle2(funcao_exp_mle_normal, start=start,method = "Nelder-Mead")

## GAMA

### nulo
funcao_nulo_mle_gama <- function(scale,shape){
  -sum(dgamma(media_adapt, scale=scale, shape=shape, log=TRUE))
}
#media_nulo_adapt_mle_gama <- mle2(funcao_nulo_mle_gama, start=list(scale=var(media_adapt)/mean(media_adapt),shape=mean(media_adapt)^2/var(media_adapt)))
media_nulo_adapt_mle_gama_nm <- mle2(funcao_nulo_mle_gama, start=list(scale=var(media_adapt)/mean(media_adapt),shape=mean(media_adapt)^2/var(media_adapt)),method = "Nelder-Mead")

### linear
funcao_linear_mle_gama <- function(a,b,shape){
  scale <- ((a*dist_adapt+b))/shape
  -sum(dgamma(media_adapt, scale=scale, shape=shape, log=TRUE))
}
#media_linear_adapt_mle_gama <- mle2(funcao_linear_mle_gama, start=list(a=coef(media_linear_adapt)[[2]],b=coef(media_linear_adapt)[[1]],shape=mean(media_adapt)^2/var(media_adapt)))
media_linear_adapt_mle_gama_nm <- mle2(funcao_linear_mle_gama, start=list(a=coef(media_linear_adapt)[[2]],b=coef(media_linear_adapt)[[1]],shape=mean(media_adapt)^2/var(media_adapt)),method = "Nelder-Mead")

### mm
funcao_mm_mle_gama <- function(a,b,c,shape){
  scale <- (((a*dist_adapt)/(b+dist_adapt))+c)/shape
  -sum(dgamma(media_adapt, scale=scale, shape=shape, log=TRUE))
}
#media_mm_adapt_mle_gama <- mle2(funcao_mm_mle_gama, start=list(a=1/media_coef_mm_linearizada_adapt[[2]],b=media_coef_mm_linearizada_adapt[[1]]*(1/media_coef_mm_linearizada_adapt[[2]]),c=0,shape=mean(media_adapt)^2/var(media_adapt)),control=list(maxit=1000))
media_mm_adapt_mle_gama_nm <- mle2(funcao_mm_mle_gama, start=list(a=1/media_coef_mm_linearizada_adapt[[2]],b=media_coef_mm_linearizada_adapt[[1]]*(1/media_coef_mm_linearizada_adapt[[2]]),c=0,shape=mean(media_adapt)^2/var(media_adapt)),method = "Nelder-Mead",control=list(maxit=2000))

### potencia
funcao_pot_mle_gama <- function(a,b,c,shape){
  scale <- ((a*(dist_adapt^b))+c)/shape
  -sum(dgamma(media_adapt, scale=scale, shape=shape, log=TRUE))
}
start<-list(a=0.0009,b=0.55,c=0,shape=mean(media_adapt)^2/var(media_adapt))
#media_pot_adapt_mle_gama <- mle2(funcao_pot_mle_gama, start=start)
media_pot_adapt_mle_gama_nm <- mle2(funcao_pot_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
funcao_logit_mle_gama <- function(a,b,c,d,shape){
  scale <- (a/(1+exp(-(b*(dist_adapt-c))))+d)/shape
  -sum(dgamma(media_adapt, scale=scale, shape=shape, log=TRUE))
}
start=list(a=0.921,b=0.000035,c=90000,d=0,shape=mean(media_adapt)^2/var(media_adapt))
#media_logit_adapt_mle_gama <- mle2(funcao_logit_mle_gama, start=start)
media_logit_adapt_mle_gama_nm <- mle2(funcao_logit_mle_gama, start=start,method = "Nelder-Mead")

### exponencial
funcao_exp_mle_gama <- function(a,b,shape){
  scale <- (exp(a*dist_adapt+b))/shape
  -sum(dgamma(media_adapt, shape=shape, scale=scale, log=TRUE))
}
start=list(a=media_coef_exp_linearizada_adapt[[2]],b=media_coef_exp_linearizada_adapt[[1]],shape=mean(media_adapt)^2/var(media_adapt))
#media_exp_adapt_mle_gama <- mle2(funcao_exp_mle_gama, start=start)
media_exp_adapt_mle_gama_nm <- mle2(funcao_exp_mle_gama, start=start,method = "Nelder-Mead")


#### selecao
AICtab(#media_nulo_adapt,
       #media_nulo_adapt_mle_normal,
       #media_nulo_adapt_mle_normal_nm,
       #media_nulo_adapt_mle_gama,
       media_nulo_adapt_mle_gama_nm,
       #media_linear_adapt,
       #media_linear_adapt_mle_normal,
       #media_linear_adapt_mle_normal_nm,
       #media_linear_adapt_mle_gama,
       media_linear_adapt_mle_gama_nm,
       #media_mm_adapt,
       #media_mm_adapt_mle_normal,
       #media_mm_adapt_mle_normal_nm,
       #media_mm_adapt_mle_gama,
       media_mm_adapt_mle_gama_nm,
       #media_pot_adapt,
       #media_pot_adapt_mle_normal,
       #media_pot_adapt_mle_normal_nm,
       #media_pot_adapt_mle_gama,
       media_pot_adapt_mle_gama_nm,
       #media_logit_adapt,
       #media_logit_adapt_mle_normal,
       #media_logit_adapt_mle_normal_nm,
       #media_logit_adapt_mle_gama,
       media_logit_adapt_mle_gama_nm,
       #media_exp_adapt,
       #media_exp_adapt_mle_normal,
       #media_exp_adapt_mle_normal_nm,
       #media_exp_adapt_mle_gama,
       media_exp_adapt_mle_gama_nm)

#### plotando
par(mar=c(5,5,4,3))
par(mgp=c(3.5,0.7,0))
par(tck=-0.02)
plot(media_adapt~dist_adapt,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio (mortes acumuladas / tamanho da comunidade)",axes=F)
axis(1,las=1,at=c(-50000,0,50000,100000,150000,200000,250000,300000,350000),labels=c("","0",expression("50"%.%10^"3"),expression("100"%.%10^"3"),expression("150"%.%10^"3"),expression("200"%.%10^"3"),expression("250"%.%10^"3"),expression("300"%.%10^"3"),""),cex.axis=0.9)
axis(2,las=1,at=c(-1,0,0.25,0.5,0.75,1,2),labels=c("","0","0,25","0,5","0,75","1",""),cex.axis=0.9)
curve((coef(media_logit_adapt_mle_gama_nm)[[1]]/(1+exp(-(coef(media_logit_adapt_mle_gama_nm)[[2]]*(x-coef(media_logit_adapt_mle_gama_nm)[[3]])))))+coef(media_logit_adapt_mle_gama_nm)[[4]],add=T,col="black")

