# MODELOS REAL OFICIAL nao linear
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

### GRAFICOS INICIAIS
media <- dados$media[-which(dados$bateria==0)]/20000
bateria <- as.factor(c(rep("Adaptação",1000),rep("Exclusão",1000),rep("Exclusão + Adaptação",1000)))
par(bty="l")
plot(media~bateria,bty="l",las=1,ylab="Média do índice de estratégia de vida",xlab="",pch="*")
points(x=c(1,2,3),y=c(mean(media[1:1000]),mean(media[1001:2000]),mean(media[2001:3000])),pch=20)

######################### MEDIA #########################

# CENARIO ADAPTACAO
media_adapt <- dados$media[dados$bateria==1]/20000
dist_adapt <- dados$dist_indice[dados$bateria==1]
## modelos
## NORMAL
### nulo
media_nulo_adapt_normal<-lm(media_adapt~1)
### linear
media_linear_adapt_normal<-lm(media_adapt~dist_adapt)
### mm
media_coef_mm_linearizada_adapt <- coef(lm(I(dist_adapt/media_adapt)~dist_adapt))
media_mm_adapt_normal <- nls(media_adapt~I(I(a*dist_adapt)/I(b+dist_adapt)+c),start=list(a=1/media_coef_mm_linearizada_adapt[[2]],b=media_coef_mm_linearizada_adapt[[1]]*(1/media_coef_mm_linearizada_adapt[[2]]),c=0))
### potencia
#plot(media_adapt~dist_adapt)
#curve(0.0009*(x^0.55)+0,add=T)
media_pot_adapt <- nls(media_adapt~I(a*I(dist_adapt^b)+c),start=list(a=0.0009,b=0.55,c=0))

funcao_pot_mle_normal <- function(a,b,c,sd){
  mean <- (a*(dist_adapt^b))+c
  -sum(dnorm(media_adapt, mean=mean, sd=sd, log=TRUE))
}
start<-list(a=0.0009,b=0.55,c=0,sd=sd(media_adapt))
media_pot_adapt_mle_normal<-mle2(funcao_pot_mle_normal, start=start,method = "Nelder-Mead",control=list(maxit=10000))

funcao_pot_mle_gama <- function(a,b,c,shape){
  scale <- exp((a*(dist_adapt^b))+c)/shape
  -sum(dgamma(media_adapt, scale=scale, shape=shape, log=TRUE))
}
start<-list(a=0.0009,b=0.55,c=0,shape=mean(media_adapt)^2/var(media_adapt))
media_pot_adapt_mle_gama <- mle2(funcao_pot_mle_gama, start=start,method = "Nelder-Mead",control=list(maxit=10000))

par(bty="l")
plot(media_adapt~dist_adapt,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio")
curve((coef(media_pot_adapt)[[1]]*(x^coef(media_pot_adapt)[[2]])+coef(media_pot_adapt)[[3]]),add=T,col="black")
curve((coef(media_pot_adapt_mle_normal)[[1]]*(x^coef(media_pot_adapt_mle_normal)[[2]])+coef(media_pot_adapt_mle_normal)[[3]]),add=T,col="red")

curve(exp((coef(media_pot_adapt_mle_gama)[[1]]*(x^coef(media_pot_adapt_mle_gama)[[2]])+coef(media_pot_adapt_mle_gama)[[3]])),add=T,col="blue") #acrescentei uma exponencial

AICtab(media_pot_adapt_mle_gama,media_pot_adapt_mle_normal,media_pot_adapt)

### logistica
media_coef_logit_linearizada_adapt <- coef(lm(log(media_adapt/(1-media_adapt))~dist_adapt))
media_logit_adapt <- nls(media_adapt~(1/(1+exp(-a*dist_adapt-b)))+c,start=list(a=media_coef_logit_linearizada_adapt[[2]],b=media_coef_logit_linearizada_adapt[[1]],c=0))
#curve((0.921/(1+exp(-(0.000035*(x-90000))))),add=T,col="red")
media_logit_adapt_2<- nls(media_adapt~(a/(1+exp(-(b*(dist_adapt-c))))+d),start=list(a=0.921,b=0.000035,c=90000,d=0))

funcao_logit_2_mle_normal <- function(a,b,c,d,sd){
  mean <- a/(1+exp(-(b*(dist_adapt-c))))+d
  -sum(dnorm(media_adapt, mean=mean, sd=sd, log=TRUE))
}
start=list(a=0.921,b=0.000035,c=90000,d=0,sd=sd(media_adapt))
media_logit_adapt_2_mle_normal <- mle2(funcao_logit_2_mle_normal, start=start)

funcao_logit_2_mle_gama <- function(a,b,c,d,shape){
  scale <- (a/(1+exp(-(b*(dist_adapt-c))))+d)/shape
  -sum(dgamma(media_adapt, scale=scale, shape=shape, log=TRUE))
}
start=list(a=0.921,b=0.000035,c=90000,d=0,shape=mean(media_adapt)^2/var(media_adapt))
media_logit_2_adapt_mle_gama <- mle2(funcao_logit_2_mle_gama, start=start)

AICtab(media_logit_2_adapt_mle_gama,media_logit_adapt_2_mle_normal,media_logit_adapt,media_logit_adapt_2)

par(bty="l")
plot(media_adapt~dist_adapt,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio")
curve((coef(media_logit_adapt_2)[[1]]/(1+exp(-(coef(media_logit_adapt_2)[[2]]*(x-coef(media_logit_adapt_2)[[3]])))))+coef(media_logit_adapt_2)[[4]],add=T,col="black")
curve((coef(media_logit_2_adapt_mle_gama)[[1]]/(1+exp(-(coef(media_logit_2_adapt_mle_gama)[[2]]*(x-coef(media_logit_2_adapt_mle_gama)[[3]])))))+coef(media_logit_2_adapt_mle_gama)[[4]],add=T,col="red")
curve((coef(media_logit_adapt_2_mle_normal)[[1]]/(1+exp(-(coef(media_logit_adapt_2_mle_normal)[[2]]*(x-coef(media_logit_adapt_2_mle_normal)[[3]])))))+coef(media_logit_adapt_2_mle_normal)[[4]],add=T,col="blue")
#media_logit_adapt_2_DAHORA <- nls(media_adapt~dgamma(x=media_adapt,scale=(a/(1+exp(-(b*(dist_adapt-c))))+d)/shape,shape=shape),start=list(a=0.921,b=0.000035,c=90000,d=0,shape=1))
#media_logit_adapt_2_DAHORA <- nls(media_adapt~dnorm(x=media_adapt,mean=(a/(1+exp(-(b*(dist_adapt-c))))+d),sd=sd),start=list(a=0.921,b=0.000035,c=90000,d=0,sd=sd(media_adapt)))

### exponencial
media_coef_exp_linearizada_adapt <- coef(lm(log(media_adapt)~dist_adapt))
media_exp_adapt <- nls(media_adapt~(exp(a*dist_adapt+b)),start=list(a=media_coef_exp_linearizada_adapt[[2]],b=media_coef_exp_linearizada_adapt[[1]]))
#### selecao
#AICtab(media_nulo_adapt,media_linear_adapt,media_mm_adapt,media_pot_adapt,media_logit_adapt,media_logit_adapt_2,media_exp_adapt)
AICtab(media_nulo_adapt,media_linear_adapt,media_mm_adapt,media_pot_adapt,media_logit_adapt_2,media_exp_adapt)
#### plotando
par(bty="l")
plot(media_adapt~dist_adapt,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio")
curve((coef(media_logit_adapt_2)[[1]]/(1+exp(-(coef(media_logit_adapt_2)[[2]]*(x-coef(media_logit_adapt_2)[[3]])))))+coef(media_logit_adapt_2)[[4]],add=T,col="black")
curve(((coef(media_mm_adapt)[[1]]*x)/(coef(media_mm_adapt)[[2]]+x)+coef(media_mm_adapt)[[3]]),add=T,col="blue")
curve((coef(media_pot_adapt)[[1]]*(x^coef(media_pot_adapt)[[2]])+coef(media_pot_adapt)[[3]]),add=T,col="red")
abline(media_linear_adapt)
curve(exp(coef(media_exp_adapt)[[1]]*x+coef(media_exp_adapt)[[2]]),add=T,col="violet")
abline(media_nulo_adapt,col="gray")


# CENARIO EXCLUSAO
media_excl <- dados$media[dados$bateria==2]/20000.0001
dist_excl <- dados$dist_indice[dados$bateria==2]
## modelos
### nulo
media_nulo_excl<-lm(media_excl~1)
### linear
media_linear_excl<-lm(media_excl~dist_excl)
### mm
coef_mm_linearizada_excl <- coef(lm(I(dist_excl/media_excl)~dist_excl))
media_mm_excl <- nls(media_excl~I(I(a*dist_excl)/I(b+dist_excl)+c),start=list(a=1/coef_mm_linearizada_excl[[2]],b=coef_mm_linearizada_excl[[1]]*(1/coef_mm_linearizada_excl[[2]]),c=0))
### potencia
plot(media_excl~dist_excl)
curve(0.36*(x^0.018)+0.55,add=T,col="red")
media_pot_excl <- nls(media_excl~I(a*I(dist_excl^b)+c),start=list(a=0.36,b=0.018,c=0.55)) # erro
##
require(nlmrt)
my_data<-data.frame(my_x=dist_excl,my_y=media_excl)
teste <- nlxb(my_y~(a*(my_x^b)+c),start=list(a=0.36,b=0.018,c=0.55),data=my_data) # nao rolou
##
#media_pot_excl <- nls(media_excl~(a*(dist_excl^b)+c),start=list(a=teste$coefficients[[1]],b=teste$coefficients[[2]],c=teste$coefficients[[3]])) # nao deu certo saporra entao vou usar o nls 2 com forca bruta
require(nls2)
media_pot_excl_fb <- nls2(media_excl~(a*(dist_excl^b)+c),start=list(a=data.frame(a=c(0.36)),b=data.frame(b=c(0.018)),c=data.frame(c=c(0.55))),algorithm = "brute-force") #porra, meio palha
## vamo de mle
funcao_pot_mle <- function(a,b,c,sd){
  mean <- (a*(dist_excl^b))+c
  -sum(dnorm(media_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=0.36,b=0.018,c=0.55,sd=sd(media_excl))
media_pot_excl_mle <- mle2(funcao_pot_mle, start=start)
media_pot_excl_mle2<-mle2(funcao_pot_mle, start=start,method = "Nelder-Mead",control=list(maxit=10000))

### logistica
coef_logit_linearizada_excl <- coef(lm(log(media_excl/(1-media_excl))~dist_excl))
media_logit_excl <- nls(media_excl~(1/(1+exp(-a*dist_excl-b)))+c,start=list(a=coef_logit_linearizada_excl[[2]],b=coef_logit_linearizada_excl[[1]],c=0))
curve((1/(1+exp(-(0.0005*(x-60))))),add=T,col="red")
media_logit_excl_2_fb<- nls2(media_excl~((a/(1+exp(-(b*(dist_excl-c)))))+d),start=list(a=1,b=0.0005,c=60,d=0),algorithm = "brute-force")
media_logit_excl_2<- nls(media_excl~((a/(1+exp(-(b*(dist_excl-c)))))+d),start=list(a=coef(media_logit_excl_2_fb)[[1]],b=coef(media_logit_excl_2_fb)[[2]],c=coef(media_logit_excl_2_fb)[[3]],d=coef(media_logit_excl_2_fb)[[4]]))
## vamo de mle
funcao_logit_mle <- function(a,b,c,d,sd){
  mean <- (a/(1+exp(-(b*(dist_excl-c))))+d)
  -sum(dnorm(media_excl, mean=mean, sd=sd, log=TRUE))
}
start <- list(a=1,b=0.0005,c=60,d=0,sd=sd(media_excl))
media_logit_excl_2_mle <- mle2(funcao_logit_mle, start=start)


### exponencial
coef_exp_linearizada_excl <- coef(lm(log(media_excl)~dist_excl))
media_exp_excl <- nls(media_excl~(exp(a*dist_excl+b)),start=list(a=coef_exp_linearizada_excl[[2]],b=coef_exp_linearizada_excl[[1]]))
#### selecao
#AICtab(media_nulo_excl,media_linear_excl,media_mm_excl,media_pot_excl_mle,media_pot_excl_mle2,media_pot_excl_fb,media_logit_excl,media_logit_excl_2_mle,media_logit_excl_2_fb,media_exp_excl)
AICtab(media_nulo_excl,media_linear_excl,media_mm_excl,media_pot_excl_mle2,media_logit_excl_2_mle,media_exp_excl)
#### plotando
plot(media_excl~dist_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio")
curve((((coef(media_mm_excl)[[1]]*x)/(coef(media_mm_excl)[[2]]+x))+coef(media_mm_excl)[[3]]),add=T,col="black")
curve((coef(media_logit_excl_2)[[1]]/(1+exp(-(coef(media_logit_excl_2)[[2]]*(x-coef(media_logit_excl_2)[[3]])))))+coef(media_logit_excl_2)[[4]],add=T,col="green4")
curve((coef(media_pot_excl_mle2)[[1]]*(x^coef(media_pot_excl_mle2)[[2]])+coef(media_pot_excl_mle2)[[3]]),add=T,col="red")
abline(media_linear_excl)
curve(exp(coef(media_exp_excl)[[1]]*x+coef(media_exp_excl)[[2]]),add=T,col="violet")
abline(media_nulo_excl,col="gray")

### saber: funcao exponencial eh bom pra variancia: curve(((-0.9999)^x)+0,add=T,col="green").. porem, como linearizar pra achar os start values?
#### um jeito possivel eh fixar a base em e, em vez d colocar a base como um coeficiente. assim, quando tiramos o log disso, lineariza: curve(exp(-0.000005*x),add=T,col="violet")

# CENARIO ADAPTACAO+EXCLUSAO
media_adapt_excl <- dados$media[dados$bateria==3]/20000.0001
dist_adapt_excl <- dados$dist_indice[dados$bateria==3]
## modelos
### nulo
media_nulo_adapt_excl<-lm(media_adapt_excl~1)
### linear
media_linear_adapt_excl<-lm(media_adapt_excl~dist_adapt_excl)
### mm
coef_mm_linearizada_adapt_excl <- coef(lm(I(dist_adapt_excl/media_adapt_excl)~dist_adapt_excl))
media_mm_adapt_excl <- nls(media_adapt_excl~I(I(a*dist_adapt_excl)/I(b+dist_adapt_excl)+c),start=list(a=1/coef_mm_linearizada_adapt_excl[[2]],b=coef_mm_linearizada_adapt_excl[[1]]*(1/coef_mm_linearizada_adapt_excl[[2]]),c=0))
### potencia
plot(media_adapt_excl~dist_adapt_excl)
curve(0.0009*(x^0.55)+0,add=T,col="red")
media_pot_adapt_excl <- nls(media_adapt_excl~I(a*I(dist_adapt_excl^b)+c),start=list(a=0.0009,b=0.55,c=0)) 
### logistica
coef_logit_linearizada_adapt_excl <- coef(lm(log(media_adapt_excl/(1-media_adapt_excl))~dist_adapt_excl))
media_logit_adapt_excl <- nls(media_adapt_excl~(1/(1+exp(-a*dist_adapt_excl-b)))+c,start=list(a=coef_logit_linearizada_adapt_excl[[2]],b=coef_logit_linearizada_adapt_excl[[1]],c=0))
curve((0.921/(1+exp(-(0.00004*(x-80000))))),add=T,col="red")
media_logit_adapt_excl_2 <- nls(media_adapt_excl~(a/(1+exp(-(b*(dist_adapt_excl-c))))+d),start=list(a=0.921,b=0.00004,c=80000,d=0))
### exponencial
coef_exp_linearizada_adapt_excl <- coef(lm(log(media_adapt_excl)~dist_adapt_excl))
media_exp_adapt_excl <- nls(media_adapt_excl~(exp(a*dist_adapt_excl+b)),start=list(a=coef_exp_linearizada_adapt_excl[[2]],b=coef_exp_linearizada_adapt_excl[[1]]))
#### selecao
#AICtab(media_nulo_adapt_excl,media_linear_adapt_excl,media_mm_adapt_excl,media_pot_adapt_excl,media_logit_adapt_excl,media_logit_adapt_excl_2,media_exp_adapt_excl)
AICtab(media_nulo_adapt_excl,media_linear_adapt_excl,media_mm_adapt_excl,media_pot_adapt_excl,media_logit_adapt_excl_2,media_exp_adapt_excl)
#### plotando
plot(media_adapt_excl~dist_adapt_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1),las=1,ylab="Média do índice de estratégia de vida",xlab="Índice de distúrbio")
curve((coef(media_logit_adapt_excl_2)[[1]]/(1+exp(-(coef(media_logit_adapt_excl_2)[[2]]*(x-coef(media_logit_adapt_excl_2)[[3]])))))+coef(media_logit_adapt_excl_2)[[4]],add=T,col="black")
curve((((coef(media_mm_adapt_excl)[[1]]*x)/(coef(media_mm_adapt_excl)[[2]]+x))+coef(media_mm_adapt_excl)[[3]]),add=T,col="blue")
curve((coef(media_pot_adapt_excl)[[1]]*(x^coef(media_pot_adapt_excl)[[2]])+coef(media_pot_adapt_excl)[[3]]),add=T,col="red")
abline(media_linear_adapt_excl)
curve(exp(coef(media_exp_adapt_excl)[[1]]*x+coef(media_exp_adapt_excl)[[2]]),add=T,col="violet")
abline(media_nulo_adapt_excl,col="gray")
