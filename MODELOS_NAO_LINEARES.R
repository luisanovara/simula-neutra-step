# MODELOS REAL OFICIAL nao linear
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

######################### MEDIA #########################

# CENARIO ADAPTACAO
media_adapt <- dados$media[dados$bateria==1]/20000
dist_adapt <- dados$dist_indice[dados$bateria==1]
## modelos
### nulo
media_nulo<-lm(media_adapt~1)
### linear
media_linear<-lm(media_adapt~dist_adapt)
### mm
coef_mm_linearizada <- coef(lm(I(dist_adapt/media_adapt)~dist_adapt))
media_mm <- nls(media_adapt~I(I(a*dist_adapt)/I(b+dist_adapt)+c),start=list(a=1/coef_mm_linearizada[[2]],b=coef_mm_linearizada[[1]]*(1/coef_mm_linearizada[[2]]),c=0))
### potencia
plot(media_adapt~dist_adapt)
curve(0.0009*(x^0.55)+0,add=T)
media_pot <- nls(media_adapt~I(a*I(dist_adapt^b)+c),start=list(a=0.0009,b=0.55,c=0))
### logistica
coef_logit_linearizada <- coef(lm(log(media_adapt/(1-media_adapt))~dist_adapt))
media_logit <- nls(media_adapt~(1/(1+exp(-a*dist_adapt-b)))+c,start=list(a=coef_logit_linearizada[[2]],b=coef_logit_linearizada[[1]],c=0))
### exponencial
coef_exp_linearizada <- coef(lm(log(media_adapt)~dist_adapt))
media_exp <- nls(media_adapt~(exp(a*dist_adapt+b)),start=list(a=coef_exp_linearizada[[2]],b=coef_exp_linearizada[[1]]))
#### selecao
AICtab(media_nulo,media_linear,media_mm,media_pot,media_logit,media_exp)
#### plotando
plot(media_adapt~dist_adapt,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1))
curve(1/(1+exp(-coef(media_logit)[[1]]*x-coef(media_logit)[[2]])),add=T,col="green")
curve((coef(media_mm)[[1]]*x)/(coef(media_mm)[[2]]+x),add=T,col="blue")
curve((coef(media_pot)[[1]]*(x^coef(media_pot)[[2]])+coef(media_pot)[[3]]),add=T,col="red")
abline(media_linear)
curve(exp(coef(media_exp)[[1]]*x+coef(media_exp)[[2]]),add=T,col="violet")
abline(media_nulo,col="gray")


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
teste <- nlxb(my_y~(a*(my_x^b)+c),start=list(a=0.36,b=0.018,c=0.55),data=my_data) # deu certo, agora vou usar os valores pro nls
##
media_pot_excl <- nls(media_excl~(a*(dist_excl^b)+c),start=list(a=teste$coefficients[[1]],b=teste$coefficients[[2]],c=teste$coefficients[[3]])) # nao deu certo saporra entao vou usar o nls 2 com forca bruta
require(nls2)
media_pot_excl <- nls2(media_excl~(a*(dist_excl^b)+c),start=list(a=data.frame(a=c(0.36,mean(0.36,teste$coefficients[[1]]),teste$coefficients[[1]])),b=data.frame(b=c(0.018,mean(0.018,teste$coefficients[[2]]),teste$coefficients[[2]])),c=data.frame(c=c(0.55,mean(0.55,teste$coefficients[[3]]),teste$coefficients[[3]]))),algorithm = "brute-force")
### logistica
coef_logit_linearizada_excl <- coef(lm(log(media_excl/(1-media_excl))~dist_excl))
media_logit_excl <- nls(media_excl~(1/(1+exp(-a*dist_excl-b)))+c,start=list(a=coef_logit_linearizada_excl[[2]],b=coef_logit_linearizada_excl[[1]],c=0))
### exponencial
coef_exp_linearizada_excl <- coef(lm(log(media_excl)~dist_excl))
media_exp_excl <- nls(media_excl~(exp(a*dist_excl+b)),start=list(a=coef_exp_linearizada_excl[[2]],b=coef_exp_linearizada_excl[[1]]))
#### selecao
AICtab(media_nulo_excl,media_linear_excl,media_mm_excl,media_pot_excl,media_logit_excl,media_exp_excl)
#### plotando
plot(media_excl~dist_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1))
curve((((coef(media_mm_excl)[[1]]*x)/(coef(media_mm_excl)[[2]]+x))+coef(media_mm_excl)[[3]]),add=T,col="blue")
curve(1/(1+exp(-coef(media_logit_excl)[[1]]*x-coef(media_logit_excl)[[2]])),add=T,col="green")
curve((coef(media_pot_excl)[[1]]*(x^coef(media_pot_excl)[[2]])+coef(media_pot_excl)[[3]]),add=T,col="red")
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
### exponencial
coef_exp_linearizada_adapt_excl <- coef(lm(log(media_adapt_excl)~dist_adapt_excl))
media_exp_adapt_excl <- nls(media_adapt_excl~(exp(a*dist_adapt_excl+b)),start=list(a=coef_exp_linearizada_adapt_excl[[2]],b=coef_exp_linearizada_adapt_excl[[1]]))
#### selecao
AICtab(media_nulo_adapt_excl,media_linear_adapt_excl,media_mm_adapt_excl,media_pot_adapt_excl,media_logit_adapt_excl,media_exp_adapt_excl)
#### plotando
plot(media_adapt_excl~dist_adapt_excl,pch=20,col="gray",bty="l",xlim=c(0,3e5),ylim=c(0,1))
curve(1/(1+exp(-coef(media_logit_adapt_excl)[[1]]*x-coef(media_logit_adapt_excl)[[2]])),add=T,col="green")
curve((((coef(media_mm_adapt_excl)[[1]]*x)/(coef(media_mm_adapt_excl)[[2]]+x))+coef(media_mm_adapt_excl)[[3]]),add=T,col="blue")
curve((coef(media_pot_adapt_excl)[[1]]*(x^coef(media_pot_adapt_excl)[[2]])+coef(media_pot_adapt_excl)[[3]]),add=T,col="red")
abline(media_linear_adapt_excl)
curve(exp(coef(media_exp_adapt_excl)[[1]]*x+coef(media_exp_adapt_excl)[[2]]),add=T,col="violet")
abline(media_nulo_adapt_excl,col="gray")

#################### VARIANCIA TOTAL #################### 