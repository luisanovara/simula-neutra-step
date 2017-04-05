# GRAFICOS REAL OFICIAL
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

## MEDIA
plot(dados$media[-c(dados$bateria==0)]~dados$dist_indice_scale[-c(dados$bateria==0)],pch=20,bty="l")
plot(dados$media~dados$dist_indice_scale,pch=20,bty="l")
#plot(dados$media~dados$dist_freq_scale,pch=20,bty="l")
#plot(residuals(lm(dados$media~dados$dist_freq_scale))~dados$dist_int_scale,pch=20,bty="l")
#plot(residuals(lm(dados$media~dados$dist_int_scale))~dados$dist_freq_scale,pch=20,bty="l")
#plot(dados$media~dados$dist_int_scale,pch=20,bty="l")
#plot(I(log(dados$media/(1-dados$media)))~dados$dist_indice_scale,pch=20,bty="l",col=alpha(dados$bateria,0.3))
#plot(dados$media~I(exp(dados$dist_indice_scale)/(1+exp(dados$dist_indice_scale))),pch=20,bty="l",col=alpha(dados$bateria,0.3))
### cenario 0
plot(dados$media[dados$bateria==0]~dados$dist_indice_scale[dados$bateria==0],pch=20,bty="l",ylim=c(0,20000))
### cenario adaptacao
plot(dados$media[dados$bateria==1]~dados$dist_indice_scale[dados$bateria==1],pch=20,bty="l",ylim=c(0,20000))
### cenario exclusao
plot(dados$media[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",ylim=c(1,20000))
### cenario adaptacao + exclusao
plot(dados$media[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l",ylim=c(1,20000))


## media sem dist
plot(dados$media~dados$bateria,pch=20,bty="l")


## VAR TOTAL
plot(dados$var_total~dados$dist_indice_scale,pch=20,bty="l")
### cenario 0
plot(dados$var_total[dados$bateria==0]~dados$dist_indice_scale[dados$bateria==0],pch=20,bty="l")
### cenario adaptacao
plot(dados$var_total[dados$bateria==1]~dados$dist_indice_scale[dados$bateria==1],pch=20,bty="l",ylim=c(0,10e5))
### cenario exclusao
plot(dados$var_total[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",ylim=c(0,10e5))
### cenario adaptacao + exclusao
plot(dados$var_total[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l",ylim=c(0,10e5))





## VAR INTER
plot(dados$var_inter~dados$dist_indice_scale,pch=20,bty="l")
### cenario exclusao
plot(dados$var_inter[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l")
plot(dados$var_inter[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",ylim=c(0,10e7))
### cenario adaptacao + exclusao
plot(dados$var_inter[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l")
