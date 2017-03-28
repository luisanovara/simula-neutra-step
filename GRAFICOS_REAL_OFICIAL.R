# GRAFICOS REAL OFICIAL
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

## MEDIA
plot(dados$media~dados$dist_indice_scale,pch=20,bty="l",col=alpha(dados$bateria,0.3))
plot(I(log(dados$media/(1-dados$media)))~dados$dist_indice_scale,pch=20,bty="l",col=alpha(dados$bateria,0.3))
plot(dados$media~I(exp(dados$dist_indice_scale)/(1+exp(dados$dist_indice_scale))),pch=20,bty="l",col=alpha(dados$bateria,0.3))
### cenario adaptacao
plot(dados$media[dados$bateria==1]~dados$dist_indice_scale[dados$bateria==1],pch=20,bty="l",col=alpha(dados$bateria,0.3),ylim=c(0,20000))
### cenario exclusao
plot(dados$media[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=alpha(dados$bateria,0.3),ylim=c(0,20000))
plot(dados$media[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=alpha(dados$bateria,0.3),ylim=c(19000,20000))
### cenario adaptacao + exclusao
plot(dados$media[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l",col=alpha(dados$bateria,0.3))
## VAR TOTAL
plot(dados$var_total~dados$dist_indice_scale,pch=20,bty="l",col=alpha(dados$bateria,0.3))
### cenario adaptacao
plot(dados$var_total[dados$bateria==1]~dados$dist_indice_scale[dados$bateria==1],pch=20,bty="l",col=alpha(dados$bateria,0.3))
### cenario exclusao
plot(dados$var_total[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=alpha(dados$bateria,0.3))
plot(dados$var_total[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=alpha(dados$bateria,0.3),ylim=c(0,5e5))
### cenario adaptacao + exclusao
plot(dados$var_total[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l",col=alpha(dados$bateria,0.3))
## VAR INTER
plot(dados$var_inter~dados$dist_indice_scale,pch=20,bty="l",col=alpha(dados$bateria,0.3))
### cenario exclusao
plot(dados$var_inter[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=alpha(dados$bateria,0.3))
plot(dados$var_inter[dados$bateria==2]~dados$dist_indice_scale[dados$bateria==2],pch=20,bty="l",col=alpha(dados$bateria,0.3),ylim=c(0,10e7))
### cenario adaptacao + exclusao
plot(dados$var_inter[dados$bateria==3]~dados$dist_indice_scale[dados$bateria==3],pch=20,bty="l",col=alpha(dados$bateria,0.3))
