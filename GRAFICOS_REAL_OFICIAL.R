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





### GRAFICOS INICIAIS
media <- dados$media[-which(dados$bateria==0)]/20000
bateria <- as.factor(c(rep("Adaptação",1000),rep("Exclusão",1000),rep("Exclusão + Adaptação",1000)))
par(bty="l")
plot(media~bateria,bty="l",las=1,ylab="Média do índice de estratégia de vida",xlab="",pch="*")
points(x=c(1,2,3),y=c(mean(media[1:1000]),mean(media[1001:2000]),mean(media[2001:3000])),pch=20)

### GRAFICOS INICIAIS
var_total <- dados$var_total[-which(dados$bateria==0)]/(20000^2)
bateria <- as.factor(c(rep("Adaptação",1000),rep("Exclusão",1000),rep("Exclusão + Adaptação",1000)))
par(bty="l")
plot(var_total~bateria,bty="l",las=1,ylab="Variância total do índice de estratégia de vida",xlab="",pch="*")
plot(var_total~bateria,bty="l",las=1,ylab="Variância total do índice de estratégia de vida",xlab="",pch="*",ylim=c(0,0.001))
points(x=c(1,2,3),y=c(mean(var_total[1:1000]),mean(var_total[1001:2000]),mean(var_total[2001:3000])),pch=20)


### GRAFICOS INICIAIS
var_inter <- dados$var_inter[-which(dados$bateria==0|dados$bateria==1)]/(20000^2)
bateria <- as.factor(c(rep("Exclusão",1000),rep("Exclusão + Adaptação",1000)))
par(bty="l")
plot(var_inter~bateria,bty="l",las=1,ylab="Variância interespecífica do índice de estratégia de vida",xlab="",pch="*")
plot(var_inter~bateria,bty="l",las=1,ylab="Variância interespecífica do índice de estratégia de vida",xlab="",pch="*",ylim=c(0,0.5))
points(x=c(1,2),y=c(mean(var_inter[1:1000]),mean(media[1001:2000])),pch=20)

