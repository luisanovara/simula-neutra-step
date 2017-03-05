# Analises exploratorias das simulacoes de 21jul16

simulacoes_output_derivado<-simulacoes_21jul16_output_derivado

#### Scatterplots
## Media
plot(dados3_21jul16[,2],simulacoes_output_derivado[,4],pch=20,xlab="Número de eventos de distúrbio",ylab="Média",log="y")
plot(dados3_21jul16[,2],lm(log(simulacoes_output_derivado[,4])~dados3_21jul16[,1])$residuals,pch=20,xlab="Número de eventos de distúrbio",ylab="Resíduos da média x xi0")
plot(dados3_21jul16[,2],lm(log(simulacoes_output_derivado[,4])~dados3_21jul16[,1])$residuals,pch=20,xlab="Número de eventos de distúrbio",ylab="Resíduos da média x xi0",log="xy")
modelo_res_media_distpos <- lm(lm(log(simulacoes_output_derivado[,4])~dados3_21jul16[,1])$residuals~dados3_21jul16[,2])
abline(modelo_res_media_distpos)

plot(dados3_21jul16[,3],simulacoes_output_derivado[,4],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Média")
plot(dados3_21jul16[,3],lm(log(simulacoes_output_derivado[,4])~dados3_21jul16[,1])$residuals,pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos da média x xi0")
modelo_res_media_distint <- lm(lm(log(simulacoes_output_derivado[,4])~dados3_21jul16[,1])$residuals~dados3_21jul16[,3])
abline(modelo_res_media_distint)
# ## Diferenca absoluta
# plot(dados3_21jul16[,2],simulacoes_output_derivado[,5],pch=20,xlab="Número de eventos de distúrbio",ylab="Diferença absoluta entre a média do número de propágulos final e o inicial")
# plot(dados3_21jul16[,3],simulacoes_output_derivado[,5],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Diferença absoluta entre a média do número de propágulos final e o inicial")
# ## Diferenca relativa
# plot(dados3_21jul16[,2],simulacoes_output_derivado[,6],pch=20,xlab="Número de eventos de distúrbio",ylab="Diferença relativa entre a média do número de propágulos final e o inicial")
# plot(dados3_21jul16[,3],simulacoes_output_derivado[,6],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Diferença relativa entre a média do número de propágulos final e o inicial")
# ## Mediana
# plot(dados3_21jul16[,2],simulacoes_output_derivado[,7],pch=20,xlab="Número de eventos de distúrbio",ylab="Mediana")
# plot(dados3_21jul16[,3],simulacoes_output_derivado[,7],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Mediana")
# ## Moda
# plot(dados3_21jul16[,2],simulacoes_output_derivado[,8],pch=20,xlab="Número de eventos de distúrbio",ylab="Moda")
# plot(dados3_21jul16[,3],simulacoes_output_derivado[,8],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Moda")
## Variância
plot(dados3_21jul16[,2],simulacoes_output_derivado[,9],pch=20,xlab="Número de eventos de distúrbio",ylab="Variância")
plot(dados3_21jul16[,2],lm(log(simulacoes_output_derivado[,9])~dados3_21jul16[,1])$residuals,pch=20,xlab="Número de eventos de distúrbio",ylab="Resíduos da variância x xi0")
modelo_res_var_distpos <- lm(lm(log(simulacoes_output_derivado[,9])~dados3_21jul16[,1])$residuals~dados3_21jul16[,2])
abline(modelo_res_var_distpos)
### retirando valores que apresentam menos ou igual a 100 mil eventos de distúrbio
plot(dados3_21jul16[dados3_21jul16[,2]>100000,2],lm(log(simulacoes_output_derivado[dados3_21jul16[,2]>100000,9])~dados3_21jul16[dados3_21jul16[,2]>100000,1])$residuals,pch=20,xlab="Número de eventos de distúrbio",ylab="Resíduos da variância x xi0")
modelo_res_var_distpos_maior <- lm(lm(log(simulacoes_output_derivado[dados3_21jul16[,2]>100000,9])~dados3_21jul16[dados3_21jul16[,2]>100000,1])$residuals~dados3_21jul16[dados3_21jul16[,2]>100000,2])
abline(modelo_res_var_distpos_maior)
modelo_nulo<-lm(lm(log(simulacoes_output_derivado[dados3_21jul16[,2]>100000,9])~dados3_21jul16[dados3_21jul16[,2]>100000,1])$residuals~1)
abline(modelo_nulo)

plot(dados3_21jul16[,3],simulacoes_output_derivado[,9],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Variância")
plot(dados3_21jul16[,3],lm(log(simulacoes_output_derivado[,9])~dados3_21jul16[,1])$residuals,pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos da variância x xi0")
modelo_res_var_distint <- lm(lm(log(simulacoes_output_derivado[,9])~dados3_21jul16[,1])$residuals~dados3_21jul16[,3])
abline(modelo_res_var_distint)
### retirando simulações que apresentam intensidade de distúrbio menor ou igual a 0.2
plot(dados3_21jul16[dados3_21jul16[,3]>0.2,3],lm(log(simulacoes_output_derivado[dados3_21jul16[,3]>0.2,9])~dados3_21jul16[dados3_21jul16[,3]>0.2,1])$residuals,pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos da variância x xi0")
modelo_res_var_distint_maior <- lm(lm(log(simulacoes_output_derivado[dados3_21jul16[,3]>0.2,9])~dados3_21jul16[dados3_21jul16[,3]>0.2,1])$residuals~dados3_21jul16[dados3_21jul16[,3]>0.2,3])
abline(modelo_res_var_distint_maior)
modelo_nulo2<-lm(lm(log(simulacoes_output_derivado[dados3_21jul16[,3]>0.2,9])~dados3_21jul16[dados3_21jul16[,3]>0.2,1])$residuals~1)
abline(modelo_nulo2)
# ## Coeficiente de variação
# plot(dados3_21jul16[,2],simulacoes_output_derivado[,10],pch=20,xlab="Número de eventos de distúrbio",ylab="Coeficiente de variação")
# plot(dados3_21jul16[,2],lm(log(simulacoes_output_derivado[,10])~dados3_21jul16[,1])$residuals,pch=20,xlab="Número de eventos de distúrbio",ylab="Resíduos do coeficiente de variação x xi0")
# modelo_res_coef_var_distpos <- lm(lm(log(simulacoes_output_derivado[,10])~dados3_21jul16[,1])$residuals~dados3_21jul16[,2])
# abline(modelo_res_coef_var_distpos)
# 
# plot(dados3_21jul16[,3],simulacoes_output_derivado[,10],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Coeficiente de variação")
# plot(dados3_21jul16[,3],lm(log(simulacoes_output_derivado[,10])~dados3_21jul16[,1])$residuals,pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos do coeficiente de variação x xi0")
# modelo_res_coef_var_distint <- lm(lm(log(simulacoes_output_derivado[,10])~dados3_21jul16[,1])$residuals~dados3_21jul16[,3])
# abline(modelo_res_coef_var_distint)
# ## Assimetria
# plot(dados3_21jul16[,2],simulacoes_output_derivado[,11],pch=20,xlab="Número de eventos de distúrbio",ylab="Assimetria")
# plot(dados3_21jul16[,2],lm(log(simulacoes_output_derivado[,11])~dados3_21jul16[,1])$residuals,pch=20,xlab="Número de eventos de distúrbio",ylab="Resíduos da assimetria x xi0")
# modelo_res_ass_distpos <- lm(lm(log(simulacoes_output_derivado[,11])~dados3_21jul16[,1])$residuals~dados3_21jul16[,2])
# abline(modelo_res_ass_distpos)
# 
# plot(dados3_21jul16[,3],simulacoes_output_derivado[,11],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Assimetria")
# plot(dados3_21jul16[,3],lm(log(simulacoes_output_derivado[,11])~dados3_21jul16[,1])$residuals,pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos da assimetria x xi0")
# modelo_res_ass_distint <- lm(lm(log(simulacoes_output_derivado[,11])~dados3_21jul16[,1])$residuals~dados3_21jul16[,3])
# abline(modelo_res_ass_distint)
# ## Excesso de curtose
# plot(dados3_21jul16[,2],simulacoes_output_derivado[,12],pch=20,xlab="Número de eventos de distúrbio",ylab="Excesso de curtose")
# plot(dados3_21jul16[,3],simulacoes_output_derivado[,12],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Excesso de curtose")
# ## Coeficiente de Pearson
# plot(dados3_21jul16[,2],simulacoes_output_derivado[,13],pch=20,xlab="Número de eventos de distúrbio",ylab="Coeficiente de Pearson")
# plot(dados3_21jul16[,3],simulacoes_output_derivado[,13],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Coeficiente de Pearson")

#### ECDF
plot(ecdf(simulacoes_output_derivado[,4]),xlab="Média",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,4])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,4])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,4])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,5]),xlab="Diferença absoluta entre a média e o valor inicial",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,5])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,5])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,5])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,6]),xlab="Diferença relativa entre a média e o valor inicial",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,6])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,6])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,6])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,7]),xlab="Mediana",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,7])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,7])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,7])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,8]),xlab="Moda",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,8])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,8])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,8])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,9]),xlab="Variância",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,9])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,9])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,9])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,10]),xlab="Coeficiente de variação",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,10])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,10])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,10])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,11]),xlab="Assimetria",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,11])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,11])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,11])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,12]),xlab="Excesso de curtose",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,12])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,12])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,12])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,13]),xlab="Coeficiente de Pearson",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,13])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,13])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,13])[c(2,5)],lwd=0.9,lty=2)

#### PRCC
require(sensitivity)
require(ggplot2)

# Media
simulacoes_output_derivado_media<-as.vector(simulacoes_output_derivado[,4])
pcc_media<-pcc(dados3_21jul16,simulacoes_output_derivado_media,nboot=50,rank=T)
par(mar=c(6,6.5,4,2))
plot(pcc_media$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("indianred4",0.7),cex.lab=0.9,font.lab=2,main="Média",cex.main=1,col.main="gray18")
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media$PRCC[,4],y1=pcc_media$PRCC[,1],col=alpha("indianred4",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media$PRCC[,5],y1=pcc_media$PRCC[,1],col=alpha("indianred4",0.7))

# Dif_Media_xi0
simulacoes_output_derivado_media_xi0<-as.vector(simulacoes_output_derivado[,5])
pcc_media_xi0<-pcc(dados3_21jul16,simulacoes_output_derivado_media_xi0,nboot=50,rank=T)
plot(pcc_media_xi0$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("indianred",0.7),cex.lab=0.9,font.lab=2,main="Diferença absoluta entre a média e o valor inicial",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media_xi0$PRCC[,4],y1=pcc_media_xi0$PRCC[,1],col=alpha("indianred",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media_xi0$PRCC[,5],y1=pcc_media_xi0$PRCC[,1],col=alpha("indianred",0.7))

# Razao_Media_xi0
simulacoes_output_derivado_raz_media_xi0<-as.vector(simulacoes_output_derivado[,6])
pcc_raz_media_xi0<-pcc(dados3_21jul16,simulacoes_output_derivado_raz_media_xi0,nboot=50,rank=T)
plot(pcc_raz_media_xi0$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("lightcoral",0.7),cex.lab=0.9,font.lab=2,main="Diferença relativa entre a média e o valor inicial",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_raz_media_xi0$PRCC[,4],y1=pcc_raz_media_xi0$PRCC[,1],col=alpha("lightcoral",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_raz_media_xi0$PRCC[,5],y1=pcc_raz_media_xi0$PRCC[,1],col=alpha("lightcoral",0.7))

# Mediana
simulacoes_output_derivado_mediana<-as.vector(simulacoes_output_derivado[,7])
pcc_mediana<-pcc(dados3_21jul16,simulacoes_output_derivado_mediana,nboot=50,rank=T)
plot(pcc_mediana$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("orange",0.7),cex.lab=0.9,font.lab=2,main="Mediana",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_mediana$PRCC[,4],y1=pcc_mediana$PRCC[,1],col=alpha("orange",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_mediana$PRCC[,5],y1=pcc_mediana$PRCC[,1],col=alpha("orange",0.7))

# Moda
simulacoes_output_derivado_moda<-as.vector(simulacoes_output_derivado[,8])
pcc_moda<-pcc(dados3_21jul16,simulacoes_output_derivado_moda,nboot=50,rank=T)
plot(pcc_moda$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("darkgoldenrod2",0.7),cex.lab=0.9,font.lab=2,main="Moda",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_moda$PRCC[,4],y1=pcc_moda$PRCC[,1],col=alpha("darkgoldenrod2",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_moda$PRCC[,5],y1=pcc_moda$PRCC[,1],col=alpha("darkgoldenrod2",0.7))

# Variancia
simulacoes_output_derivado_variancia<-as.vector(simulacoes_output_derivado[,9])
pcc_variancia<-pcc(dados3_21jul16,simulacoes_output_derivado_variancia,nboot=50,rank=T)
plot(pcc_variancia$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("turquoise4",0.7),cex.lab=0.9,font.lab=2,main="Variância",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_variancia$PRCC[,4],y1=pcc_variancia$PRCC[,1],col=alpha("turquoise4",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_variancia$PRCC[,5],y1=pcc_variancia$PRCC[,1],col=alpha("turquoise4",0.7))

# Coeficiente de Variacao
simulacoes_output_derivado_coef_var<-as.vector(simulacoes_output_derivado[,10])
pcc_coef_var<-pcc(dados3_21jul16,simulacoes_output_derivado_coef_var,nboot=50,rank=T)
plot(pcc_coef_var$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("turquoise3",0.7),cex.lab=0.9,font.lab=2,main="Coeficiente de variação",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_coef_var$PRCC[,4],y1=pcc_coef_var$PRCC[,1],col=alpha("turquoise3",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_coef_var$PRCC[,5],y1=pcc_coef_var$PRCC[,1],col=alpha("turquoise3",0.7))

# Assimetria
simulacoes_output_derivado_assimetria<-as.vector(simulacoes_output_derivado[,11])
pcc_assimetria<-pcc(dados3_21jul16,simulacoes_output_derivado_assimetria,nboot=50,rank=T)
plot(pcc_assimetria$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("orchid3",0.7),cex.lab=0.9,font.lab=2,main="Assimetria",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_assimetria$PRCC[,4],y1=pcc_assimetria$PRCC[,1],col=alpha("orchid3",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_assimetria$PRCC[,5],y1=pcc_assimetria$PRCC[,1],col=alpha("orchid3",0.7))

# Excesso de Curtose
simulacoes_output_derivado_curtose<-as.vector(simulacoes_output_derivado[,12])
pcc_curtose<-pcc(dados3_21jul16,simulacoes_output_derivado_curtose,nboot=50,rank=T)
plot(pcc_curtose$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("royalblue",0.7),cex.lab=0.9,font.lab=2,main="Excesso de curtose",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_curtose$PRCC[,4],y1=pcc_curtose$PRCC[,1],col=alpha("royalblue",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_curtose$PRCC[,5],y1=pcc_curtose$PRCC[,1],col=alpha("royalblue",0.7))

# Pearson
simulacoes_output_derivado_pearson<-as.vector(simulacoes_output_derivado[,13])
pcc_pearson<-pcc(dados3_21jul16,simulacoes_output_derivado_pearson,nboot=50,rank=T)
plot(pcc_pearson$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("lightslateblue",0.7),cex.lab=0.9,font.lab=2,main="Coeficiente de Pearson",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_pearson$PRCC[,4],y1=pcc_pearson$PRCC[,1],col=alpha("lightslateblue",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_pearson$PRCC[,5],y1=pcc_pearson$PRCC[,1],col=alpha("lightslateblue",0.7))

## Para grafico com todos os pcc
# leg.text<-c("média","diferença absoluta entre a média e o valor inicial", "diferença relativa entre a média e o valor inicial", "variância", "coeficiente de variação", "assimetria", "excesso de curtose", "coeficiente de Pearson")
# leg.col<-c(alpha("indianred4",0.7),alpha("indianred",0.7),alpha("lightcoral",0.7),alpha("turquoise4",0.7),alpha("turquoise3",0.7),alpha("orchid3",0.7),alpha("royalblue",0.7),alpha("lightslateblue",0.7))
# legend(locator(),legend=leg.text,col=leg.col,pch=16,bty="n",cex=0.6,text.col="gray18",title=expression(paste("Descritores do ","n"^"o"," de propágulos/ciclo após 2 mil gerações")),title.col="gray18",title.adj=0.1)

## Histogramas

data_teste<-cbind(dados3_21jul16,simulacoes_21jul16_output_derivado)
distúrbio<-data_teste[,3]>=0.5 & data_teste[,2]>150000
distúrbio[which(distúrbio==T)]<-"alto"
distúrbio[data_teste[,3]<0.5 & data_teste[,2]<=150000]<-"baixo"
distúrbio[which(distúrbio==F)]<-"regular"

# Media
qplot(Media, data=data_teste, geom="density", xlab="Média de propágulos/ciclo",ylab="Densidade")
qplot(Media, data=data_teste, geom="density", xlab="Média de propágulos/ciclo",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Dif_Media_xi0
qplot(Dif_Media_xi0, data=data_teste, geom="density", xlab="Diferença absoluta entre a média e o valor inicial de propágulos/ciclo",ylab="Densidade")
qplot(Dif_Media_xi0, data=data_teste, geom="density", xlab="Diferença absoluta entre a média e o valor inicial de propágulos/ciclo",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Razao_Media_xi0
qplot(Razao_Media_xi0, data=data_teste, geom="density", xlab="Diferença relativa entre a média e o valor inicial de propágulos/ciclo",ylab="Densidade")
qplot(Razao_Media_xi0, data=data_teste, geom="density", xlab="Diferença relativa entre a média e o valor inicial de propágulos/ciclo",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Mediana
qplot(Mediana, data=data_teste, geom="density", xlab="Mediana de propágulos/ciclo",ylab="Densidade")
qplot(Mediana, data=data_teste, geom="density", xlab="Mediana de propágulos/ciclo",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Moda
qplot(Moda, data=data_teste, geom="density", xlab="Moda de propágulos/ciclo",ylab="Densidade")
qplot(Moda, data=data_teste, geom="density", xlab="Moda de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Variancia
qplot(Variancia, data=data_teste, geom="density", xlab="Variância de propágulos/ciclo",ylab="Densidade")
qplot(Variancia, data=data_teste, geom="density", xlab="Variância de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Coeficiente_Variação
qplot(Coeficiente_Variacao, data=data_teste, geom="density", xlab="Coeficiente de variação de propágulos/ciclo",ylab="Densidade")
qplot(Coeficiente_Variacao, data=data_teste, geom="density", xlab="Coeficiente de variação de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Assimetria
qplot(Assimetria, data=data_teste, geom="density", xlab="Assimetria de propágulos/ciclo",ylab="Densidade")
qplot(Assimetria, data=data_teste, geom="density", xlab="Assimetria de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Excesso de curtose
qplot(Excesso_Curtose, data=data_teste, geom="density", xlab="Excesso de curtose de/ propágulos/ciclo",ylab="Densidade")
qplot(Excesso_Curtose, data=data_teste, geom="density", xlab="Excesso de curtose de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Coeficiente de Pearson
qplot(Coeficiente_Pearson, data=data_teste, geom="density", xlab="Coeficiente de Pearson de propágulos/ciclo",ylab="Densidade")
qplot(Coeficiente_Pearson, data=data_teste, geom="density", xlab="Coeficiente de Pearson de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

## Histogramas de cada simulacao do tempo nger5000

### Para 21jul16
# Transformar matrix em data frame com apenas duas colunas: uma com o numero de propagulos e outra com a simulacao a que pertencem os individuos
prop_nger_geral <- prop_nger_geral_21jul16
teste<-as.data.frame(prop_nger_geral)
testinho<-matrix(ncol=2,nrow=(5000*1000))
for(i in 1:1000){
  testinho[((5000*(i-1))+1):(5000*(i)),1]<-teste[1:5000,i]
  testinho[((5000*(i-1))+1):(5000*(i)),2]<-rep(i,5000)
}
testinho<-as.data.frame(testinho)
colnames(testinho)<-c("prop","sim")
# Acrescentar coluna com a "categoria" do disturbio das simulacoes
dist<-rep(NA,1000)
dist[which(dados3_21jul16[,3]>=0.5 & dados3_21jul16[,2]>150000)]<-"alto"
dist[which(dados3_21jul16[,3]<0.5 & dados3_21jul16[,2]<=150000)]<-"baixo"
dist[which(dados3_21jul16[,3]<0.5 & dados3_21jul16[,2]<=100000)]<-"baixo_var"
dist[which(is.na(dist))]<-"regular"
dist<-rep(dist,each=5000)
testinho$dist<-dist
# Plotar graficos
testinho_b_a<-testinho[testinho[,3]=="baixo"|testinho[,3]=="alto",]
oi_b_a<-ggplot(testinho_b_a,aes(prop,group=sim,colour=factor(dist)))+geom_density(size=0.1) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_b_a
testinho_baixo<-testinho[testinho[,3]=="baixo",]
oi_baixo<-ggplot(testinho_baixo,aes(prop,group=sim))+geom_density(size=0.1)+xlim(0,20000) +ylim(0,0.0030) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_baixo
testinho_alto<-testinho[testinho[,3]=="alto",]
oi_alto<-ggplot(testinho_alto,aes(prop,group=sim))+geom_density(size=0.1) +xlim(0,20000) +ylim(0,0.0030) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_alto
oi<-ggplot(testinho,aes(prop,group=sim,colour=factor(dist)))+geom_density(size=0.1) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi

testinho_baixo_2<-testinho[testinho[,3]=="baixo"|testinho[,3]=="baixo_var",]
oi_baixo2<-ggplot(testinho_baixo_2,aes(prop,group=sim,colour=factor(dist)))+geom_density(size=0.1)+xlim(0,5000) +ylim(0,0.0015) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_baixo2


testinho_baixo_2_alto<-testinho[testinho[,3]=="baixo"|testinho[,3]=="alto",]
oi_baixo2<-ggplot(testinho_baixo_2_alto,aes(prop,group=sim,colour=factor(dist)))+geom_density(size=0.1)+xlim(0,20000) +ylim(0,0.0015) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_baixo2

## Histogramas

data_teste<-cbind(bat1_indice_dist,bat1_indice_media)
distúrbio<-data_teste[,1]>=2e5
distúrbio[which(distúrbio==T)]<-"alto"
distúrbio[data_teste[,1]<1e5]<-"baixo"
distúrbio[which(distúrbio==F)]<-"intermediario"
data_teste <- as.data.frame(data_teste)

# Media
qplot(bat1_residuos_padr, data=data_teste,geom="density", xlab="Índice de estratégia de vida",ylab="Densidade",xlim=c(0,1))
qplot(bat1_indice_media, data=data_teste, geom="density",xlab="Índice de estratégia de vida",ylab="Densidade",alpha=I(.5),color=distúrbio,xlim=c(0,1))

# Dif_Media_xi0
qplot(Dif_Media_xi0, data=data_teste, geom="density", xlab="Diferença absoluta entre a média e o valor inicial de propágulos/ciclo",ylab="Densidade")
qplot(Dif_Media_xi0, data=data_teste, geom="density", xlab="Diferença absoluta entre a média e o valor inicial de propágulos/ciclo",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Razao_Media_xi0
qplot(Razao_Media_xi0, data=data_teste, geom="density", xlab="Diferença relativa entre a média e o valor inicial de propágulos/ciclo",ylab="Densidade")
qplot(Razao_Media_xi0, data=data_teste, geom="density", xlab="Diferença relativa entre a média e o valor inicial de propágulos/ciclo",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Mediana
qplot(Mediana, data=data_teste, geom="density", xlab="Mediana de propágulos/ciclo",ylab="Densidade")
qplot(Mediana, data=data_teste, geom="density", xlab="Mediana de propágulos/ciclo",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Moda
qplot(Moda, data=data_teste, geom="density", xlab="Moda de propágulos/ciclo",ylab="Densidade")
qplot(Moda, data=data_teste, geom="density", xlab="Moda de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Variancia
qplot(Variancia, data=data_teste, geom="density", xlab="Variância de propágulos/ciclo",ylab="Densidade")
qplot(Variancia, data=data_teste, geom="density", xlab="Variância de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Coeficiente_Variação
qplot(Coeficiente_Variacao, data=data_teste, geom="density", xlab="Coeficiente de variação de propágulos/ciclo",ylab="Densidade")
qplot(Coeficiente_Variacao, data=data_teste, geom="density", xlab="Coeficiente de variação de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Assimetria
qplot(Assimetria, data=data_teste, geom="density", xlab="Assimetria de propágulos/ciclo",ylab="Densidade")
qplot(Assimetria, data=data_teste, geom="density", xlab="Assimetria de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Excesso de curtose
qplot(Excesso_Curtose, data=data_teste, geom="density", xlab="Excesso de curtose de/ propágulos/ciclo",ylab="Densidade")
qplot(Excesso_Curtose, data=data_teste, geom="density", xlab="Excesso de curtose de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Coeficiente de Pearson
qplot(Coeficiente_Pearson, data=data_teste, geom="density", xlab="Coeficiente de Pearson de propágulos/ciclo",ylab="Densidade")
qplot(Coeficiente_Pearson, data=data_teste, geom="density", xlab="Coeficiente de Pearson de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

## Histogramas de cada simulacao do tempo nger5000

### Para 21jul16
# Transformar matrix em data frame com apenas duas colunas: uma com o numero de propagulos e outra com a simulacao a que pertencem os individuos
prop_nger_geral <- prop_nger_geral_21jul16
bat1_df_indice_media<-as.data.frame(prop_nger_geral)
bat1_df_final<-matrix(ncol=2,nrow=(5000*1000))
for(i in 1:1000){
  bat1_df_final[((5000*(i-1))+1):(5000*(i)),1]<-bat1_df_indice_media[1:5000,i]
  bat1_df_final[((5000*(i-1))+1):(5000*(i)),2]<-rep(i,5000)
}
bat1_df_final<-as.data.frame(bat1_df_final)
colnames(bat1_df_final)<-c("prop","sim")
# Acrescentar coluna com a "categoria" do disturbio das simulacoes
dist<-rep(NA,1000)
dist[which(dados3_21jul16[,3]>=0.5 & dados3_21jul16[,2]>150000)]<-"alto"
dist[which(dados3_21jul16[,3]<0.5 & dados3_21jul16[,2]<=150000)]<-"baixo"
dist[which(dados3_21jul16[,3]<0.5 & dados3_21jul16[,2]<=100000)]<-"baixo_var"
dist[which(is.na(dist))]<-"regular"
dist<-rep(dist,each=5000)
testinho$dist<-dist
# Plotar graficos
testinho_b_a<-testinho[testinho[,3]=="baixo"|testinho[,3]=="alto",]
oi_b_a<-ggplot(testinho_b_a,aes(prop,group=sim,colour=factor(dist)))+geom_density(size=0.1) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_b_a
testinho_baixo<-testinho[testinho[,3]=="baixo",]
oi_baixo<-ggplot(testinho_baixo,aes(prop,group=sim))+geom_density(size=0.1)+xlim(0,20000) +ylim(0,0.0030) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_baixo
testinho_alto<-testinho[testinho[,3]=="alto",]
oi_alto<-ggplot(testinho_alto,aes(prop,group=sim))+geom_density(size=0.1) +xlim(0,20000) +ylim(0,0.0030) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_alto
oi<-ggplot(testinho,aes(prop,group=sim,colour=factor(dist)))+geom_density(size=0.1) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi

testinho_baixo_2<-testinho[testinho[,3]=="baixo"|testinho[,3]=="baixo_var",]
oi_baixo2<-ggplot(testinho_baixo_2,aes(prop,group=sim,colour=factor(dist)))+geom_density(size=0.1)+xlim(0,5000) +ylim(0,0.0015) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_baixo2


testinho_baixo_2_alto<-testinho[testinho[,3]=="baixo"|testinho[,3]=="alto",]
oi_baixo2<-ggplot(testinho_baixo_2_alto,aes(prop,group=sim,colour=factor(dist)))+geom_density(size=0.1)+xlim(0,20000) +ylim(0,0.0015) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_baixo2

