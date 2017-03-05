# Analises exploratorias das simulacoes de 25jul16

simulacoes_output_derivado<-simulacoes_25jul16_output_derivado

#### Scatterplots
## Perda de especies
plot(dados3_25jul16[,2],-simulacoes_output_derivado[,3],pch=20,xlab="Número de eventos de distúrbio",ylab="Perda de espécies")
plot(dados3_25jul16[,2],lm(log(-simulacoes_output_derivado[,3])~dados3_25jul16[,1])$residuals,pch=20,xlab="Número de eventos de distúrbio",ylab="Resíduos da perda de espécies x S")
modelo_res_perda_distpos <- lm(lm(log(-simulacoes_output_derivado[,3])~dados3_25jul16[,1])$residuals~dados3_25jul16[,2])
abline(modelo_res_perda_distpos)

plot(dados3_25jul16[,3],-simulacoes_output_derivado[,3],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Perda de espécies")
plot(dados3_25jul16[,3],lm(log(-simulacoes_output_derivado[,3])~dados3_25jul16[,1])$residuals,pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos da perda de espécies x S")
modelo_res_perda_distint <- lm(lm(log(-simulacoes_output_derivado[,3])~dados3_25jul16[,1])$residuals~dados3_25jul16[,3])
abline(modelo_res_perda_distint)
## Media
plot(dados3_25jul16[,2],simulacoes_output_derivado[,5],pch=20,xlab="Número de eventos de distúrbio",ylab="Média")
plot(dados3_25jul16[,2],lm(log(simulacoes_output_derivado[,5])~dados3_25jul16[,1])$residuals,pch=20,xlab="Número de eventos de distúrbio",ylab="Resíduos da média x S")
modelo_res_media_distpos <- lm(lm(log(simulacoes_output_derivado[,5])~dados3_25jul16[,1])$residuals~dados3_25jul16[,2])
abline(modelo_res_media_distpos)

plot(dados3_25jul16[,3],simulacoes_output_derivado[,5],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Média")
plot(dados3_25jul16[,3],lm(log(simulacoes_output_derivado[,5])~dados3_25jul16[,1])$residuals,pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos da média x S")
modelo_res_media_distint <- lm(lm(log(simulacoes_output_derivado[,5])~dados3_25jul16[,1])$residuals~dados3_25jul16[,3])
abline(modelo_res_media_distint)
# ## Diferenca absoluta
# plot(dados3_25jul16[,2],simulacoes_output_derivado[,6],pch=20,xlab="Número de eventos de distúrbio",ylab="Diferença absoluta entre a média do número de propágulos final e o inicial")
# plot(dados3_25jul16[,3],simulacoes_output_derivado[,6],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Diferença absoluta entre a média do número de propágulos final e o inicial")
# ## Diferenca relativa
# plot(dados3_25jul16[,2],simulacoes_output_derivado[,7],pch=20,xlab="Número de eventos de distúrbio",ylab="Diferença relativa entre a média do número de propágulos final e o inicial")
# plot(dados3_25jul16[,3],simulacoes_output_derivado[,7],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Diferença relativa entre a média do número de propágulos final e o inicial")
# ## Mediana
# plot(dados3_25jul16[,2],simulacoes_output_derivado[,8],pch=20,xlab="Número de eventos de distúrbio",ylab="Mediana")
# plot(dados3_25jul16[,3],simulacoes_output_derivado[,8],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Mediana")
# ## Moda
# plot(dados3_25jul16[,2],simulacoes_output_derivado[,9],pch=20,xlab="Número de eventos de distúrbio",ylab="Moda")
# plot(dados3_25jul16[,3],simulacoes_output_derivado[,9],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Moda")
## Variância
## atencao: apenas as variancias maiores que 0 foram incluidas
plot(dados3_25jul16[,2],simulacoes_output_derivado[,10],pch=20,xlab="Número de eventos de distúrbio",ylab="Variância")
plot(dados3_25jul16[,2][which(simulacoes_output_derivado[,10]!=0)],lm(log(simulacoes_output_derivado[c(which(simulacoes_output_derivado[,10]!=0)),10])~dados3_25jul16[,1][which(simulacoes_output_derivado[,10]!=0)])$residuals,pch=20,xlab="Número de eventos de distúrbio",ylab="Resíduos da variância x S")
modelo_res_var_distpos <- lm(lm(log(simulacoes_output_derivado[c(which(simulacoes_output_derivado[,10]!=0)),10])~dados3_25jul16[,1][which(simulacoes_output_derivado[,10]!=0)])$residuals~dados3_25jul16[,2][which(simulacoes_output_derivado[,10]!=0)])
abline(modelo_res_var_distpos)
### retirando valores que apresentam menos ou igual a 100 mil eventos de distúrbio
plot(dados3_25jul16[which(dados3_25jul16[,2]>100000 & simulacoes_output_derivado[,10]!=0),2],lm(log(simulacoes_output_derivado[c(which(simulacoes_output_derivado[,10]!=0 & dados3_25jul16[,2]>100000)),10])~dados3_25jul16[which(dados3_25jul16[,2]>100000 & simulacoes_output_derivado[,10]!=0),1])$residuals,pch=20,xlab="Número de eventos de distúrbio",ylab="Resíduos da variância x S")
modelo_res_var_distpos_maior <- lm(lm(log(simulacoes_output_derivado[c(which(simulacoes_output_derivado[,10]!=0 & dados3_25jul16[,2]>100000)),10])~dados3_25jul16[which(dados3_25jul16[,2]>100000 & simulacoes_output_derivado[,10]!=0),1])$residuals~dados3_25jul16[which(dados3_25jul16[,2]>100000 & simulacoes_output_derivado[,10]!=0),2])
abline(modelo_res_var_distpos_maior)
modelo_nulo<-lm(lm(log(simulacoes_output_derivado[c(which(simulacoes_output_derivado[,10]!=0 & dados3_25jul16[,2]>100000)),10])~dados3_25jul16[which(dados3_25jul16[,2]>100000 & simulacoes_output_derivado[,10]!=0),1])$residuals~1)
abline(modelo_nulo)

plot(dados3_25jul16[,3],simulacoes_output_derivado[,10],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Variância")
plot(dados3_25jul16[,3][which(simulacoes_output_derivado[,10]!=0)],lm(log(simulacoes_output_derivado[c(which(simulacoes_output_derivado[,10]!=0)),10])~dados3_25jul16[,1][which(simulacoes_output_derivado[,10]!=0)])$residuals,pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos da variância x S")
modelo_res_var_distint <- lm(lm(log(simulacoes_output_derivado[c(which(simulacoes_output_derivado[,10]!=0)),10])~dados3_25jul16[,1][which(simulacoes_output_derivado[,10]!=0)])$residuals~dados3_25jul16[,3][which(simulacoes_output_derivado[,10]!=0)])
abline(modelo_res_var_distint)
### retirando simulações que apresentam intensidade de distúrbio menor ou igual a 0.2
plot(dados3_25jul16[which(dados3_25jul16[,3]>0.2 & simulacoes_output_derivado[,10]!=0),3],lm(log(simulacoes_output_derivado[c(which(simulacoes_output_derivado[,10]!=0 & dados3_25jul16[,3]>0.2)),10])~dados3_25jul16[which(dados3_25jul16[,3]>0.2 & simulacoes_output_derivado[,10]!=0),1])$residuals,pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos da variância x S")
modelo_res_var_distint_maior <- lm(lm(log(simulacoes_output_derivado[c(which(simulacoes_output_derivado[,10]!=0 & dados3_25jul16[,3]>0.2)),10])~dados3_25jul16[which(dados3_25jul16[,3]>0.2 & simulacoes_output_derivado[,10]!=0),1])$residuals~dados3_25jul16[which(dados3_25jul16[,3]>0.2 & simulacoes_output_derivado[,10]!=0),3])
abline(modelo_res_var_distint_maior)
modelo_nulo2<-lm(lm(log(simulacoes_output_derivado[c(which(simulacoes_output_derivado[,10]!=0 & dados3_25jul16[,3]>0.2)),10])~dados3_25jul16[which(dados3_25jul16[,3]>0.2 & simulacoes_output_derivado[,10]!=0),1])$residuals~1)
abline(modelo_nulo2)

# ## Coeficiente de variação
# plot(dados3_25jul16[,2],simulacoes_output_derivado[,11],pch=20,xlab="Número de eventos de distúrbio",ylab="Coeficiente de variação")
# plot(dados3_25jul16[,3],simulacoes_output_derivado[,11],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Coeficiente de variação")
# ## Assimetria
# plot(dados3_25jul16[,2],simulacoes_output_derivado[,12],pch=20,xlab="Número de eventos de distúrbio",ylab="Assimetria")
# plot(dados3_25jul16[,3],simulacoes_output_derivado[,12],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Assimetria")
# ## Excesso de curtose
# plot(dados3_25jul16[,2],simulacoes_output_derivado[,13],pch=20,xlab="Número de eventos de distúrbio",ylab="Excesso de curtose")
# plot(dados3_25jul16[,3],simulacoes_output_derivado[,13],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Excesso de curtose")
# ## Coeficiente de Pearson
# plot(dados3_25jul16[,2],simulacoes_output_derivado[,14],pch=20,xlab="Número de eventos de distúrbio",ylab="Coeficiente de Pearson")
# plot(dados3_25jul16[,3],simulacoes_output_derivado[,14],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Coeficiente de Pearson")
## Média por espécie
plot(dados3_25jul16[,2],simulacoes_output_derivado[,15],pch=20,xlab="Número de eventos de distúrbio",ylab="Média por espécie")
plot(dados3_25jul16[,2],lm(log(simulacoes_output_derivado[,15])~dados3_25jul16[,1])$residuals,pch=20,xlab="Número de eventos de distúrbio",ylab="Resíduos da média por espécie x S")
modelo_res_med_sp_distpos <- lm(lm(log(simulacoes_output_derivado[,15])~dados3_25jul16[,1])$residuals~dados3_25jul16[,2])
abline(modelo_res_med_sp_distpos)

plot(dados3_25jul16[,3],simulacoes_output_derivado[,15],pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Média por espécie")
plot(dados3_25jul16[,3],lm(log(simulacoes_output_derivado[,15])~dados3_25jul16[,1])$residuals,pch=20,xlab="Intensidade dos eventos de distúrbio",ylab="Resíduos da média por espécie x S")
modelo_res_med_sp_distint <- lm(lm(log(simulacoes_output_derivado[,15])~dados3_25jul16[,1])$residuals~dados3_25jul16[,3])
abline(modelo_res_med_sp_distint)

#### ECDF
plot(ecdf(simulacoes_output_derivado[,3]),xlab="Perda de espécies",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,3])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,3])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,3])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,5]),xlab="Média",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,5])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,5])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,5])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,6]),xlab="Diferença absoluta entre a média e o valor inicial",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,6])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,6])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,6])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,7]),xlab="Diferença relativa entre a média e o valor inicial",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,7])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,7])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,7])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,8]),xlab="Mediana",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,8])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,8])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,8])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,9]),xlab="Moda",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,9])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,9])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,9])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,10]),xlab="Variância",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,10])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,10])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,10])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,11]),xlab="Coeficiente de variação",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,11])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,11])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,11])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,12]),xlab="Assimetria",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,12])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,12])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,12])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,13]),xlab="Excesso de curtose",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,13])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,13])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,13])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,14]),xlab="Coeficiente de Pearson",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,14])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,14])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,14])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[simulacoes_output_derivado[,2]>1,15]),xlab="Média das Médias/Sp",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[simulacoes_output_derivado[,2]>1,15])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[simulacoes_output_derivado[,2]>1,15])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[simulacoes_output_derivado[,2]>1,15])[c(2,5)],lwd=0.9,lty=2)
## foram utilizadas apenas as simulacoes cuja riqueza final eh maior que 1

#### PRCC
require(sensitivity)
require(ggplot2)

# Perda de especies
simulacoes_output_derivado_perda<-as.vector(simulacoes_output_derivado[,3])
pcc_perda<-pcc(dados3_25jul16,simulacoes_output_derivado_perda,nboot=50,rank=T)
par(mar=c(6,6.5,4,2))
plot(pcc_perda$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("gray18",0.7),cex.lab=0.9,font.lab=2,main="Perda de espécies",cex.main=1,col.main="gray18")
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_perda$PRCC[,4],y1=pcc_perda$PRCC[,1],col=alpha("gray18",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_perda$PRCC[,5],y1=pcc_perda$PRCC[,1],col=alpha("gray18",0.7))

# Numero final de especies
simulacoes_output_derivado_riqfin<-as.vector(simulacoes_output_derivado[,2])
pcc_riqfin<-pcc(dados3_25jul16,simulacoes_output_derivado_riqfin,nboot=50,rank=T)
par(mar=c(6,6.5,4,2))
plot(pcc_riqfin$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("gray18",0.7),cex.lab=0.9,font.lab=2,main="Riqueza final",cex.main=1,col.main="gray18")
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_riqfin$PRCC[,4],y1=pcc_riqfin$PRCC[,1],col=alpha("gray18",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_riqfin$PRCC[,5],y1=pcc_riqfin$PRCC[,1],col=alpha("gray18",0.7))

# Media
simulacoes_output_derivado_media<-as.vector(simulacoes_output_derivado[,5])
pcc_media<-pcc(dados3_25jul16,simulacoes_output_derivado_media,nboot=50,rank=T)
par(mar=c(6,6.5,4,2))
plot(pcc_media$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("indianred4",0.7),cex.lab=0.9,font.lab=2,main="Média",cex.main=1,col.main="gray18")
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media$PRCC[,4],y1=pcc_media$PRCC[,1],col=alpha("indianred4",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media$PRCC[,5],y1=pcc_media$PRCC[,1],col=alpha("indianred4",0.7))
## Media (sp)
simulacoes_output_derivado_media_sp<-as.vector(simulacoes_output_derivado[simulacoes_output_derivado[,2]>1,15])
pcc_media_sp<-pcc(dados3_25jul16[which(simulacoes_output_derivado[,2]>1,15),],simulacoes_output_derivado_media_sp,nboot=50,rank=T)
points(c(1,2,3),pcc_media_sp$PRCC[,1],pch=17,col=alpha("indianred4",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media_sp$PRCC[,4],y1=pcc_media_sp$PRCC[,1],col=alpha("indianred4",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media_sp$PRCC[,5],y1=pcc_media_sp$PRCC[,1],col=alpha("indianred4",0.7))
## foram utilizadas apenas as simulacoes cuja riqueza final eh maior que 1

# Dif Media
simulacoes_output_derivado_dif_media<-as.vector(simulacoes_output_derivado[,6])
pcc_dif_media<-pcc(dados3_25jul16,simulacoes_output_derivado_dif_media,nboot=50,rank=T)
par(mar=c(6,6.5,4,2))
plot(pcc_dif_media$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("indianred",0.7),cex.lab=0.9,font.lab=2,main="Diferença absoluta entre média final e inicial",cex.main=1,col.main="gray18")
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_dif_media$PRCC[,4],y1=pcc_dif_media$PRCC[,1],col=alpha("indianred",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_dif_media$PRCC[,5],y1=pcc_dif_media$PRCC[,1],col=alpha("indianred",0.7))

# Razao Media
simulacoes_output_derivado_raz_media<-as.vector(simulacoes_output_derivado[,7])
pcc_raz_media<-pcc(dados3_25jul16,simulacoes_output_derivado_raz_media,nboot=50,rank=T)
par(mar=c(6,6.5,4,2))
plot(pcc_raz_media$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("lightcoral",0.7),cex.lab=0.9,font.lab=2,main="Diferença relativa entre média inicial e final",cex.main=1,col.main="gray18")
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_raz_media$PRCC[,4],y1=pcc_raz_media$PRCC[,1],col=alpha("lightcoral",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_raz_media$PRCC[,5],y1=pcc_raz_media$PRCC[,1],col=alpha("lightcoral",0.7))

# Mediana
simulacoes_output_derivado_mediana<-as.vector(simulacoes_output_derivado[,8])
pcc_mediana<-pcc(dados3_25jul16,simulacoes_output_derivado_mediana,nboot=50,rank=T)
plot(pcc_mediana$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("orange",0.7),cex.lab=0.9,font.lab=2,main="Mediana",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_mediana$PRCC[,4],y1=pcc_mediana$PRCC[,1],col=alpha("orange",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_mediana$PRCC[,5],y1=pcc_mediana$PRCC[,1],col=alpha("orange",0.7))

# Moda
simulacoes_output_derivado_moda<-as.vector(simulacoes_output_derivado[,9])
pcc_moda<-pcc(dados3_25jul16,simulacoes_output_derivado_moda,nboot=50,rank=T)
plot(pcc_moda$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("darkgoldenrod2",0.7),cex.lab=0.9,font.lab=2,main="Moda",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_moda$PRCC[,4],y1=pcc_moda$PRCC[,1],col=alpha("darkgoldenrod2",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_moda$PRCC[,5],y1=pcc_moda$PRCC[,1],col=alpha("darkgoldenrod2",0.7))

# Variancia
simulacoes_output_derivado_variancia<-as.vector(simulacoes_output_derivado[simulacoes_output_derivado[,2]>1,10])
pcc_variancia<-pcc(dados3_25jul16[which(simulacoes_output_derivado[,2]>1,10),],simulacoes_output_derivado_variancia,nboot=50,rank=T)
plot(pcc_variancia$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("turquoise4",0.7),cex.lab=0.9,font.lab=2,main="Variância",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_variancia$PRCC[,4],y1=pcc_variancia$PRCC[,1],col=alpha("turquoise4",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_variancia$PRCC[,5],y1=pcc_variancia$PRCC[,1],col=alpha("turquoise4",0.7))
## foram utilizadas apenas as simulacoes cuja riqueza final eh maior que 1

# Coeficiente de Variacao
simulacoes_output_derivado_coef_var<-as.vector(simulacoes_output_derivado[simulacoes_output_derivado[,2]>1,11])
pcc_coef_var<-pcc(dados3_25jul16[which(simulacoes_output_derivado[,2]>1,11),],simulacoes_output_derivado_coef_var,nboot=50,rank=T)
plot(pcc_coef_var$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("turquoise3",0.7),cex.lab=0.9,font.lab=2,main="Coeficiente de variação",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_coef_var$PRCC[,4],y1=pcc_coef_var$PRCC[,1],col=alpha("turquoise3",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_coef_var$PRCC[,5],y1=pcc_coef_var$PRCC[,1],col=alpha("turquoise3",0.7))
## foram utilizadas apenas as simulacoes cuja riqueza final eh maior que 1

# Assimetria
simulacoes_output_derivado_assimetria<-as.vector(simulacoes_output_derivado[,12])
pcc_assimetria<-pcc(dados3_25jul16[which(is.na(simulacoes_output_derivado_assimetria)==F),],na.omit(simulacoes_output_derivado_assimetria),nboot=50,rank=T)
plot(pcc_assimetria$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("orchid3",0.7),cex.lab=0.9,font.lab=2,main="Assimetria",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_assimetria$PRCC[,4],y1=pcc_assimetria$PRCC[,1],col=alpha("orchid3",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_assimetria$PRCC[,5],y1=pcc_assimetria$PRCC[,1],col=alpha("orchid3",0.7))

# Excesso de Curtose
simulacoes_output_derivado_curtose<-as.vector(simulacoes_output_derivado[,13])
pcc_curtose<-pcc(dados3_25jul16[which(is.na(simulacoes_output_derivado_curtose)==F),],na.omit(simulacoes_output_derivado_curtose),nboot=50,rank=T)
plot(pcc_curtose$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("royalblue",0.7),cex.lab=0.9,font.lab=2,main="Excesso de curtose",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_curtose$PRCC[,4],y1=pcc_curtose$PRCC[,1],col=alpha("royalblue",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_curtose$PRCC[,5],y1=pcc_curtose$PRCC[,1],col=alpha("royalblue",0.7))

# Pearson
simulacoes_output_derivado_pearson<-as.vector(simulacoes_output_derivado[,14])
pcc_pearson<-pcc(dados3_25jul16[which(is.na(simulacoes_output_derivado_pearson)==F),],na.omit(simulacoes_output_derivado_pearson),nboot=50,rank=T)
plot(pcc_pearson$PRCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("lightslateblue",0.7),cex.lab=0.9,font.lab=2,main="Coeficiente de Pearson",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,expression(paste("n"^"o","de eventos")),padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"de distúrbios",padj=2.7,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"espécies",padj=2.7,col="gray18",cex=0.9,adj=0.04)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_pearson$PRCC[,4],y1=pcc_pearson$PRCC[,1],col=alpha("lightslateblue",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_pearson$PRCC[,5],y1=pcc_pearson$PRCC[,1],col=alpha("lightslateblue",0.7))

## Para grafico com todos os pcc
# leg.text<-c("média","diferença absoluta entre a média e o valor inicial", "diferença relativa entre a média e o valor inicial", "variância", "coeficiente de variação", "assimetria", "excesso de curtose", "coeficiente de Pearson")
# leg.col<-c(alpha("indianred4",0.7),alpha("indianred",0.7),alpha("lightcoral",0.7),alpha("turquoise4",0.7),alpha("turquoise3",0.7),alpha("orchid3",0.7),alpha("royalblue",0.7),alpha("lightslateblue",0.7))
# legend(locator(),legend=leg.text,col=leg.col,pch=16,bty="n",cex=0.6,text.col="gray18",title=expression(paste("Descritores do ","n"^"o"," de propágulos/ciclo após 2 mil gerações")),title.col="gray18",title.adj=0.1)

## Histogramas

data_teste<-cbind(dados3_25jul16,simulacoes_output_derivado)
distúrbio<-data_teste[,3]>=0.5 & data_teste[,2]>150000
distúrbio[which(distúrbio==T)]<-"alto"
distúrbio[data_teste[,3]<0.5 & data_teste[,2]<=150000]<-"baixo"
distúrbio[which(distúrbio==F)]<-"regular"

# Perda de especies
qplot(Perda_Sp, data=data_teste, geom="density", xlab="Perda de espécies",ylab="Densidade")
qplot(Perda_Sp, data=data_teste, geom="density", xlab="Perda de espécies",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Media
qplot(Media, data=data_teste, geom="density", xlab="Média do número de propágulos/ciclo",ylab="Densidade")
qplot(Media, data=data_teste, geom="density", xlab="Média do número de propágulos/ciclo",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Media das medias/sp
data<-data_teste[data_teste[,5]>1,]
dist<-data[,3]>=0.5 & data[,2]>150000
dist[which(dist==T)]<-"alto"
dist[data[,3]<0.5 & data[,2]<=150000]<-"baixo"
dist[which(dist==F)]<-"regular"
qplot(Media_Sp, data=data, geom="density", xlab="Média das médias do número de propágulos/ciclo/sp",ylab="Densidade")
qplot(Media_Sp, data=data, geom="density", xlab="Média das médias do número de propágulos/ciclo/sp",ylab="Densidade",alpha=I(.5),color=dist)
## foram utilizadas as simulacoes com riqueza final maior que 1

# Dif_Media
qplot(Dif_Media, data=data_teste, geom="density", xlab="Diferença absoluta entre a média e o valor inicial do número de propágulos/ciclo",ylab="Densidade")
qplot(Dif_Media, data=data_teste, geom="density", xlab="Diferença absoluta entre a média e o valor inicial do número de propágulos/ciclo",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Razao_Media
qplot(Raz_Media, data=data_teste, geom="density", xlab="Diferença relativa entre a média e o valor inicial do número de propágulos/ciclo",ylab="Densidade")
qplot(Raz_Media, data=data_teste, geom="density", xlab="Diferença relativa entre a média e o valor inicial do número de propágulos/ciclo",alpha=I(.5),color=distúrbio)

# Mediana
qplot(Mediana, data=data_teste, geom="density", xlab="Mediana do número de propágulos/ciclo",ylab="Densidade")
qplot(Mediana, data=data_teste, geom="density", xlab="Mediana do número de propágulos/ciclo",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Moda
qplot(Moda, data=data_teste, geom="density", xlab="Moda do número de propágulos/ciclo",ylab="Densidade")
qplot(Moda, data=data_teste, geom="density", xlab="Moda do número de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Variancia
qplot(Variancia, data=data_teste, geom="density", xlab="Variância do número de propágulos/ciclo",ylab="Densidade")
qplot(Variancia, data=data_teste, geom="density", xlab="Variância do número de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)
qplot(Variancia, data=data, geom="density", xlab="Variância do número de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=dist,ylim=c(0,5e-04))

# Coeficiente_Variação
qplot(Coeficiente_Variacao, data=data_teste, geom="density", xlab="Coeficiente de variação do número de propágulos/ciclo",ylab="Densidade")
qplot(Coeficiente_Variacao, data=data_teste, geom="density", xlab="Coeficiente de variação do número de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio,ylim=c(0,120))
qplot(Coeficiente_Variacao, data=data, geom="density", xlab="Coeficiente de variação do número de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=dist,ylim=c(0,120))

# Assimetria
qplot(Assimetria, data=data_teste, geom="density", xlab="Assimetria do número de propágulos/ciclo",ylab="Densidade")
qplot(Assimetria, data=data_teste, geom="density", xlab="Assimetria do número de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Excesso de curtose
qplot(Excesso_Curtose, data=data_teste, geom="density", xlab="Excesso de curtose do número de propágulos/ciclo",ylab="Densidade")
qplot(Excesso_Curtose, data=data_teste, geom="density", xlab="Excesso de curtose do número de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Coeficiente de Pearson
qplot(Coeficiente_Pearson, data=data_teste, geom="density", xlab="Coeficiente de Pearson do número de propágulos/ciclo",ylab="Densidade")
qplot(Coeficiente_Pearson, data=data_teste, geom="density", xlab="Coeficiente de Pearson do número de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

## Histogramas de cada simulacao do tempo nger5000

### Para 25jul16
# Transformar matrix em data frame com apenas duas colunas: uma com o numero de propagulos e outra com a simulacao a que pertencem os individuos
teste<-as.data.frame(prop_nger_geral)
testinho<-matrix(ncol=2,nrow=(5500*1000))
for(i in 1:1000){
  testinho[((5500*(i-1))+1):(5500*(i)),1]<-teste[1:5500,i]
  testinho[((5500*(i-1))+1):(5500*(i)),2]<-rep(i,5500)
}
testinho<-as.data.frame(testinho)
colnames(testinho)<-c("prop","sim")
# Acrescentar coluna com a "categoria" do disturbio das simulacoes
dist<-rep(NA,1000)
dist[which(dados3_25jul16[,3]>=0.5 & dados3_25jul16[,2]>150000)]<-"alto"
dist[which(dados3_25jul16[,3]<0.5 & dados3_25jul16[,2]<=150000)]<-"baixo"
dist[which(is.na(dist))]<-"regular"
dist<-rep(dist,each=5500)
testinho$dist<-dist
# Plotar graficos
testinho_b_a<-testinho[testinho[,3]=="baixo"|testinho[,3]=="alto",]
oi_b_a<-ggplot(testinho_b_a,aes(prop,group=sim,colour=factor(dist)))+geom_density(size=0.1) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_b_a
testinho_baixo<-testinho[testinho[,3]=="baixo",]
oi_baixo<-ggplot(testinho_baixo,aes(prop,group=sim))+geom_density(size=0.1)+xlim(0,20000) +ylim(0,0.0015) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_baixo
testinho_alto<-testinho[testinho[,3]=="alto",]
oi_alto<-ggplot(testinho_alto,aes(prop,group=sim))+geom_density(size=0.1) +xlim(0,20000) +ylim(0,0.0015) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi_alto
oi<-ggplot(testinho,aes(prop,group=sim,colour=factor(dist)))+geom_density(size=0.1) +ylim(0,0.1) +xlab("Propágulos/ciclo após 5000 geraçãoes") +ylab("Densidade")
oi