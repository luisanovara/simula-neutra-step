# Analises exploratorias das simulacoes de 27mai16

#### ECDF
plot(ecdf(simulacoes_output_derivado[,1]),xlab="Média",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,1])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,1])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,1])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,2]),xlab="Diferença absoluta entre a média e o valor inicial",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,2])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,2])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,2])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,3]),xlab="Diferença relativa entre a média e o valor inicial",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,3])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,3])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,3])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,4]),xlab="Moda",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,4])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,4])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,4])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,5]),xlab="Variância",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,5])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,5])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,5])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,6]),xlab="Coeficiente de variação",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,6])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,6])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,6])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,7]),xlab="Assimetria",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,7])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,7])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,7])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,8]),xlab="Excesso de curtose",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,8])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,8])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,8])[c(2,5)],lwd=0.9,lty=2)

plot(ecdf(simulacoes_output_derivado[,9]),xlab="Coeficiente de Pearson",ylab="Proporção cumulativa",main="",bty="l",col.lab="gray18",col.axis="gray18",pch=20,col="gray18")
abline(v=summary(simulacoes_output_derivado[,9])[4],col="firebrick",lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,9])[3],lwd=0.9)
abline(v=summary(simulacoes_output_derivado[,9])[c(2,5)],lwd=0.9,lty=2)

#### PCC
require(sensitivity)

# Media
simulacoes_output_derivado_media<-as.vector(simulacoes_output_derivado[,1])
pcc_media<-pcc(dados3_27mai16,simulacoes_output_derivado_media,nboot=50)
par(mar=c(6,6.5,4,2))
plot(pcc_media$PCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("indianred4",0.7),cex.lab=0.9,font.lab=2,main="Média",cex.main=1,col.main="gray18")
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,"intervalo \nentre distúrbios",padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media$PCC[,4],y1=pcc_media$PCC[,1],col=alpha("indianred4",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media$PCC[,5],y1=pcc_media$PCC[,1],col=alpha("indianred4",0.7))

# Dif_Media_xi0
simulacoes_output_derivado_media_xi0<-as.vector(simulacoes_output_derivado[,2])
pcc_media_xi0<-pcc(dados3_27mai16,simulacoes_output_derivado_media_xi0,nboot=50)
plot(pcc_media_xi0$PCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("indianred",0.7),cex.lab=0.9,font.lab=2,main="Diferença absoluta entre a média e o valor inicial",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,"intervalo \nentre distúrbios",padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media_xi0$PCC[,4],y1=pcc_media_xi0$PCC[,1],col=alpha("indianred",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_media_xi0$PCC[,5],y1=pcc_media_xi0$PCC[,1],col=alpha("indianred",0.7))

# Razao_Media_xi0
simulacoes_output_derivado_raz_media_xi0<-as.vector(simulacoes_output_derivado[,3])
pcc_raz_media_xi0<-pcc(dados3_27mai16,simulacoes_output_derivado_raz_media_xi0,nboot=50)
plot(pcc_raz_media_xi0$PCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("lightcoral",0.7),cex.lab=0.9,font.lab=2,main="Diferença relativa entre a média e o valor inicial",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,"intervalo \nentre distúrbios",padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_raz_media_xi0$PCC[,4],y1=pcc_raz_media_xi0$PCC[,1],col=alpha("lightcoral",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_raz_media_xi0$PCC[,5],y1=pcc_raz_media_xi0$PCC[,1],col=alpha("lightcoral",0.7))

# Moda
simulacoes_output_derivado_moda<-as.vector(simulacoes_output_derivado[,4])
pcc_moda<-pcc(dados3_27mai16,simulacoes_output_derivado_moda,nboot=50)
plot(pcc_moda$PCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("darkgoldenrod2",0.7),cex.lab=0.9,font.lab=2,main="Moda",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,"intervalo \nentre distúrbios",padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_moda$PCC[,4],y1=pcc_moda$PCC[,1],col=alpha("darkgoldenrod2",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_moda$PCC[,5],y1=pcc_moda$PCC[,1],col=alpha("darkgoldenrod2",0.7))

# Variancia
simulacoes_output_derivado_variancia<-as.vector(simulacoes_output_derivado[,5])
pcc_variancia<-pcc(dados3_27mai16,simulacoes_output_derivado_variancia,nboot=50)
plot(pcc_variancia$PCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("turquoise4",0.7),cex.lab=0.9,font.lab=2,main="Variância",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,"intervalo \nentre distúrbios",padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_variancia$PCC[,4],y1=pcc_variancia$PCC[,1],col=alpha("turquoise4",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_variancia$PCC[,5],y1=pcc_variancia$PCC[,1],col=alpha("turquoise4",0.7))

# Coeficiente de Variacao
simulacoes_output_derivado_coef_var<-as.vector(simulacoes_output_derivado[,6])
pcc_coef_var<-pcc(dados3_27mai16,simulacoes_output_derivado_coef_var,nboot=50)
plot(pcc_coef_var$PCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("turquoise3",0.7),cex.lab=0.9,font.lab=2,main="Coeficiente de variação",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,"intervalo \nentre distúrbios",padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_coef_var$PCC[,4],y1=pcc_coef_var$PCC[,1],col=alpha("turquoise3",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_coef_var$PCC[,5],y1=pcc_coef_var$PCC[,1],col=alpha("turquoise3",0.7))

# Assimetria
simulacoes_output_derivado_assimetria<-as.vector(simulacoes_output_derivado[,7])
pcc_assimetria<-pcc(dados3_27mai16,simulacoes_output_derivado_assimetria,nboot=50)
plot(pcc_assimetria$PCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("orchid3",0.7),cex.lab=0.9,font.lab=2,main="Assimetria",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,"intervalo \nentre distúrbios",padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_assimetria$PCC[,4],y1=pcc_assimetria$PCC[,1],col=alpha("orchid3",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_assimetria$PCC[,5],y1=pcc_assimetria$PCC[,1],col=alpha("orchid3",0.7))

# Excesso de Curtose
simulacoes_output_derivado_curtose<-as.vector(simulacoes_output_derivado[,8])
pcc_curtose<-pcc(dados3_27mai16,simulacoes_output_derivado_curtose,nboot=50)
plot(pcc_curtose$PCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("royalblue",0.7),cex.lab=0.9,font.lab=2,main="Excesso de curtose",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,"intervalo \nentre distúrbios",padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_curtose$PCC[,4],y1=pcc_curtose$PCC[,1],col=alpha("royalblue",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_curtose$PCC[,5],y1=pcc_curtose$PCC[,1],col=alpha("royalblue",0.7))

# Pearson
simulacoes_output_derivado_pearson<-as.vector(simulacoes_output_derivado[,9])
pcc_pearson<-pcc(dados3_27mai16,simulacoes_output_derivado_pearson,nboot=50)
plot(pcc_pearson$PCC[,1],axes=F,ylab="Coeficiente de correlação parcial \n entre os parâmetros iniciais e \nas variáveis-resposta descritoras",xlab="Parâmetros iniciais",pch=16,ylim=c(-1.2,1.2),xlim=c(0.8,3.1),col.lab="gray18",col=alpha("lightslateblue",0.7),cex.lab=0.9,font.lab=2,main="Coeficiente de Pearson",col.main="gray18",cex.main=1)
abline(h=0,lty=2,col="gray18",lwd=0.9,cex.lab=0.8)
axis(2,seq(-1.5,1.5,0.5),seq(-1.5,1.5,0.5),col="gray18",col.axis="gray18",las=1,cex.axis=0.9)
axis(1,c(0:4),c("","","","",""),col="gray18",cex.axis=0.9)
mtext(side=1,"intervalo \nentre distúrbios",padj=1,col="gray18",cex=0.9,adj=0.53)
mtext(side=1,"intensidade \ndos distúrbios",padj=1,col="gray18",cex=0.9,adj=1.04)
mtext(side=1,expression(paste("n"^"o","inicial de")),padj=1,col="gray18",cex=0.9,adj=0.03)
mtext(side=1,"propágulos/ciclo",padj=2.7,col="gray18",cex=0.9,adj=-0.01)
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_pearson$PCC[,4],y1=pcc_pearson$PCC[,1],col=alpha("lightslateblue",0.7))
segments(x0=c(1,2,3),x1=c(1,2,3),y0=pcc_pearson$PCC[,5],y1=pcc_pearson$PCC[,1],col=alpha("lightslateblue",0.7))

## Para grafico com todos os pcc
# leg.text<-c("média","diferença absoluta entre a média e o valor inicial", "diferença relativa entre a média e o valor inicial", "variância", "coeficiente de variação", "assimetria", "excesso de curtose", "coeficiente de Pearson")
# leg.col<-c(alpha("indianred4",0.7),alpha("indianred",0.7),alpha("lightcoral",0.7),alpha("turquoise4",0.7),alpha("turquoise3",0.7),alpha("orchid3",0.7),alpha("royalblue",0.7),alpha("lightslateblue",0.7))
# legend(locator(),legend=leg.text,col=leg.col,pch=16,bty="n",cex=0.6,text.col="gray18",title=expression(paste("Descritores do ","n"^"o"," de propágulos/ciclo após 2 mil gerações")),title.col="gray18",title.adj=0.1)

## Histogramas

data_teste<-cbind(dados3_27mai16,simulacoes_27mai16_output_derivado)
distúrbio<-data_teste[,3]>=0.8
distúrbio[which(distúrbio==T)]<-"alto"
distúrbio[data_teste[,3]<=0.2]<-"baixo"
distúrbio[which(distúrbio==F)]<-"regular"
distúrbio[which(distúrbio=="regular")[1:200]]<-"reg_peq"
distúrbio[which(distúrbio=="regular" & data_teste[,1]>9200)]<-"reg_fert"

data_teste<-cbind(dados3_27mai16,simulacoes_27mai16_output_derivado)
distúrbio<-data_teste[,3]>=0.8 & data_teste[,2]<=8000
distúrbio[which(distúrbio==T)]<-"alto"
distúrbio[data_teste[,3]<=0.2 & data_teste[,2]>=290000]<-"baixo"
distúrbio[which(distúrbio==F)]<-"regular"
distúrbio[which(distúrbio=="regular")[1:10]]<-"reg_peq"
distúrbio[which(distúrbio=="regular" & data_teste[,1]>9200)]<-"reg_fert"

# Media
qplot(Media, data=data_teste, geom="density", xlab="Média do número de propágulos/ciclo",ylab="Densidade")
qplot(Media, data=data_teste, geom="density", xlab="Média do número de propágulos/ciclo",ylab="Densidade",alpha=I(.5),color=distúrbio)

# Moda
qplot(Moda, data=data_teste, geom="density", xlab="Moda do número de propágulos/ciclo",ylab="Densidade")
qplot(Moda, data=data_teste, geom="density", xlab="Moda do número de propágulos/ciclo",ylab="Densidade",alpha=I(.2),color=distúrbio)

# Variancia
qplot(Variância, data=data_teste, geom="density", xlab="Variância do número de propágulos/ciclo",ylab="Densidade")
