# simulacoes dp 500 1 sp

#### carregando dados output
# media
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_1sp_media_temporal.RData")
# ss total
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_1sp_ss_total_temporal.RData")
# var total
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_1sp_var_total_temporal.RData")
# ss inter
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_1sp_ss_inter_temporal.RData")
# var inter
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_1sp_var_inter_temporal.RData")
# riqueza
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_1sp_riqueza_temporal.RData")
# mortes cumulativas
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_1sp_mortes_cumulativas_temporal.RData")

#### carregando dados disturbio
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_hipercubo/dados_hipercubo_27jul16/dados_arredond_hipercubo_27jul16.RData")
#### arrumando dados disturbio
bat3_indice_dist <- dados3_27jul16[,2]*dados3_27jul16[,3]
#quantile(bat3_indice_dist,probs=c(0.33,0.66))
dist_classe_1 <- which(bat3_indice_dist<28849.08)
dist_classe_2 <- which(bat3_indice_dist>=28849.08 & bat3_indice_dist<=90183.74)
dist_classe_3 <- which(bat3_indice_dist>90183.74)
dist_classes <- c()
dist_classes[dist_classe_1] <- 203
dist_classes[dist_classe_2] <- 183
dist_classes[dist_classe_3] <- 153

dist_classes3 <- c()
dist_classes3[dist_classe_1] <- 59 #salmao
dist_classes3[dist_classe_2] <- 27 #azul
dist_classes3[dist_classe_3] <- 50 #verde

#### GRAFICOS
require(ggplot2)

### media e riqueza

# dp 500
par(mar=c(5,5,4,5))
par(mgp=c(3.5,1,0))
matplot(t(pos_comite_dp500_1sp_media_temporal)[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="u",main="Taxa de mutação = 500 e riqueza = 1",axes=F)
axis(1,at=c(-200,1,751,1501,2251,3001,3500),labels=c("",0,75000,150000,225000,300000,""))
axis(2,at=c(-1000,1,5000,10000,15000,20000,25000),labels=c("",1,5000,10000,15000,20000,""),las=1)
par(new=T)
matplot(t(pos_comite_dp500_1sp_riqueza_temporal)[1:3001,],type="l",col=alpha(colors()[dist_classes2],0.5),lty=1,ylim=c(1,500),axes=F,ylab="")
axis(4,col.axis="blue",las=1,at=c(-100,1,125,250,375,500,600),labels=c("",1,125,250,375,500,""))
mtext("Riqueza",4,padj=4,col="blue")
# zoom: 10 000 primeiros ciclos (101 tempos)
par(mar=c(5,5,4,5))
par(mgp=c(3.5,1,0))
matplot(t(pos_comite_dp500_1sp_media_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="u",main="Taxa de mutação = 500 e riqueza = 1",axes=F)
axis(1,at=c(-100,1,26,51,76,101,200),labels=c("",0,2500,5000,7500,10000,""))
axis(2,at=c(-1000,1,5000,10000,15000,20000,25000),labels=c("",1,5000,10000,15000,20000,""),las=1)
par(new=T)
matplot(t(pos_comite_dp500_1sp_riqueza_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes2],0.5),lty=1,ylim=c(1,500),axes=F,ylab="")
axis(4,col.axis="blue",las=1,at=c(-100,1,125,250,375,500,600),labels=c("",1,125,250,375,500,""))
mtext("Riqueza",4,padj=4,col="blue")
# zoom: 1000 primeiros ciclos (11 tempos)
par(mar=c(5,5,4,5))
par(mgp=c(3.5,1,0))
matplot(t(pos_comite_dp500_1sp_media_temporal)[1:11,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="u",main="Taxa de mutação = 500 e riqueza = 1",axes=F)
axis(1,at=c(0,1,3.5,6,8.5,11,15),labels=c("",0,250,500,750,1000,""))
axis(2,at=c(-1000,1,5000,10000,15000,20000,25000),labels=c("",1,5000,10000,15000,20000,""),las=1)
par(new=T)
matplot(t(pos_comite_dp500_1sp_riqueza_temporal)[1:10,],type="l",col=alpha(colors()[dist_classes2],0.3),lty=1,ylim=c(1,500),axes=F,ylab="")
axis(4,col.axis="blue",las=1,at=c(-100,1,125,250,375,500,600),labels=c("",1,125,250,375,500,""))
mtext("Riqueza",4,padj=4,col="blue")


### ss total

# dp 500
# zoom: todos os ciclos
#par(mfrow=c(2,1))
par(mar=c(5,5,4,5))
par(mgp=c(3.5,1,0))
# matplot(t(pos_comite_dp500_var_total_temporal)[1:3001,],type="l",col=alpha(colors()[dist_classes5],0.3),lty=1,ylim=c(1,8e7),ylab="Variância da estratégia de vida",bty="u",main="Taxa de mutação = 500",axes=F)
# axis(1,at=c(-200,1,751,1501,2251,3001,3500),labels=c("",0,75000,150000,225000,300000,""),cex.axis=0.8)
# axis(2,at=c(-1e7,0,2e7,4e7,6e7,8e7,9e7),labels=c("",0,expression(text=paste("20x10"^"6",sep="")),expression(text=paste("40x10"^"6",sep="")),expression(text=paste("60x10"^"6",sep="")),expression(text=paste("80x10"^"6",sep="")),""),las=1,cex.axis=0.8)
# matplot(t(pos_comite_dp500_var_inter_temporal)[1:3001,],type="l",col=alpha(colors()[dist_classes4],0.3),lty=1,ylab="Variância interespecífica relativa",axes=F,ylim=c(0,5000))
# axis(1,at=c(-200,1,751,1501,2251,3001,3500),labels=c("",0,75000,150000,225000,300000,""),cex.axis=0.8)
# axis(2,las=1,at=c(-1000,1,1250,2500,3750,5000,6000),labels=c("",1,1250,2500,3750,5000,""),cex.axis=0.8)
# matplot(t(pos_comite_dp500_var_inter_absoluta_temporal)[1:3001,],type="l",col=alpha(colors()[dist_classes3],0.3),lty=1,xlab="Ciclos",ylab="Variância interespecífica",axes=F,ylim=c(0,3e11))
# axis(1,at=c(-200,1,751,1501,2251,3001,3500),labels=c("",0,75000,150000,225000,300000,""),cex.axis=0.8)
# axis(2,at=c(-1e11,0,0.75e11,1.5e11,2.25e11,3e11,4e11),labels=c("",0,expression(text=paste("0,75x10"^"11",sep="")),expression(text=paste("1,5x10"^"11",sep="")),expression(text=paste("2,25x10"^"11",sep="")),expression(text=paste("3x10"^"11",sep="")),""),las=1,cex.axis=0.7)
matplot(t(pos_comite_dp500_1sp_ss_total_temporal)[1:3001,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",axes=F,ylim=c(0,4e11),main="Taxa de mutação = 500 e riqueza = 1")
axis(1,at=c(-200,1,751,1501,2251,3001,3500),labels=c("",0,75000,150000,225000,300000,""),cex.axis=0.8)
axis(2,at=c(-1e11,0,1e11,2e11,3e11,4e11,5e11),labels=c("",0,expression(text=paste("1x10"^"11",sep="")),expression(text=paste("2x10"^"11",sep="")),expression(text=paste("3x10"^"11",sep="")),expression(text=paste("4x10"^"11",sep="")),""),las=1,cex.axis=0.7)
# zoom: 10 000 primeiros ciclos (101 tempos)
#par(mfrow=c(2,1))
par(mar=c(5,5,4,5))
par(mgp=c(3.5,1,0))
# matplot(t(pos_comite_dp500_var_total_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes5],0.3),lty=1,ylim=c(1,8e7),ylab="Variância da estratégia de vida",bty="u",main="Taxa de mutação = 500",axes=F)
# axis(1,at=c(-100,1,26,51,76,101,200),labels=c("",0,2500,5000,7500,10000,""),cex.axis=0.8)
# axis(2,at=c(-1e7,0,2e7,4e7,6e7,8e7,9e7),labels=c("",0,expression(text=paste("20x10"^"6",sep="")),expression(text=paste("40x10"^"6",sep="")),expression(text=paste("60x10"^"6",sep="")),expression(text=paste("80x10"^"6",sep="")),""),las=1,cex.axis=0.8)
# matplot(t(pos_comite_dp500_var_inter_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes4],0.3),lty=1,ylab="Variância interespecífica relativa",axes=F,ylim=c(0,5000))
# axis(1,at=c(-100,1,26,51,76,101,200),labels=c("",0,2500,5000,7500,10000,""),cex.axis=0.8)
# axis(2,las=1,at=c(-1000,1,1250,2500,3750,5000,6000),labels=c("",1,1250,2500,3750,5000,""),cex.axis=0.8)
# matplot(t(pos_comite_dp500_var_inter_absoluta_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes3],0.3),lty=1,xlab="Ciclos",ylab="Variância interespecífica",axes=F,ylim=c(0,3e11))
# axis(1,at=c(-100,1,26,51,76,101,200),labels=c("",0,2500,5000,7500,10000,""),cex.axis=0.8)
# axis(2,at=c(-1e11,0,0.75e11,1.5e11,2.25e11,3e11,4e11),labels=c("",0,expression(text=paste("0,75x10"^"11",sep="")),expression(text=paste("1,5x10"^"11",sep="")),expression(text=paste("2,25x10"^"11",sep="")),expression(text=paste("3x10"^"11",sep="")),""),las=1,cex.axis=0.7)
matplot(t(pos_comite_dp500_1sp_ss_total_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",axes=F,ylim=c(0,2e11),main="Taxa de mutação = 500 e riqueza = 1")
axis(1,at=c(-100,1,26,51,76,101,200),labels=c("",0,2500,5000,7500,10000,""),cex.axis=0.8)
axis(2,at=c(-1e11,0,1e11,2e11,3e11,4e11,5e11),labels=c("",0,expression(text=paste("1x10"^"11",sep="")),expression(text=paste("2x10"^"11",sep="")),expression(text=paste("3x10"^"11",sep="")),expression(text=paste("4x10"^"11",sep="")),""),las=1,cex.axis=0.7)
matplot(t(pos_comite_dp500_1sp_ss_total_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",axes=F,ylim=c(0,5e9),main="Taxa de mutação = 500 e riqueza = 1")
axis(1,at=c(-100,1,26,51,76,101,200),labels=c("",0,2500,5000,7500,10000,""),cex.axis=0.8)
axis(2,at=c(-1e9,0,1.25e9,2.5e9,3.75e9,5e9,6e9),labels=c("",0,expression(text=paste("1,25x10"^"9",sep="")),expression(text=paste("2,5x10"^"9",sep="")),expression(text=paste("3,75x10"^"9",sep="")),expression(text=paste("5x10"^"9",sep="")),""),las=1,cex.axis=0.7)
# zoom: 1000 primeiros ciclos (11 tempos)
#par(mfrow=c(2,1))
par(mar=c(5,5,4,5))
par(mgp=c(3.5,1,0))
# matplot(t(pos_comite_dp500_var_total_temporal)[1:11,],type="l",col=alpha(colors()[dist_classes5],0.3),lty=1,ylim=c(1,8e7),ylab="Variância da estratégia de vida",bty="u",main="Taxa de mutação = 500",axes=F)
# axis(1,at=c(0,1,3.5,6,8.5,11,15),labels=c("",0,250,500,750,1000,""),cex.axis=0.8)
# axis(2,at=c(-1e7,0,2e7,4e7,6e7,8e7,9e7),labels=c("",0,expression(text=paste("20x10"^"6",sep="")),expression(text=paste("40x10"^"6",sep="")),expression(text=paste("60x10"^"6",sep="")),expression(text=paste("80x10"^"6",sep="")),""),las=1,cex.axis=0.8)
# matplot(t(pos_comite_dp500_var_inter_temporal)[1:11,],type="l",col=alpha(colors()[dist_classes4],0.3),lty=1,ylab="Variância interespecífica relativa",axes=F,ylim=c(0,5000))
# axis(1,at=c(0,1,3.5,6,8.5,11,15),labels=c("",0,250,500,750,1000,""),cex.axis=0.8)
# axis(2,las=1,at=c(-1000,1,1250,2500,3750,5000,6000),labels=c("",1,1250,2500,3750,5000,""),cex.axis=0.8)
# matplot(t(pos_comite_dp500_var_inter_absoluta_temporal)[1:11,],type="l",col=alpha(colors()[dist_classes3],0.3),lty=1,xlab="Ciclos",ylab="Variância interespecífica",axes=F,ylim=c(0,3e11))
# axis(1,at=c(0,1,3.5,6,8.5,11,15),labels=c("",0,250,500,750,1000,""),cex.axis=0.8)
# axis(2,at=c(-1e11,0,0.75e11,1.5e11,2.25e11,3e11,4e11),labels=c("",0,expression(text=paste("0,75x10"^"11",sep="")),expression(text=paste("1,5x10"^"11",sep="")),expression(text=paste("2,25x10"^"11",sep="")),expression(text=paste("3x10"^"11",sep="")),""),las=1,cex.axis=0.7)
matplot(t(pos_comite_dp500_1sp_ss_total_temporal)[1:11,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",axes=F,ylim=c(0,2e11),main="Taxa de mutação = 500 e riqueza = 1")
axis(1,at=c(0,1,3.5,6,8.5,11,15),labels=c("",0,250,500,750,1000,""),cex.axis=0.8)
axis(2,at=c(-1e11,0,1e11,2e11,3e11,4e11,5e11),labels=c("",0,expression(text=paste("1x10"^"11",sep="")),expression(text=paste("2x10"^"11",sep="")),expression(text=paste("3x10"^"11",sep="")),expression(text=paste("4x10"^"11",sep="")),""),las=1,cex.axis=0.7)
matplot(t(pos_comite_dp500_1sp_ss_total_temporal)[1:11,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",axes=F,ylim=c(0,5e9),main="Taxa de mutação = 500 e riqueza = 1")
axis(1,at=c(0,1,3.5,6,8.5,11,15),labels=c("",0,250,500,750,1000,""),cex.axis=0.8)
axis(2,at=c(-1e9,0,1.25e9,2.5e9,3.75e9,5e9,6e9),labels=c("",0,expression(text=paste("1,25x10"^"9",sep="")),expression(text=paste("2,5x10"^"9",sep="")),expression(text=paste("3,75x10"^"9",sep="")),expression(text=paste("5x10"^"9",sep="")),""),las=1,cex.axis=0.7)





bat_indice_dist <- dados3_04mar17[,2]*dados3_04mar17[,3]

dist_classe_1_mais <- which(bat_indice_dist<7450.809)
dist_classe_2_mais <- which(bat_indice_dist>=7450.809 & bat_indice_dist<=22681.605)
dist_classe_3_mais <- which(bat_indice_dist>22681.605)
dist_classes_mais <- c()
dist_classes_mais[dist_classe_1_mais] <- 203
dist_classes_mais[dist_classe_2_mais] <- 183
dist_classes_mais[dist_classe_3_mais] <- 153
matplot(t(pos_comite_dp500_mais_ind_media_temporal)[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="l",main="Taxa de mutação = 500 e ntotal = 20 mil")
matplot(t(pos_comite_dp500_mais_ind_media_temporal)[1:751,dist_classes_mais==203],type="l",col=alpha(colors()[203],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="l",main="Taxa de mutação = 500 e ntotal = 20 mil")
matplot(t(pos_comite_dp500_mais_ind_media_temporal)[1:751,dist_classes_mais==183],type="l",col=alpha(colors()[183],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="l",main="Taxa de mutação = 500 e ntotal = 20 mil")
matplot(t(pos_comite_dp500_mais_ind_media_temporal)[1:751,dist_classes_mais==153],type="l",col=alpha(colors()[153],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="l",main="Taxa de mutação = 500 e ntotal = 20 mil")

matplot(t(pos_comite_dp500_mais_ind_media_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="l",main="Taxa de mutação = 500 e ntotal = 20 mil")
#matplot(t(pos_comite_dp500_media_temporal)[1:101,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="l",main="Taxa de mutação = 500 e ntotal = 20 mil")
matplot(t(pos_comite_dp500_mais_ind_media_temporal)[1:11,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="l",main="Taxa de mutação = 500 e ntotal = 20 mil")
#matplot(t(pos_comite_dp500_media_temporal)[1:11,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="l",main="Taxa de mutação = 500 e ntotal = 20 mil")
par(new=T)
matplot(t(pos_comite_dp500_mais_ind_riqueza_temporal)[1:11,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,500))
matplot(t(pos_comite_dp500_riqueza_temporal)[1:11,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500))
matplot(t(pos_comite_dp500_media_temporal)[1:11,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Ciclos",bty="l",main="Taxa de mutação = 500")
