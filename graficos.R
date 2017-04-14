# SELECAO DE GRAFICOS PARA MOSTRAR PRO PI - 16/03/17
require(ggplot2)

# Carregando dados
## DISTURBIO
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_hipercubo/dados_hipercubo_27jul16/dados_arredond_hipercubo_27jul16.RData")
bat3_indice_dist <- dados3_27jul16[,2]*dados3_27jul16[,3]
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
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_hipercubo/dados_hipercubo_04mar17/dados_arredond_hipercubo_04mar17.RData")
bat_indice_dist <- dados3_04mar17[,2]*dados3_04mar17[,3]
dist_classe_1_mais <- which(bat_indice_dist<7450.809)
dist_classe_2_mais <- which(bat_indice_dist>=7450.809 & bat_indice_dist<=22681.605)
dist_classe_3_mais <- which(bat_indice_dist>22681.605)
dist_classes_mais <- c()
dist_classes_mais[dist_classe_1_mais] <- 203
dist_classes_mais[dist_classe_2_mais] <- 183
dist_classes_mais[dist_classe_3_mais] <- 153
dist_classes3_mais <- c()
dist_classes3_mais[dist_classe_1_mais] <- 59 #salmao
dist_classes3_mais[dist_classe_2_mais] <- 27 #azul
dist_classes3_mais[dist_classe_3_mais] <- 50 #verde

### testando aqui uma paradinha
rbPal <- colorRampPalette(c('blue','red'))
#This adds a column of color values
# based on the y values
opa <- rbPal(1000)[as.numeric(cut(dados$dist_indice_scale[dados$bateria==3],breaks = 1000))]
###


## MEDIA
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp500_media_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp0_media_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_1sp_media_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_mais_ind_media_temporal.RData")
## MORTES ACUMULADAS
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp500_mortes_cumulativas_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp0_mortes_cumulativas_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_1sp_mortes_cumulativas_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_mais_ind_mortes_cumulativas_temporal.RData")
## RIQUEZA
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp500_riqueza_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp0_riqueza_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_mais_ind_riqueza_temporal.RData")
## SS TOTAL
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp500_ss_total_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp0_ss_total_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_1sp_ss_total_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_mais_ind_ss_total_temporal.RData")
## SS INTER
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp500_ss_inter_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_15fev17_dp0_ss_inter_temporal.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_pos_comite_15fev17_hipercubo27jul17/pos_comite_4mar17_dp500_mais_ind_ss_inter_temporal.RData")

##################  MEDIA DA ESTRATÉGIA DE VIDA ##################

## GRAFICOS VARIAVEL~GERACOES
par(mar=c(5,5,4,5))
par(mgp=c(3.5,1,0))

### DP 500, >1SP, 5e3 INDIVIDUOS
#### todas as geracoes
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",cex.axis=0.8)
par(new=T)
matplot(y=t(pos_comite_dp500_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F)
#### ate geracao 10 mil
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,10000))
par(new=T)
matplot(y=t(pos_comite_dp500_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,10000))
#### ate geracao 2 mil
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(opa,0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,2000),lty=1)
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,2000))
par(new=T)
matplot(y=t(pos_comite_dp500_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,2000))
#### ate geracao 1 mil
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,1000))
par(new=T)
matplot(y=t(pos_comite_dp500_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,1000))

### DP 0, >1SP, 5e3 INDIVIDUOS
#### todas as geracoes
matplot(y=t(pos_comite_dp0_media_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 0, n = 5 mil e riqueza > 1",cex.axis=0.8)
par(new=T)
matplot(y=t(pos_comite_dp0_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F)
#### ate geracao 10 mil
matplot(y=t(pos_comite_dp0_media_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 0, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,10000))
par(new=T)
matplot(y=t(pos_comite_dp0_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,10000))
#### ate geracao 2 mil
matplot(y=t(pos_comite_dp0_media_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 0, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,2000))
par(new=T)
matplot(y=t(pos_comite_dp0_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,2000))
#### ate geracao 1 mil
matplot(y=t(pos_comite_dp0_media_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 0, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,1000))
par(new=T)
matplot(y=t(pos_comite_dp0_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,1000))

### DP 500, 1SP, 5e3 INDIVIDUOS
#### todas as geracoes
matplot(y=t(pos_comite_dp500_1sp_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza = 1",cex.axis=0.8)
par(new=T)
matplot(y=t(pos_comite_dp500_1sp_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F)
#### ate geracao 10 mil
matplot(y=t(pos_comite_dp500_1sp_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza = 1",cex.axis=0.8,xlim=c(0,10000))
par(new=T)
matplot(y=t(pos_comite_dp500_1sp_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,10000))
#### ate geracao 2 mil
matplot(y=t(pos_comite_dp500_1sp_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza = 1",cex.axis=0.8,xlim=c(0,2000))
par(new=T)
matplot(y=t(pos_comite_dp500_1sp_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,2000))
#### ate geracao 1 mil
matplot(y=t(pos_comite_dp500_1sp_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza = 1",cex.axis=0.8,xlim=c(0,1000))
par(new=T)
matplot(y=t(pos_comite_dp500_1sp_riqueza_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,1000))

### DP 500, >1SP, 20e3 INDIVIDUOS
#### todas as geracoes
matplot(y=t(pos_comite_dp500_mais_ind_media_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 20 mil e riqueza > 1",cex.axis=0.8,las=1)
par(new=T)
matplot(y=t(pos_comite_dp500_mais_ind_riqueza_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F)
#### ate geracao 10 mil
matplot(y=t(pos_comite_dp500_mais_ind_media_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 20 mil e riqueza > 1",cex.axis=0.8,las=1,xlim=c(0,10000))
par(new=T)
matplot(y=t(pos_comite_dp500_mais_ind_riqueza_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,10000))
#### ate geracao 2 mil
matplot(y=t(pos_comite_dp500_mais_ind_media_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 20 mil e riqueza > 1",cex.axis=0.8,las=1,xlim=c(0,2000))
par(new=T)
matplot(y=t(pos_comite_dp500_mais_ind_riqueza_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,2000))
#### ate geracao 1 mil
matplot(y=t(pos_comite_dp500_mais_ind_media_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 20 mil e riqueza > 1",cex.axis=0.8,las=1,xlim=c(0,1000))
par(new=T)
matplot(y=t(pos_comite_dp500_mais_ind_riqueza_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,500),ylab="",xlab="",bty="l",main="",axes=F,xlim=c(0,1000))

## GRAFICOS VARIAVEL~DISTURBIO (NGER 5e3)











################  SS TOTAL DA ESTRATÉGIA DE VIDA #################

## GRAFICOS VARIAVEL~GERACOES
par(mar=c(5,5,4,5))
par(mgp=c(3.5,1,0))

### DP 500, >1SP, 5e3 INDIVIDUOS
#### todas as geracoes
matplot(y=t(pos_comite_dp500_ss_total_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",ylim=c(0,4e11),main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",xlab="Geração",bty="l",las=1,cex.axis=0.8)

#### ate geracao 10 mil
matplot(y=t(pos_comite_dp500_ss_total_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",ylim=c(0,4e11),main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",xlab="Geração",bty="l",las=1,cex.axis=0.8,xlim=c(0,10000))
matplot(y=t(pos_comite_dp500_ss_total_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=opa,lty=1,ylab="Desvio quadrático total",ylim=c(0,4e11),main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",xlab="Geração",bty="l",las=1,cex.axis=0.8,xlim=c(0,10000))

#### ate geracao 2 mil
matplot(y=t(pos_comite_dp500_ss_total_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",ylim=c(0,4e11),main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",xlab="Geração",bty="l",las=1,cex.axis=0.8,xlim=c(0,2000))

#### ate geracao 1 mil
matplot(y=t(pos_comite_dp500_ss_total_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",ylim=c(0,4e11),main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",xlab="Geração",bty="l",las=1,cex.axis=0.8,xlim=c(0,1000))

### DP 0, >1SP, 5e3 INDIVIDUOS
#### todas as geracoes
matplot(y=t(pos_comite_dp0_ss_total_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",ylim=c(0,4e11),main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",xlab="Geração",bty="l",las=1,cex.axis=0.8)

#### ate geracao 10 mil
matplot(y=t(pos_comite_dp0_ss_total_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",ylim=c(0,4e11),main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",xlab="Geração",bty="l",las=1,cex.axis=0.8,xlim=c(0,10000))

#### ate geracao 2 mil
matplot(y=t(pos_comite_dp0_ss_total_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",ylim=c(0,4e11),main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",xlab="Geração",bty="l",las=1,cex.axis=0.8,xlim=c(0,2000))

#### ate geracao 1 mil
matplot(y=t(pos_comite_dp0_ss_total_temporal)[1:3001,],x=t(I(pos_comite_dp0_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.3),lty=1,ylab="Desvio quadrático total",ylim=c(0,4e11),main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",xlab="Geração",bty="l",las=1,cex.axis=0.8,xlim=c(0,1000))

### DP 500, 1SP, 5e3 INDIVIDUOS
#### todas as geracoes
matplot(y=t(pos_comite_dp500_1sp_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza = 1",cex.axis=0.8)

#### ate geracao 10 mil
matplot(y=t(pos_comite_dp500_1sp_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza = 1",cex.axis=0.8,xlim=c(0,10000))

#### ate geracao 2 mil
matplot(y=t(pos_comite_dp500_1sp_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza = 1",cex.axis=0.8,xlim=c(0,2000))

#### ate geracao 1 mil
matplot(y=t(pos_comite_dp500_1sp_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_1sp_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza = 1",cex.axis=0.8,xlim=c(0,1000))

### DP 500, >1SP, 20e3 INDIVIDUOS
#### todas as geracoes
matplot(y=t(pos_comite_dp500_mais_ind_media_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 20 mil e riqueza > 1",cex.axis=0.8,las=1)

#### ate geracao 10 mil
matplot(y=t(pos_comite_dp500_mais_ind_media_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 20 mil e riqueza > 1",cex.axis=0.8,las=1,xlim=c(0,10000))

#### ate geracao 2 mil
matplot(y=t(pos_comite_dp500_mais_ind_media_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 20 mil e riqueza > 1",cex.axis=0.8,las=1,xlim=c(0,2000))

#### ate geracao 1 mil
matplot(y=t(pos_comite_dp500_mais_ind_media_temporal)[1:751,],x=t(I(pos_comite_dp500_mais_ind_mortes_cumulativas_temporal/20000))[1:751,],type="l",col=alpha(colors()[dist_classes_mais],0.5),lty=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 20 mil e riqueza > 1",cex.axis=0.8,las=1,xlim=c(0,1000))

