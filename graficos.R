# SELECAO DE GRAFICOS PARA MOSTRAR PRO PI - 16/03/17

##################  MEDIA DA ESTRATÉGIA DE VIDA ##################

## GRAFICOS VARIAVEL~GERACOES
par(mar=c(5,5,4,5))
par(mgp=c(3.5,1,0))

### DP 500, >1SP, 5e3 INDIVIDUOS
#### todas as geracoes
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",cex.axis=0.8)
#### ate geracao 10 mil
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,10000))
#### ate geracao 2 mil
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,2000))
#### ate geracao 1 mil
matplot(y=t(pos_comite_dp500_media_temporal)[1:3001,],x=t(I(pos_comite_dp500_mortes_cumulativas_temporal/5000))[1:3001,],type="l",col=alpha(colors()[dist_classes],0.5),las=1,ylim=c(1,20000),ylab="Estratégia de vida média",xlab="Geração",bty="l",main="Taxa de mutação = 500, n = 5 mil e riqueza > 1",cex.axis=0.8,xlim=c(0,1000))

### DP 0, >1SP, 5e3 INDIVIDUOS

### DP 500, 1SP, 5e3 INDIVIDUOS

### DP 500, >1SP, 20e3 INDIVIDUOS

## GRAFICOS VARIAVEL~DISTURBIO (NGER 5e3)
