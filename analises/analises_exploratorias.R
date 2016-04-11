# Analises exploratorias

### Organizando resultados gerais (e nao intraespecificos)
resultados_geral<-matrix(nrow=100,ncol=6,dimnames=list(c(1:100),c("Abundancia","Media","Variancia","Assimetria","Curtose","Coeficiente_Pearson")))
for(i in 1:100){
resultados_geral[i,]<-sim_todas[[i]][1,]
}
resulta<-resultados_geral[,-1]

### Tentando usar pacote pse 
# Acoplando resultados e dados de entrada
colnames(dados3_25mar16)<-c("S","xi0","dp","dist.pos","dist.int")
hipercubo_resultados<-tell(dados3_25mar16,resulta) #nao funcionou porque dados3_25mar16 nao eh um objeto do tipo LHS, e sim um data frame

#### Boxplot


#### Scatterplot
## Media
# Media ~ S
plot(dados3_25mar16[,1],resulta[,1],pch=20,xlab="S",ylab="Media")
modelo_media_S<-lm(resulta[,1]~dados3_25mar16[,1])
abline(modelo_media_S)
# Media ~ xi0
plot(dados3_25mar16[,2],resulta[,1],pch=20,xlab="xi0",ylab="Media")
modelo_media_xi0<-lm(resulta[,1]~dados3_25mar16[,2])
abline(modelo_media_xi0)
# Media ~ dp
plot(dados3_25mar16[,3],resulta[,1],pch=20,xlab="dp",ylab="Media")
modelo_media_dp<-lm(resulta[,1]~dados3_25mar16[,3])
abline(modelo_media_dp)
# Media ~ dist.pos
plot(dados3_25mar16[,4],resulta[,1],pch=20,xlab="dist.pos",ylab="Media")
modelo_media_dist.pos<-lm(resulta[,1]~dados3_25mar16[,4])
abline(modelo_media_dist.pos)
# Media ~ dist.int
plot(dados3_25mar16[,5],resulta[,1],pch=20,xlab="dist.int",ylab="Media")
modelo_media_dist.int<-lm(resulta[,1]~dados3_25mar16[,5])
abline(modelo_media_dist.int)
## Variancia
# Variancia ~ S
plot(dados3_25mar16[,1],resulta[,2],pch=20,xlab="S",ylab="Variancia")
modelo_variancia_S<-lm(resulta[,2]~dados3_25mar16[,1])
abline(modelo_variancia_S)
# Variancia ~ xi0
plot(dados3_25mar16[,2],resulta[,2],pch=20,xlab="xi0",ylab="Variancia")
modelo_variancia_xi0<-lm(resulta[,2]~dados3_25mar16[,2])
abline(modelo_variancia_xi0)
# Variancia ~ dp
plot(dados3_25mar16[,3],resulta[,2],pch=20,xlab="dp",ylab="Variancia")
modelo_variancia_dp<-lm(resulta[,2]~dados3_25mar16[,3])
abline(modelo_variancia_dp)
# Variancia ~ dist.pos
plot(dados3_25mar16[,4],resulta[,2],pch=20,xlab="dist.pos",ylab="Variancia")
modelo_variancia_dist.pos<-lm(resulta[,2]~dados3_25mar16[,4])
abline(modelo_variancia_dist.pos)
# Variancia ~ dist.int
plot(dados3_25mar16[,5],resulta[,2],pch=20,xlab="dist.int",ylab="Variancia")
modelo_variancia_dist.int<-lm(resulta[,2]~dados3_25mar16[,5])
abline(modelo_variancia_dist.int)
## Assimetria
# Assimetria ~ S
plot(dados3_25mar16[,1],resulta[,3],pch=20,xlab="S",ylab="Assimetria")
modelo_assimetria_S<-lm(resulta[,3]~dados3_25mar16[,1])
abline(modelo_assimetria_S)
# Assimetria ~ xi0
plot(dados3_25mar16[,2],resulta[,3],pch=20,xlab="xi0",ylab="Assimetria")
modelo_assimetria_xi0<-lm(resulta[,3]~dados3_25mar16[,2])
abline(modelo_assimetria_xi0)
# Assimetria ~ dp
plot(dados3_25mar16[,3],resulta[,3],pch=20,xlab="dp",ylab="Assimetria")
modelo_assimetria_dp<-lm(resulta[,3]~dados3_25mar16[,3])
abline(modelo_assimetria_dp)
# Assimetria ~ dist.pos
plot(dados3_25mar16[,4],resulta[,3],pch=20,xlab="dist.pos",ylab="Assimetria")
modelo_assimetria_dist.pos<-lm(resulta[,3]~dados3_25mar16[,4])
abline(modelo_assimetria_dist.pos)
# Assimetria ~ dist.int
plot(dados3_25mar16[,5],resulta[,3],pch=20,xlab="dist.int",ylab="Assimetria")
modelo_assimetria_dist.int<-lm(resulta[,3]~dados3_25mar16[,5])
abline(modelo_assimetria_dist.int)
## Curtose
# Curtose ~ S
plot(dados3_25mar16[,1],resulta[,4],pch=20,xlab="S",ylab="Curtose")
modelo_curtose_S<-lm(resulta[,4]~dados3_25mar16[,1])
abline(modelo_curtose_S)
# Curtose ~ xi0
plot(dados3_25mar16[,2],resulta[,4],pch=20,xlab="xi0",ylab="Curtose")
modelo_curtose_xi0<-lm(resulta[,4]~dados3_25mar16[,2])
abline(modelo_curtose_xi0)
# Curtose ~ dp
plot(dados3_25mar16[,3],resulta[,4],pch=20,xlab="dp",ylab="Curtose")
modelo_curtose_dp<-lm(resulta[,4]~dados3_25mar16[,3])
abline(modelo_curtose_dp)
# Curtose ~ dist.pos
plot(dados3_25mar16[,4],resulta[,4],pch=20,xlab="dist.pos",ylab="Curtose")
modelo_curtose_dist.pos<-lm(resulta[,4]~dados3_25mar16[,4])
abline(modelo_curtose_dist.pos)
# Curtose ~ dist.int
plot(dados3_25mar16[,5],resulta[,4],pch=20,xlab="dist.int",ylab="Curtose")
modelo_curtose_dist.int<-lm(resulta[,4]~dados3_25mar16[,5])
abline(modelo_curtose_dist.int)
## Coeficiente de Pearson
# Coeficiente de Pearson ~ S
plot(dados3_25mar16[,1],resulta[,5],pch=20,xlab="S",ylab="Coeficiente de Pearson")
modelo_pearson_S<-lm(resulta[,5]~dados3_25mar16[,1])
abline(modelo_pearson_S)
# Coeficiente de Pearson ~ xi0
plot(dados3_25mar16[,2],resulta[,5],pch=20,xlab="xi0",ylab="Coeficiente de Pearson")
modelo_pearson_xi0<-lm(resulta[,5]~dados3_25mar16[,2])
abline(modelo_pearson_xi0)
# Coeficiente de Pearson ~ dp
plot(dados3_25mar16[,3],resulta[,5],pch=20,xlab="dp",ylab="Coeficiente de Pearson")
modelo_pearson_dp<-lm(resulta[,5]~dados3_25mar16[,3])
abline(modelo_pearson_dp)
# Coeficiente de Pearson ~ dist.pos
plot(dados3_25mar16[,4],resulta[,5],pch=20,xlab="dist.pos",ylab="Coeficiente de Pearson")
modelo_pearson_dist.pos<-lm(resulta[,5]~dados3_25mar16[,4])
abline(modelo_pearson_dist.pos)
# Coeficiente de Pearson ~ dist.int
plot(dados3_25mar16[,5],resulta[,5],pch=20,xlab="dist.int",ylab="Coeficiente de Pearson")
modelo_pearson_dist.int<-lm(resulta[,5]~dados3_25mar16[,5])
abline(modelo_pearson_dist.int)

#### ECDF
plot(ecdf(resulta[,1]),pch=20,xlab="Média",ylab="Proporção cumulativa",main="")
plot(ecdf(resulta[,2]),pch=20,xlab="Variância",ylab="Proporção cumulativa",main="")
plot(ecdf(resulta[,3]),pch=20,xlab="Assimetria",ylab="Proporção cumulativa",main="")
plot(ecdf(resulta[,4]),pch=20,xlab="Curtose",ylab="Proporção cumulativa",main="")
plot(ecdf(resulta[,5]),pch=20,xlab="Coeficiente de Pearson",ylab="Proporção cumulativa",main="")

#### PCC
require(sensitivity)
# Media
resulta_media<-as.vector(resulta[,1])
pcc_media<-pcc(dados3_25mar16,resulta_media,nboot=50)
plot(pcc_media)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Média")
# Variancia
resulta_variancia<-as.vector(resulta[,2])
pcc_variancia<-pcc(dados3_25mar16,resulta_variancia,nboot=50)
plot(pcc_variancia)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Variância")
# Assimetria
resulta_assimetria<-as.vector(resulta[,3])
pcc_assimetria<-pcc(dados3_25mar16,resulta_assimetria,nboot=50)
plot(pcc_assimetria)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Assimetria")
# Curtose
resulta_curtose<-as.vector(resulta[,4])
pcc_curtose<-pcc(dados3_25mar16,resulta_curtose,nboot=50)
plot(pcc_curtose)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Curtose")
# Pearson
resulta_pearson<-as.vector(resulta[,5])
pcc_pearson<-pcc(dados3_25mar16,resulta_pearson,nboot=50)
plot(pcc_pearson)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Coeficiente de Pearson")

# Analises exploratorias 2

### Organizando resultados gerais
resultados_geral_2<-matrix(nrow=100,ncol=6,dimnames=list(c(1:100),c("Abundancia","Media","Variancia","Assimetria","Curtose","Coeficiente_Pearson")))
for(i in 1:100){
  resultados_geral_2[i,]<-sim_todas[[i]][1,]
}
resulta_2<-resultados_geral_2[,-1]
resulta_2<-as.data.frame(resulta_2)
### Acrescentando a diferenca entre a estrategia de vida final e a inicial como variavel resposta
resulta_2$Dif_Media_xi0<-resulta_2[,1]-dados3_25mar16[,2]
### Acrescentando o coeficiente de variacao das estrategias na comunidade como variavel resposta
resulta_2$Coeficiente_Variacao<-(sqrt(resulta_2[,2]))/resulta_2[,1]
### Acrescentando a variancia entre as medias das estrategias por especie como variavel resposta (variancia interespecifica)
var_inter<-c()
for(i in 1:100){
  var_inter[i]<-var(sim_todas[[i]][-1,2])
}
resulta_2$Variancia_Inter<-var_inter
### Acrescentando o coeficiente de variacao das medias das estrategias por especie como variavel resposta (coeficiente de variacao interespecifica)
media_medias.sp<-c()
for(i in 1:100){
  media_medias.sp[i]<-mean(sim_todas[[i]][-1,2])
}
resulta_2$Coeficiente_Variacao_Inter<-(sqrt(var_inter))/media_medias.sp
### Oranizando o data frame 
resulta_2<-cbind(resulta_2[,1],resulta_2[,6],resulta_2[,2],resulta_2[,7],resulta_2[,3],resulta_2[,4],resulta_2[,5],resulta_2[,8],resulta_2[,9])
colnames(resulta_2)<-c("Média","Dif_Média_xi0","Variância","Coeficiente_Variação","Assimetria","Curtose","Coeficiente_Pearson","Variância_Inter","Coeficiente_Variação_Inter")
resulta_2
save(resulta_2,file="output_novas_variaveis.RData")

#### PCC
require(sensitivity)
# Dif_Media_xi0
resulta_2_media_xi0<-as.vector(resulta_2[,2])
pcc_media_xi0<-pcc(dados3_25mar16,resulta_2_media_xi0,nboot=50)
plot(pcc_media_xi0)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Dif_Média_xi0")
# Coeficiente de Variacao
resulta_2_coef_var<-as.vector(resulta_2[,4])
pcc_coef_var<-pcc(dados3_25mar16,resulta_2_coef_var,nboot=50)
plot(pcc_coef_var)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Coeficiente de Variação")
# Variancia Interespecifica
resulta_2_var_inter<-as.vector(resulta_2[,8])
pcc_var_inter<-pcc(dados3_25mar16,resulta_2_var_inter,nboot=50)
plot(pcc_var_inter)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Variância Interespecífica")
# Coeficiente de Variacao Interespecifica
resulta_2_coef_var_inter<-as.vector(resulta_2[,9])
pcc_coef_var_inter<-pcc(dados3_25mar16,resulta_2_coef_var_inter,nboot=50)
plot(pcc_coef_var_inter)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com \nCoeficiente de Variação Interespecífica")

#### ECDF
plot(ecdf(resulta_2[,2]),pch=20,xlab="Dif_Média_xi0",ylab="Proporção cumulativa",main="")
plot(ecdf(resulta_2[,9]),pch=20,xlab="Coef_Var_Inter",ylab="Proporção cumulativa",main="")
