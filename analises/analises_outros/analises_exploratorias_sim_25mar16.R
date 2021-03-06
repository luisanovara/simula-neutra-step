# Analises exploratorias das simulacoes de 25mar16

colnames(dados3_25mar16)<-c("S","xi0","dp","dist.pos","dist.int")

#### Boxplot
# fazer?

#### Scatterplot (fiz apenas para o output original - 4 momentos - e o coeficiente de Pearson geral)
## Media
# Media ~ S
plot(dados3_25mar16[,1],simulacoes_output_derivado[,1],pch=20,xlab="S",ylab="Media")
modelo_media_S<-lm(simulacoes_output_derivado[,1]~dados3_25mar16[,1])
abline(modelo_media_S)
# Media ~ xi0
plot(dados3_25mar16[,2],simulacoes_output_derivado[,1],pch=20,xlab="xi0",ylab="Media")
modelo_media_xi0<-lm(simulacoes_output_derivado[,1]~dados3_25mar16[,2])
abline(modelo_media_xi0)
# Media ~ dp
plot(dados3_25mar16[,3],simulacoes_output_derivado[,1],pch=20,xlab="dp",ylab="Media")
modelo_media_dp<-lm(simulacoes_output_derivado[,1]~dados3_25mar16[,3])
abline(modelo_media_dp)
# Media ~ dist.pos
plot(dados3_25mar16[,4],simulacoes_output_derivado[,1],pch=20,xlab="dist.pos",ylab="Media")
modelo_media_dist.pos<-lm(simulacoes_output_derivado[,1]~dados3_25mar16[,4])
abline(modelo_media_dist.pos)
# Media ~ dist.int
plot(dados3_25mar16[,5],simulacoes_output_derivado[,1],pch=20,xlab="dist.int",ylab="Media")
modelo_media_dist.int<-lm(simulacoes_output_derivado[,1]~dados3_25mar16[,5])
abline(modelo_media_dist.int)
## Variancia
# Variancia ~ S
plot(dados3_25mar16[,1],simulacoes_output_derivado[,4],pch=20,xlab="S",ylab="Variancia")
modelo_variancia_S<-lm(simulacoes_output_derivado[,4]~dados3_25mar16[,1])
abline(modelo_variancia_S)
# Variancia ~ xi0
plot(dados3_25mar16[,2],simulacoes_output_derivado[,4],pch=20,xlab="xi0",ylab="Variancia")
modelo_variancia_xi0<-lm(simulacoes_output_derivado[,4]~dados3_25mar16[,2])
abline(modelo_variancia_xi0)
# Variancia ~ dp
plot(dados3_25mar16[,3],simulacoes_output_derivado[,4],pch=20,xlab="dp",ylab="Variancia")
modelo_variancia_dp<-lm(simulacoes_output_derivado[,4]~dados3_25mar16[,3])
abline(modelo_variancia_dp)
# Variancia ~ dist.pos
plot(dados3_25mar16[,4],simulacoes_output_derivado[,4],pch=20,xlab="dist.pos",ylab="Variancia")
modelo_variancia_dist.pos<-lm(simulacoes_output_derivado[,4]~dados3_25mar16[,4])
abline(modelo_variancia_dist.pos)
# Variancia ~ dist.int
plot(dados3_25mar16[,5],simulacoes_output_derivado[,4],pch=20,xlab="dist.int",ylab="Variancia")
modelo_variancia_dist.int<-lm(simulacoes_output_derivado[,4]~dados3_25mar16[,5])
abline(modelo_variancia_dist.int)
## Assimetria
# Assimetria ~ S
plot(dados3_25mar16[,1],simulacoes_output_derivado[,6],pch=20,xlab="S",ylab="Assimetria")
modelo_assimetria_S<-lm(simulacoes_output_derivado[,6]~dados3_25mar16[,1])
abline(modelo_assimetria_S)
# Assimetria ~ xi0
plot(dados3_25mar16[,2],simulacoes_output_derivado[,6],pch=20,xlab="xi0",ylab="Assimetria")
modelo_assimetria_xi0<-lm(simulacoes_output_derivado[,6]~dados3_25mar16[,2])
abline(modelo_assimetria_xi0)
# Assimetria ~ dp
plot(dados3_25mar16[,3],simulacoes_output_derivado[,6],pch=20,xlab="dp",ylab="Assimetria")
modelo_assimetria_dp<-lm(simulacoes_output_derivado[,6]~dados3_25mar16[,3])
abline(modelo_assimetria_dp)
# Assimetria ~ dist.pos
plot(dados3_25mar16[,4],simulacoes_output_derivado[,6],pch=20,xlab="dist.pos",ylab="Assimetria")
modelo_assimetria_dist.pos<-lm(simulacoes_output_derivado[,6]~dados3_25mar16[,4])
abline(modelo_assimetria_dist.pos)
# Assimetria ~ dist.int
plot(dados3_25mar16[,5],simulacoes_output_derivado[,6],pch=20,xlab="dist.int",ylab="Assimetria")
modelo_assimetria_dist.int<-lm(simulacoes_output_derivado[,6]~dados3_25mar16[,5])
abline(modelo_assimetria_dist.int)
## Curtose
# Curtose ~ S
plot(dados3_25mar16[,1],simulacoes_output_derivado[,7],pch=20,xlab="S",ylab="Excesso de Curtose")
modelo_curtose_S<-lm(simulacoes_output_derivado[,7]~dados3_25mar16[,1])
abline(modelo_curtose_S)
# Curtose ~ xi0
plot(dados3_25mar16[,2],simulacoes_output_derivado[,7],pch=20,xlab="xi0",ylab="Excesso de Curtose")
modelo_curtose_xi0<-lm(simulacoes_output_derivado[,7]~dados3_25mar16[,2])
abline(modelo_curtose_xi0)
# Curtose ~ dp
plot(dados3_25mar16[,3],simulacoes_output_derivado[,7],pch=20,xlab="dp",ylab="Excesso de Curtose")
modelo_curtose_dp<-lm(simulacoes_output_derivado[,7]~dados3_25mar16[,3])
abline(modelo_curtose_dp)
# Curtose ~ dist.pos
plot(dados3_25mar16[,4],simulacoes_output_derivado[,7],pch=20,xlab="dist.pos",ylab="Excesso de Curtose")
modelo_curtose_dist.pos<-lm(simulacoes_output_derivado[,7]~dados3_25mar16[,4])
abline(modelo_curtose_dist.pos)
# Curtose ~ dist.int
plot(dados3_25mar16[,5],simulacoes_output_derivado[,7],pch=20,xlab="dist.int",ylab="Excesso de Curtose")
modelo_curtose_dist.int<-lm(simulacoes_output_derivado[,7]~dados3_25mar16[,5])
abline(modelo_curtose_dist.int)
## Coeficiente de Pearson
# Coeficiente de Pearson ~ S
plot(dados3_25mar16[,1],simulacoes_output_derivado[,8],pch=20,xlab="S",ylab="Coeficiente de Pearson")
modelo_pearson_S<-lm(simulacoes_output_derivado[,8]~dados3_25mar16[,1])
abline(modelo_pearson_S)
# Coeficiente de Pearson ~ xi0
plot(dados3_25mar16[,2],simulacoes_output_derivado[,8],pch=20,xlab="xi0",ylab="Coeficiente de Pearson")
modelo_pearson_xi0<-lm(simulacoes_output_derivado[,8]~dados3_25mar16[,2])
abline(modelo_pearson_xi0)
# Coeficiente de Pearson ~ dp
plot(dados3_25mar16[,3],simulacoes_output_derivado[,8],pch=20,xlab="dp",ylab="Coeficiente de Pearson")
modelo_pearson_dp<-lm(simulacoes_output_derivado[,8]~dados3_25mar16[,3])
abline(modelo_pearson_dp)
# Coeficiente de Pearson ~ dist.pos
plot(dados3_25mar16[,4],simulacoes_output_derivado[,8],pch=20,xlab="dist.pos",ylab="Coeficiente de Pearson")
modelo_pearson_dist.pos<-lm(simulacoes_output_derivado[,8]~dados3_25mar16[,4])
abline(modelo_pearson_dist.pos)
# Coeficiente de Pearson ~ dist.int
plot(dados3_25mar16[,5],simulacoes_output_derivado[,8],pch=20,xlab="dist.int",ylab="Coeficiente de Pearson")
modelo_pearson_dist.int<-lm(simulacoes_output_derivado[,8]~dados3_25mar16[,5])
abline(modelo_pearson_dist.int)

#### ECDF
plot(ecdf(simulacoes_output_derivado[,1]),pch=20,xlab="Média",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,2]),pch=20,xlab="Dif_Média_xi0",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,3]),pch=20,xlab="Razão_Média_xi0",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,4]),pch=20,xlab="Variância",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,5]),pch=20,xlab="Coef_Variação",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,6]),pch=20,xlab="Assimetria",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,7]),pch=20,xlab="Excesso de Curtose",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,8]),pch=20,xlab="Coeficiente de Pearson",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,9]),pch=20,xlab="Média_Sp",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,10]),pch=20,xlab="Variância_Inter",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,11]),pch=20,xlab="Coef_Variação_Inter",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,12]),pch=20,xlab="Assimetria_Sp",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,13]),pch=20,xlab="Excesso_Curtose_Sp",ylab="Proporção cumulativa",main="")
plot(ecdf(simulacoes_output_derivado[,14]),pch=20,xlab="Coef_Pearson_Sp",ylab="Proporção cumulativa",main="")

#### PCC
require(sensitivity)
par(mar=c(5,6,4,2))
# Media
simulacoes_output_derivado_media<-as.vector(simulacoes_output_derivado[,1])
pcc_media<-pcc(dados3_25mar16,simulacoes_output_derivado_media,nboot=50)
plot(pcc_media)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Média")
# Dif_Media_xi0
simulacoes_output_derivado_media_xi0<-as.vector(simulacoes_output_derivado[,2])
pcc_media_xi0<-pcc(dados3_25mar16,simulacoes_output_derivado_media_xi0,nboot=50)
plot(pcc_media_xi0)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Dif_Média_xi0")
# Razao_Media_xi0
simulacoes_output_derivado_raz_media_xi0<-as.vector(simulacoes_output_derivado[,3])
pcc_raz_media_xi0<-pcc(dados3_25mar16,simulacoes_output_derivado_raz_media_xi0,nboot=50)
plot(pcc_raz_media_xi0)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com \nRazão_Média_xi0")
# Variancia
simulacoes_output_derivado_variancia<-as.vector(simulacoes_output_derivado[,4])
pcc_variancia<-pcc(dados3_25mar16,simulacoes_output_derivado_variancia,nboot=50)
plot(pcc_variancia)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Variância")
# Coeficiente de Variacao
simulacoes_output_derivado_coef_var<-as.vector(simulacoes_output_derivado[,5])
pcc_coef_var<-pcc(dados3_25mar16,simulacoes_output_derivado_coef_var,nboot=50)
plot(pcc_coef_var)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com \nCoeficiente de Variação")
# Assimetria
simulacoes_output_derivado_assimetria<-as.vector(simulacoes_output_derivado[,6])
nan_assimetria<-which(is.nan(simulacoes_output_derivado_assimetria))
pcc_assimetria<-pcc(dados3_25mar16[-nan_assimetria,],simulacoes_output_derivado_assimetria[-nan_assimetria],nboot=50)
plot(pcc_assimetria)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com Assimetria")
# Excesso de Curtose
simulacoes_output_derivado_curtose<-as.vector(simulacoes_output_derivado[,7])
nan_curtose<-which(is.nan(simulacoes_output_derivado_curtose))
pcc_curtose<-pcc(dados3_25mar16[-nan_curtose,],simulacoes_output_derivado_curtose[-nan_curtose],nboot=50)
plot(pcc_curtose)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com \nExcesso de Curtose")
# Pearson
simulacoes_output_derivado_pearson<-as.vector(simulacoes_output_derivado[,8])
nan_pearson<-which(is.nan(simulacoes_output_derivado_pearson))
pcc_pearson<-pcc(dados3_25mar16[-nan_pearson,],simulacoes_output_derivado_pearson[-nan_pearson],nboot=50)
plot(pcc_pearson)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com \nCoeficiente de Pearson")
# Media das medias/sp
simulacoes_output_derivado_media_medias<-as.vector(simulacoes_output_derivado[,9])
pcc_media_medias<-pcc(dados3_25mar16,simulacoes_output_derivado_media_medias,nboot=50)
plot(pcc_media_medias)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com \nMédia das Médias/Sp")
# Variancia Interespecifica
simulacoes_output_derivado_var_inter<-as.vector(simulacoes_output_derivado[,10])
pcc_var_inter<-pcc(dados3_25mar16,simulacoes_output_derivado_var_inter,nboot=50)
plot(pcc_var_inter)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com \nVariância Interespecífica")
# Coeficiente de Variacao Interespecifica
simulacoes_output_derivado_coef_var_inter<-as.vector(simulacoes_output_derivado[,11])
pcc_coef_var_inter<-pcc(dados3_25mar16,simulacoes_output_derivado_coef_var_inter,nboot=50)
plot(pcc_coef_var_inter)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com \nCoeficiente de Variação Interespecífica")
# Assimetria/Sp
simulacoes_output_derivado_assimetria_sp<-as.vector(simulacoes_output_derivado[,12])
nan_assimetria_sp<-which(is.nan(simulacoes_output_derivado_assimetria_sp))
pcc_assimetria_sp<-pcc(dados3_25mar16[-nan_assimetria_sp,],simulacoes_output_derivado_assimetria_sp[-nan_assimetria_sp],nboot=50)
plot(pcc_assimetria_sp)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com \nAssimetria das Médias/Sp")
# Excesso de Curtose/Sp
simulacoes_output_derivado_curtose_sp<-as.vector(simulacoes_output_derivado[,13])
nan_curtose_sp<-which(is.nan(simulacoes_output_derivado_curtose_sp))
pcc_curtose_sp<-pcc(dados3_25mar16[-nan_curtose_sp,],simulacoes_output_derivado_curtose_sp[-nan_curtose_sp],nboot=50)
plot(pcc_curtose_sp)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com \nExcesso de Curtose das Médias/Sp")
# Pearson/Sp
simulacoes_output_derivado_pearson_sp<-as.vector(simulacoes_output_derivado[,14])
nan_pearson_sp<-which(is.nan(simulacoes_output_derivado_pearson_sp))
pcc_pearson_sp<-pcc(dados3_25mar16[-nan_pearson_sp,],simulacoes_output_derivado_pearson_sp[-nan_pearson_sp],nboot=50)
plot(pcc_pearson_sp)
abline(h=0,lty=2)
title(xlab="Parâmetros",ylab="Correlação Parcial com \nCoeficiente de Pearson das Médias/Sp")
