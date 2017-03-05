### Salvando output original

save(simulacoes_25jul16_output,file="simulacoes_25jul16_output.RData")
simulacoes_output<-simulacoes_25jul16_output

### Isolando os momentos "gerais" de cada simulacao e guardando em uma matriz

resulta_geral<-matrix(nrow=length(simulacoes_output),ncol=9,dimnames=list(c(1:length(simulacoes_output)),c("Riq_Inic","Riq_Fin","Abundancia","Media","Mediana","Moda","Variancia","Assimetria","Excesso_Curtose")))
for(i in 1:length(simulacoes_output)){
  resulta_geral[i,]<-simulacoes_output[[i]][1,]
}
resulta_geral<-as.data.frame(resulta_geral)

### Gerando outros outputs que podem ser derivados dos outputs originais (4 momentos)

#### Outputs gerais
### Output #10: perda de especies
resulta_geral$Perda_Sp<- (resulta_geral[,2] - resulta_geral[,1])/resulta_geral[,1]
### Output #11: diferenca entre a media final e a media inicial
resulta_geral$Dif_Media<- resulta_geral[,4] - 10000.5
### Output #12: diferenca entre a media final e a media inicial relativa a media inicial
resulta_geral$Raz_Media<- (resulta_geral[,4] - 10000.5)/10000.5
### Output #13: coeficiente de variacao das estrategias na comunidade
resulta_geral$Coeficiente_Variacao<-(sqrt(resulta_geral[,7]))/resulta_geral[,4]
### Output #14: coeficiente de Pearson
resulta_geral$Coef_Pearson<- (resulta_geral[,8])^2 - resulta_geral[,9]

#### Outputs especificos
### Output #15: media das medias de estrategia por especie 
media_medias.sp<-c()
for(i in 1:length(simulacoes_output)){  
  media_medias.sp[i]<-mean(simulacoes_output[[i]][-1,4])
}
resulta_geral$Media_Medias_Sp<-media_medias.sp

### Organizando o data frame 
simulacoes_25jul16_output_derivado<-cbind(resulta_geral[,1],resulta_geral[,2],resulta_geral[,10],resulta_geral[,3],resulta_geral[,4],resulta_geral[,11],resulta_geral[,12],resulta_geral[,5],resulta_geral[,6],resulta_geral[,7],resulta_geral[,13],resulta_geral[,8],resulta_geral[,9],resulta_geral[,14],resulta_geral[,15])
colnames(simulacoes_25jul16_output_derivado)<-c("Riq_Inic","Riq_Fin","Perda_Sp","Abundancia","Media","Dif_Media","Raz_Media","Mediana","Moda","Variancia","Coeficiente_Variacao","Assimetria","Excesso_Curtose","Coeficiente_Pearson","Media_Sp")
head(simulacoes_25jul16_output_derivado)
save(simulacoes_25jul16_output_derivado,file="simulacoes_25jul16_output_derivado.RData")

## Script para extrair info sobre todos os individuos no tempo nger

prop_nger_geral <- matrix(ncol=1000,nrow=5500)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados25jul16-",i,"_",i+7,".RData",sep=""))
  x<-resultados
  prop_nger_8 <- matrix(ncol=8,nrow=5500)
  for(j in 1:length(x)){
    tam_com <- attributes(x[[j]])$start[[1]]*attributes(x[[j]])$start[[2]]
    mortes_acumuladas <- x[[j]]$n.mortes.cumulativo
    n.mortes_prox_nger <- mortes_acumuladas - (nger*tam_com)
    ngeracao <- which(abs(n.mortes_prox_nger)==min(abs(n.mortes_prox_nger)))
    prop <- as.matrix(x[[j]]$sementes)
    prop_nger <- prop[,ngeracao]
    prop_nger_8[,j] <- c(prop_nger,rep(NA,5500-tam_com))
  }
  prop_nger_geral[,i:(i+7)] <- prop_nger_8
}
