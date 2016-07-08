#### Todas os objetos do tipo simulacoes_25mar16_# serviram de entrada para a funcao simula_output, que tem como saida os 4 momentos da distribuicao das estrategias de vida gerais e especificas no tempo de meia vida das especies. Esses resultados foram salvos em um objeto chamado simulacoes_25mar16_output (uma lista de 100 elementos).

simulacoes_27mai16_output<-c()
for(i in seq(1,1000,8)[25:125][1:8]){
  load(paste("resultados",i,"_",i+7,".RData",sep=""))
  simulacoes_27mai16_output[(i):(i+7)]<-simula_output(get(ls()[2]))
  rm(list=ls()[2])
}
save(simulacoes_27mai16_output,file="simulacoes_27mai16_output.RData")
simulacoes_output<-simulacoes_27mai16_output

### Isolando os momentos "gerais" de cada simulacao e guardando em uma matriz

resultados_geral<-matrix(nrow=length(simulacoes_output),ncol=6,dimnames=list(c(1:length(simulacoes_output)),c("Abundancia","Media","Moda","Variancia","Assimetria","Excesso_Curtose")))
for(i in 1:length(simulacoes_output)){
  resultados_geral[i,]<-simulacoes_output[[i]][1,]
}
resulta_geral<-resultados_geral[,-1]
resulta_geral<-as.data.frame(resulta_geral)

### Gerando outros outputs que podem ser derivados dos outputs originais (4 momentos)

#### Outputs gerais
### Output #6: coeficiente de Pearson
resulta_geral$Coef_Pearson<- (resulta_geral[,4])^2 - resulta_geral[,5]
### Output #7: coeficiente de variacao das estrategias na comunidade
resulta_geral$Coeficiente_Variacao<-(sqrt(resulta_geral[,3]))/resulta_geral[,1]
### Output #8: diferenca entre a estrategia de vida final e a inicial
resulta_geral$Dif_Media_xi0<-resulta_geral[,1]-dados3_27mai16[,2]
### Output #9: diferenca entre a estrategia de vida final e a inicial / inicial
resulta_geral$Razao_Media_xi0<-(resulta_geral[,1]-dados3_27mai16[,2])/dados3_27mai16[,2]

#### Outputs especificos
### Output #10: media das medias de estrategia por especie 
media_medias.sp<-c()
for(i in 1:length(simulacoes_output)){  
  media_medias.sp[i]<-mean(simulacoes_output[[i]][-1,2])
}
resulta_geral$Media_Medias_Sp<-media_medias.sp
### Output #11: moda das medias de estrategia por especie 
moda_medias.sp<-c()
for(i in 1:length(simulacoes_output)){  
  moda_medias.sp[i]<-simulacoes_output[[i]][-1,2][max(table(simulacoes_output[[i]][-1,2]))]
}
resulta_geral$Moda_Medias_Sp<-moda_medias.sp
### Output #12: variancia das medias de estrategia por especie (variancia interespecifica)
var_inter<-c()
for(i in 1:length(simulacoes_output)){
  var_inter[i]<-var(simulacoes_output[[i]][-1,2])
}
resulta_geral$Variancia_Inter<-var_inter
### Output #13: assimetria das medias de estrategia por especie 
ass_medias.sp<-c()
for(i in 1:length(simulacoes_output)){  
  ass_medias.sp[i]<-skewness(simulacoes_output[[i]][-1,2])
}
resulta_geral$Assimetria_Medias_Sp<-ass_medias.sp
### Output #14: excesso de curtose das medias de estrategia por especie 
curt_medias.sp<-c()
for(i in 1:length(simulacoes_output)){  
  curt_medias.sp[i]<-kurtosis(simulacoes_output[[i]][-1,2])-3
}
resulta_geral$Excesso_Curtose_Medias_Sp<-curt_medias.sp
### Output #15: media do coeficiente de Pearson das especies
media_coef_pearson <- ass_medias.sp^2 - curt_medias.sp
resulta_geral$Coef_Pearson_Sp<-media_coef_pearson
### Output #16: coeficiente de variacao das medias das estrategias por especie (coeficiente de variacao interespecifica)
resulta_geral$Coeficiente_Variacao_Inter<-(sqrt(var_inter))/media_medias.sp
### Output #17:
### Output #18:

### Organizando o data frame 
simulacoes_output_derivado<-cbind(resulta_geral[,1],resulta_geral[,8],resulta_geral[,9],resulta_geral[,2],resulta_geral[,3],resulta_geral[,7],resulta_geral[,4],resulta_geral[,5],resulta_geral[,6],resulta_geral[,10],resulta_geral[,11],resulta_geral[,12],resulta_geral[,16],resulta_geral[,13],resulta_geral[,14],resulta_geral[,15])
colnames(simulacoes_output_derivado)<-c("Média","Dif_Média_xi0","Razão_Média_xi0","Moda","Variância","Coeficiente_Variação","Assimetria","Excesso_Curtose","Coeficiente_Pearson","Média_Médias_Sp","Moda_Médias_Sp","Variância_Inter","Coeficiente_Variação_Inter","Assimetria_Sp","Excesso_Curtose_Sp","Coeficiente_Pearson_Sp")
head(simulacoes_output_derivado)
save(simulacoes_output_derivado,file="simulacoes_output_derivado.RData")

