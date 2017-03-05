### Salvando output original

save(simulacoes_27jul16_output,file="simulacoes_27jul16_output.RData")
simulacoes_output<-simulacoes_27jul16_output

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
### Output #16: media das diferencas de medias de estrategia por especie 
media_dif_medias.sp<-c()
for(i in 1:length(simulacoes_output)){
  sp_resta_car<-unlist(strsplit(x=row.names(as.data.frame(simulacoes_output[[i]]))[-1],split="sp"))
  sp_resta_num<-as.numeric(sp_resta_car[seq(2,length(sp_resta_car),2)])
  med_sp_inicial<-unique(rep(seq(1,20000,length.out = dados3_27jul16[i,1]),each=round(5000/dados3_27jul16[i,1])))[sp_resta_num]
  media_dif_medias.sp[i]<-mean(simulacoes_output[[i]][-1,4]-med_sp_inicial)
}
resulta_geral$Media_Dif_Medias_Sp<-media_dif_medias.sp
### Output #17: media das razoes de medias de estrategia por especie 
media_raz_medias.sp<-c()
for(i in 1:length(simulacoes_output)){
  sp_resta_car<-unlist(strsplit(x=row.names(as.data.frame(simulacoes_output[[i]]))[-1],split="sp"))
  sp_resta_num<-as.numeric(sp_resta_car[seq(2,length(sp_resta_car),2)])
  med_sp_inicial<-unique(rep(seq(1,20000,length.out = dados3_27jul16[i,1]),each=round(5000/dados3_27jul16[i,1])))[sp_resta_num]
  media_raz_medias.sp[i]<-mean((simulacoes_output[[i]][-1,4]-med_sp_inicial)/med_sp_inicial)
}
resulta_geral$Media_Raz_Medias_Sp<-media_raz_medias.sp
### Output #18: media das medianas de estrategia por especie 
media_medianas.sp<-c()
for(i in 1:length(simulacoes_output)){  
  media_medianas.sp[i]<-mean(simulacoes_output[[i]][-1,5])
}
resulta_geral$Media_Medianas_Sp<-media_medianas.sp
### Output #19: media das modas de estrategia por especie 
media_modas.sp<-c()
for(i in 1:length(simulacoes_output)){  
  media_modas.sp[i]<-mean(simulacoes_output[[i]][-1,6])
}
resulta_geral$Media_Modas_Sp<-media_modas.sp
### Output #20: media das variancias de estrategia por especie 
media_var.sp<-c()
for(i in 1:length(simulacoes_output)){  
  media_var.sp[i]<-mean(simulacoes_output[[i]][-1,7])
}
resulta_geral$Media_Var_Sp<-media_var.sp
### Output #21: media dos coeficientes de variacao de estrategia por especie 
media_coef_var.sp<-c()
for(i in 1:length(simulacoes_output)){  
  media_coef_var.sp[i]<-mean(simulacoes_output[[i]][-1,7]/simulacoes_output[[i]][-1,4])
}
resulta_geral$Media_Coef_Var_Sp<-media_coef_var.sp
### Output #22: media das assimetrias de estrategia por especie 
media_ass.sp<-c()
for(i in 1:length(simulacoes_output)){  
  media_ass.sp[i]<-mean(simulacoes_output[[i]][-1,8])
}
resulta_geral$Media_Ass_Sp<-media_ass.sp
### Output #23: media das curtoses de estrategia por especie 
media_curt.sp<-c()
for(i in 1:length(simulacoes_output)){  
  media_curt.sp[i]<-mean(simulacoes_output[[i]][-1,9])
}
resulta_geral$Media_Curt_Sp<-media_curt.sp
### Output #24: media dos coeficientes de pearson de estrategia por especie 
media_coef_pear.sp<-c()
for(i in 1:length(simulacoes_output)){  
  media_coef_pear.sp[i]<-mean((simulacoes_output[[i]][-1,8])^2 - simulacoes_output[[i]][-1,9])
}
resulta_geral$Media_Coef_Pear_Sp<-media_coef_pear.sp

### Organizando o data frame 
simulacoes_27jul16_output_derivado<-cbind(resulta_geral[,1],resulta_geral[,2],resulta_geral[,10],resulta_geral[,3],resulta_geral[,4],resulta_geral[,11],resulta_geral[,12],resulta_geral[,5],resulta_geral[,6],resulta_geral[,7],resulta_geral[,13],resulta_geral[,8],resulta_geral[,9],resulta_geral[,14],resulta_geral[,15],resulta_geral[,16],resulta_geral[,17],resulta_geral[,18],resulta_geral[,19],resulta_geral[,20],resulta_geral[,21],resulta_geral[,22],resulta_geral[,23],resulta_geral[,24])
colnames(simulacoes_27jul16_output_derivado)<-c("Riq_Inic","Riq_Fin","Perda_Sp","Abundancia","Media","Dif_Media","Raz_Media","Mediana","Moda","Variancia","Coeficiente_Variacao","Assimetria","Excesso_Curtose","Coeficiente_Pearson","Media_Sp","Dif_Media_Sp","Raz_Media_Sp","Mediana_Sp","Moda_Sp","Variancia_Sp","Coeficiente_Variacao_Sp","Assimetria_Sp","Excesso_Curtose_Sp","Coeficiente_Pearson_Sp")
head(simulacoes_27jul16_output_derivado)
save(simulacoes_27jul16_output_derivado,file="simulacoes_27jul16_output_derivado.RData")

## Script para extrair info sobre todos os individuos no tempo nger

prop_nger_geral <- matrix(ncol=1000,nrow=5500)
for(i in seq(from=1,to=1000,by=8)){ 
  load(paste("resultados27jul16-",i,"_",i+7,".RData",sep=""))
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