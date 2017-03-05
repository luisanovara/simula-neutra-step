#### Todas os objetos do tipo simulacoes_25mar16_# serviram de entrada para a funcao simula_output, que tem como saida os 4 momentos da distribuicao das estrategias de vida gerais e especificas no tempo de meia vida das especies. Esses resultados foram salvos em um objeto chamado simulacoes_25mar16_output (uma lista de 100 elementos).

# simulacoes_27mai16_output<-c()
# for(i in seq(1,1000,8)[25:125][1:8]){
#   load(paste("resultados",i,"_",i+7,".RData",sep=""))
#   simulacoes_27mai16_output[(i):(i+7)]<-simula_output(get(ls()[2]))
#   rm(list=ls()[2])
# }
save(simulacoes_27mai16_output,file="simulacoes_27mai16_output.RData")
simulacoes_output<-simulacoes_27mai16_output

### Isolando os momentos "gerais" de cada simulacao e guardando em uma matriz

resultados_geral<-matrix(nrow=length(simulacoes_output),ncol=7,dimnames=list(c(1:length(simulacoes_output)),c("Abundancia","Media","Moda","Variancia","Assimetria","Excesso_Curtose")))
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
resulta_geral$Dif_Media_xi0<-resulta_geral[,1]-dados3_27mai16[,1]
### Output #9: diferenca entre a estrategia de vida final e a inicial / inicial
resulta_geral$Razao_Media_xi0<-(resulta_geral[,1]-dados3_27mai16[,1])/dados3_27mai16[,1]

### Organizando o data frame 
simulacoes_27mai16_output_derivado<-cbind(resulta_geral[,1],resulta_geral[,8],resulta_geral[,9],resulta_geral[,2],resulta_geral[,3],resulta_geral[,7],resulta_geral[,4],resulta_geral[,5],resulta_geral[,6])
colnames(simulacoes_27mai16_output_derivado)<-c("Media","Dif_Media_xi0","Razao_Media_xi0","Moda","Variância","Coeficiente_Variação","Assimetria","Excesso_Curtose","Coeficiente_Pearson")
head(simulacoes_27mai16_output_derivado)
save(simulacoes_27mai16_output_derivado,file="simulacoes_27mai16_output_derivado.RData")

