############### SIMULACOES RODADAS EM 27 DE ABRIL DE 2016 ###############
###### O data frame oriundo do hipercubo apresenta 1000 linhas (1000 simulacoes).
simulacoes_27abr16<-list()
for(i in 1:250){
    simulacoes_27abr16<-simula_input(dados3_27abr16[((4*i)-3):(4*i),])
    save(simulacoes_27abr16,file=(paste("sim_27abr16_",i,".RData",sep="")))
}
