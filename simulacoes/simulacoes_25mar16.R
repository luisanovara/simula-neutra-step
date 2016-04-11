############### SIMULACOES RODADAS EM 25 DE MARCO DE 2016 ###############
###### O data frame oriundo do hipercubo apresenta 100 linhas (100 simulacoes). Irei separa-lo em conjuntos de 3 simulacoes. Espero que cada conjunto demore, em media, 1h para rodar.

simulacoes_25mar16_1<-simula_input(dados3_25mar16[1:3,],Jm=5000,ciclos=100000,steps=100)
save(simulacoes_25mar16_1,file="sim_25mar16_1.RData")

simulacoes_25mar16_2<-simula_input(dados3_25mar16[4:6,],Jm=5000,ciclos=100000,steps=100)
save(simulacoes_25mar16_2,file="sim_25mar16_2.RData")

sim_1_output<-simula_output(simulacoes_25mar16_1)
sim_2_output<-simula_output(simulacoes_25mar16_2)
sim_1_2_output<-c(sim_1_output,sim_2_output)
save(sim_1_output,sim_2_output,sim_1_2_output,file="sim_1_2_output.RData")
save(sim_1_2_output,file="sim_1_2_output_reduzido.RData")
