teste_analise1<-simula_externa_input(input=matrix(data=c(20,1,40,1,1,0,100,10,10,2,40,1,1,0,100,10),ncol=8,byrow=T))
teste_analise2<-simula_externa_input(input=data.frame(matrix(data=c(20,1,40,1,c(1,10,100),0.5,100,10,10,2,40,1,0,0,100,10),ncol=8,byrow=T))) #nao consegue receber os valores de dist.pos... PQP
teste_analise3<-simula_externa_input(input=data.frame(matrix(data=c(20,1,40,1,0,0,100,10,10,2,40,1,0,0,100,10),ncol=8,byrow=T)))
teste_analise4<-simula_externa_input(input=matrix(data=c(100,1,1000,0.5,0,0,100,10,50,2,1000,1,0,0,100,10),ncol=8,byrow=T))
teste_analise5<-simula_externa_input(input=matrix(data=c(100,1,1000,0.5,0,0,100,10,50,2,1000,1,101,0,100,10),ncol=8,byrow=T)) #deu erro, como deveria (:
teste_analise6<-simula_externa_input(input=matrix(data=c(100,1,1000,0.5,0,0,100,10,50,2,1000,1,101,0.5,100,10),ncol=8,byrow=T)) #deu erro, como deveria (:

input<-matrix(data=c(20,10,1,2,40,40,1,1,0,0,0,0,100,100,10,10),nrow=2)
input<-data.frame(input)
input

input<-matrix(data=c(20,10,250,500,10000,10000,1,1,0,0,0,0,100,100,10,10),nrow=2)
input2<-data.frame(input2)
input2

teste_analise7<-simula_externa_input(input=data.frame(c(50,11),c(200,200),c(1,1),c(0,0),c(0,0),c(100,100),c(10,10)),Jt=100)
teste_analise7<-simula_externa_input(input=data.frame(c(20,10),c(5000,5000),c(1,1),c(0,0),c(0,0),c(100,100),c(10,10)))

S=10
j=500
X=10000
dp=0.5
ciclo=100
step=10

