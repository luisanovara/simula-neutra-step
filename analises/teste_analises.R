teste_analise1<-simula_externa_input(input=matrix(data=c(20,1,40,1,1,0,100,10,10,2,40,1,1,0,100,10),ncol=8,byrow=T))
teste_analise2<-simula_externa_input(input=data.frame(matrix(data=c(20,1,40,1,c(1,10,100),0.5,100,10,10,2,40,1,0,0,100,10),ncol=8,byrow=T))) #nao consegue receber os valores de dist.pos... PQP
teste_analise3<-simula_externa_input(input=data.frame(matrix(data=c(20,1,40,1,0,0,100,10,10,2,40,1,0,0,100,10),ncol=8,byrow=T)))
teste_analise4<-simula_externa_input(input=matrix(data=c(100,1,1000,0.5,0,0,100,10,50,2,1000,1,0,0,100,10),ncol=8,byrow=T))
teste_analise5<-simula_externa_input(input=matrix(data=c(100,1,1000,0.5,0,0,100,10,50,2,1000,1,101,0,100,10),ncol=8,byrow=T)) #deu erro, como deveria (:
teste_analise6<-simula_externa_input(input=matrix(data=c(100,1,1000,0.5,0,0,100,10,50,2,1000,1,101,0.5,100,10),ncol=8,byrow=T)) #deu erro, como deveria (:

input<-matrix(data=c(20,10,1,2,40,40,1,1,0,0,0,0,100,100,10,10),nrow=2)
input<-data.frame(input)
input

input<-matrix(data=c(20,10,40,40,1,1,0,0,0,0),nrow=2)
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

#### novos testes
teste_analise8<-simula_externa_input(input=dados1,Jt=100)
teste_analise9<-simula_externa_input(input=data.frame(c(100,100),c(10000,10000),c(0.5,0.5),c(1,1),c(0.2,0.2)),Jt=1000)
teste_analise10<-simula_externa_input(input=data.frame(c(100,100),c(10000,10000),c(2,2),c(1,1),c(0.2,0.2)),Jt=1000) #dá muito o problema
teste_analise11<-simula_externa_input(input=data.frame(c(10,10),c(1000,1000),c(2,2),c(1,1),c(0.2,0.2)),Jt=100)
teste_analise12<-simula_externa_input(input=data.frame(c(5,5),c(1000,1000),c(2,2),c(1,1),c(0.2,0.2)),Jt=100) #dá mais o problema do que o 11
teste_analise13<-simula_externa_input(input=data.frame(c(100,100),c(10000,10000),c(5,5),c(1,1),c(0.2,0.2)),Jt=1000) #dá muito o problema 

teste_analise14<-simula_externa_input(input=data.frame(c(5,5),c(500,500),c(0.5,0.5),c(1,1),c(0.2,0.2)),Jt=50) #não deu problema
teste_analise15<-simula_externa_input(input=data.frame(c(5,5),c(500,500),c(0.5,0.5),c(1,1),c(0.2,0.2)),Jt=100) #deu problema às vezes 
teste_analise16<-simula_externa_input(input=data.frame(c(5,5),c(500,500),c(0.5,0.5),c(1,1),c(0.2,0.2)),Jt=250) #deu problema sempre
teste_analise17<-simula_externa_input(input=data.frame(c(5,5),c(750,750),c(0.5,0.5),c(1,1),c(0.2,0.2)),Jt=250) #deu problema sempre
teste_analise18<-simula_externa_input(input=data.frame(c(5,5),c(1000,1000),c(0.5,0.5),c(1,1),c(0.2,0.2)),Jt=50) #não deu problema
teste_analise19<-simula_externa_input(input=data.frame(c(5,5),c(2000,2000),c(0.5,0.5),c(1,1),c(0.2,0.2)),Jt=50) #não deu problema
teste_analise20<-simula_externa_input(input=data.frame(c(5,5),c(10000,10000),c(0.5,0.5),c(1,1),c(0.2,0.2)),Jt=50) #nao deu problema

##
teste_analise21<-simula_externa_input(input=data.frame(c(5,5),c(12255,10000),c(0.5,0.5),c(1,1),c(0.2,0.2)),Jt=50) #problema do valor de X/J: resolvido
teste_analise22<-simula_externa_input(input=data.frame(c(6,5),c(12250,10000),c(0.5,0.5),c(1,1),c(0.2,0.2)),Jt=50) #problema do valor de S: ok
teste_analise23<-simula_externa_input(input_hipercubo=input,Jt=50) #problema da nomenclatura do input_hipercubo
teste_analise24<-simula_externa_input(input_hipercubo=round(get.data(uncoupled_hipercubo)),Jt=50) #muito lento, nao roda... o jeito é resolver o problema da nomenclatura na propria simula_externa_input.. o que eh melhor mesmo, ja que eu quero arredondar alguns valores sorteados pelo hipercubo (nao faz sentido 0.5 especies, por exemplo)
teste_analise25<-simula_externa_input(fun=simula_externa_output,input_hipercubo=input,Jt=50,ciclos=1000,steps=100) #igual a teste_analise14, mas com input embutido. Nao existia problema de input, era soh qual input estava sendo usado..
teste_analise26<-simula_externa_input(input_hipercubo=input,Jt=50) #deu certo

teste_analise27<-simula_externa_input(input_hipercubo=input) #DEU PAU... porque X eh menor do que J.

####### Testes com valores que irei utilizar antes de resolver as issues
# Variando a riqueza de 1 a 50
input=data.frame(c(1,seq(0,50,5)[-1]),rep(5000,11),rep(0.5,11),rep(0,11),rep(0,11))
teste_analise28<-simula_externa_input(input_hipercubo=input,Jt=50) #quando S=1, nao houve variancia entre individuos, o que eh estranho..
# Variando o X de 5000 a 500000
input=data.frame(rep(5,11),seq(5000,500000,length=11),rep(0.5,11),rep(0,11),rep(0,11))
teste_analise29<-simula_externa_input(input_hipercubo=input,Jt=50)
# Variando o dp de 0 a 1
input=data.frame(rep(5,11),rep(5000,11),seq(0,1,length=11),rep(0,11),rep(0,11))
teste_analise30<-simula_externa_input(input_hipercubo=input,Jt=50) #quando dp=0, 0.1 e 0.3, nao houve variancia entre individuos. Mas 0.2 houve
# Variando o dist.int de 0 a 1 (no ciclo 1)
input=data.frame(rep(5,11),rep(5000,11),rep(0.5,11),c(0,rep(1,10)),seq(0,1,length=11))
teste_analise31<-simula_externa_input(input_hipercubo=input,Jt=50)

input=data.frame(rep(100,10),rep(10000,10),rep(0.5,10),rep(1,10),rep(0.5,10))
teste_analise32<-simula_externa_input(input_hipercubo=input,Jt=1000)
input=data.frame(rep(1000,10),rep(100000,10),rep(0.5,10),rep(1,10),rep(0.5,10))
teste_analise33<-simula_externa_input(input_hipercubo=input,Jt=10000)

################ Testes apos reuniao com o Ale ################
## Correcao do problema do ciclo
input=data.frame(c(5,5),c(10,10),c(2,2),c(3,900),c(0.2,0.5))
teste_analise34<-simula_externa_input(input_hipercubo=input,Jm=5000)
input=data.frame(c(5,5),c(10,10),c(2,2),c(1,1),c(0.2,0))
teste_analise34<-simula_externa_input(input_hipercubo=input,Jm=50)

input=data.frame(c(7,11),c(10,10),c(2,2),c(1000,1000),c(0.5,0.5))

## testes 05/03/2016
t1<-simula.neutra.step(S=10,j=10,xi0=10,dp=1,dist.pos=100,dist.int=0.5,ciclo=1000,step=100)
t2<-simula.neutra.step(5,1000,10,2,1,0.2,1000,100)

t3<-simula_input(input,100,1000,100)
t3o<-simula_output(t3)

######## Testes "finais"
input=data.frame(c(500,5),c(1000,10),c(5,2),c(50,1),c(0.5,0))
t4<-simula_input(input,5000,1000,100)
t4o<-simula_output(t4)
input=data.frame(c(5,5),c(100,100),c(5,0.1),c(100,100),c(0.5,0.5))
t5<-simula_input(input,5000,1000,100)
t5o<-simula_output(t5)
