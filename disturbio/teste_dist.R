
teste1<-simula.neutra.step(10,1,100,0.1,c(1,3,5),0.5,10,1)
teste1

teste2<-simula.neutra.step(10,1,100,0.01,c(1,3,5),0.1,2,1) #gerar erro quando ha mais posicoes de disturbio do que ciclos
teste2.1<-simula.neutra.step(10,1,100,0.01,c(0,1,2),0.1,2,1) #gerar erro quando ha mais posicoes de disturbio do que ciclos

teste3<-simula.neutra.step(10,1,100,0.01,c(1,3,5),0.1,100,100)
teste3

teste4<-simula.neutra.step(10,1,100,0.01,c(1,3,5),0.8,1000,1)
str(teste4)

teste5<-simula.neutra.step(10,1,100,0.01,c(1,3,5),0.8,1000,1000)
str(teste5)

teste6<-simula.neutra.step(400,10,4000,0.0001,c(1,10,20,30,40,50),0.9,50,10)
teste6

teste7<-simula.neutra.step(400,10,4000,0.0001,c(1,10,20,30,40,50),0.9,1000,10)
teste7

teste8<-simula.neutra.step(400,10,4000,0.0001,c(1,100,200,300,400,500,600,700,800,900,1000,10000),0.9,10000,10) # o dp baixo está fazendo com que as espécies não mudem...
str(teste8)

teste9<-simula.neutra.step(400,10,40000,0.0001,c(1,100,200,300,400,500,600,700,800,900,1000,10000),0.9,10000,10)
str(teste9)
head(teste9)

teste10<-simula.neutra.step(400,10,40000,0.1,c(1,100,200,300,400,500,600,700,800,900,1000,10000),0.9,10000,10)
str(teste10)
head(teste10)

teste11<-simula.neutra.step(400,10,40000,0.5,c(1,100,200,300,400,500,600,700,800,900,1000,10000),0.9,10000,10)
str(teste11)
head(teste11)
tail(teste11)

teste12<-simula.neutra.step(10,2,40,0.5,c(0,1,3,5),0.5,100,10) #OLHAR NUMERO DE MORTES... IDENTIFICAR PROBLEMA E CONSERTAR
teste12

teste13<-simula.neutra.step(10,2,40,0.5,c(0,1,3,5),0.5,100,1)
teste13

teste13.1<-simula.neutra.step(10,2,40,0.5,c(0,1,3,5),0.5,100,1) #teste realizado no branch 'ciclo'
teste13.1

teste14<-simula.neutra.step(10,2,40,0.5,c(1,3,5),0.5,100,1) #teste realizado no branch 'ciclo'
teste14

