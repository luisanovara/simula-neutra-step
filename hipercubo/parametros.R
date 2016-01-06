#################### RIQUEZA E ABUNDANCIA ####################

# dados BCI
### area = 10 hectares
### total de individuos com DAP maior ou igual a 10 cm = 4510
### riqueza = 170

require(vegan)
data(BCI)
dim(BCI[1:10,apply(BCI[1:10,],2,sum)!=0])[2] #numero de especies encontradas em 10 plots (10 hectares)
sum(BCI[1:10,apply(BCI[1:10,],2,sum)!=0]) #abundancia total nos 10 plots

# dados Amazonia (Steege 2013)
### area = 
### total de individuos com DAP maior ou igual a 10 cm = 
### riqueza = 

# dados Mata Atlantica (TREECO)
### area = 10 hectares
### total de individuos com DAP maior ou igual a 10 cm = 4817 e 4920
### riqueza =  108 e 325

mata<-read.table("tabmata.csv",header=T,sep=",")
mata10<-mata[mata$effort_ha==10&(mata$dbh_cutoff=="DBH>=10.0cm"|mata$dbh_cutoff=="DBH>10.0cm"),]
head(mata10)
mata10

# dados Inventario Floristico Rio Grande do Sul

# dados Sta Barbara 

stababi<-read.table("stababi.csv",header=T,sep=",")
str(stababi)
head(stababi)
stababi10<-stababi[stababi$dbh>=10,]
str(stababi10)

#### CONCLUSAO
### Fixar abundancia total em 5000 individuos
### Variar a riqueza (hipercubo) de 5 a 500 especies

#################### TAXA DE MUTACAO ####################

# Rosindell 2015
### taxa de mutacao variou de 10e-2 a 10e-7
#### CONCLUSAO
### Variar taxa de mutacao no hipercubo de 0 a 10e-6 


#################### NUMERO DE PROPAGULOS ####################
#################### TOTAL POR INDIVIDUO #####################


