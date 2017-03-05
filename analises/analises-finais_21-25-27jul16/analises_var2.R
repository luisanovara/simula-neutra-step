##########################################################
##########################################################
######## Variancia do indice de estrategia de vida #######
##########################################################
##########################################################

# para decidir quais sim devemos pegar:
summary(sqrt(simulacoes_21jul16_output_derivado[,9][simulacoes_21jul16_output_derivado[,4]>5000 & simulacoes_21jul16_output_derivado[,4]<10000])*3)
summary(sqrt(simulacoes_25jul16_output_derivado[,10][simulacoes_25jul16_output_derivado[,5]>5000 & simulacoes_25jul16_output_derivado[,5]<10000])*3)
summary(sqrt(simulacoes_27jul16_output_derivado[,10][simulacoes_27jul16_output_derivado[,5]>5000 & simulacoes_27jul16_output_derivado[,5]<10000])*3)

# Serao utilizadas as simulacoes cuja media final eh maior ou igual a 3000

bat1_indice_var_2 <- bat1_indice_var[-which(simulacoes_21jul16_output_derivado[,4]<3000)]
bat1_indice_dist_2 <- bat1_indice_dist[-which(simulacoes_21jul16_output_derivado[,4]<3000)]
bat2_indice_var_2 <- bat2_indice_var[-which(simulacoes_25jul16_output_derivado[,5]<3000)]
bat2_indice_dist_2 <- bat2_indice_dist[-which(simulacoes_25jul16_output_derivado[,5]<3000)]
bat3_indice_var_2 <- bat3_indice_var[-which(simulacoes_27jul16_output_derivado[,5]<3000)]
bat3_indice_dist_2 <- bat3_indice_dist[-which(simulacoes_27jul16_output_derivado[,5]<3000)]

##########################################################
##########################################################
##### Analises das baterias de simulacao EM SEPARADO #####
##########################################################
##########################################################

##########################################################
######################## Bateria 1 #######################
##########################################################

# bat1_indice_var_2 <- bat1_indice_var[-which(simulacoes_21jul16_output_derivado[,11]>0.05)]
# bat1_indice_dist_2 <- bat1_indice_dist[-which(simulacoes_21jul16_output_derivado[,11]>0.05)]

#plot(bat1_indice_var_2~bat1_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015),pch=20,col=alpha("black",0.2))

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat1_indice_var_2_lin <- lm(bat1_indice_var_2~bat1_indice_dist_2)
bat1_indice_var_2_a_lin_start <- coef(bat1_indice_var_2_lin)[[2]]
bat1_indice_var_2_b_lin_start <- coef(bat1_indice_var_2_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat1_indice_var_2_mm_linearizada <- lm(bat1_indice_dist_2/bat1_indice_var_2~bat1_indice_dist_2) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
bat1_indice_var_2_a_mm_start <- 1/(coef(bat1_indice_var_2_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
bat1_indice_var_2_b_mm_start <- (coef(bat1_indice_var_2_mm_linearizada)[[1]])*(1/(coef(bat1_indice_var_2_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat1_indice_var_2_a_pot_start <- 0.001
bat1_indice_var_2_b_pot_start <- 0.018
#plot(bat1_indice_var_2~bat1_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(pot(x,0.001,0.018),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="black")

# Valores de start para os coeficientes a e b da funcao quadratica usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat1_indice_var_2_a_quadr_start <- -0.00000000000001
bat1_indice_var_2_b_quadr_start <- 0.000000003
bat1_indice_var_2_c_quadr_start <- 0.0011
#plot(bat1_indice_var_2~bat1_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(quadra(x,-0.00000000000001,0.000000003,0.0011),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="red")

# GAMA

## Nulo
bat1_indice_var_2_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat1_indice_var_2_gama_nulo_start <- list(shape=mean(bat1_indice_var_2)^2/var(bat1_indice_var_2),scale=var(bat1_indice_var_2)/mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat1_indice_var_2_gama_nulo_mle <- mle2(bat1_indice_var_2_gama_nulo, start=bat1_indice_var_2_gama_nulo_start,method="Nelder-Mead")
###### ou:
bat1_indice_var_2_gama_nulo_glm <- glm(bat1_indice_var_2~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat1_indice_var_2_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat1_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat1_indice_var_2_gama_shape_dist_lin_start <- list(a=bat1_indice_var_2_a_lin_start,b=bat1_indice_var_2_b_lin_start,scale=var(bat1_indice_var_2)/mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_indice_var_2_gama_shape_dist_lin_mle <- mle2(bat1_indice_var_2_gama_shape_dist_lin, start=bat1_indice_var_2_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat1_indice_var_2_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat1_indice_dist_2+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat1_indice_var_2_gama_scale_dist_lin_start <- list(a=bat1_indice_var_2_a_lin_start,b=bat1_indice_var_2_b_lin_start,shape=mean(bat1_indice_var_2)^2/var(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_indice_var_2_gama_scale_dist_lin_mle <- mle2(bat1_indice_var_2_gama_scale_dist_lin, start=bat1_indice_var_2_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
bat1_indice_var_2_gama_dist_lin_glm <- glm(bat1_indice_var_2~bat1_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao MM sobre shape
# bat1_indice_var_2_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat1_indice_dist_2/(b+bat1_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat1_indice_var_2_gama_shape_dist_mm_start <- list(a=bat1_indice_var_2_a_mm_start,b=bat1_indice_var_2_b_mm_start,scale=var(bat1_indice_var_2)/mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_indice_var_2_gama_shape_dist_mm_mle <- mle2(bat1_indice_var_2_gama_shape_dist_mm, start=bat1_indice_var_2_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat1_indice_var_2_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat1_indice_dist_2/(b+bat1_indice_dist_2))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat1_indice_var_2_gama_scale_dist_mm_start <- list(a=bat1_indice_var_2_a_mm_start,b=bat1_indice_var_2_b_mm_start,shape=mean(bat1_indice_var_2)^2/var(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_indice_var_2_gama_scale_dist_mm_mle <- mle2(bat1_indice_var_2_gama_scale_dist_mm, start=bat1_indice_var_2_gama_scale_dist_mm_start,method="Nelder-Mead")
######## ou:
bat1_indice_var_2_gama_dist_mm_glm <- glm(bat1_indice_dist_2/bat1_indice_var_2~bat1_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre shape
# bat1_indice_var_2_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(bat1_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat1_indice_var_2_gama_shape_dist_pot_start <- list(a=bat1_indice_var_2_a_pot_start,b=bat1_indice_var_2_b_pot_start,scale=var(bat1_indice_var_2)/mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_indice_var_2_gama_shape_dist_pot_mle <- mle2(bat1_indice_var_2_gama_shape_dist_pot, start=bat1_indice_var_2_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat1_indice_var_2_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado<- (a*(bat1_indice_dist_2^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat1_indice_var_2_gama_scale_dist_pot_start <- list(a=bat1_indice_var_2_a_pot_start,b=bat1_indice_var_2_b_pot_start,shape=mean(bat1_indice_var_2)^2/var(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_indice_var_2_gama_scale_dist_pot_mle <- mle2(bat1_indice_var_2_gama_scale_dist_pot, start=bat1_indice_var_2_gama_scale_dist_pot_start,method="Nelder-Mead")
######## ou:
#bat1_indice_var_2_gama_dist_pot_glm <- glm(bat1_indice_dist_2/bat1_indice_var_2~bat1_indice_dist_2,family="Gapota"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre shape
# bat1_indice_var_2_gama_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(bat1_indice_dist_2^2) + b*(bat1_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat1_indice_var_2_gama_shape_dist_quadr_start <- list(a=bat1_indice_var_2_a_quadr_start,b=bat1_indice_var_2_b_quadr_start,c=bat1_indice_var_2_c_quadr_start,scale=var(bat1_indice_var_2)/mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_indice_var_2_gama_shape_dist_quadr_mle <- mle2(bat1_indice_var_2_gama_shape_dist_quadr, start=bat1_indice_var_2_gama_shape_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat1_indice_var_2_gama_scale_dist_quadr <- function(a,b,c,shape){
  esperancaaoquadrado<- (a*(bat1_indice_dist_2^2) + b*(bat1_indice_dist_2) + c)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat1_indice_var_2_gama_scale_dist_quadr_start <- list(a=bat1_indice_var_2_a_quadr_start,b=bat1_indice_var_2_b_quadr_start,c=bat1_indice_var_2_c_quadr_start,shape=mean(bat1_indice_var_2)^2/var(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_indice_var_2_gama_scale_dist_quadr_mle <- mle2(bat1_indice_var_2_gama_scale_dist_quadr, start=bat1_indice_var_2_gama_scale_dist_quadr_start)
######## ou:
#bat1_indice_var_2_gama_dist_quadr_glm <- glm(bat1_indice_dist_2/bat1_indice_var_2~bat1_indice_dist_2,family="Gaquadra"(link="identity"))

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat1_indice_var_2_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat1_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat1_indice_var_2_gama_var_shape_dist_lin_start <- list(a=bat1_indice_var_2_a_lin_start,b=bat1_indice_var_2_b_lin_start,scale=var(bat1_indice_var_2)/mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_indice_var_2_gama_var_shape_dist_lin_mle <- mle2(bat1_indice_var_2_gama_var_shape_dist_lin, start=bat1_indice_var_2_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat1_indice_var_2_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat1_indice_dist_2+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat1_indice_var_2_gama_var_scale_dist_lin_start <- list(a=bat1_indice_var_2_a_lin_start,b=bat1_indice_var_2_b_lin_start,shape=mean(bat1_indice_var_2)^2/var(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_indice_var_2_gama_var_scale_dist_lin_mle <- mle2(bat1_indice_var_2_gama_var_scale_dist_lin, start=bat1_indice_var_2_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat1_indice_var_2_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat1_indice_dist_2/(b+bat1_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat1_indice_var_2_gama_var_shape_dist_mm_start <- list(a=bat1_indice_var_2_a_mm_start,b=bat1_indice_var_2_b_mm_start,scale=var(bat1_indice_var_2)/mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_indice_var_2_gama_var_shape_dist_mm_mle <- mle2(bat1_indice_var_2_gama_var_shape_dist_mm, start=bat1_indice_var_2_gama_var_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat1_indice_var_2_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat1_indice_dist_2/(b+bat1_indice_dist_2))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat1_indice_var_2_gama_var_scale_dist_mm_start <- list(a=bat1_indice_var_2_a_mm_start,b=bat1_indice_var_2_b_mm_start,shape=mean(bat1_indice_var_2)^2/var(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_indice_var_2_gama_var_scale_dist_mm_mle <- mle2(bat1_indice_var_2_gama_var_scale_dist_mm, start=bat1_indice_var_2_gama_var_scale_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat1_indice_var_2_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat1_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat1_indice_var_2_gama_var_shape_dist_pot_start <- list(a=bat1_indice_var_2_a_pot_start,b=bat1_indice_var_2_b_pot_start,scale=var(bat1_indice_var_2)/mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_indice_var_2_gama_var_shape_dist_pot_mle <- mle2(bat1_indice_var_2_gama_var_shape_dist_pot, start=bat1_indice_var_2_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat1_indice_var_2_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado<- (a*(bat1_indice_dist_2^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat1_indice_var_2_gama_var_scale_dist_pot_start <- list(a=bat1_indice_var_2_a_pot_start,b=bat1_indice_var_2_b_pot_start,shape=mean(bat1_indice_var_2)^2/var(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_indice_var_2_gama_var_scale_dist_pot_mle <- mle2(bat1_indice_var_2_gama_var_scale_dist_pot, start=bat1_indice_var_2_gama_var_scale_dist_pot_start,method="Nelder-Mead",control=list(maxit=2000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat1_indice_var_2_gama_var_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat1_indice_dist_2^2) + b*(bat1_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat1_indice_var_2_gama_var_shape_dist_quadr_start <- list(a=bat1_indice_var_2_a_quadr_start,b=bat1_indice_var_2_b_quadr_start,c=bat1_indice_var_2_c_quadr_start,scale=var(bat1_indice_var_2)/mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_indice_var_2_gama_var_shape_dist_quadr_mle <- mle2(bat1_indice_var_2_gama_var_shape_dist_quadr, start=bat1_indice_var_2_gama_var_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat1_indice_var_2_gama_var_scale_dist_quadr <- function(a,b,c,shape){
  varianciaaoquadrado<- (a*(bat1_indice_dist_2^2) + b*(bat1_indice_dist_2) + c)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat1_indice_var_2_gama_var_scale_dist_quadr_start <- list(a=bat1_indice_var_2_a_quadr_start,b=bat1_indice_var_2_b_quadr_start,c=bat1_indice_var_2_c_quadr_start,shape=mean(bat1_indice_var_2)^2/var(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_indice_var_2_gama_var_scale_dist_quadr_mle <- mle2(bat1_indice_var_2_gama_var_scale_dist_quadr, start=bat1_indice_var_2_gama_var_scale_dist_quadr_start,method="Nelder-Mead",control=list(maxit=10000))


# NORMAL

## Nulo
bat1_indice_var_2_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_nulo_start <- list(mean=mean(bat1_indice_var_2),sd=sd(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat1_indice_var_2_norm_nulo_mle <- mle2(bat1_indice_var_2_norm_nulo, start=bat1_indice_var_2_norm_nulo_start,method="Nelder-Mead")
########## ou:
bat1_indice_var_2_norm_nulo_glm <- glm(bat1_indice_var_2~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
bat1_indice_var_2_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*bat1_indice_dist_2+b
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_mean_dist_lin_start <- list(a=bat1_indice_var_2_a_lin_start,b=bat1_indice_var_2_b_lin_start,sd=sd(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_indice_var_2_norm_mean_dist_lin_mle <- mle2(bat1_indice_var_2_norm_mean_dist_lin, start=bat1_indice_var_2_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
bat1_indice_var_2_norm_dist_lin_glm <- glm(bat1_indice_var_2~bat1_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
bat1_indice_var_2_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*bat1_indice_dist_2+b)^2)
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_sd_dist_lin_start <- list(a=bat1_indice_var_2_a_lin_start,b=bat1_indice_var_2_b_lin_start,mean=mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_indice_var_2_norm_sd_dist_lin_mle <- mle2(bat1_indice_var_2_norm_sd_dist_lin, start=bat1_indice_var_2_norm_sd_dist_lin_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media e sd
bat1_indice_var_2_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*bat1_indice_dist_2+b
  sd <- sqrt((c*bat1_indice_dist_2+d)^2)
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_mean_sd_dist_lin_start <- list(a=bat1_indice_var_2_a_lin_start,b=bat1_indice_var_2_b_lin_start,c=bat1_indice_var_2_a_lin_start,d=bat1_indice_var_2_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_indice_var_2_norm_mean_sd_dist_lin_mle <- mle2(bat1_indice_var_2_norm_mean_sd_dist_lin, start=bat1_indice_var_2_norm_mean_sd_dist_lin_start)


## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media
bat1_indice_var_2_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*bat1_indice_dist_2/(b+bat1_indice_dist_2)
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_mean_dist_mm_start <- list(a=bat1_indice_var_2_a_mm_start,b=bat1_indice_var_2_b_mm_start,sd=sd(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_indice_var_2_norm_mean_dist_mm_mle <- mle2(bat1_indice_var_2_norm_mean_dist_mm, start=bat1_indice_var_2_norm_mean_dist_mm_start,method="Nelder-Mead")
######### ou:
bat1_indice_var_2_norm_dist_mm_glm <- glm(bat1_indice_dist_2/bat1_indice_var_2~bat1_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre sd
bat1_indice_var_2_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*bat1_indice_dist_2/(b+bat1_indice_dist_2))^2)
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_sd_dist_mm_start <- list(a=bat1_indice_var_2_a_mm_start,b=bat1_indice_var_2_b_mm_start,mean=mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_indice_var_2_norm_sd_dist_mm_mle <- mle2(bat1_indice_var_2_norm_sd_dist_mm, start=bat1_indice_var_2_norm_sd_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media e sd
bat1_indice_var_2_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*bat1_indice_dist_2/(b+bat1_indice_dist_2)
  sd <- sqrt((c*bat1_indice_dist_2/(d+bat1_indice_dist_2))^2)
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_mean_sd_dist_mm_start <- list(a=bat1_indice_var_2_a_mm_start,b=bat1_indice_var_2_b_mm_start,c=bat1_indice_var_2_a_mm_start,d=bat1_indice_var_2_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_indice_var_2_norm_mean_sd_dist_mm_mle <- mle2(bat1_indice_var_2_norm_mean_sd_dist_mm, start=bat1_indice_var_2_norm_mean_sd_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media
bat1_indice_var_2_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(bat1_indice_dist_2^b)
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_mean_dist_pot_start <- list(a=bat1_indice_var_2_a_pot_start,b=bat1_indice_var_2_b_pot_start,sd=sd(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos

bat1_indice_var_2_norm_mean_dist_pot_mle <- mle2(bat1_indice_var_2_norm_mean_dist_pot, start=bat1_indice_var_2_norm_mean_dist_pot_start,method="Nelder-Mead")
######### ou:
bat1_indice_var_2_norm_dist_pot_glm <- glm(bat1_indice_dist_2/bat1_indice_var_2~bat1_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre sd
bat1_indice_var_2_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(bat1_indice_dist_2^b))^2)
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_sd_dist_pot_start <- list(a=bat1_indice_var_2_a_pot_start,b=bat1_indice_var_2_b_pot_start,mean=mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_indice_var_2_norm_sd_dist_pot_mle <- mle2(bat1_indice_var_2_norm_sd_dist_pot, start=bat1_indice_var_2_norm_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media e sd
bat1_indice_var_2_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(bat1_indice_dist_2^b)
  sd <- sqrt((c*(bat1_indice_dist_2^d))^2)
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_mean_sd_dist_pot_start <- list(a=bat1_indice_var_2_a_pot_start,b=bat1_indice_var_2_b_pot_start,c=bat1_indice_var_2_a_pot_start,d=bat1_indice_var_2_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_indice_var_2_norm_mean_sd_dist_pot_mle <- mle2(bat1_indice_var_2_norm_mean_sd_dist_pot, start=bat1_indice_var_2_norm_mean_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media
bat1_indice_var_2_norm_mean_dist_quadr <- function(a,b,c,sd){
  mean <- a*(bat1_indice_dist_2^2) + b*(bat1_indice_dist_2) + c
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_mean_dist_quadr_start <- list(a=bat1_indice_var_2_a_quadr_start,b=bat1_indice_var_2_b_quadr_start,c=bat1_indice_var_2_c_quadr_start,sd=sd(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_indice_var_2_norm_mean_dist_quadr_mle <- mle2(bat1_indice_var_2_norm_mean_dist_quadr, start=bat1_indice_var_2_norm_mean_dist_quadr_start,method="Nelder-Mead")
######### ou:
bat1_indice_var_2_norm_dist_quadr_glm <- glm(bat1_indice_dist_2/bat1_indice_var_2~bat1_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre sd
bat1_indice_var_2_norm_sd_dist_quadr <- function(a,b,c,mean){
  sd <- sqrt((a*(bat1_indice_dist_2^2) + b*(bat1_indice_dist_2) + c)^2)
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_sd_dist_quadr_start <- list(a=bat1_indice_var_2_a_quadr_start,b=bat1_indice_var_2_b_quadr_start,c=bat1_indice_var_2_c_quadr_start,mean=mean(bat1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_indice_var_2_norm_sd_dist_quadr_mle <- mle2(bat1_indice_var_2_norm_sd_dist_quadr, start=bat1_indice_var_2_norm_sd_dist_quadr_start)

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media e sd
bat1_indice_var_2_norm_mean_sd_dist_quadr <- function(a,b,c,d,e,f){
  mean <- a*(bat1_indice_dist_2^2) + b*(bat1_indice_dist_2) + c
  sd <- sqrt((d*(bat1_indice_dist_2^2) + e*(bat1_indice_dist_2) + f)^2)
  -sum(dnorm(bat1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat1_indice_var_2_norm_mean_sd_dist_quadr_start <- list(a=bat1_indice_var_2_a_quadr_start,b=bat1_indice_var_2_b_quadr_start,c=bat1_indice_var_2_c_quadr_start,d=bat1_indice_var_2_a_quadr_start,e=bat1_indice_var_2_b_quadr_start,f=bat1_indice_var_2_c_quadr_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_indice_var_2_norm_mean_sd_dist_quadr_mle <- mle2(bat1_indice_var_2_norm_mean_sd_dist_quadr, start=bat1_indice_var_2_norm_mean_sd_dist_quadr_start)


################### SELECAO DE MODELOS ####################
AICtab(bat1_indice_var_2_gama_nulo_mle,bat1_indice_var_2_gama_scale_dist_lin_mle,bat1_indice_var_2_gama_scale_dist_mm_mle,bat1_indice_var_2_gama_scale_dist_pot_mle,bat1_indice_var_2_gama_scale_dist_quadr_mle,bat1_indice_var_2_gama_var_scale_dist_lin_mle,bat1_indice_var_2_gama_var_scale_dist_mm_mle,bat1_indice_var_2_gama_var_scale_dist_pot_mle,bat1_indice_var_2_gama_var_scale_dist_quadr_mle,bat1_indice_var_2_norm_nulo_mle,bat1_indice_var_2_norm_mean_dist_lin_mle,bat1_indice_var_2_norm_sd_dist_lin_mle,bat1_indice_var_2_norm_mean_sd_dist_lin_mle,bat1_indice_var_2_norm_mean_dist_mm_mle,bat1_indice_var_2_norm_sd_dist_mm_mle,bat1_indice_var_2_norm_mean_sd_dist_mm_mle,bat1_indice_var_2_norm_mean_dist_pot_mle,bat1_indice_var_2_norm_sd_dist_pot_mle,bat1_indice_var_2_norm_mean_sd_dist_pot_mle,bat1_indice_var_2_norm_mean_dist_quadr_mle,bat1_indice_var_2_norm_sd_dist_quadr_mle,bat1_indice_var_2_norm_mean_sd_dist_quadr_mle,weights=T)
### modelo selecionado: bat1_indice_var_2_gama_scale_dist_mm_mle

##########################################################
######################## Bateria 2 #######################
##########################################################

#################### DADOS ORIGINAIS ######################
################# Transformacao dos dados #################
# bat2_indice_var_2 <- bat2_indice_var[-which(simulacoes_25jul16_output_derivado[,12]>0.05)]
# bat2_indice_dist_2 <- bat2_indice_dist[-which(simulacoes_25jul16_output_derivado[,12]>0.05)]

#points(bat2_indice_var_2~bat2_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015),pch=20,col=alpha("red",0.2))

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat2_indice_var_2_lin <- lm(bat2_indice_var_2~bat2_indice_dist_2)
bat2_indice_var_2_a_lin_start <- coef(bat2_indice_var_2_lin)[[2]]
bat2_indice_var_2_b_lin_start <- coef(bat2_indice_var_2_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat2_indice_var_2_mm_linearizada <- lm(bat2_indice_dist_2/bat2_indice_var_2~bat2_indice_dist_2) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
bat2_indice_var_2_a_mm_start <- 1/(coef(bat2_indice_var_2_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
bat2_indice_var_2_b_mm_start <- (coef(bat2_indice_var_2_mm_linearizada)[[1]])*(1/(coef(bat2_indice_var_2_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat2_indice_var_2_a_pot_start <- 0.000000000000015
bat2_indice_var_2_b_pot_start <- 1.9

#plot(bat2_indice_var_2~bat2_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(pot(x,0.000000000000015,1.9),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="red")

# Valores de start para os coeficientes a e b da funcao quadratica usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat2_indice_var_2_a_quadr_start <- bat1_indice_var_2_a_quadr_start
bat2_indice_var_2_b_quadr_start <- bat1_indice_var_2_b_quadr_start
bat2_indice_var_2_c_quadr_start <- 0
#plot(bat2_indice_var_2~bat2_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(quadra(x,-0.00000000000003,0.000000008,0),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="red")

# GAMA

## Nulo
bat2_indice_var_2_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat2_indice_var_2_gama_nulo_start <- list(shape=mean(bat2_indice_var_2)^2/var(bat2_indice_var_2),scale=var(bat2_indice_var_2)/mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat2_indice_var_2_gama_nulo_mle <- mle2(bat2_indice_var_2_gama_nulo, start=bat2_indice_var_2_gama_nulo_start,method="Nelder-Mead")
###### ou:
bat2_indice_var_2_gama_nulo_glm <- glm(bat2_indice_var_2~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat2_indice_var_2_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat2_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat2_indice_var_2_gama_shape_dist_lin_start <- list(a=bat2_indice_var_2_a_lin_start,b=bat2_indice_var_2_b_lin_start,scale=var(bat2_indice_var_2)/mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_indice_var_2_gama_shape_dist_lin_mle <- mle2(bat2_indice_var_2_gama_shape_dist_lin, start=bat2_indice_var_2_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_indice_var_2_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat2_indice_dist_2+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat2_indice_var_2_gama_scale_dist_lin_start <- list(a=bat2_indice_var_2_a_lin_start,b=bat2_indice_var_2_b_lin_start,shape=mean(bat2_indice_var_2)^2/var(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_indice_var_2_gama_scale_dist_lin_mle <- mle2(bat2_indice_var_2_gama_scale_dist_lin, start=bat2_indice_var_2_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
bat2_indice_var_2_gama_dist_lin_glm <- glm(bat2_indice_var_2~bat2_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao MM sobre shape
# bat2_indice_var_2_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat2_indice_dist_2/(b+bat2_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat2_indice_var_2_gama_shape_dist_mm_start <- list(a=bat2_indice_var_2_a_mm_start,b=bat2_indice_var_2_b_mm_start,scale=var(bat2_indice_var_2)/mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_indice_var_2_gama_shape_dist_mm_mle <- mle2(bat2_indice_var_2_gama_shape_dist_mm, start=bat2_indice_var_2_gama_shape_dist_mm_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_indice_var_2_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat2_indice_dist_2/(b+bat2_indice_dist_2))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat2_indice_var_2_gama_scale_dist_mm_start <- list(a=bat2_indice_var_2_a_mm_start,b=bat2_indice_var_2_b_mm_start,shape=mean(bat2_indice_var_2)^2/var(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_indice_var_2_gama_scale_dist_mm_mle <- mle2(bat2_indice_var_2_gama_scale_dist_mm, start=bat2_indice_var_2_gama_scale_dist_mm_start,method="Nelder-Mead")
######## ou:
#bat2_indice_var_2_gama_dist_mm_glm <- glm(bat2_indice_dist_2/bat2_indice_var_2~bat2_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre shape
# bat2_indice_var_2_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(bat2_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat2_indice_var_2_gama_shape_dist_pot_start <- list(a=bat2_indice_var_2_a_pot_start,b=bat2_indice_var_2_b_pot_start,scale=var(bat2_indice_var_2)/mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_indice_var_2_gama_shape_dist_pot_mle <- mle2(bat2_indice_var_2_gama_shape_dist_pot, start=bat2_indice_var_2_gama_shape_dist_pot_start,method="Nelder-Mead",control=list(maxit=2000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_indice_var_2_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado<- (a*(bat2_indice_dist_2^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat2_indice_var_2_gama_scale_dist_pot_start <- list(a=bat2_indice_var_2_a_pot_start,b=bat2_indice_var_2_b_pot_start,shape=mean(bat2_indice_var_2)^2/var(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_indice_var_2_gama_scale_dist_pot_mle <- mle2(bat2_indice_var_2_gama_scale_dist_pot, start=bat2_indice_var_2_gama_scale_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))
######## ou:
#bat2_indice_var_2_gama_dist_pot_glm <- glm(bat2_indice_dist_2/bat2_indice_var_2~bat2_indice_dist_2,family="Gapota"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre shape
# bat2_indice_var_2_gama_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(bat2_indice_dist_2^2) + b*(bat2_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat2_indice_var_2_gama_shape_dist_quadr_start <- list(a=bat2_indice_var_2_a_quadr_start,b=bat2_indice_var_2_b_quadr_start,c=bat2_indice_var_2_c_quadr_start,scale=var(bat2_indice_var_2)/mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_indice_var_2_gama_shape_dist_quadr_mle <- mle2(bat2_indice_var_2_gama_shape_dist_quadr, start=bat2_indice_var_2_gama_shape_dist_quadr_start,control=list(maxit=1000),method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_indice_var_2_gama_scale_dist_quadr <- function(a,b,c,shape){
  esperancaaoquadrado<- (a*(bat2_indice_dist_2^2) + b*(bat2_indice_dist_2) + c)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat2_indice_var_2_gama_scale_dist_quadr_start <- list(a=bat2_indice_var_2_a_quadr_start,b=bat2_indice_var_2_b_quadr_start,c=bat2_indice_var_2_c_quadr_start,shape=mean(bat2_indice_var_2)^2/var(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_indice_var_2_gama_scale_dist_quadr_mle <- mle2(bat2_indice_var_2_gama_scale_dist_quadr, start=bat2_indice_var_2_gama_scale_dist_quadr_start,method="Nelder-Mead")
######## ou:
#bat2_indice_var_2_gama_dist_quadr_glm <- glm(bat2_indice_dist_2/bat2_indice_var_2~bat2_indice_dist_2,family="Gaquadra"(link="identity"))

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat2_indice_var_2_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat2_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat2_indice_var_2_gama_var_shape_dist_lin_start <- list(a=bat2_indice_var_2_a_lin_start,b=bat2_indice_var_2_b_lin_start,scale=var(bat2_indice_var_2)/mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_indice_var_2_gama_var_shape_dist_lin_mle <- mle2(bat2_indice_var_2_gama_var_shape_dist_lin, start=bat2_indice_var_2_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_indice_var_2_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat2_indice_dist_2+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat2_indice_var_2_gama_var_scale_dist_lin_start <- list(a=bat2_indice_var_2_a_lin_start,b=bat2_indice_var_2_b_lin_start,shape=mean(bat2_indice_var_2)^2/var(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_indice_var_2_gama_var_scale_dist_lin_mle <- mle2(bat2_indice_var_2_gama_var_scale_dist_lin, start=bat2_indice_var_2_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat2_indice_var_2_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat2_indice_dist_2/(b+bat2_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat2_indice_var_2_gama_var_shape_dist_mm_start <- list(a=bat2_indice_var_2_a_mm_start,b=bat2_indice_var_2_b_mm_start,scale=var(bat2_indice_var_2)/mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_indice_var_2_gama_var_shape_dist_mm_mle <- mle2(bat2_indice_var_2_gama_var_shape_dist_mm, start=bat2_indice_var_2_gama_var_shape_dist_mm_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_indice_var_2_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat2_indice_dist_2/(b+bat2_indice_dist_2))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat2_indice_var_2_gama_var_scale_dist_mm_start <- list(a=bat2_indice_var_2_a_mm_start,b=bat2_indice_var_2_b_mm_start,shape=mean(bat2_indice_var_2)^2/var(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_indice_var_2_gama_var_scale_dist_mm_mle <- mle2(bat2_indice_var_2_gama_var_scale_dist_mm, start=bat2_indice_var_2_gama_var_scale_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat2_indice_var_2_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat2_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat2_indice_var_2_gama_var_shape_dist_pot_start <- list(a=bat2_indice_var_2_a_pot_start,b=bat2_indice_var_2_b_pot_start,scale=var(bat2_indice_var_2)/mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_indice_var_2_gama_var_shape_dist_pot_mle <- mle2(bat2_indice_var_2_gama_var_shape_dist_pot, start=bat2_indice_var_2_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_indice_var_2_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado<- (a*(bat2_indice_dist_2^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat2_indice_var_2_gama_var_scale_dist_pot_start <- list(a=bat2_indice_var_2_a_pot_start,b=bat2_indice_var_2_b_pot_start,shape=mean(bat2_indice_var_2)^2/var(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_indice_var_2_gama_var_scale_dist_pot_mle <- mle2(bat2_indice_var_2_gama_var_scale_dist_pot, start=bat2_indice_var_2_gama_var_scale_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat2_indice_var_2_gama_var_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat2_indice_dist_2^2) + b*(bat2_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat2_indice_var_2_gama_var_shape_dist_quadr_start <- list(a=bat2_indice_var_2_a_quadr_start,b=bat2_indice_var_2_b_quadr_start,c=bat2_indice_var_2_c_quadr_start,scale=var(bat2_indice_var_2)/mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_indice_var_2_gama_var_shape_dist_quadr_mle <- mle2(bat2_indice_var_2_gama_var_shape_dist_quadr, start=bat2_indice_var_2_gama_var_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_indice_var_2_gama_var_scale_dist_quadr <- function(a,b,c,shape){
  varianciaaoquadrado<- (a*(bat2_indice_dist_2^2) + b*(bat2_indice_dist_2) + c)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat2_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat2_indice_var_2_gama_var_scale_dist_quadr_start <- list(a=bat2_indice_var_2_a_quadr_start,b=bat2_indice_var_2_b_quadr_start,c=bat2_indice_var_2_c_quadr_start,shape=mean(bat2_indice_var_2)^2/var(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_indice_var_2_gama_var_scale_dist_quadr_mle <- mle2(bat2_indice_var_2_gama_var_scale_dist_quadr, start=bat2_indice_var_2_gama_var_scale_dist_quadr_start)

# NORMAL

## Nulo
bat2_indice_var_2_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_nulo_start <- list(mean=mean(bat2_indice_var_2),sd=sd(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat2_indice_var_2_norm_nulo_mle <- mle2(bat2_indice_var_2_norm_nulo, start=bat2_indice_var_2_norm_nulo_start,method="Nelder-Mead")
########## ou:
bat2_indice_var_2_norm_nulo_glm <- glm(bat2_indice_var_2~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
bat2_indice_var_2_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*bat2_indice_dist_2+b
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_mean_dist_lin_start <- list(a=bat2_indice_var_2_a_lin_start,b=bat2_indice_var_2_b_lin_start,sd=sd(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_mean_dist_lin_mle <- mle2(bat2_indice_var_2_norm_mean_dist_lin, start=bat2_indice_var_2_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
bat2_indice_var_2_norm_dist_lin_glm <- glm(bat2_indice_var_2~bat2_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
bat2_indice_var_2_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*bat2_indice_dist_2+b)^2)
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_sd_dist_lin_start <- list(a=bat2_indice_var_2_a_lin_start,b=bat2_indice_var_2_b_lin_start,mean=mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_sd_dist_lin_mle <- mle2(bat2_indice_var_2_norm_sd_dist_lin, start=bat2_indice_var_2_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media e sd
bat2_indice_var_2_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*bat2_indice_dist_2+b
  sd <- sqrt((c*bat2_indice_dist_2+d)^2)
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_mean_sd_dist_lin_start <- list(a=bat2_indice_var_2_a_lin_start,b=bat2_indice_var_2_b_lin_start,c=bat2_indice_var_2_a_lin_start,d=bat2_indice_var_2_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_mean_sd_dist_lin_mle <- mle2(bat2_indice_var_2_norm_mean_sd_dist_lin, start=bat2_indice_var_2_norm_mean_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media
bat2_indice_var_2_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*bat2_indice_dist_2/(b+bat2_indice_dist_2)
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_mean_dist_mm_start <- list(a=bat2_indice_var_2_a_mm_start,b=bat2_indice_var_2_b_mm_start,sd=sd(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_mean_dist_mm_mle <- mle2(bat2_indice_var_2_norm_mean_dist_mm, start=bat2_indice_var_2_norm_mean_dist_mm_start,method="Nelder-Mead")
######### ou:
bat2_indice_var_2_norm_dist_mm_glm <- glm(bat2_indice_dist_2/bat2_indice_var_2~bat2_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre sd
bat2_indice_var_2_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*bat2_indice_dist_2/(b+bat2_indice_dist_2))^2)
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_sd_dist_mm_start <- list(a=bat2_indice_var_2_a_mm_start,b=bat2_indice_var_2_b_mm_start,mean=mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_sd_dist_mm_mle <- mle2(bat2_indice_var_2_norm_sd_dist_mm, start=bat2_indice_var_2_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media e sd
bat2_indice_var_2_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*bat2_indice_dist_2/(b+bat2_indice_dist_2)
  sd <- sqrt((c*bat2_indice_dist_2/(d+bat2_indice_dist_2))^2)
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_mean_sd_dist_mm_start <- list(a=bat2_indice_var_2_a_mm_start,b=bat2_indice_var_2_b_mm_start,c=bat2_indice_var_2_a_mm_start,d=bat2_indice_var_2_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_mean_sd_dist_mm_mle <- mle2(bat2_indice_var_2_norm_mean_sd_dist_mm, start=bat2_indice_var_2_norm_mean_sd_dist_mm_start,control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media
bat2_indice_var_2_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(bat2_indice_dist_2^b)
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_mean_dist_pot_start <- list(a=bat2_indice_var_2_a_pot_start,b=bat2_indice_var_2_b_pot_start,sd=sd(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_mean_dist_pot_mle <- mle2(bat2_indice_var_2_norm_mean_dist_pot, start=bat2_indice_var_2_norm_mean_dist_pot_start,method="Nelder-Mead")
######### ou:
bat2_indice_var_2_norm_dist_pot_glm <- glm(bat2_indice_dist_2/bat2_indice_var_2~bat2_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre sd
bat2_indice_var_2_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(bat2_indice_dist_2^b))^2)
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_sd_dist_pot_start <- list(a=bat2_indice_var_2_a_pot_start,b=bat2_indice_var_2_b_pot_start,mean=mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_sd_dist_pot_mle <- mle2(bat2_indice_var_2_norm_sd_dist_pot, start=bat2_indice_var_2_norm_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media e sd
bat2_indice_var_2_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(bat2_indice_dist_2^b)
  sd <- sqrt((c*(bat2_indice_dist_2^d))^2)
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_mean_sd_dist_pot_start <- list(a=bat2_indice_var_2_a_pot_start,b=bat2_indice_var_2_b_pot_start,c=bat2_indice_var_2_a_pot_start,d=bat2_indice_var_2_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_mean_sd_dist_pot_mle <- mle2(bat2_indice_var_2_norm_mean_sd_dist_pot, start=bat2_indice_var_2_norm_mean_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=10000))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media
bat2_indice_var_2_norm_mean_dist_quadr <- function(a,b,c,sd){
  mean <- a*(bat2_indice_dist_2^2) + b*(bat2_indice_dist_2) + c
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_mean_dist_quadr_start <- list(a=bat2_indice_var_2_a_quadr_start,b=bat2_indice_var_2_b_quadr_start,c=bat2_indice_var_2_c_quadr_start,sd=sd(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_mean_dist_quadr_mle <- mle2(bat2_indice_var_2_norm_mean_dist_quadr, start=bat2_indice_var_2_norm_mean_dist_quadr_start,method="Nelder-Mead")
######### ou:
bat2_indice_var_2_norm_dist_quadr_glm <- glm(bat2_indice_dist_2/bat2_indice_var_2~bat2_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre sd
bat2_indice_var_2_norm_sd_dist_quadr <- function(a,b,c,mean){
  sd <- sqrt((a*(bat2_indice_dist_2^2) + b*(bat2_indice_dist_2) + c)^2)
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_sd_dist_quadr_start <- list(a=bat2_indice_var_2_a_quadr_start,b=bat2_indice_var_2_b_quadr_start,c=bat2_indice_var_2_c_quadr_start,mean=mean(bat2_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_sd_dist_quadr_mle <- mle2(bat2_indice_var_2_norm_sd_dist_quadr, start=bat2_indice_var_2_norm_sd_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media e sd
bat2_indice_var_2_norm_mean_sd_dist_quadr <- function(a,b,c,d,e,f){
  mean <- a*(bat2_indice_dist_2^2) + b*(bat2_indice_dist_2) + c
  sd <- sqrt((d*(bat2_indice_dist_2^2) + e*(bat2_indice_dist_2) + f)^2)
  -sum(dnorm(bat2_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat2_indice_var_2_norm_mean_sd_dist_quadr_start <- list(a=bat2_indice_var_2_a_quadr_start,b=bat2_indice_var_2_b_quadr_start,c=bat2_indice_var_2_c_quadr_start,d=bat2_indice_var_2_a_quadr_start,e=bat2_indice_var_2_b_quadr_start,f=bat2_indice_var_2_c_quadr_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_indice_var_2_norm_mean_sd_dist_quadr_mle <- mle2(bat2_indice_var_2_norm_mean_sd_dist_quadr, start=bat2_indice_var_2_norm_mean_sd_dist_quadr_start,method="Nelder-Mead")



################### SELECAO DE MODELOS ####################
AICtab(bat2_indice_var_2_gama_nulo_mle,bat2_indice_var_2_gama_scale_dist_lin_mle,bat2_indice_var_2_gama_scale_dist_mm_mle,bat2_indice_var_2_gama_scale_dist_pot_mle,bat2_indice_var_2_gama_scale_dist_quadr_mle,bat2_indice_var_2_gama_var_scale_dist_lin_mle,bat2_indice_var_2_gama_var_scale_dist_mm_mle,bat2_indice_var_2_gama_var_scale_dist_pot_mle,bat2_indice_var_2_gama_var_scale_dist_quadr_mle,bat2_indice_var_2_norm_nulo_mle,bat2_indice_var_2_norm_mean_dist_lin_mle,bat2_indice_var_2_norm_sd_dist_lin_mle,bat2_indice_var_2_norm_mean_sd_dist_lin_mle,bat2_indice_var_2_norm_mean_dist_mm_mle,bat2_indice_var_2_norm_sd_dist_mm_mle,bat2_indice_var_2_norm_mean_sd_dist_mm_mle,bat2_indice_var_2_norm_mean_dist_pot_mle,bat2_indice_var_2_norm_sd_dist_pot_mle,bat2_indice_var_2_norm_mean_sd_dist_pot_mle,bat2_indice_var_2_norm_mean_dist_quadr_mle,bat2_indice_var_2_norm_sd_dist_quadr_mle,bat2_indice_var_2_norm_mean_sd_dist_quadr_mle,weights=T)
### modelo selecionado: bat2_indice_var_2_gama_scale_dist_quadr_mle

##########################################################
######################## Bateria 3 #######################
##########################################################

#################### DADOS ORIGINAIS ######################
################# Transformacao dos dados #################
# bat3_indice_var_2 <- bat3_indice_var[-which(simulacoes_27jul16_output_derivado[,12]>0.05)]
# bat3_indice_dist_2 <- bat3_indice_dist[-which(simulacoes_27jul16_output_derivado[,12]>0.05)]

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat3_indice_var_2_lin <- lm(bat3_indice_var_2~bat3_indice_dist_2)
bat3_indice_var_2_a_lin_start <- coef(bat3_indice_var_2_lin)[[2]]
bat3_indice_var_2_b_lin_start <- coef(bat3_indice_var_2_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat3_indice_var_2_mm_linearizada <- lm(bat3_indice_dist_2/bat3_indice_var_2~bat3_indice_dist_2) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
bat3_indice_var_2_a_mm_start <- 1/(coef(bat3_indice_var_2_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
bat3_indice_var_2_b_mm_start <- (coef(bat3_indice_var_2_mm_linearizada)[[1]])*(1/(coef(bat3_indice_var_2_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat3_indice_var_2_a_pot_start <- 0.0009
bat3_indice_var_2_b_pot_start <- 0.028
#plot(bat3_indice_var_2~bat3_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(pot(x,0.0009,0.028),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="purple")

# Valores de start para os coeficientes a e b da funcao quadratica usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat3_indice_var_2_a_quadr_start <- bat1_indice_var_2_a_quadr_start
bat3_indice_var_2_b_quadr_start <- bat1_indice_var_2_b_quadr_start
bat3_indice_var_2_c_quadr_start <- bat1_indice_var_2_c_quadr_start+0.0012
#plot(bat3_indice_var_2~bat3_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(quadra(x,-0.0000000000001,0.000000022,0.00025),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="green")

# GAMA

## Nulo
bat3_indice_var_2_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat3_indice_var_2_gama_nulo_start <- list(shape=mean(bat3_indice_var_2)^2/var(bat3_indice_var_2),scale=var(bat3_indice_var_2)/mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat3_indice_var_2_gama_nulo_mle <- mle2(bat3_indice_var_2_gama_nulo, start=bat3_indice_var_2_gama_nulo_start,method="Nelder-Mead")
###### ou:
bat3_indice_var_2_gama_nulo_glm <- glm(bat3_indice_var_2~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat3_indice_var_2_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat3_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat3_indice_var_2_gama_shape_dist_lin_start <- list(a=bat3_indice_var_2_a_lin_start,b=bat3_indice_var_2_b_lin_start,scale=var(bat3_indice_var_2)/mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_indice_var_2_gama_shape_dist_lin_mle <- mle2(bat3_indice_var_2_gama_shape_dist_lin, start=bat3_indice_var_2_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_indice_var_2_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat3_indice_dist_2+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat3_indice_var_2_gama_scale_dist_lin_start <- list(a=bat3_indice_var_2_a_lin_start,b=bat3_indice_var_2_b_lin_start,shape=mean(bat3_indice_var_2)^2/var(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_indice_var_2_gama_scale_dist_lin_mle <- mle2(bat3_indice_var_2_gama_scale_dist_lin, start=bat3_indice_var_2_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
bat3_indice_var_2_gama_dist_lin_glm <- glm(bat3_indice_var_2~bat3_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao MM sobre shape
# bat3_indice_var_2_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat3_indice_dist_2/(b+bat3_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat3_indice_var_2_gama_shape_dist_mm_start <- list(a=bat3_indice_var_2_a_mm_start,b=bat3_indice_var_2_b_mm_start,scale=var(bat3_indice_var_2)/mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_indice_var_2_gama_shape_dist_mm_mle <- mle2(bat3_indice_var_2_gama_shape_dist_mm, start=bat3_indice_var_2_gama_shape_dist_mm_start,control=list(maxit=1000),method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_indice_var_2_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat3_indice_dist_2/(b+bat3_indice_dist_2))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat3_indice_var_2_gama_scale_dist_mm_start <- list(a=bat3_indice_var_2_a_mm_start,b=bat3_indice_var_2_b_mm_start,shape=mean(bat3_indice_var_2)^2/var(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_indice_var_2_gama_scale_dist_mm_mle <- mle2(bat3_indice_var_2_gama_scale_dist_mm, start=bat3_indice_var_2_gama_scale_dist_mm_start,method="Nelder-Mead")
######## ou:
#bat3_indice_var_2_gama_dist_mm_glm <- glm(bat3_indice_dist_2/bat3_indice_var_2~bat3_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre shape
# bat3_indice_var_2_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(bat3_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat3_indice_var_2_gama_shape_dist_pot_start <- list(a=bat3_indice_var_2_a_pot_start,b=bat3_indice_var_2_b_pot_start,scale=var(bat3_indice_var_2)/mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_indice_var_2_gama_shape_dist_pot_mle <- mle2(bat3_indice_var_2_gama_shape_dist_pot, start=bat3_indice_var_2_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_indice_var_2_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado<- (a*(bat3_indice_dist_2^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat3_indice_var_2_gama_scale_dist_pot_start <- list(a=bat3_indice_var_2_a_pot_start,b=bat3_indice_var_2_b_pot_start,shape=mean(bat3_indice_var_2)^2/var(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_indice_var_2_gama_scale_dist_pot_mle <- mle2(bat3_indice_var_2_gama_scale_dist_pot, start=bat3_indice_var_2_gama_scale_dist_pot_start,method="Nelder-Mead")
######## ou:
#bat3_indice_var_2_gama_dist_pot_glm <- glm(bat3_indice_dist_2/bat3_indice_var_2~bat3_indice_dist_2,family="Gapota"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre shape
# bat3_indice_var_2_gama_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(bat3_indice_dist_2^2) + b*(bat3_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat3_indice_var_2_gama_shape_dist_quadr_start <- list(a=bat3_indice_var_2_a_quadr_start,b=bat3_indice_var_2_b_quadr_start,c=bat3_indice_var_2_c_quadr_start,scale=var(bat3_indice_var_2)/mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_indice_var_2_gama_shape_dist_quadr_mle <- mle2(bat3_indice_var_2_gama_shape_dist_quadr, start=bat3_indice_var_2_gama_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_indice_var_2_gama_scale_dist_quadr <- function(a,b,c,shape){
  esperancaaoquadrado<- (a*(bat3_indice_dist_2^2) + b*(bat3_indice_dist_2) + c)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat3_indice_var_2_gama_scale_dist_quadr_start <- list(a=bat3_indice_var_2_a_quadr_start,b=bat3_indice_var_2_b_quadr_start,c=bat3_indice_var_2_c_quadr_start,shape=mean(bat3_indice_var_2)^2/var(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_indice_var_2_gama_scale_dist_quadr_mle <- mle2(bat3_indice_var_2_gama_scale_dist_quadr, start=bat3_indice_var_2_gama_scale_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat3_indice_var_2_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat3_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat3_indice_var_2_gama_var_shape_dist_lin_start <- list(a=bat3_indice_var_2_a_lin_start,b=bat3_indice_var_2_b_lin_start,scale=var(bat3_indice_var_2)/mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_indice_var_2_gama_var_shape_dist_lin_mle <- mle2(bat3_indice_var_2_gama_var_shape_dist_lin, start=bat3_indice_var_2_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_indice_var_2_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat3_indice_dist_2+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat3_indice_var_2_gama_var_scale_dist_lin_start <- list(a=bat3_indice_var_2_a_lin_start,b=bat3_indice_var_2_b_lin_start,shape=mean(bat3_indice_var_2)^2/var(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_indice_var_2_gama_var_scale_dist_lin_mle <- mle2(bat3_indice_var_2_gama_var_scale_dist_lin, start=bat3_indice_var_2_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat3_indice_var_2_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat3_indice_dist_2/(b+bat3_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat3_indice_var_2_gama_var_shape_dist_mm_start <- list(a=bat3_indice_var_2_a_mm_start,b=bat3_indice_var_2_b_mm_start,scale=var(bat3_indice_var_2)/mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_indice_var_2_gama_var_shape_dist_mm_mle <- mle2(bat3_indice_var_2_gama_var_shape_dist_mm, start=bat3_indice_var_2_gama_var_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_indice_var_2_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat3_indice_dist_2/(b+bat3_indice_dist_2))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat3_indice_var_2_gama_var_scale_dist_mm_start <- list(a=bat3_indice_var_2_a_mm_start,b=bat3_indice_var_2_b_mm_start,shape=mean(bat3_indice_var_2)^2/var(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_indice_var_2_gama_var_scale_dist_mm_mle <- mle2(bat3_indice_var_2_gama_var_scale_dist_mm, start=bat3_indice_var_2_gama_var_scale_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat3_indice_var_2_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat3_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat3_indice_var_2_gama_var_shape_dist_pot_start <- list(a=bat3_indice_var_2_a_pot_start,b=bat3_indice_var_2_b_pot_start,scale=var(bat3_indice_var_2)/mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_indice_var_2_gama_var_shape_dist_pot_mle <- mle2(bat3_indice_var_2_gama_var_shape_dist_pot, start=bat3_indice_var_2_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_indice_var_2_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado<- (a*(bat3_indice_dist_2^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat3_indice_var_2_gama_var_scale_dist_pot_start <- list(a=bat3_indice_var_2_a_pot_start,b=bat3_indice_var_2_b_pot_start,shape=mean(bat3_indice_var_2)^2/var(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_indice_var_2_gama_var_scale_dist_pot_mle <- mle2(bat3_indice_var_2_gama_var_scale_dist_pot, start=bat3_indice_var_2_gama_var_scale_dist_pot_start,method="Nelder-Mead",control=list(maxit=2000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat3_indice_var_2_gama_var_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat3_indice_dist_2^2) + b*(bat3_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# bat3_indice_var_2_gama_var_shape_dist_quadr_start <- list(a=bat3_indice_var_2_a_quadr_start,b=bat3_indice_var_2_b_quadr_start,c=bat3_indice_var_2_c_quadr_start,scale=var(bat3_indice_var_2)/mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_indice_var_2_gama_var_shape_dist_quadr_mle <- mle2(bat3_indice_var_2_gama_var_shape_dist_quadr, start=bat3_indice_var_2_gama_var_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_indice_var_2_gama_var_scale_dist_quadr <- function(a,b,c,shape){
  varianciaaoquadrado<- (a*(bat3_indice_dist_2^2) + b*(bat3_indice_dist_2) + c)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat3_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
bat3_indice_var_2_gama_var_scale_dist_quadr_start <- list(a=bat3_indice_var_2_a_quadr_start,b=bat3_indice_var_2_b_quadr_start,c=bat3_indice_var_2_c_quadr_start,shape=mean(bat3_indice_var_2)^2/var(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_indice_var_2_gama_var_scale_dist_quadr_mle <- mle2(bat3_indice_var_2_gama_var_scale_dist_quadr, start=bat3_indice_var_2_gama_var_scale_dist_quadr_start,method="Nelder-Mead")

# NORMAL

## Nulo
bat3_indice_var_2_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_nulo_start <- list(mean=mean(bat3_indice_var_2),sd=sd(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat3_indice_var_2_norm_nulo_mle <- mle2(bat3_indice_var_2_norm_nulo, start=bat3_indice_var_2_norm_nulo_start,method="Nelder-Mead")
########## ou:
bat3_indice_var_2_norm_nulo_glm <- glm(bat3_indice_var_2~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
bat3_indice_var_2_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*bat3_indice_dist_2+b
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_mean_dist_lin_start <- list(a=bat3_indice_var_2_a_lin_start,b=bat3_indice_var_2_b_lin_start,sd=sd(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_mean_dist_lin_mle <- mle2(bat3_indice_var_2_norm_mean_dist_lin, start=bat3_indice_var_2_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
bat3_indice_var_2_norm_dist_lin_glm <- glm(bat3_indice_var_2~bat3_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
bat3_indice_var_2_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*bat3_indice_dist_2+b)^2)
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_sd_dist_lin_start <- list(a=bat3_indice_var_2_a_lin_start,b=bat3_indice_var_2_b_lin_start,mean=mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_sd_dist_lin_mle <- mle2(bat3_indice_var_2_norm_sd_dist_lin, start=bat3_indice_var_2_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media e sd
bat3_indice_var_2_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*bat3_indice_dist_2+b
  sd <- sqrt((c*bat3_indice_dist_2+d)^2)
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_mean_sd_dist_lin_start <- list(a=bat3_indice_var_2_a_lin_start,b=bat3_indice_var_2_b_lin_start,c=bat3_indice_var_2_a_lin_start,d=bat3_indice_var_2_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_mean_sd_dist_lin_mle <- mle2(bat3_indice_var_2_norm_mean_sd_dist_lin, start=bat3_indice_var_2_norm_mean_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media
bat3_indice_var_2_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*bat3_indice_dist_2/(b+bat3_indice_dist_2)
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_mean_dist_mm_start <- list(a=bat3_indice_var_2_a_mm_start,b=bat3_indice_var_2_b_mm_start,sd=sd(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_mean_dist_mm_mle <- mle2(bat3_indice_var_2_norm_mean_dist_mm, start=bat3_indice_var_2_norm_mean_dist_mm_start,method="Nelder-Mead")
######### ou:
bat3_indice_var_2_norm_dist_mm_glm <- glm(bat3_indice_dist_2/bat3_indice_var_2~bat3_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre sd
bat3_indice_var_2_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*bat3_indice_dist_2/(b+bat3_indice_dist_2))^2)
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_sd_dist_mm_start <- list(a=bat3_indice_var_2_a_mm_start,b=bat3_indice_var_2_b_mm_start,mean=mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_sd_dist_mm_mle <- mle2(bat3_indice_var_2_norm_sd_dist_mm, start=bat3_indice_var_2_norm_sd_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media e sd
bat3_indice_var_2_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*bat3_indice_dist_2/(b+bat3_indice_dist_2)
  sd <- sqrt((c*bat3_indice_dist_2/(d+bat3_indice_dist_2))^2)
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_mean_sd_dist_mm_start <- list(a=bat3_indice_var_2_a_mm_start,b=bat3_indice_var_2_b_mm_start,c=bat3_indice_var_2_a_mm_start,d=bat3_indice_var_2_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_mean_sd_dist_mm_mle <- mle2(bat3_indice_var_2_norm_mean_sd_dist_mm, start=bat3_indice_var_2_norm_mean_sd_dist_mm_start,method="Nelder-Mead",control=list(maxit=2000))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media
bat3_indice_var_2_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(bat3_indice_dist_2^b)
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_mean_dist_pot_start <- list(a=bat3_indice_var_2_a_pot_start,b=bat3_indice_var_2_b_pot_start,sd=sd(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_mean_dist_pot_mle <- mle2(bat3_indice_var_2_norm_mean_dist_pot, start=bat3_indice_var_2_norm_mean_dist_pot_start,method="Nelder-Mead")
######### ou:
bat3_indice_var_2_norm_dist_pot_glm <- glm(bat3_indice_dist_2/bat3_indice_var_2~bat3_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre sd
bat3_indice_var_2_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(bat3_indice_dist_2^b))^2)
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_sd_dist_pot_start <- list(a=bat3_indice_var_2_a_pot_start,b=bat3_indice_var_2_b_pot_start,mean=mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_sd_dist_pot_mle <- mle2(bat3_indice_var_2_norm_sd_dist_pot, start=bat3_indice_var_2_norm_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=2000))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media e sd
bat3_indice_var_2_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(bat3_indice_dist_2^b)
  sd <- sqrt((c*(bat3_indice_dist_2^d))^2)
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_mean_sd_dist_pot_start <- list(a=bat3_indice_var_2_a_pot_start,b=bat3_indice_var_2_b_pot_start,c=bat3_indice_var_2_a_pot_start,d=bat3_indice_var_2_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_mean_sd_dist_pot_mle <- mle2(bat3_indice_var_2_norm_mean_sd_dist_pot, start=bat3_indice_var_2_norm_mean_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media
bat3_indice_var_2_norm_mean_dist_quadr <- function(a,b,c,sd){
  mean <- a*(bat3_indice_dist_2^2) + b*(bat3_indice_dist_2) + c
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_mean_dist_quadr_start <- list(a=bat3_indice_var_2_a_quadr_start,b=bat3_indice_var_2_b_quadr_start,c=bat3_indice_var_2_c_quadr_start,sd=sd(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_mean_dist_quadr_mle <- mle2(bat3_indice_var_2_norm_mean_dist_quadr, start=bat3_indice_var_2_norm_mean_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre sd
bat3_indice_var_2_norm_sd_dist_quadr <- function(a,b,c,mean){
  sd <- sqrt((a*(bat3_indice_dist_2^2) + b*(bat3_indice_dist_2) + c)^2)
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_sd_dist_quadr_start <- list(a=bat3_indice_var_2_a_quadr_start,b=bat3_indice_var_2_b_quadr_start,c=bat3_indice_var_2_c_quadr_start,mean=mean(bat3_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_sd_dist_quadr_mle <- mle2(bat3_indice_var_2_norm_sd_dist_quadr, start=bat3_indice_var_2_norm_sd_dist_quadr_start,method="Nelder-Mead",control=list(maxit=2000))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media e sd
bat3_indice_var_2_norm_mean_sd_dist_quadr <- function(a,b,c,d,e,f){
  mean <- a*(bat3_indice_dist_2^2) + b*(bat3_indice_dist_2) + c
  sd <- sqrt((d*(bat3_indice_dist_2^2) + e*(bat3_indice_dist_2) + f)^2)
  -sum(dnorm(bat3_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
bat3_indice_var_2_norm_mean_sd_dist_quadr_start <- list(a=bat3_indice_var_2_a_quadr_start,b=bat3_indice_var_2_b_quadr_start,c=bat3_indice_var_2_c_quadr_start,d=bat3_indice_var_2_a_quadr_start,e=bat3_indice_var_2_b_quadr_start,f=bat3_indice_var_2_c_quadr_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_indice_var_2_norm_mean_sd_dist_quadr_mle <- mle2(bat3_indice_var_2_norm_mean_sd_dist_quadr, start=bat3_indice_var_2_norm_mean_sd_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))


AICtab(bat3_indice_var_2_gama_nulo_mle,bat3_indice_var_2_gama_scale_dist_lin_mle,bat3_indice_var_2_gama_scale_dist_mm_mle,bat3_indice_var_2_gama_scale_dist_pot_mle,bat3_indice_var_2_gama_scale_dist_quadr_mle,bat3_indice_var_2_gama_var_scale_dist_lin_mle,bat3_indice_var_2_gama_var_scale_dist_mm_mle,bat3_indice_var_2_gama_var_scale_dist_pot_mle,bat3_indice_var_2_gama_var_scale_dist_quadr_mle,bat3_indice_var_2_norm_nulo_mle,bat3_indice_var_2_norm_mean_dist_lin_mle,bat3_indice_var_2_norm_sd_dist_lin_mle,bat3_indice_var_2_norm_mean_sd_dist_lin_mle,bat3_indice_var_2_norm_mean_dist_mm_mle,bat3_indice_var_2_norm_sd_dist_mm_mle,bat3_indice_var_2_norm_mean_sd_dist_mm_mle,bat3_indice_var_2_norm_mean_dist_pot_mle,bat3_indice_var_2_norm_sd_dist_pot_mle,bat3_indice_var_2_norm_mean_sd_dist_pot_mle,bat3_indice_var_2_norm_mean_dist_quadr_mle,bat3_indice_var_2_norm_sd_dist_quadr_mle,bat3_indice_var_2_norm_mean_sd_dist_quadr_mle,weights=T)
### modelo selecionado: bat3_indice_var_2_norm_sd_dist_quadr_mle

##########################################################
############# CALCULO DO AIC INDIVIDUAL TOTAL ############
##########################################################
aic_mle_soma_individual_var_2 <- AIC(bat1_indice_var_2_gama_scale_dist_mm_mle) + AIC(bat2_indice_var_2_gama_scale_dist_quadr_mle) + AIC(bat3_indice_var_2_norm_sd_dist_quadr_mle) #-36559.78




##########################################################
##########################################################
### Analises das baterias de simulacao EM SUBCONJUNTOS ###
###################### por MUTACAO #######################
##########################################################
##########################################################

##########################################################
######################## MUTACAO 0 #######################
##########################################################

mut0_indice_var_2 <- bat2_indice_var_2
mut0_indice_dist_2 <- bat2_indice_dist_2

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut0_indice_var_2_lin <- lm(mut0_indice_var_2~mut0_indice_dist_2)
mut0_indice_var_2_a_lin_start <- coef(mut0_indice_var_2_lin)[[2]]
mut0_indice_var_2_b_lin_start <- coef(mut0_indice_var_2_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut0_indice_var_2_mm_linearizada <- lm(mut0_indice_dist_2/mut0_indice_var_2~mut0_indice_dist_2) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
mut0_indice_var_2_a_mm_start <- 1/(coef(mut0_indice_var_2_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
mut0_indice_var_2_b_mm_start <- (coef(mut0_indice_var_2_mm_linearizada)[[1]])*(1/(coef(mut0_indice_var_2_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut0_indice_var_2_a_pot_start <- 0.000000000000015
mut0_indice_var_2_b_pot_start <- 1.9

#plot(bat2_indice_var_2~bat2_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(pot(x,0.000000000000015,1.9),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="purple")

# Valores de start para os coeficientes a e b da funcao quadratica usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut0_indice_var_2_a_quadr_start <- bat2_indice_var_2_a_quadr_start
mut0_indice_var_2_b_quadr_start <- bat2_indice_var_2_b_quadr_start
mut0_indice_var_2_c_quadr_start <- bat2_indice_var_2_c_quadr_start
#plot(bat2_indice_var_2~bat2_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(quadra(x,-0.00000000000003,0.000000008,0),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="blue")


# GAMA

## Nulo
mut0_indice_var_2_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut0_indice_var_2_gama_nulo_start <- list(shape=mean(mut0_indice_var_2)^2/var(mut0_indice_var_2),scale=var(mut0_indice_var_2)/mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
mut0_indice_var_2_gama_nulo_mle <- mle2(mut0_indice_var_2_gama_nulo, start=mut0_indice_var_2_gama_nulo_start,method="Nelder-Mead")
###### ou:
mut0_indice_var_2_gama_nulo_glm <- glm(mut0_indice_var_2~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut0_indice_var_2_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*mut0_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut0_indice_var_2_gama_shape_dist_lin_start <- list(a=mut0_indice_var_2_a_lin_start,b=mut0_indice_var_2_b_lin_start,scale=var(mut0_indice_var_2)/mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_indice_var_2_gama_shape_dist_lin_mle <- mle2(mut0_indice_var_2_gama_shape_dist_lin, start=mut0_indice_var_2_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut0_indice_var_2_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*mut0_indice_dist_2+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut0_indice_var_2_gama_scale_dist_lin_start <- list(a=mut0_indice_var_2_a_lin_start,b=mut0_indice_var_2_b_lin_start,shape=mean(mut0_indice_var_2)^2/var(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_indice_var_2_gama_scale_dist_lin_mle <- mle2(mut0_indice_var_2_gama_scale_dist_lin, start=mut0_indice_var_2_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
mut0_indice_var_2_gama_dist_lin_glm <- glm(mut0_indice_var_2~mut0_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao MM sobre shape
# mut0_indice_var_2_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*mut0_indice_dist_2/(b+mut0_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut0_indice_var_2_gama_shape_dist_mm_start <- list(a=mut0_indice_var_2_a_mm_start,b=mut0_indice_var_2_b_mm_start,scale=var(mut0_indice_var_2)/mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_indice_var_2_gama_shape_dist_mm_mle <- mle2(mut0_indice_var_2_gama_shape_dist_mm, start=mut0_indice_var_2_gama_shape_dist_mm_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut0_indice_var_2_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado<- (a*mut0_indice_dist_2/(b+mut0_indice_dist_2))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut0_indice_var_2_gama_scale_dist_mm_start <- list(a=mut0_indice_var_2_a_mm_start,b=mut0_indice_var_2_b_mm_start,shape=mean(mut0_indice_var_2)^2/var(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_indice_var_2_gama_scale_dist_mm_mle <- mle2(mut0_indice_var_2_gama_scale_dist_mm, start=mut0_indice_var_2_gama_scale_dist_mm_start,method="Nelder-Mead")
######## ou:
#mut0_indice_var_2_gama_dist_mm_glm <- glm(mut0_indice_dist_2/mut0_indice_var_2~mut0_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre shape
# mut0_indice_var_2_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(mut0_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut0_indice_var_2_gama_shape_dist_pot_start <- list(a=mut0_indice_var_2_a_pot_start,b=mut0_indice_var_2_b_pot_start,scale=var(mut0_indice_var_2)/mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_indice_var_2_gama_shape_dist_pot_mle <- mle2(mut0_indice_var_2_gama_shape_dist_pot, start=mut0_indice_var_2_gama_shape_dist_pot_start,method="Nelder-Mead",control=list(maxit=2000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut0_indice_var_2_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado<- (a*(mut0_indice_dist_2^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut0_indice_var_2_gama_scale_dist_pot_start <- list(a=mut0_indice_var_2_a_pot_start,b=mut0_indice_var_2_b_pot_start,shape=mean(mut0_indice_var_2)^2/var(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_indice_var_2_gama_scale_dist_pot_mle <- mle2(mut0_indice_var_2_gama_scale_dist_pot, start=mut0_indice_var_2_gama_scale_dist_pot_start,method="Nelder-Mead",control=list(maxit=2000))
######## ou:
#mut0_indice_var_2_gama_dist_pot_glm <- glm(mut0_indice_dist_2/mut0_indice_var_2~mut0_indice_dist_2,family="Gapota"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre shape
# mut0_indice_var_2_gama_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(mut0_indice_dist_2^2) + b*(mut0_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut0_indice_var_2_gama_shape_dist_quadr_start <- list(a=mut0_indice_var_2_a_quadr_start,b=mut0_indice_var_2_b_quadr_start,c=mut0_indice_var_2_c_quadr_start,scale=var(mut0_indice_var_2)/mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_indice_var_2_gama_shape_dist_quadr_mle <- mle2(mut0_indice_var_2_gama_shape_dist_quadr, start=mut0_indice_var_2_gama_shape_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut0_indice_var_2_gama_scale_dist_quadr <- function(a,b,c,shape){
  esperancaaoquadrado<- (a*(mut0_indice_dist_2^2) + b*(mut0_indice_dist_2) + c)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut0_indice_var_2_gama_scale_dist_quadr_start <- list(a=mut0_indice_var_2_a_quadr_start,b=mut0_indice_var_2_b_quadr_start,c=mut0_indice_var_2_c_quadr_start,shape=mean(mut0_indice_var_2)^2/var(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_indice_var_2_gama_scale_dist_quadr_mle <- mle2(mut0_indice_var_2_gama_scale_dist_quadr, start=mut0_indice_var_2_gama_scale_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))
######## ou:
#mut0_indice_var_2_gama_dist_quadr_glm <- glm(mut0_indice_dist_2/mut0_indice_var_2~mut0_indice_dist_2,family="Gaquadra"(link="identity"))

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut0_indice_var_2_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*mut0_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut0_indice_var_2_gama_var_shape_dist_lin_start <- list(a=mut0_indice_var_2_a_lin_start,b=mut0_indice_var_2_b_lin_start,scale=var(mut0_indice_var_2)/mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_indice_var_2_gama_var_shape_dist_lin_mle <- mle2(mut0_indice_var_2_gama_var_shape_dist_lin, start=mut0_indice_var_2_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut0_indice_var_2_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*mut0_indice_dist_2+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut0_indice_var_2_gama_var_scale_dist_lin_start <- list(a=mut0_indice_var_2_a_lin_start,b=mut0_indice_var_2_b_lin_start,shape=mean(mut0_indice_var_2)^2/var(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_indice_var_2_gama_var_scale_dist_lin_mle <- mle2(mut0_indice_var_2_gama_var_scale_dist_lin, start=mut0_indice_var_2_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut0_indice_var_2_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*mut0_indice_dist_2/(b+mut0_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut0_indice_var_2_gama_var_shape_dist_mm_start <- list(a=mut0_indice_var_2_a_mm_start,b=mut0_indice_var_2_b_mm_start,scale=var(mut0_indice_var_2)/mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_indice_var_2_gama_var_shape_dist_mm_mle <- mle2(mut0_indice_var_2_gama_var_shape_dist_mm, start=mut0_indice_var_2_gama_var_shape_dist_mm_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut0_indice_var_2_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado<- (a*mut0_indice_dist_2/(b+mut0_indice_dist_2))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut0_indice_var_2_gama_var_scale_dist_mm_start <- list(a=mut0_indice_var_2_a_mm_start,b=mut0_indice_var_2_b_mm_start,shape=mean(mut0_indice_var_2)^2/var(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_indice_var_2_gama_var_scale_dist_mm_mle <- mle2(mut0_indice_var_2_gama_var_scale_dist_mm, start=mut0_indice_var_2_gama_var_scale_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut0_indice_var_2_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(mut0_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut0_indice_var_2_gama_var_shape_dist_pot_start <- list(a=mut0_indice_var_2_a_pot_start,b=mut0_indice_var_2_b_pot_start,scale=var(mut0_indice_var_2)/mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_indice_var_2_gama_var_shape_dist_pot_mle <- mle2(mut0_indice_var_2_gama_var_shape_dist_pot, start=mut0_indice_var_2_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut0_indice_var_2_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado<- (a*(mut0_indice_dist_2^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut0_indice_var_2_gama_var_scale_dist_pot_start <- list(a=mut0_indice_var_2_a_pot_start,b=mut0_indice_var_2_b_pot_start,shape=mean(mut0_indice_var_2)^2/var(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_indice_var_2_gama_var_scale_dist_pot_mle <- mle2(mut0_indice_var_2_gama_var_scale_dist_pot, start=mut0_indice_var_2_gama_var_scale_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut0_indice_var_2_gama_var_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(mut0_indice_dist_2^2) + b*(mut0_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut0_indice_var_2_gama_var_shape_dist_quadr_start <- list(a=mut0_indice_var_2_a_quadr_start,b=mut0_indice_var_2_b_quadr_start,c=mut0_indice_var_2_c_quadr_start,scale=var(mut0_indice_var_2)/mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_indice_var_2_gama_var_shape_dist_quadr_mle <- mle2(mut0_indice_var_2_gama_var_shape_dist_quadr, start=mut0_indice_var_2_gama_var_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut0_indice_var_2_gama_var_scale_dist_quadr <- function(a,b,c,shape){
  varianciaaoquadrado<- (a*(mut0_indice_dist_2^2) + b*(mut0_indice_dist_2) + c)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut0_indice_var_2_gama_var_scale_dist_quadr_start <- list(a=mut0_indice_var_2_a_quadr_start,b=mut0_indice_var_2_b_quadr_start,c=mut0_indice_var_2_c_quadr_start,shape=mean(mut0_indice_var_2)^2/var(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_indice_var_2_gama_var_scale_dist_quadr_mle <- mle2(mut0_indice_var_2_gama_var_scale_dist_quadr, start=mut0_indice_var_2_gama_var_scale_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

# NORMAL

## Nulo
mut0_indice_var_2_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_nulo_start <- list(mean=mean(mut0_indice_var_2),sd=sd(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
mut0_indice_var_2_norm_nulo_mle <- mle2(mut0_indice_var_2_norm_nulo, start=mut0_indice_var_2_norm_nulo_start,method="Nelder-Mead")
########## ou:
mut0_indice_var_2_norm_nulo_glm <- glm(mut0_indice_var_2~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
mut0_indice_var_2_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*mut0_indice_dist_2+b
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_mean_dist_lin_start <- list(a=mut0_indice_var_2_a_lin_start,b=mut0_indice_var_2_b_lin_start,sd=sd(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_mean_dist_lin_mle <- mle2(mut0_indice_var_2_norm_mean_dist_lin, start=mut0_indice_var_2_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
mut0_indice_var_2_norm_dist_lin_glm <- glm(mut0_indice_var_2~mut0_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
mut0_indice_var_2_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*mut0_indice_dist_2+b)^2)
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_sd_dist_lin_start <- list(a=mut0_indice_var_2_a_lin_start,b=mut0_indice_var_2_b_lin_start,mean=mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_sd_dist_lin_mle <- mle2(mut0_indice_var_2_norm_sd_dist_lin, start=mut0_indice_var_2_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media e sd
mut0_indice_var_2_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*mut0_indice_dist_2+b
  sd <- sqrt((c*mut0_indice_dist_2+d)^2)
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_mean_sd_dist_lin_start <- list(a=mut0_indice_var_2_a_lin_start,b=mut0_indice_var_2_b_lin_start,c=mut0_indice_var_2_a_lin_start,d=mut0_indice_var_2_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_mean_sd_dist_lin_mle <- mle2(mut0_indice_var_2_norm_mean_sd_dist_lin, start=mut0_indice_var_2_norm_mean_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media
mut0_indice_var_2_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*mut0_indice_dist_2/(b+mut0_indice_dist_2)
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_mean_dist_mm_start <- list(a=mut0_indice_var_2_a_mm_start,b=mut0_indice_var_2_b_mm_start,sd=sd(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_mean_dist_mm_mle <- mle2(mut0_indice_var_2_norm_mean_dist_mm, start=mut0_indice_var_2_norm_mean_dist_mm_start,method="Nelder-Mead")
######### ou:
mut0_indice_var_2_norm_dist_mm_glm <- glm(mut0_indice_dist_2/mut0_indice_var_2~mut0_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre sd
mut0_indice_var_2_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*mut0_indice_dist_2/(b+mut0_indice_dist_2))^2)
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_sd_dist_mm_start <- list(a=mut0_indice_var_2_a_mm_start,b=mut0_indice_var_2_b_mm_start,mean=mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_sd_dist_mm_mle <- mle2(mut0_indice_var_2_norm_sd_dist_mm, start=mut0_indice_var_2_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media e sd
mut0_indice_var_2_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*mut0_indice_dist_2/(b+mut0_indice_dist_2)
  sd <- sqrt((c*mut0_indice_dist_2/(d+mut0_indice_dist_2))^2)
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_mean_sd_dist_mm_start <- list(a=mut0_indice_var_2_a_mm_start,b=mut0_indice_var_2_b_mm_start,c=mut0_indice_var_2_a_mm_start,d=mut0_indice_var_2_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_mean_sd_dist_mm_mle <- mle2(mut0_indice_var_2_norm_mean_sd_dist_mm, start=mut0_indice_var_2_norm_mean_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media
mut0_indice_var_2_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(mut0_indice_dist_2^b)
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_mean_dist_pot_start <- list(a=mut0_indice_var_2_a_pot_start,b=mut0_indice_var_2_b_pot_start,sd=sd(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_mean_dist_pot_mle <- mle2(mut0_indice_var_2_norm_mean_dist_pot, start=mut0_indice_var_2_norm_mean_dist_pot_start,method="Nelder-Mead")
######### ou:
mut0_indice_var_2_norm_dist_pot_glm <- glm(mut0_indice_dist_2/mut0_indice_var_2~mut0_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre sd
mut0_indice_var_2_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(mut0_indice_dist_2^b))^2)
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_sd_dist_pot_start <- list(a=mut0_indice_var_2_a_pot_start,b=mut0_indice_var_2_b_pot_start,mean=mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_sd_dist_pot_mle <- mle2(mut0_indice_var_2_norm_sd_dist_pot, start=mut0_indice_var_2_norm_sd_dist_pot_start)

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media e sd
mut0_indice_var_2_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(mut0_indice_dist_2^b)
  sd <- sqrt((c*(mut0_indice_dist_2^d))^2)
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_mean_sd_dist_pot_start <- list(a=mut0_indice_var_2_a_pot_start,b=mut0_indice_var_2_b_pot_start,c=mut0_indice_var_2_a_pot_start,d=mut0_indice_var_2_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_mean_sd_dist_pot_mle <- mle2(mut0_indice_var_2_norm_mean_sd_dist_pot, start=mut0_indice_var_2_norm_mean_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media
mut0_indice_var_2_norm_mean_dist_quadr <- function(a,b,c,sd){
  mean <- a*(mut0_indice_dist_2^2) + b*(mut0_indice_dist_2) + c
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_mean_dist_quadr_start <- list(a=mut0_indice_var_2_a_quadr_start,b=mut0_indice_var_2_b_quadr_start,c=mut0_indice_var_2_c_quadr_start,sd=sd(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_mean_dist_quadr_mle <- mle2(mut0_indice_var_2_norm_mean_dist_quadr, start=mut0_indice_var_2_norm_mean_dist_quadr_start,method="Nelder-Mead")
######### ou:
mut0_indice_var_2_norm_dist_quadr_glm <- glm(mut0_indice_dist_2/mut0_indice_var_2~mut0_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre sd
mut0_indice_var_2_norm_sd_dist_quadr <- function(a,b,c,mean){
  sd <- sqrt((a*(mut0_indice_dist_2^2) + b*(mut0_indice_dist_2) + c)^2)
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_sd_dist_quadr_start <- list(a=mut0_indice_var_2_a_quadr_start,b=mut0_indice_var_2_b_quadr_start,c=mut0_indice_var_2_c_quadr_start,mean=mean(mut0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_sd_dist_quadr_mle <- mle2(mut0_indice_var_2_norm_sd_dist_quadr, start=mut0_indice_var_2_norm_sd_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media e sd
mut0_indice_var_2_norm_mean_sd_dist_quadr <- function(a,b,c,d,e,f){
  mean <- a*(mut0_indice_dist_2^2) + b*(mut0_indice_dist_2) + c
  sd <- sqrt((d*(mut0_indice_dist_2^2) + e*(mut0_indice_dist_2) + f)^2)
  -sum(dnorm(mut0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut0_indice_var_2_norm_mean_sd_dist_quadr_start <- list(a=mut0_indice_var_2_a_quadr_start,b=mut0_indice_var_2_b_quadr_start,c=mut0_indice_var_2_c_quadr_start,d=mut0_indice_var_2_a_quadr_start,e=mut0_indice_var_2_b_quadr_start,f=mut0_indice_var_2_c_quadr_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_indice_var_2_norm_mean_sd_dist_quadr_mle <- mle2(mut0_indice_var_2_norm_mean_sd_dist_quadr, start=mut0_indice_var_2_norm_mean_sd_dist_quadr_start)


################### SELECAO DE MODELOS ####################
AICtab(mut0_indice_var_2_gama_nulo_mle,mut0_indice_var_2_gama_scale_dist_lin_mle,mut0_indice_var_2_gama_scale_dist_mm_mle,mut0_indice_var_2_gama_scale_dist_pot_mle,mut0_indice_var_2_gama_scale_dist_quadr_mle,mut0_indice_var_2_gama_var_scale_dist_lin_mle,mut0_indice_var_2_gama_var_scale_dist_mm_mle,mut0_indice_var_2_gama_var_scale_dist_pot_mle,mut0_indice_var_2_gama_var_scale_dist_quadr_mle,mut0_indice_var_2_norm_nulo_mle,mut0_indice_var_2_norm_mean_dist_lin_mle,mut0_indice_var_2_norm_sd_dist_lin_mle,mut0_indice_var_2_norm_mean_sd_dist_lin_mle,mut0_indice_var_2_norm_mean_dist_mm_mle,mut0_indice_var_2_norm_sd_dist_mm_mle,mut0_indice_var_2_norm_mean_sd_dist_mm_mle,mut0_indice_var_2_norm_mean_dist_pot_mle,mut0_indice_var_2_norm_sd_dist_pot_mle,mut0_indice_var_2_norm_mean_sd_dist_pot_mle,mut0_indice_var_2_norm_mean_dist_quadr_mle,mut0_indice_var_2_norm_sd_dist_quadr_mle,mut0_indice_var_2_norm_mean_sd_dist_quadr_mle,weights=T)
### modelo selecionado: mut0_indice_var_2_gama_scale_dist_pot_mle

##########################################################
######################## MUTACAO 1 #######################
##########################################################

mut1_indice_var_2 <- c(bat1_indice_var_2,bat3_indice_var_2)
mut1_indice_dist_2 <- c(bat1_indice_dist_2,bat3_indice_dist_2)

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut1_indice_var_2_lin <- lm(mut1_indice_var_2~mut1_indice_dist_2)
mut1_indice_var_2_a_lin_start <- coef(mut1_indice_var_2_lin)[[2]]
mut1_indice_var_2_b_lin_start <- coef(mut1_indice_var_2_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut1_indice_var_2_mm_linearizada <- lm(mut1_indice_dist_2/mut1_indice_var_2~mut1_indice_dist_2) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
mut1_indice_var_2_a_mm_start <- 1/(coef(mut1_indice_var_2_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
mut1_indice_var_2_b_mm_start <- (coef(mut1_indice_var_2_mm_linearizada)[[1]])*(1/(coef(mut1_indice_var_2_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut1_indice_var_2_a_pot_start <- 0.0009
mut1_indice_var_2_b_pot_start <- 0.028
#plot(bat3_indice_var_2~bat3_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(pot(x,0.0009,0.028),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="purple")

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut1_indice_var_2_a_quadr_start <- bat1_indice_var_2_a_quadr_start
mut1_indice_var_2_b_quadr_start <- bat1_indice_var_2_b_quadr_start
mut1_indice_var_2_c_quadr_start <- bat1_indice_var_2_c_quadr_start

# GAMA

## Nulo
mut1_indice_var_2_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut1_indice_var_2_gama_nulo_start <- list(shape=mean(mut1_indice_var_2)^2/var(mut1_indice_var_2),scale=var(mut1_indice_var_2)/mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
mut1_indice_var_2_gama_nulo_mle <- mle2(mut1_indice_var_2_gama_nulo, start=mut1_indice_var_2_gama_nulo_start,method="Nelder-Mead")
###### ou:
mut1_indice_var_2_gama_nulo_glm <- glm(mut1_indice_var_2~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut1_indice_var_2_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*mut1_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut1_indice_var_2_gama_shape_dist_lin_start <- list(a=mut1_indice_var_2_a_lin_start,b=mut1_indice_var_2_b_lin_start,scale=var(mut1_indice_var_2)/mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_indice_var_2_gama_shape_dist_lin_mle <- mle2(mut1_indice_var_2_gama_shape_dist_lin, start=mut1_indice_var_2_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut1_indice_var_2_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*mut1_indice_dist_2+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut1_indice_var_2_gama_scale_dist_lin_start <- list(a=mut1_indice_var_2_a_lin_start,b=mut1_indice_var_2_b_lin_start,shape=mean(mut1_indice_var_2)^2/var(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_indice_var_2_gama_scale_dist_lin_mle <- mle2(mut1_indice_var_2_gama_scale_dist_lin, start=mut1_indice_var_2_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
mut1_indice_var_2_gama_dist_lin_glm <- glm(mut1_indice_var_2~mut1_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao MM sobre shape
# mut1_indice_var_2_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*mut1_indice_dist_2/(b+mut1_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut1_indice_var_2_gama_shape_dist_mm_start <- list(a=mut1_indice_var_2_a_mm_start,b=mut1_indice_var_2_b_mm_start,scale=var(mut1_indice_var_2)/mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_indice_var_2_gama_shape_dist_mm_mle <- mle2(mut1_indice_var_2_gama_shape_dist_mm, start=mut1_indice_var_2_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut1_indice_var_2_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado<- (a*mut1_indice_dist_2/(b+mut1_indice_dist_2))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut1_indice_var_2_gama_scale_dist_mm_start <- list(a=mut1_indice_var_2_a_mm_start,b=mut1_indice_var_2_b_mm_start,shape=mean(mut1_indice_var_2)^2/var(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_indice_var_2_gama_scale_dist_mm_mle <- mle2(mut1_indice_var_2_gama_scale_dist_mm, start=mut1_indice_var_2_gama_scale_dist_mm_start,method="Nelder-Mead")
######## ou:
#mut1_indice_var_2_gama_dist_mm_glm <- glm(mut1_indice_dist_2/mut1_indice_var_2~mut1_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre shape
# mut1_indice_var_2_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(mut1_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut1_indice_var_2_gama_shape_dist_pot_start <- list(a=mut1_indice_var_2_a_pot_start,b=mut1_indice_var_2_b_pot_start,scale=var(mut1_indice_var_2)/mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_indice_var_2_gama_shape_dist_pot_mle <- mle2(mut1_indice_var_2_gama_shape_dist_pot, start=mut1_indice_var_2_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut1_indice_var_2_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado<- (a*(mut1_indice_dist_2^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut1_indice_var_2_gama_scale_dist_pot_start <- list(a=mut1_indice_var_2_a_pot_start,b=mut1_indice_var_2_b_pot_start,shape=mean(mut1_indice_var_2)^2/var(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_indice_var_2_gama_scale_dist_pot_mle <- mle2(mut1_indice_var_2_gama_scale_dist_pot, start=mut1_indice_var_2_gama_scale_dist_pot_start,method="Nelder-Mead")
######## ou:
#mut1_indice_var_2_gama_dist_pot_glm <- glm(mut1_indice_dist_2/mut1_indice_var_2~mut1_indice_dist_2,family="Gapota"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre shape
# mut1_indice_var_2_gama_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(mut1_indice_dist_2^2) + b*(mut1_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut1_indice_var_2_gama_shape_dist_quadr_start <- list(a=mut1_indice_var_2_a_quadr_start,b=mut1_indice_var_2_b_quadr_start,c=mut1_indice_var_2_c_quadr_start,scale=var(mut1_indice_var_2)/mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_indice_var_2_gama_shape_dist_quadr_mle <- mle2(mut1_indice_var_2_gama_shape_dist_quadr, start=mut1_indice_var_2_gama_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut1_indice_var_2_gama_scale_dist_quadr <- function(a,b,c,shape){
  esperancaaoquadrado<- (a*(mut1_indice_dist_2^2) + b*(mut1_indice_dist_2) + c)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut1_indice_var_2_gama_scale_dist_quadr_start <- list(a=mut1_indice_var_2_a_quadr_start,b=mut1_indice_var_2_b_quadr_start,c=mut1_indice_var_2_c_quadr_start,shape=mean(mut1_indice_var_2)^2/var(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_indice_var_2_gama_scale_dist_quadr_mle <- mle2(mut1_indice_var_2_gama_scale_dist_quadr, start=mut1_indice_var_2_gama_scale_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut1_indice_var_2_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*mut1_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut1_indice_var_2_gama_var_shape_dist_lin_start <- list(a=mut1_indice_var_2_a_lin_start,b=mut1_indice_var_2_b_lin_start,scale=var(mut1_indice_var_2)/mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_indice_var_2_gama_var_shape_dist_lin_mle <- mle2(mut1_indice_var_2_gama_var_shape_dist_lin, start=mut1_indice_var_2_gama_var_shape_dist_lin_start,method="SANN")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut1_indice_var_2_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*mut1_indice_dist_2+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut1_indice_var_2_gama_var_scale_dist_lin_start <- list(a=mut1_indice_var_2_a_lin_start,b=mut1_indice_var_2_b_lin_start,shape=mean(mut1_indice_var_2)^2/var(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_indice_var_2_gama_var_scale_dist_lin_mle <- mle2(mut1_indice_var_2_gama_var_scale_dist_lin, start=mut1_indice_var_2_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut1_indice_var_2_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*mut1_indice_dist_2/(b+mut1_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut1_indice_var_2_gama_var_shape_dist_mm_start <- list(a=mut1_indice_var_2_a_mm_start,b=mut1_indice_var_2_b_mm_start,scale=var(mut1_indice_var_2)/mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_indice_var_2_gama_var_shape_dist_mm_mle <- mle2(mut1_indice_var_2_gama_var_shape_dist_mm, start=mut1_indice_var_2_gama_var_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut1_indice_var_2_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado<- (a*mut1_indice_dist_2/(b+mut1_indice_dist_2))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut1_indice_var_2_gama_var_scale_dist_mm_start <- list(a=mut1_indice_var_2_a_mm_start,b=mut1_indice_var_2_b_mm_start,shape=mean(mut1_indice_var_2)^2/var(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_indice_var_2_gama_var_scale_dist_mm_mle <- mle2(mut1_indice_var_2_gama_var_scale_dist_mm, start=mut1_indice_var_2_gama_var_scale_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut1_indice_var_2_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(mut1_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut1_indice_var_2_gama_var_shape_dist_pot_start <- list(a=mut1_indice_var_2_a_pot_start,b=mut1_indice_var_2_b_pot_start,scale=var(mut1_indice_var_2)/mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_indice_var_2_gama_var_shape_dist_pot_mle <- mle2(mut1_indice_var_2_gama_var_shape_dist_pot, start=mut1_indice_var_2_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut1_indice_var_2_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado<- (a*(mut1_indice_dist_2^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut1_indice_var_2_gama_var_scale_dist_pot_start <- list(a=mut1_indice_var_2_a_pot_start,b=mut1_indice_var_2_b_pot_start,shape=mean(mut1_indice_var_2)^2/var(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_indice_var_2_gama_var_scale_dist_pot_mle <- mle2(mut1_indice_var_2_gama_var_scale_dist_pot, start=mut1_indice_var_2_gama_var_scale_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut1_indice_var_2_gama_var_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(mut1_indice_dist_2^2) + b*(mut1_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# mut1_indice_var_2_gama_var_shape_dist_quadr_start <- list(a=mut1_indice_var_2_a_quadr_start,b=mut1_indice_var_2_b_quadr_start,c=mut1_indice_var_2_c_quadr_start,scale=var(mut1_indice_var_2)/mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_indice_var_2_gama_var_shape_dist_quadr_mle <- mle2(mut1_indice_var_2_gama_var_shape_dist_quadr, start=mut1_indice_var_2_gama_var_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut1_indice_var_2_gama_var_scale_dist_quadr <- function(a,b,c,shape){
  varianciaaoquadrado<- (a*(mut1_indice_dist_2^2) + b*(mut1_indice_dist_2) + c)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
mut1_indice_var_2_gama_var_scale_dist_quadr_start <- list(a=mut1_indice_var_2_a_quadr_start,b=mut1_indice_var_2_b_quadr_start,c=mut1_indice_var_2_c_quadr_start,shape=mean(mut1_indice_var_2)^2/var(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_indice_var_2_gama_var_scale_dist_quadr_mle <- mle2(mut1_indice_var_2_gama_var_scale_dist_quadr, start=mut1_indice_var_2_gama_var_scale_dist_quadr_start)

# NORMAL

## Nulo
mut1_indice_var_2_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_nulo_start <- list(mean=mean(mut1_indice_var_2),sd=sd(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
mut1_indice_var_2_norm_nulo_mle <- mle2(mut1_indice_var_2_norm_nulo, start=mut1_indice_var_2_norm_nulo_start,method="Nelder-Mead")
########## ou:
mut1_indice_var_2_norm_nulo_glm <- glm(mut1_indice_var_2~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
mut1_indice_var_2_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*mut1_indice_dist_2+b
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_mean_dist_lin_start <- list(a=mut1_indice_var_2_a_lin_start,b=mut1_indice_var_2_b_lin_start,sd=sd(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_mean_dist_lin_mle <- mle2(mut1_indice_var_2_norm_mean_dist_lin, start=mut1_indice_var_2_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
mut1_indice_var_2_norm_dist_lin_glm <- glm(mut1_indice_var_2~mut1_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
mut1_indice_var_2_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*mut1_indice_dist_2+b)^2)
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_sd_dist_lin_start <- list(a=mut1_indice_var_2_a_lin_start,b=mut1_indice_var_2_b_lin_start,mean=mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_sd_dist_lin_mle <- mle2(mut1_indice_var_2_norm_sd_dist_lin, start=mut1_indice_var_2_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media e sd
mut1_indice_var_2_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*mut1_indice_dist_2+b
  sd <- sqrt((c*mut1_indice_dist_2+d)^2)
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_mean_sd_dist_lin_start <- list(a=mut1_indice_var_2_a_lin_start,b=mut1_indice_var_2_b_lin_start,c=mut1_indice_var_2_a_lin_start,d=mut1_indice_var_2_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_mean_sd_dist_lin_mle <- mle2(mut1_indice_var_2_norm_mean_sd_dist_lin, start=mut1_indice_var_2_norm_mean_sd_dist_lin_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media
mut1_indice_var_2_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*mut1_indice_dist_2/(b+mut1_indice_dist_2)
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_mean_dist_mm_start <- list(a=mut1_indice_var_2_a_mm_start,b=mut1_indice_var_2_b_mm_start,sd=sd(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_mean_dist_mm_mle <- mle2(mut1_indice_var_2_norm_mean_dist_mm, start=mut1_indice_var_2_norm_mean_dist_mm_start,method="Nelder-Mead")
######### ou:
mut1_indice_var_2_norm_dist_mm_glm <- glm(mut1_indice_dist_2/mut1_indice_var_2~mut1_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre sd
mut1_indice_var_2_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*mut1_indice_dist_2/(b+mut1_indice_dist_2))^2)
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_sd_dist_mm_start <- list(a=mut1_indice_var_2_a_mm_start,b=mut1_indice_var_2_b_mm_start,mean=mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_sd_dist_mm_mle <- mle2(mut1_indice_var_2_norm_sd_dist_mm, start=mut1_indice_var_2_norm_sd_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media e sd
mut1_indice_var_2_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*mut1_indice_dist_2/(b+mut1_indice_dist_2)
  sd <- sqrt((c*mut1_indice_dist_2/(d+mut1_indice_dist_2))^2)
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_mean_sd_dist_mm_start <- list(a=mut1_indice_var_2_a_mm_start,b=mut1_indice_var_2_b_mm_start,c=mut1_indice_var_2_a_mm_start,d=mut1_indice_var_2_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_mean_sd_dist_mm_mle <- mle2(mut1_indice_var_2_norm_mean_sd_dist_mm, start=mut1_indice_var_2_norm_mean_sd_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media
mut1_indice_var_2_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(mut1_indice_dist_2^b)
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_mean_dist_pot_start <- list(a=mut1_indice_var_2_a_pot_start,b=mut1_indice_var_2_b_pot_start,sd=sd(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_mean_dist_pot_mle <- mle2(mut1_indice_var_2_norm_mean_dist_pot, start=mut1_indice_var_2_norm_mean_dist_pot_start,method="Nelder-Mead")
######### ou:
mut1_indice_var_2_norm_dist_pot_glm <- glm(mut1_indice_dist_2/mut1_indice_var_2~mut1_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre sd
mut1_indice_var_2_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(mut1_indice_dist_2^b))^2)
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_sd_dist_pot_start <- list(a=mut1_indice_var_2_a_pot_start,b=mut1_indice_var_2_b_pot_start,mean=mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_sd_dist_pot_mle <- mle2(mut1_indice_var_2_norm_sd_dist_pot, start=mut1_indice_var_2_norm_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media e sd
mut1_indice_var_2_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(mut1_indice_dist_2^b)
  sd <- sqrt((c*(mut1_indice_dist_2^d))^2)
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_mean_sd_dist_pot_start <- list(a=mut1_indice_var_2_a_pot_start,b=mut1_indice_var_2_b_pot_start,c=mut1_indice_var_2_a_pot_start,d=mut1_indice_var_2_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_mean_sd_dist_pot_mle <- mle2(mut1_indice_var_2_norm_mean_sd_dist_pot, start=mut1_indice_var_2_norm_mean_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media
mut1_indice_var_2_norm_mean_dist_quadr <- function(a,b,c,sd){
  mean <- a*(mut1_indice_dist_2^2) + b*(mut1_indice_dist_2) + c
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_mean_dist_quadr_start <- list(a=mut1_indice_var_2_a_quadr_start,b=mut1_indice_var_2_b_quadr_start,c=mut1_indice_var_2_c_quadr_start,sd=sd(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_mean_dist_quadr_mle <- mle2(mut1_indice_var_2_norm_mean_dist_quadr, start=mut1_indice_var_2_norm_mean_dist_quadr_start,control=list(maxit=1000),method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre sd
mut1_indice_var_2_norm_sd_dist_quadr <- function(a,b,c,mean){
  sd <- sqrt((a*(mut1_indice_dist_2^2) + b*(mut1_indice_dist_2) + c)^2)
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_sd_dist_quadr_start <- list(a=mut1_indice_var_2_a_quadr_start,b=mut1_indice_var_2_b_quadr_start,c=mut1_indice_var_2_c_quadr_start,mean=mean(mut1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_sd_dist_quadr_mle <- mle2(mut1_indice_var_2_norm_sd_dist_quadr, start=mut1_indice_var_2_norm_sd_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media e sd
mut1_indice_var_2_norm_mean_sd_dist_quadr <- function(a,b,c,d,e,f){
  mean <- a*(mut1_indice_dist_2^2) + b*(mut1_indice_dist_2) + c
  sd <- sqrt((d*(mut1_indice_dist_2^2) + e*(mut1_indice_dist_2) + f)^2)
  -sum(dnorm(mut1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
mut1_indice_var_2_norm_mean_sd_dist_quadr_start <- list(a=mut1_indice_var_2_a_quadr_start,b=mut1_indice_var_2_b_quadr_start,c=mut1_indice_var_2_c_quadr_start,d=mut1_indice_var_2_a_quadr_start,e=mut1_indice_var_2_b_quadr_start,f=mut1_indice_var_2_c_quadr_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_indice_var_2_norm_mean_sd_dist_quadr_mle <- mle2(mut1_indice_var_2_norm_mean_sd_dist_quadr, start=mut1_indice_var_2_norm_mean_sd_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

################### SELECAO DE MODELOS ####################
AICtab(mut1_indice_var_2_gama_nulo_mle,mut1_indice_var_2_gama_scale_dist_lin_mle,mut1_indice_var_2_gama_scale_dist_mm_mle,mut1_indice_var_2_gama_scale_dist_pot_mle,mut1_indice_var_2_gama_scale_dist_quadr_mle,mut1_indice_var_2_gama_var_scale_dist_lin_mle,mut1_indice_var_2_gama_var_scale_dist_mm_mle,mut1_indice_var_2_gama_var_scale_dist_pot_mle,mut1_indice_var_2_gama_var_scale_dist_quadr_mle,mut1_indice_var_2_norm_nulo_mle,mut1_indice_var_2_norm_mean_dist_lin_mle,mut1_indice_var_2_norm_sd_dist_lin_mle,mut1_indice_var_2_norm_mean_sd_dist_lin_mle,mut1_indice_var_2_norm_mean_dist_mm_mle,mut1_indice_var_2_norm_sd_dist_mm_mle,mut1_indice_var_2_norm_mean_sd_dist_mm_mle,mut1_indice_var_2_norm_mean_dist_pot_mle,mut1_indice_var_2_norm_sd_dist_pot_mle,mut1_indice_var_2_norm_mean_sd_dist_pot_mle,mut1_indice_var_2_norm_mean_dist_quadr_mle,mut1_indice_var_2_norm_sd_dist_quadr_mle,mut1_indice_var_2_norm_mean_sd_dist_quadr_mle,weights=T)
### modelo selecionado: mut1_indice_var_2_norm_sd_dist_quadr_mle


##########################################################
######## CALCULO DO AIC SUBCONJUNTO MUTACAO TOTAL ########
##########################################################
aic_mle_soma_sub_mutacao_var_2 <- AIC(mut0_indice_var_2_gama_scale_dist_pot_mle) + AIC(mut1_indice_var_2_norm_sd_dist_quadr_mle) #-54192.93





##########################################################
##########################################################
### Analises das baterias de simulacao EM SUBCONJUNTOS ###
###################### por RIQUEZA #######################
##########################################################
##########################################################

##########################################################
######################## RIQUEZA 0 #######################
##########################################################

riq0_indice_var_2<- bat1_indice_var_2
riq0_indice_dist_2 <- bat1_indice_dist_2

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq0_indice_var_2_lin <- lm(riq0_indice_var_2~riq0_indice_dist_2)
riq0_indice_var_2_a_lin_start <- coef(riq0_indice_var_2_lin)[[2]]
riq0_indice_var_2_b_lin_start <- coef(riq0_indice_var_2_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq0_indice_var_2_mm_linearizada <- lm(riq0_indice_dist_2/riq0_indice_var_2~riq0_indice_dist_2) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
riq0_indice_var_2_a_mm_start <- 1/(coef(riq0_indice_var_2_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
riq0_indice_var_2_b_mm_start <- (coef(riq0_indice_var_2_mm_linearizada)[[1]])*(1/(coef(riq0_indice_var_2_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq0_indice_var_2_a_pot_start <- 0.001
riq0_indice_var_2_b_pot_start <- 0.018
#plot(bat1_indice_var_2~bat1_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(pot(x,0.001,0.018),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="purple")

# Valores de start para os coeficientes a e b da funcao quadratica usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq0_indice_var_2_a_quadr_start <- bat1_indice_var_2_a_quadr_start
riq0_indice_var_2_b_quadr_start <- bat1_indice_var_2_b_quadr_start
riq0_indice_var_2_c_quadr_start <- bat1_indice_var_2_c_quadr_start
#plot(bat1_indice_var_2~bat1_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(quadra(x,-0.0000000000001,0.000000022,0.00025),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="green")

# GAMA

## Nulo
riq0_indice_var_2_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq0_indice_var_2_gama_nulo_start <- list(shape=mean(riq0_indice_var_2)^2/var(riq0_indice_var_2),scale=var(riq0_indice_var_2)/mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
riq0_indice_var_2_gama_nulo_mle <- mle2(riq0_indice_var_2_gama_nulo, start=riq0_indice_var_2_gama_nulo_start,method="Nelder-Mead")
###### ou:
riq0_indice_var_2_gama_nulo_glm <- glm(riq0_indice_var_2~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq0_indice_var_2_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*riq0_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq0_indice_var_2_gama_shape_dist_lin_start <- list(a=riq0_indice_var_2_a_lin_start,b=riq0_indice_var_2_b_lin_start,scale=var(riq0_indice_var_2)/mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_indice_var_2_gama_shape_dist_lin_mle <- mle2(riq0_indice_var_2_gama_shape_dist_lin, start=riq0_indice_var_2_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq0_indice_var_2_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*riq0_indice_dist_2+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq0_indice_var_2_gama_scale_dist_lin_start <- list(a=riq0_indice_var_2_a_lin_start,b=riq0_indice_var_2_b_lin_start,shape=mean(riq0_indice_var_2)^2/var(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_indice_var_2_gama_scale_dist_lin_mle <- mle2(riq0_indice_var_2_gama_scale_dist_lin, start=riq0_indice_var_2_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
riq0_indice_var_2_gama_dist_lin_glm <- glm(riq0_indice_var_2~riq0_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao MM sobre shape
# riq0_indice_var_2_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*riq0_indice_dist_2/(b+riq0_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq0_indice_var_2_gama_shape_dist_mm_start <- list(a=riq0_indice_var_2_a_mm_start,b=riq0_indice_var_2_b_mm_start,scale=var(riq0_indice_var_2)/mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_indice_var_2_gama_shape_dist_mm_mle <- mle2(riq0_indice_var_2_gama_shape_dist_mm, start=riq0_indice_var_2_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq0_indice_var_2_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado<- (a*riq0_indice_dist_2/(b+riq0_indice_dist_2))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq0_indice_var_2_gama_scale_dist_mm_start <- list(a=riq0_indice_var_2_a_mm_start,b=riq0_indice_var_2_b_mm_start,shape=mean(riq0_indice_var_2)^2/var(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_indice_var_2_gama_scale_dist_mm_mle <- mle2(riq0_indice_var_2_gama_scale_dist_mm, start=riq0_indice_var_2_gama_scale_dist_mm_start,method="Nelder-Mead")
######## ou:
#riq0_indice_var_2_gama_dist_mm_glm <- glm(riq0_indice_dist_2/riq0_indice_var_2~riq0_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre shape
# riq0_indice_var_2_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(riq0_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq0_indice_var_2_gama_shape_dist_pot_start <- list(a=riq0_indice_var_2_a_pot_start,b=riq0_indice_var_2_b_pot_start,scale=var(riq0_indice_var_2)/mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_indice_var_2_gama_shape_dist_pot_mle <- mle2(riq0_indice_var_2_gama_shape_dist_pot, start=riq0_indice_var_2_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq0_indice_var_2_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado<- (a*(riq0_indice_dist_2^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq0_indice_var_2_gama_scale_dist_pot_start <- list(a=riq0_indice_var_2_a_pot_start,b=riq0_indice_var_2_b_pot_start,shape=mean(riq0_indice_var_2)^2/var(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_indice_var_2_gama_scale_dist_pot_mle <- mle2(riq0_indice_var_2_gama_scale_dist_pot, start=riq0_indice_var_2_gama_scale_dist_pot_start,method="Nelder-Mead")
######## ou:
#riq0_indice_var_2_gama_dist_pot_glm <- glm(riq0_indice_dist_2/riq0_indice_var_2~riq0_indice_dist_2,family="Gapota"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre shape
# riq0_indice_var_2_gama_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(riq0_indice_dist_2^2) + b*(riq0_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq0_indice_var_2_gama_shape_dist_quadr_start <- list(a=riq0_indice_var_2_a_quadr_start,b=riq0_indice_var_2_b_quadr_start,c=riq0_indice_var_2_c_quadr_start,scale=var(riq0_indice_var_2)/mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_indice_var_2_gama_shape_dist_quadr_mle <- mle2(riq0_indice_var_2_gama_shape_dist_quadr, start=riq0_indice_var_2_gama_shape_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq0_indice_var_2_gama_scale_dist_quadr <- function(a,b,c,shape){
  esperancaaoquadrado<- (a*(riq0_indice_dist_2^2) + b*(riq0_indice_dist_2) + c)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq0_indice_var_2_gama_scale_dist_quadr_start <- list(a=riq0_indice_var_2_a_quadr_start,b=riq0_indice_var_2_b_quadr_start,c=riq0_indice_var_2_c_quadr_start,shape=mean(riq0_indice_var_2)^2/var(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_indice_var_2_gama_scale_dist_quadr_mle <- mle2(riq0_indice_var_2_gama_scale_dist_quadr, start=riq0_indice_var_2_gama_scale_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))
######## ou:
#riq0_indice_var_2_gama_dist_quadr_glm <- glm(riq0_indice_dist_2/riq0_indice_var_2~riq0_indice_dist_2,family="Gaquadra"(link="identity"))

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq0_indice_var_2_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*riq0_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq0_indice_var_2_gama_var_shape_dist_lin_start <- list(a=riq0_indice_var_2_a_lin_start,b=riq0_indice_var_2_b_lin_start,scale=var(riq0_indice_var_2)/mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_indice_var_2_gama_var_shape_dist_lin_mle <- mle2(riq0_indice_var_2_gama_var_shape_dist_lin, start=riq0_indice_var_2_gama_var_shape_dist_lin_start,method="SANN")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq0_indice_var_2_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*riq0_indice_dist_2+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq0_indice_var_2_gama_var_scale_dist_lin_start <- list(a=riq0_indice_var_2_a_lin_start,b=riq0_indice_var_2_b_lin_start,shape=mean(riq0_indice_var_2)^2/var(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_indice_var_2_gama_var_scale_dist_lin_mle <- mle2(riq0_indice_var_2_gama_var_scale_dist_lin, start=riq0_indice_var_2_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq0_indice_var_2_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*riq0_indice_dist_2/(b+riq0_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq0_indice_var_2_gama_var_shape_dist_mm_start <- list(a=riq0_indice_var_2_a_mm_start,b=riq0_indice_var_2_b_mm_start,scale=var(riq0_indice_var_2)/mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_indice_var_2_gama_var_shape_dist_mm_mle <- mle2(riq0_indice_var_2_gama_var_shape_dist_mm, start=riq0_indice_var_2_gama_var_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq0_indice_var_2_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado<- (a*riq0_indice_dist_2/(b+riq0_indice_dist_2))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq0_indice_var_2_gama_var_scale_dist_mm_start <- list(a=riq0_indice_var_2_a_mm_start,b=riq0_indice_var_2_b_mm_start,shape=mean(riq0_indice_var_2)^2/var(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_indice_var_2_gama_var_scale_dist_mm_mle <- mle2(riq0_indice_var_2_gama_var_scale_dist_mm, start=riq0_indice_var_2_gama_var_scale_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq0_indice_var_2_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(riq0_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq0_indice_var_2_gama_var_shape_dist_pot_start <- list(a=riq0_indice_var_2_a_pot_start,b=riq0_indice_var_2_b_pot_start,scale=var(riq0_indice_var_2)/mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_indice_var_2_gama_var_shape_dist_pot_mle <- mle2(riq0_indice_var_2_gama_var_shape_dist_pot, start=riq0_indice_var_2_gama_var_shape_dist_pot_start,method="SANN")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq0_indice_var_2_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado<- (a*(riq0_indice_dist_2^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq0_indice_var_2_gama_var_scale_dist_pot_start <- list(a=riq0_indice_var_2_a_pot_start,b=riq0_indice_var_2_b_pot_start,shape=mean(riq0_indice_var_2)^2/var(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_indice_var_2_gama_var_scale_dist_pot_mle <- mle2(riq0_indice_var_2_gama_var_scale_dist_pot, start=riq0_indice_var_2_gama_var_scale_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq0_indice_var_2_gama_var_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(riq0_indice_dist_2^2) + b*(riq0_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq0_indice_var_2_gama_var_shape_dist_quadr_start <- list(a=riq0_indice_var_2_a_quadr_start,b=riq0_indice_var_2_b_quadr_start,c=riq0_indice_var_2_c_quadr_start,scale=var(riq0_indice_var_2)/mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_indice_var_2_gama_var_shape_dist_quadr_mle <- mle2(riq0_indice_var_2_gama_var_shape_dist_quadr, start=riq0_indice_var_2_gama_var_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq0_indice_var_2_gama_var_scale_dist_quadr <- function(a,b,c,shape){
  varianciaaoquadrado<- (a*(riq0_indice_dist_2^2) + b*(riq0_indice_dist_2) + c)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq0_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq0_indice_var_2_gama_var_scale_dist_quadr_start <- list(a=riq0_indice_var_2_a_quadr_start,b=riq0_indice_var_2_b_quadr_start,c=riq0_indice_var_2_c_quadr_start,shape=mean(riq0_indice_var_2)^2/var(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_indice_var_2_gama_var_scale_dist_quadr_mle <- mle2(riq0_indice_var_2_gama_var_scale_dist_quadr, start=riq0_indice_var_2_gama_var_scale_dist_quadr_start)

# NORMAL

## Nulo
riq0_indice_var_2_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_nulo_start <- list(mean=mean(riq0_indice_var_2),sd=sd(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
riq0_indice_var_2_norm_nulo_mle <- mle2(riq0_indice_var_2_norm_nulo, start=riq0_indice_var_2_norm_nulo_start,method="Nelder-Mead")
########## ou:
riq0_indice_var_2_norm_nulo_glm <- glm(riq0_indice_var_2~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
riq0_indice_var_2_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*riq0_indice_dist_2+b
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_mean_dist_lin_start <- list(a=riq0_indice_var_2_a_lin_start,b=riq0_indice_var_2_b_lin_start,sd=sd(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_mean_dist_lin_mle <- mle2(riq0_indice_var_2_norm_mean_dist_lin, start=riq0_indice_var_2_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
riq0_indice_var_2_norm_dist_lin_glm <- glm(riq0_indice_var_2~riq0_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
riq0_indice_var_2_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*riq0_indice_dist_2+b)^2)
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_sd_dist_lin_start <- list(a=riq0_indice_var_2_a_lin_start,b=riq0_indice_var_2_b_lin_start,mean=mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_sd_dist_lin_mle <- mle2(riq0_indice_var_2_norm_sd_dist_lin, start=riq0_indice_var_2_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media e sd
riq0_indice_var_2_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*riq0_indice_dist_2+b
  sd <- sqrt((c*riq0_indice_dist_2+d)^2)
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_mean_sd_dist_lin_start <- list(a=riq0_indice_var_2_a_lin_start,b=riq0_indice_var_2_b_lin_start,c=riq0_indice_var_2_a_lin_start,d=riq0_indice_var_2_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_mean_sd_dist_lin_mle <- mle2(riq0_indice_var_2_norm_mean_sd_dist_lin, start=riq0_indice_var_2_norm_mean_sd_dist_lin_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media
riq0_indice_var_2_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*riq0_indice_dist_2/(b+riq0_indice_dist_2)
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_mean_dist_mm_start <- list(a=riq0_indice_var_2_a_mm_start,b=riq0_indice_var_2_b_mm_start,sd=sd(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_mean_dist_mm_mle <- mle2(riq0_indice_var_2_norm_mean_dist_mm, start=riq0_indice_var_2_norm_mean_dist_mm_start,method="Nelder-Mead")
######### ou:
riq0_indice_var_2_norm_dist_mm_glm <- glm(riq0_indice_dist_2/riq0_indice_var_2~riq0_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre sd
riq0_indice_var_2_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*riq0_indice_dist_2/(b+riq0_indice_dist_2))^2)
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_sd_dist_mm_start <- list(a=riq0_indice_var_2_a_mm_start,b=riq0_indice_var_2_b_mm_start,mean=mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_sd_dist_mm_mle <- mle2(riq0_indice_var_2_norm_sd_dist_mm, start=riq0_indice_var_2_norm_sd_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media e sd
riq0_indice_var_2_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*riq0_indice_dist_2/(b+riq0_indice_dist_2)
  sd <- sqrt((c*riq0_indice_dist_2/(d+riq0_indice_dist_2))^2)
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_mean_sd_dist_mm_start <- list(a=riq0_indice_var_2_a_mm_start,b=riq0_indice_var_2_b_mm_start,c=riq0_indice_var_2_a_mm_start,d=riq0_indice_var_2_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_mean_sd_dist_mm_mle <- mle2(riq0_indice_var_2_norm_mean_sd_dist_mm, start=riq0_indice_var_2_norm_mean_sd_dist_mm_start,control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media
riq0_indice_var_2_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(riq0_indice_dist_2^b)
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_mean_dist_pot_start <- list(a=riq0_indice_var_2_a_pot_start,b=riq0_indice_var_2_b_pot_start,sd=sd(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_mean_dist_pot_mle <- mle2(riq0_indice_var_2_norm_mean_dist_pot, start=riq0_indice_var_2_norm_mean_dist_pot_start,method="Nelder-Mead")


## Com variavel preditora (disturbio) por meio de uma funcao pot sobre sd
riq0_indice_var_2_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(riq0_indice_dist_2^b))^2)
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_sd_dist_pot_start <- list(a=riq0_indice_var_2_a_pot_start,b=riq0_indice_var_2_b_pot_start,mean=mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_sd_dist_pot_mle <- mle2(riq0_indice_var_2_norm_sd_dist_pot, start=riq0_indice_var_2_norm_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media e sd
riq0_indice_var_2_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(riq0_indice_dist_2^b)
  sd <- sqrt((c*(riq0_indice_dist_2^d))^2)
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_mean_sd_dist_pot_start <- list(a=riq0_indice_var_2_a_pot_start,b=riq0_indice_var_2_b_pot_start,c=riq0_indice_var_2_a_pot_start,d=riq0_indice_var_2_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_mean_sd_dist_pot_mle <- mle2(riq0_indice_var_2_norm_mean_sd_dist_pot, start=riq0_indice_var_2_norm_mean_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=2000))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media
riq0_indice_var_2_norm_mean_dist_quadr <- function(a,b,c,sd){
  mean <- a*(riq0_indice_dist_2^2) + b*(riq0_indice_dist_2) + c
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_mean_dist_quadr_start <- list(a=riq0_indice_var_2_a_quadr_start,b=riq0_indice_var_2_b_quadr_start,c=riq0_indice_var_2_c_quadr_start,sd=sd(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_mean_dist_quadr_mle <- mle2(riq0_indice_var_2_norm_mean_dist_quadr, start=riq0_indice_var_2_norm_mean_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre sd
riq0_indice_var_2_norm_sd_dist_quadr <- function(a,b,c,mean){
  sd <- sqrt((a*(riq0_indice_dist_2^2) + b*(riq0_indice_dist_2) + c)^2)
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_sd_dist_quadr_start <- list(a=riq0_indice_var_2_a_quadr_start,b=riq0_indice_var_2_b_quadr_start,c=riq0_indice_var_2_c_quadr_start,mean=mean(riq0_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_sd_dist_quadr_mle <- mle2(riq0_indice_var_2_norm_sd_dist_quadr, start=riq0_indice_var_2_norm_sd_dist_quadr_start)

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media e sd
riq0_indice_var_2_norm_mean_sd_dist_quadr <- function(a,b,c,d,e,f){
  mean <- a*(riq0_indice_dist_2^2) + b*(riq0_indice_dist_2) + c
  sd <- sqrt((d*(riq0_indice_dist_2^2) + e*(riq0_indice_dist_2) + f)^2)
  -sum(dnorm(riq0_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq0_indice_var_2_norm_mean_sd_dist_quadr_start <- list(a=riq0_indice_var_2_a_quadr_start,b=riq0_indice_var_2_b_quadr_start,c=riq0_indice_var_2_c_quadr_start,d=riq0_indice_var_2_a_quadr_start,e=riq0_indice_var_2_b_quadr_start,f=riq0_indice_var_2_c_quadr_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_indice_var_2_norm_mean_sd_dist_quadr_mle <- mle2(riq0_indice_var_2_norm_mean_sd_dist_quadr, start=riq0_indice_var_2_norm_mean_sd_dist_quadr_start)

################### SELECAO DE MODELOS ####################
AICtab(riq0_indice_var_2_gama_nulo_mle,riq0_indice_var_2_gama_scale_dist_lin_mle,riq0_indice_var_2_gama_scale_dist_mm_mle,riq0_indice_var_2_gama_scale_dist_pot_mle,riq0_indice_var_2_gama_scale_dist_quadr_mle,riq0_indice_var_2_gama_var_scale_dist_lin_mle,riq0_indice_var_2_gama_var_scale_dist_mm_mle,riq0_indice_var_2_gama_var_scale_dist_pot_mle,riq0_indice_var_2_gama_var_scale_dist_quadr_mle,riq0_indice_var_2_norm_nulo_mle,riq0_indice_var_2_norm_mean_dist_lin_mle,riq0_indice_var_2_norm_sd_dist_lin_mle,riq0_indice_var_2_norm_mean_sd_dist_lin_mle,riq0_indice_var_2_norm_mean_dist_mm_mle,riq0_indice_var_2_norm_sd_dist_mm_mle,riq0_indice_var_2_norm_mean_sd_dist_mm_mle,riq0_indice_var_2_norm_mean_dist_pot_mle,riq0_indice_var_2_norm_sd_dist_pot_mle,riq0_indice_var_2_norm_mean_sd_dist_pot_mle,riq0_indice_var_2_norm_mean_dist_quadr_mle,riq0_indice_var_2_norm_sd_dist_quadr_mle,riq0_indice_var_2_norm_mean_sd_dist_quadr_mle,weights=T)
### modelo selecionado: riq0_indice_var_2_norm_mean_dist_mm_mle

##########################################################
######################## RIQUEZA 1 #######################
##########################################################

riq1_indice_var_2<- c(bat2_indice_var_2,bat3_indice_var_2)
riq1_indice_dist_2 <- c(bat2_indice_dist_2,bat3_indice_dist_2)

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq1_indice_var_2_lin <- lm(riq1_indice_var_2~riq1_indice_dist_2)
riq1_indice_var_2_a_lin_start <- coef(riq1_indice_var_2_lin)[[2]]
riq1_indice_var_2_b_lin_start <- coef(riq1_indice_var_2_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq1_indice_var_2_mm_linearizada <- lm(riq1_indice_dist_2/riq1_indice_var_2~riq1_indice_dist_2) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
riq1_indice_var_2_a_mm_start <- 1/(coef(riq1_indice_var_2_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
riq1_indice_var_2_b_mm_start <- (coef(riq1_indice_var_2_mm_linearizada)[[1]])*(1/(coef(riq1_indice_var_2_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq1_indice_var_2_a_pot_start <- 0.0002
riq1_indice_var_2_b_pot_start <- 0.1
#plot(riq1_indice_var_2~riq1_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(pot(x,0.0002,0.1),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="purple")

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq1_indice_var_2_a_quadr_start <- bat1_indice_var_2_a_quadr_start
riq1_indice_var_2_b_quadr_start <- bat1_indice_var_2_b_quadr_start
riq1_indice_var_2_c_quadr_start <- 0.0006
#plot(riq1_indice_var_2~riq1_indice_dist_2,xlim=c(1,3e5),ylim=c(0,0.0015))
#curve(quadra(x,-0.0000000000001,0.000000022,0.00025),xlim=c(1,3e5),ylim=c(0,0.0015),add=T,col="green")

# GAMA

## Nulo
riq1_indice_var_2_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq1_indice_var_2_gama_nulo_start <- list(shape=mean(riq1_indice_var_2)^2/var(riq1_indice_var_2),scale=var(riq1_indice_var_2)/mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
riq1_indice_var_2_gama_nulo_mle <- mle2(riq1_indice_var_2_gama_nulo, start=riq1_indice_var_2_gama_nulo_start,method="Nelder-Mead")
###### ou:
riq1_indice_var_2_gama_nulo_glm <- glm(riq1_indice_var_2~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq1_indice_var_2_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*riq1_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq1_indice_var_2_gama_shape_dist_lin_start <- list(a=riq1_indice_var_2_a_lin_start,b=riq1_indice_var_2_b_lin_start,scale=var(riq1_indice_var_2)/mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_indice_var_2_gama_shape_dist_lin_mle <- mle2(riq1_indice_var_2_gama_shape_dist_lin, start=riq1_indice_var_2_gama_shape_dist_lin_start,method="SANN")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq1_indice_var_2_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*riq1_indice_dist_2+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq1_indice_var_2_gama_scale_dist_lin_start <- list(a=riq1_indice_var_2_a_lin_start,b=riq1_indice_var_2_b_lin_start,shape=mean(riq1_indice_var_2)^2/var(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_indice_var_2_gama_scale_dist_lin_mle <- mle2(riq1_indice_var_2_gama_scale_dist_lin, start=riq1_indice_var_2_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
riq1_indice_var_2_gama_dist_lin_glm <- glm(riq1_indice_var_2~riq1_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao MM sobre shape
# riq1_indice_var_2_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*riq1_indice_dist_2/(b+riq1_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq1_indice_var_2_gama_shape_dist_mm_start <- list(a=riq1_indice_var_2_a_mm_start,b=riq1_indice_var_2_b_mm_start,scale=var(riq1_indice_var_2)/mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_indice_var_2_gama_shape_dist_mm_mle <- mle2(riq1_indice_var_2_gama_shape_dist_mm, start=riq1_indice_var_2_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq1_indice_var_2_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado<- (a*riq1_indice_dist_2/(b+riq1_indice_dist_2))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq1_indice_var_2_gama_scale_dist_mm_start <- list(a=riq1_indice_var_2_a_mm_start,b=riq1_indice_var_2_b_mm_start,shape=mean(riq1_indice_var_2)^2/var(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_indice_var_2_gama_scale_dist_mm_mle <- mle2(riq1_indice_var_2_gama_scale_dist_mm, start=riq1_indice_var_2_gama_scale_dist_mm_start)
######## ou:
#riq1_indice_var_2_gama_dist_mm_glm <- glm(riq1_indice_dist_2/riq1_indice_var_2~riq1_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre shape
# riq1_indice_var_2_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(riq1_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq1_indice_var_2_gama_shape_dist_pot_start <- list(a=riq1_indice_var_2_a_pot_start,b=riq1_indice_var_2_b_pot_start,scale=var(riq1_indice_var_2)/mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_indice_var_2_gama_shape_dist_pot_mle <- mle2(riq1_indice_var_2_gama_shape_dist_pot, start=riq1_indice_var_2_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq1_indice_var_2_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado<- (a*(riq1_indice_dist_2^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq1_indice_var_2_gama_scale_dist_pot_start <- list(a=riq1_indice_var_2_a_pot_start,b=riq1_indice_var_2_b_pot_start,shape=mean(riq1_indice_var_2)^2/var(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_indice_var_2_gama_scale_dist_pot_mle <- mle2(riq1_indice_var_2_gama_scale_dist_pot, start=riq1_indice_var_2_gama_scale_dist_pot_start)
######## ou:
#riq1_indice_var_2_gama_dist_pot_glm <- glm(riq1_indice_dist_2/riq1_indice_var_2~riq1_indice_dist_2,family="Gapota"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre shape
# riq1_indice_var_2_gama_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(riq1_indice_dist_2^2) + b*(riq1_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq1_indice_var_2_gama_shape_dist_quadr_start <- list(a=riq1_indice_var_2_a_quadr_start,b=riq1_indice_var_2_b_quadr_start,c=riq1_indice_var_2_c_quadr_start,scale=var(riq1_indice_var_2)/mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_indice_var_2_gama_shape_dist_quadr_mle <- mle2(riq1_indice_var_2_gama_shape_dist_quadr, start=riq1_indice_var_2_gama_shape_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq1_indice_var_2_gama_scale_dist_quadr <- function(a,b,c,shape){
  esperancaaoquadrado<- (a*(riq1_indice_dist_2^2) + b*(riq1_indice_dist_2) + c)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq1_indice_var_2_gama_scale_dist_quadr_start <- list(a=riq1_indice_var_2_a_quadr_start,b=riq1_indice_var_2_b_quadr_start,c=riq1_indice_var_2_c_quadr_start,shape=mean(riq1_indice_var_2)^2/var(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_indice_var_2_gama_scale_dist_quadr_mle <- mle2(riq1_indice_var_2_gama_scale_dist_quadr, start=riq1_indice_var_2_gama_scale_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq1_indice_var_2_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*riq1_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq1_indice_var_2_gama_var_shape_dist_lin_start <- list(a=riq1_indice_var_2_a_lin_start,b=riq1_indice_var_2_b_lin_start,scale=var(riq1_indice_var_2)/mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_indice_var_2_gama_var_shape_dist_lin_mle <- mle2(riq1_indice_var_2_gama_var_shape_dist_lin, start=riq1_indice_var_2_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq1_indice_var_2_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*riq1_indice_dist_2+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq1_indice_var_2_gama_var_scale_dist_lin_start <- list(a=riq1_indice_var_2_a_lin_start,b=riq1_indice_var_2_b_lin_start,shape=mean(riq1_indice_var_2)^2/var(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_indice_var_2_gama_var_scale_dist_lin_mle <- mle2(riq1_indice_var_2_gama_var_scale_dist_lin, start=riq1_indice_var_2_gama_var_scale_dist_lin_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq1_indice_var_2_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*riq1_indice_dist_2/(b+riq1_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq1_indice_var_2_gama_var_shape_dist_mm_start <- list(a=riq1_indice_var_2_a_mm_start,b=riq1_indice_var_2_b_mm_start,scale=var(riq1_indice_var_2)/mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_indice_var_2_gama_var_shape_dist_mm_mle <- mle2(riq1_indice_var_2_gama_var_shape_dist_mm, start=riq1_indice_var_2_gama_var_shape_dist_mm_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq1_indice_var_2_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado<- (a*riq1_indice_dist_2/(b+riq1_indice_dist_2))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq1_indice_var_2_gama_var_scale_dist_mm_start <- list(a=riq1_indice_var_2_a_mm_start,b=riq1_indice_var_2_b_mm_start,shape=mean(riq1_indice_var_2)^2/var(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_indice_var_2_gama_var_scale_dist_mm_mle <- mle2(riq1_indice_var_2_gama_var_scale_dist_mm, start=riq1_indice_var_2_gama_var_scale_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq1_indice_var_2_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(riq1_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq1_indice_var_2_gama_var_shape_dist_pot_start <- list(a=riq1_indice_var_2_a_pot_start,b=riq1_indice_var_2_b_pot_start,scale=var(riq1_indice_var_2)/mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_indice_var_2_gama_var_shape_dist_pot_mle <- mle2(riq1_indice_var_2_gama_var_shape_dist_pot, start=riq1_indice_var_2_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq1_indice_var_2_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado<- (a*(riq1_indice_dist_2^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq1_indice_var_2_gama_var_scale_dist_pot_start <- list(a=riq1_indice_var_2_a_pot_start,b=riq1_indice_var_2_b_pot_start,shape=mean(riq1_indice_var_2)^2/var(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_indice_var_2_gama_var_scale_dist_pot_mle <- mle2(riq1_indice_var_2_gama_var_scale_dist_pot, start=riq1_indice_var_2_gama_var_scale_dist_pot_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq1_indice_var_2_gama_var_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(riq1_indice_dist_2^2) + b*(riq1_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# riq1_indice_var_2_gama_var_shape_dist_quadr_start <- list(a=riq1_indice_var_2_a_quadr_start,b=riq1_indice_var_2_b_quadr_start,c=riq1_indice_var_2_c_quadr_start,scale=var(riq1_indice_var_2)/mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_indice_var_2_gama_var_shape_dist_quadr_mle <- mle2(riq1_indice_var_2_gama_var_shape_dist_quadr, start=riq1_indice_var_2_gama_var_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq1_indice_var_2_gama_var_scale_dist_quadr <- function(a,b,c,shape){
  varianciaaoquadrado<- (a*(riq1_indice_dist_2^2) + b*(riq1_indice_dist_2) + c)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq1_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
riq1_indice_var_2_gama_var_scale_dist_quadr_start <- list(a=riq1_indice_var_2_a_quadr_start,b=riq1_indice_var_2_b_quadr_start,c=riq1_indice_var_2_c_quadr_start,shape=mean(riq1_indice_var_2)^2/var(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_indice_var_2_gama_var_scale_dist_quadr_mle <- mle2(riq1_indice_var_2_gama_var_scale_dist_quadr, start=riq1_indice_var_2_gama_var_scale_dist_quadr_start)

# NORMAL

## Nulo
riq1_indice_var_2_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_nulo_start <- list(mean=mean(riq1_indice_var_2),sd=sd(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
riq1_indice_var_2_norm_nulo_mle <- mle2(riq1_indice_var_2_norm_nulo, start=riq1_indice_var_2_norm_nulo_start,method="Nelder-Mead")
########## ou:
riq1_indice_var_2_norm_nulo_glm <- glm(riq1_indice_var_2~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
riq1_indice_var_2_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*riq1_indice_dist_2+b
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_mean_dist_lin_start <- list(a=riq1_indice_var_2_a_lin_start,b=riq1_indice_var_2_b_lin_start,sd=sd(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_mean_dist_lin_mle <- mle2(riq1_indice_var_2_norm_mean_dist_lin, start=riq1_indice_var_2_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
riq1_indice_var_2_norm_dist_lin_glm <- glm(riq1_indice_var_2~riq1_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
riq1_indice_var_2_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*riq1_indice_dist_2+b)^2)
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_sd_dist_lin_start <- list(a=riq1_indice_var_2_a_lin_start,b=riq1_indice_var_2_b_lin_start,mean=mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_sd_dist_lin_mle <- mle2(riq1_indice_var_2_norm_sd_dist_lin, start=riq1_indice_var_2_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media e sd
riq1_indice_var_2_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*riq1_indice_dist_2+b
  sd <- sqrt((c*riq1_indice_dist_2+d)^2)
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_mean_sd_dist_lin_start <- list(a=riq1_indice_var_2_a_lin_start,b=riq1_indice_var_2_b_lin_start,c=riq1_indice_var_2_a_lin_start,d=riq1_indice_var_2_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_mean_sd_dist_lin_mle <- mle2(riq1_indice_var_2_norm_mean_sd_dist_lin, start=riq1_indice_var_2_norm_mean_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media
riq1_indice_var_2_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*riq1_indice_dist_2/(b+riq1_indice_dist_2)
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_mean_dist_mm_start <- list(a=riq1_indice_var_2_a_mm_start,b=riq1_indice_var_2_b_mm_start,sd=sd(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_mean_dist_mm_mle <- mle2(riq1_indice_var_2_norm_mean_dist_mm, start=riq1_indice_var_2_norm_mean_dist_mm_start,method="Nelder-Mead")
######### ou:
riq1_indice_var_2_norm_dist_mm_glm <- glm(riq1_indice_dist_2/riq1_indice_var_2~riq1_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre sd
riq1_indice_var_2_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*riq1_indice_dist_2/(b+riq1_indice_dist_2))^2)
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_sd_dist_mm_start <- list(a=riq1_indice_var_2_a_mm_start,b=riq1_indice_var_2_b_mm_start,mean=mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_sd_dist_mm_mle <- mle2(riq1_indice_var_2_norm_sd_dist_mm, start=riq1_indice_var_2_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media e sd
riq1_indice_var_2_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*riq1_indice_dist_2/(b+riq1_indice_dist_2)
  sd <- sqrt((c*riq1_indice_dist_2/(d+riq1_indice_dist_2))^2)
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_mean_sd_dist_mm_start <- list(a=riq1_indice_var_2_a_mm_start,b=riq1_indice_var_2_b_mm_start,c=riq1_indice_var_2_a_mm_start,d=riq1_indice_var_2_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_mean_sd_dist_mm_mle <- mle2(riq1_indice_var_2_norm_mean_sd_dist_mm, start=riq1_indice_var_2_norm_mean_sd_dist_mm_start,control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media
riq1_indice_var_2_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(riq1_indice_dist_2^b)
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_mean_dist_pot_start <- list(a=riq1_indice_var_2_a_pot_start,b=riq1_indice_var_2_b_pot_start,sd=sd(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_mean_dist_pot_mle <- mle2(riq1_indice_var_2_norm_mean_dist_pot, start=riq1_indice_var_2_norm_mean_dist_pot_start,method="Nelder-Mead")
######### ou:
riq1_indice_var_2_norm_dist_pot_glm <- glm(riq1_indice_dist_2/riq1_indice_var_2~riq1_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre sd
riq1_indice_var_2_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(riq1_indice_dist_2^b))^2)
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_sd_dist_pot_start <- list(a=riq1_indice_var_2_a_pot_start,b=riq1_indice_var_2_b_pot_start,mean=mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_sd_dist_pot_mle <- mle2(riq1_indice_var_2_norm_sd_dist_pot, start=riq1_indice_var_2_norm_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media e sd
riq1_indice_var_2_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(riq1_indice_dist_2^b)
  sd <- sqrt((c*(riq1_indice_dist_2^d))^2)
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_mean_sd_dist_pot_start <- list(a=riq1_indice_var_2_a_pot_start,b=riq1_indice_var_2_b_pot_start,c=riq1_indice_var_2_a_pot_start,d=riq1_indice_var_2_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_mean_sd_dist_pot_mle <- mle2(riq1_indice_var_2_norm_mean_sd_dist_pot, start=riq1_indice_var_2_norm_mean_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=2000))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media
riq1_indice_var_2_norm_mean_dist_quadr <- function(a,b,c,sd){
  mean <- a*(riq1_indice_dist_2^2) + b*(riq1_indice_dist_2) + c
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_mean_dist_quadr_start <- list(a=riq1_indice_var_2_a_quadr_start,b=riq1_indice_var_2_b_quadr_start,c=riq1_indice_var_2_c_quadr_start,sd=sd(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_mean_dist_quadr_mle <- mle2(riq1_indice_var_2_norm_mean_dist_quadr, start=riq1_indice_var_2_norm_mean_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre sd
riq1_indice_var_2_norm_sd_dist_quadr <- function(a,b,c,mean){
  sd <- sqrt((a*(riq1_indice_dist_2^2) + b*(riq1_indice_dist_2) + c)^2)
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_sd_dist_quadr_start <- list(a=riq1_indice_var_2_a_quadr_start,b=riq1_indice_var_2_b_quadr_start,c=riq1_indice_var_2_c_quadr_start,mean=mean(riq1_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_sd_dist_quadr_mle <- mle2(riq1_indice_var_2_norm_sd_dist_quadr, start=riq1_indice_var_2_norm_sd_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media e sd
riq1_indice_var_2_norm_mean_sd_dist_quadr <- function(a,b,c,d,e,f){
  mean <- a*(riq1_indice_dist_2^2) + b*(riq1_indice_dist_2) + c
  sd <- sqrt((d*(riq1_indice_dist_2^2) + e*(riq1_indice_dist_2) + f)^2)
  -sum(dnorm(riq1_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
riq1_indice_var_2_norm_mean_sd_dist_quadr_start <- list(a=riq1_indice_var_2_a_quadr_start,b=riq1_indice_var_2_b_quadr_start,c=riq1_indice_var_2_c_quadr_start,d=riq1_indice_var_2_a_quadr_start,e=riq1_indice_var_2_b_quadr_start,f=riq1_indice_var_2_c_quadr_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_indice_var_2_norm_mean_sd_dist_quadr_mle <- mle2(riq1_indice_var_2_norm_mean_sd_dist_quadr, start=riq1_indice_var_2_norm_mean_sd_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))


################### SELECAO DE MODELOS ####################
AICtab(riq1_indice_var_2_gama_nulo_mle,riq1_indice_var_2_gama_scale_dist_lin_mle,riq1_indice_var_2_gama_scale_dist_mm_mle,riq1_indice_var_2_gama_scale_dist_pot_mle,riq1_indice_var_2_gama_scale_dist_quadr_mle,riq1_indice_var_2_gama_var_scale_dist_lin_mle,riq1_indice_var_2_gama_var_scale_dist_mm_mle,riq1_indice_var_2_gama_var_scale_dist_pot_mle,riq1_indice_var_2_gama_var_scale_dist_quadr_mle,riq1_indice_var_2_norm_nulo_mle,riq1_indice_var_2_norm_mean_dist_lin_mle,riq1_indice_var_2_norm_sd_dist_lin_mle,riq1_indice_var_2_norm_mean_sd_dist_lin_mle,riq1_indice_var_2_norm_mean_dist_mm_mle,riq1_indice_var_2_norm_sd_dist_mm_mle,riq1_indice_var_2_norm_mean_sd_dist_mm_mle,riq1_indice_var_2_norm_mean_dist_pot_mle,riq1_indice_var_2_norm_sd_dist_pot_mle,riq1_indice_var_2_norm_mean_sd_dist_pot_mle,riq1_indice_var_2_norm_mean_dist_quadr_mle,riq1_indice_var_2_norm_sd_dist_quadr_mle,riq1_indice_var_2_norm_mean_sd_dist_quadr_mle,weights=T)
### modelo selecionado: riq1_indice_var_2_gama_scale_dist_mm_mle


##########################################################
######## CALCULO DO AIC SUBCONJUNTO RIQUEZA TOTAL ########
##########################################################
aic_mle_soma_sub_riqueza_var_2 <- AIC(riq0_indice_var_2_norm_mean_dist_mm_mle) + AIC(riq1_indice_var_2_gama_scale_dist_mm_mle) #-36539.16



##########################################################
##########################################################
##### Analises das baterias de simulacao EM CONJUNTO #####
##########################################################
##########################################################

geral_indice_var_2 <- c(bat1_indice_var_2,bat2_indice_var_2,bat3_indice_var_2)
geral_indice_dist_2 <- c(bat1_indice_dist_2,bat2_indice_dist_2,bat3_indice_dist_2)

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
geral_indice_var_2_lin <- lm(geral_indice_var_2~geral_indice_dist_2)
geral_indice_var_2_a_lin_start <- coef(geral_indice_var_2_lin)[[2]]
geral_indice_var_2_b_lin_start <- coef(geral_indice_var_2_lin)[[1]]


# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
geral_indice_var_2_mm_linearizada <- lm(geral_indice_dist_2/geral_indice_var_2~geral_indice_dist_2) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
geral_indice_var_2_a_mm_start <- 1/(coef(geral_indice_var_2_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
geral_indice_var_2_b_mm_start <- (coef(geral_indice_var_2_mm_linearizada)[[1]])*(1/(coef(geral_indice_var_2_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
geral_indice_var_2_a_pot_start <- 0.0002
geral_indice_var_2_b_pot_start <- 0.1

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
geral_indice_var_2_a_quadr_start <- bat1_indice_var_2_a_quadr_start
geral_indice_var_2_b_quadr_start <- bat1_indice_var_2_b_quadr_start
geral_indice_var_2_c_quadr_start <- 0.0006

# GAMA

## Nulo
geral_indice_var_2_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
geral_indice_var_2_gama_nulo_start <- list(shape=mean(geral_indice_var_2)^2/var(geral_indice_var_2),scale=var(geral_indice_var_2)/mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
geral_indice_var_2_gama_nulo_mle <- mle2(geral_indice_var_2_gama_nulo, start=geral_indice_var_2_gama_nulo_start,method="Nelder-Mead")
###### ou:
geral_indice_var_2_gama_nulo_glm <- glm(geral_indice_var_2~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# geral_indice_var_2_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*geral_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# geral_indice_var_2_gama_shape_dist_lin_start <- list(a=geral_indice_var_2_a_lin_start,b=geral_indice_var_2_b_lin_start,scale=var(geral_indice_var_2)/mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_indice_var_2_gama_shape_dist_lin_mle <- mle2(geral_indice_var_2_gama_shape_dist_lin, start=geral_indice_var_2_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_indice_var_2_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*geral_indice_dist_2+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
geral_indice_var_2_gama_scale_dist_lin_start <- list(a=geral_indice_var_2_a_lin_start,b=geral_indice_var_2_b_lin_start,shape=mean(geral_indice_var_2)^2/var(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_indice_var_2_gama_scale_dist_lin_mle <- mle2(geral_indice_var_2_gama_scale_dist_lin, start=geral_indice_var_2_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
geral_indice_var_2_gama_dist_lin_glm <- glm(geral_indice_var_2~geral_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao MM sobre shape
# geral_indice_var_2_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*geral_indice_dist_2/(b+geral_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# geral_indice_var_2_gama_shape_dist_mm_start <- list(a=geral_indice_var_2_a_mm_start,b=geral_indice_var_2_b_mm_start,scale=var(geral_indice_var_2)/mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_indice_var_2_gama_shape_dist_mm_mle <- mle2(geral_indice_var_2_gama_shape_dist_mm, start=geral_indice_var_2_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_indice_var_2_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado<- (a*geral_indice_dist_2/(b+geral_indice_dist_2))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
geral_indice_var_2_gama_scale_dist_mm_start <- list(a=geral_indice_var_2_a_mm_start,b=geral_indice_var_2_b_mm_start,shape=mean(geral_indice_var_2)^2/var(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_indice_var_2_gama_scale_dist_mm_mle <- mle2(geral_indice_var_2_gama_scale_dist_mm, start=geral_indice_var_2_gama_scale_dist_mm_start,method="Nelder-Mead")
######## ou:
#geral_indice_var_2_gama_dist_mm_glm <- glm(geral_indice_dist_2/geral_indice_var_2~geral_indice_dist_2,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre shape
# geral_indice_var_2_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(geral_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# geral_indice_var_2_gama_shape_dist_pot_start <- list(a=geral_indice_var_2_a_pot_start,b=geral_indice_var_2_b_pot_start,scale=var(geral_indice_var_2)/mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_indice_var_2_gama_shape_dist_pot_mle <- mle2(geral_indice_var_2_gama_shape_dist_pot, start=geral_indice_var_2_gama_shape_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_indice_var_2_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado<- (a*(geral_indice_dist_2^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
geral_indice_var_2_gama_scale_dist_pot_start <- list(a=geral_indice_var_2_a_pot_start,b=geral_indice_var_2_b_pot_start,shape=mean(geral_indice_var_2)^2/var(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_indice_var_2_gama_scale_dist_pot_mle <- mle2(geral_indice_var_2_gama_scale_dist_pot, start=geral_indice_var_2_gama_scale_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))
######## ou:
#geral_indice_var_2_gama_dist_pot_glm <- glm(geral_indice_dist_2/geral_indice_var_2~geral_indice_dist_2,family="Gapota"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre shape
# geral_indice_var_2_gama_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(geral_indice_dist_2^2) + b*(geral_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# geral_indice_var_2_gama_shape_dist_quadr_start <- list(a=geral_indice_var_2_a_quadr_start,b=geral_indice_var_2_b_quadr_start,c=geral_indice_var_2_c_quadr_start,scale=var(geral_indice_var_2)/mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_indice_var_2_gama_shape_dist_quadr_mle <- mle2(geral_indice_var_2_gama_shape_dist_quadr, start=geral_indice_var_2_gama_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_indice_var_2_gama_scale_dist_quadr <- function(a,b,c,shape){
  esperancaaoquadrado<- (a*(geral_indice_dist_2^2) + b*(geral_indice_dist_2) + c)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
geral_indice_var_2_gama_scale_dist_quadr_start <- list(a=geral_indice_var_2_a_quadr_start,b=geral_indice_var_2_b_quadr_start,c=geral_indice_var_2_c_quadr_start,shape=mean(geral_indice_var_2)^2/var(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_indice_var_2_gama_scale_dist_quadr_mle <- mle2(geral_indice_var_2_gama_scale_dist_quadr, start=geral_indice_var_2_gama_scale_dist_quadr_start)

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# geral_indice_var_2_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*geral_indice_dist_2+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# geral_indice_var_2_gama_var_shape_dist_lin_start <- list(a=geral_indice_var_2_a_lin_start,b=geral_indice_var_2_b_lin_start,scale=var(geral_indice_var_2)/mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_indice_var_2_gama_var_shape_dist_lin_mle <- mle2(geral_indice_var_2_gama_var_shape_dist_lin, start=geral_indice_var_2_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_indice_var_2_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*geral_indice_dist_2+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
geral_indice_var_2_gama_var_scale_dist_lin_start <- list(a=geral_indice_var_2_a_lin_start,b=geral_indice_var_2_b_lin_start,shape=mean(geral_indice_var_2)^2/var(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_indice_var_2_gama_var_scale_dist_lin_mle <- mle2(geral_indice_var_2_gama_var_scale_dist_lin, start=geral_indice_var_2_gama_var_scale_dist_lin_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# geral_indice_var_2_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*geral_indice_dist_2/(b+geral_indice_dist_2))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# geral_indice_var_2_gama_var_shape_dist_mm_start <- list(a=geral_indice_var_2_a_mm_start,b=geral_indice_var_2_b_mm_start,scale=var(geral_indice_var_2)/mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_indice_var_2_gama_var_shape_dist_mm_mle <- mle2(geral_indice_var_2_gama_var_shape_dist_mm, start=geral_indice_var_2_gama_var_shape_dist_mm_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_indice_var_2_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado<- (a*geral_indice_dist_2/(b+geral_indice_dist_2))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
geral_indice_var_2_gama_var_scale_dist_mm_start <- list(a=geral_indice_var_2_a_mm_start,b=geral_indice_var_2_b_mm_start,shape=mean(geral_indice_var_2)^2/var(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_indice_var_2_gama_var_scale_dist_mm_mle <- mle2(geral_indice_var_2_gama_var_scale_dist_mm, start=geral_indice_var_2_gama_var_scale_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# geral_indice_var_2_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(geral_indice_dist_2^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# geral_indice_var_2_gama_var_shape_dist_pot_start <- list(a=geral_indice_var_2_a_pot_start,b=geral_indice_var_2_b_pot_start,scale=var(geral_indice_var_2)/mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_indice_var_2_gama_var_shape_dist_pot_mle <- mle2(geral_indice_var_2_gama_var_shape_dist_pot, start=geral_indice_var_2_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_indice_var_2_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado<- (a*(geral_indice_dist_2^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
geral_indice_var_2_gama_var_scale_dist_pot_start <- list(a=geral_indice_var_2_a_pot_start,b=geral_indice_var_2_b_pot_start,shape=mean(geral_indice_var_2)^2/var(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_indice_var_2_gama_var_scale_dist_pot_mle <- mle2(geral_indice_var_2_gama_var_scale_dist_pot, start=geral_indice_var_2_gama_var_scale_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# geral_indice_var_2_gama_var_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(geral_indice_dist_2^2) + b*(geral_indice_dist_2) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
# }
# geral_indice_var_2_gama_var_shape_dist_quadr_start <- list(a=geral_indice_var_2_a_quadr_start,b=geral_indice_var_2_b_quadr_start,c=geral_indice_var_2_c_quadr_start,scale=var(geral_indice_var_2)/mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_indice_var_2_gama_var_shape_dist_quadr_mle <- mle2(geral_indice_var_2_gama_var_shape_dist_quadr, start=geral_indice_var_2_gama_var_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_indice_var_2_gama_var_scale_dist_quadr <- function(a,b,c,shape){
  varianciaaoquadrado<- (a*(geral_indice_dist_2^2) + b*(geral_indice_dist_2) + c)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(geral_indice_var_2, shape=shape, scale=scale, log=TRUE))
}
geral_indice_var_2_gama_var_scale_dist_quadr_start <- list(a=geral_indice_var_2_a_quadr_start,b=geral_indice_var_2_b_quadr_start,c=geral_indice_var_2_c_quadr_start,shape=mean(geral_indice_var_2)^2/var(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_indice_var_2_gama_var_scale_dist_quadr_mle <- mle2(geral_indice_var_2_gama_var_scale_dist_quadr, start=geral_indice_var_2_gama_var_scale_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

# NORMAL

## Nulo
geral_indice_var_2_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_nulo_start <- list(mean=mean(geral_indice_var_2),sd=sd(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
geral_indice_var_2_norm_nulo_mle <- mle2(geral_indice_var_2_norm_nulo, start=geral_indice_var_2_norm_nulo_start,method="Nelder-Mead")
########## ou:
geral_indice_var_2_norm_nulo_glm <- glm(geral_indice_var_2~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
geral_indice_var_2_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*geral_indice_dist_2+b
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_mean_dist_lin_start <- list(a=geral_indice_var_2_a_lin_start,b=geral_indice_var_2_b_lin_start,sd=sd(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_mean_dist_lin_mle <- mle2(geral_indice_var_2_norm_mean_dist_lin, start=geral_indice_var_2_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
geral_indice_var_2_norm_dist_lin_glm <- glm(geral_indice_var_2~geral_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
geral_indice_var_2_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*geral_indice_dist_2+b)^2)
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_sd_dist_lin_start <- list(a=geral_indice_var_2_a_lin_start,b=geral_indice_var_2_b_lin_start,mean=mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_sd_dist_lin_mle <- mle2(geral_indice_var_2_norm_sd_dist_lin, start=geral_indice_var_2_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media e sd
geral_indice_var_2_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*geral_indice_dist_2+b
  sd <- sqrt((c*geral_indice_dist_2+d)^2)
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_mean_sd_dist_lin_start <- list(a=geral_indice_var_2_a_lin_start,b=geral_indice_var_2_b_lin_start,c=geral_indice_var_2_a_lin_start,d=geral_indice_var_2_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_mean_sd_dist_lin_mle <- mle2(geral_indice_var_2_norm_mean_sd_dist_lin, start=geral_indice_var_2_norm_mean_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media
geral_indice_var_2_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*geral_indice_dist_2/(b+geral_indice_dist_2)
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_mean_dist_mm_start <- list(a=geral_indice_var_2_a_mm_start,b=geral_indice_var_2_b_mm_start,sd=sd(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_mean_dist_mm_mle <- mle2(geral_indice_var_2_norm_mean_dist_mm, start=geral_indice_var_2_norm_mean_dist_mm_start,method="Nelder-Mead")
######### ou:
geral_indice_var_2_norm_dist_mm_glm <- glm(geral_indice_dist_2/geral_indice_var_2~geral_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre sd
geral_indice_var_2_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*geral_indice_dist_2/(b+geral_indice_dist_2))^2)
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_sd_dist_mm_start <- list(a=geral_indice_var_2_a_mm_start,b=geral_indice_var_2_b_mm_start,mean=mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_sd_dist_mm_mle <- mle2(geral_indice_var_2_norm_sd_dist_mm, start=geral_indice_var_2_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media e sd
geral_indice_var_2_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*geral_indice_dist_2/(b+geral_indice_dist_2)
  sd <- sqrt((c*geral_indice_dist_2/(d+geral_indice_dist_2))^2)
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_mean_sd_dist_mm_start <- list(a=geral_indice_var_2_a_mm_start,b=geral_indice_var_2_b_mm_start,c=geral_indice_var_2_a_mm_start,d=geral_indice_var_2_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_mean_sd_dist_mm_mle <- mle2(geral_indice_var_2_norm_mean_sd_dist_mm, start=geral_indice_var_2_norm_mean_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media
geral_indice_var_2_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(geral_indice_dist_2^b)
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_mean_dist_pot_start <- list(a=geral_indice_var_2_a_pot_start,b=geral_indice_var_2_b_pot_start,sd=sd(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_mean_dist_pot_mle <- mle2(geral_indice_var_2_norm_mean_dist_pot, start=geral_indice_var_2_norm_mean_dist_pot_start,method="Nelder-Mead")
######### ou:
geral_indice_var_2_norm_dist_pot_glm <- glm(geral_indice_dist_2/geral_indice_var_2~geral_indice_dist_2,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre sd
geral_indice_var_2_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(geral_indice_dist_2^b))^2)
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_sd_dist_pot_start <- list(a=geral_indice_var_2_a_pot_start,b=geral_indice_var_2_b_pot_start,mean=mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_sd_dist_pot_mle <- mle2(geral_indice_var_2_norm_sd_dist_pot, start=geral_indice_var_2_norm_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao pot sobre a media e sd
geral_indice_var_2_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(geral_indice_dist_2^b)
  sd <- sqrt((c*(geral_indice_dist_2^d))^2)
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_mean_sd_dist_pot_start <- list(a=geral_indice_var_2_a_pot_start,b=geral_indice_var_2_b_pot_start,c=geral_indice_var_2_a_pot_start,d=geral_indice_var_2_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_mean_sd_dist_pot_mle <- mle2(geral_indice_var_2_norm_mean_sd_dist_pot, start=geral_indice_var_2_norm_mean_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media
geral_indice_var_2_norm_mean_dist_quadr <- function(a,b,c,sd){
  mean <- a*(geral_indice_dist_2^2) + b*(geral_indice_dist_2) + c
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_mean_dist_quadr_start <- list(a=geral_indice_var_2_a_quadr_start,b=geral_indice_var_2_b_quadr_start,c=geral_indice_var_2_c_quadr_start,sd=sd(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_mean_dist_quadr_mle <- mle2(geral_indice_var_2_norm_mean_dist_quadr, start=geral_indice_var_2_norm_mean_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre sd
geral_indice_var_2_norm_sd_dist_quadr <- function(a,b,c,mean){
  sd <- sqrt((a*(geral_indice_dist_2^2) + b*(geral_indice_dist_2) + c)^2)
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_sd_dist_quadr_start <- list(a=geral_indice_var_2_a_quadr_start,b=geral_indice_var_2_b_quadr_start,c=geral_indice_var_2_c_quadr_start,mean=mean(geral_indice_var_2)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_sd_dist_quadr_mle <- mle2(geral_indice_var_2_norm_sd_dist_quadr, start=geral_indice_var_2_norm_sd_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media e sd
geral_indice_var_2_norm_mean_sd_dist_quadr <- function(a,b,c,d,e,f){
  mean <- a*(geral_indice_dist_2^2) + b*(geral_indice_dist_2) + c
  sd <- sqrt((d*(geral_indice_dist_2^2) + e*(geral_indice_dist_2) + f)^2)
  -sum(dnorm(geral_indice_var_2, mean=mean, sd=sd, log=TRUE))
}
geral_indice_var_2_norm_mean_sd_dist_quadr_start <- list(a=geral_indice_var_2_a_quadr_start,b=geral_indice_var_2_b_quadr_start,c=geral_indice_var_2_c_quadr_start,d=geral_indice_var_2_a_quadr_start,e=geral_indice_var_2_b_quadr_start,f=geral_indice_var_2_c_quadr_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_indice_var_2_norm_mean_sd_dist_quadr_mle <- mle2(geral_indice_var_2_norm_mean_sd_dist_quadr, start=geral_indice_var_2_norm_mean_sd_dist_quadr_start,control=list(maxit=1000),method="Nelder-Mead")

################### SELECAO DE MODELOS ####################
AICtab(geral_indice_var_2_gama_nulo_mle,geral_indice_var_2_gama_scale_dist_lin_mle,geral_indice_var_2_gama_scale_dist_mm_mle,geral_indice_var_2_gama_scale_dist_pot_mle,geral_indice_var_2_gama_scale_dist_quadr_mle,geral_indice_var_2_gama_var_scale_dist_lin_mle,geral_indice_var_2_gama_var_scale_dist_mm_mle,geral_indice_var_2_gama_var_scale_dist_pot_mle,geral_indice_var_2_gama_var_scale_dist_quadr_mle,geral_indice_var_2_norm_nulo_mle,geral_indice_var_2_norm_mean_dist_lin_mle,geral_indice_var_2_norm_sd_dist_lin_mle,geral_indice_var_2_norm_mean_sd_dist_lin_mle,geral_indice_var_2_norm_mean_dist_mm_mle,geral_indice_var_2_norm_sd_dist_mm_mle,geral_indice_var_2_norm_mean_sd_dist_mm_mle,geral_indice_var_2_norm_mean_dist_pot_mle,geral_indice_var_2_norm_sd_dist_pot_mle,geral_indice_var_2_norm_mean_sd_dist_pot_mle,geral_indice_var_2_norm_mean_dist_quadr_mle,geral_indice_var_2_norm_sd_dist_quadr_mle,geral_indice_var_2_norm_mean_sd_dist_quadr_mle,weights=T)
### modelo selecionado: geral_indice_var_2_gama_scale_dist_lin_mle 

##########################################################
################## CALCULO DO AIC GERAL ##################
##########################################################
aic_mle_geral_var_2 <- AIC(geral_indice_var_2_gama_scale_dist_lin_mle) #-31227.12

##########################################################
##########################################################
################## COMPARACAO DOS AICS ###################
##########################################################
##########################################################
aics_var_2 <- c(por_bateria=aic_mle_soma_individual_var_2,por_mutacao=aic_mle_soma_sub_mutacao_var_2,por_riqueza=aic_mle_soma_sub_riqueza_var_2,geral=aic_mle_geral_var_2)
min_aic_var_2 <- min(aics_var_2)
daic_var_2 <- aics_var_2 - min_aic_var_2
sort(daic_var_2)

##########################################################
##########################################################
######################## GRAFICOS ########################
##########################################################
##########################################################
hist(geral_indice_var_2,breaks=100,las=1,xlab="Variância da estratégia de vida",ylab="Frequência",main="")

plot(bat1_indice_dist_2,bat1_indice_var_2,pch=20,col=alpha("dimgray",0.2),xlim=c(0,3e5),xlab="Índice de distúrbio",ylab="Variância da estratégia de vida",bty="l",las=1,ylim=c(0,0.003))
par(new=T)
plot(bat2_indice_dist_2,bat2_indice_var_2,pch=20,col=alpha("pink",0.2),xlim=c(0,3e5),xlab="",ylab="",axes=F,ylim=c(0,0.003))
par(new=T)
plot(bat3_indice_dist_2,bat3_indice_var_2,pch=20,col=alpha("lightblue",0.2),xlim=c(0,3e5),xlab="",ylab="",axes=F,ylim=c(0,0.003))

### plotar previstos
mm <- function(x,a,b){a*x/(b+x)}
quadra <- function(x,a,b,c){a*(x^2)+b*(x)+c}
#### individual
#bat1_indice_var_2_gama_scale_dist_mm_mle
curve(mm(x,coef(bat1_indice_var_2_gama_scale_dist_mm_mle)[[1]],coef(bat1_indice_var_2_gama_scale_dist_mm_mle)[[2]]),add=T,col="black")
#bat2_indice_var_2_gama_scale_dist_quadr_mle
curve(quadra(x,coef(bat2_indice_var_2_gama_scale_dist_quadr_mle)[[1]],coef(bat2_indice_var_2_gama_scale_dist_quadr_mle)[[2]],coef(bat2_indice_var_2_gama_scale_dist_quadr_mle)[[3]]),add=T,col="red")
#bat3_indice_var_2_norm_sd_dist_quadr_mle
curve(quadra(x,coef(bat3_indice_var_2_norm_sd_dist_quadr_mle)[[1]],coef(bat3_indice_var_2_norm_sd_dist_quadr_mle)[[2]],coef(bat3_indice_var_2_norm_sd_dist_quadr_mle)[[3]]),add=T,col="blue")
