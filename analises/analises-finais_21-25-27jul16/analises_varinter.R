################ Carregamento dos pacotes ################
require(ggplot2)
require(bbmle)


################# Carregamento dos dados #################
# para analisar no tempo geracao 5000
#load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_25jul16/bat2_var_intersp_indice_media.RData")
#load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_25jul16/bat2_meia_vida.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_25jul16/bat2_indice_dist_riq2.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_25jul16/bat2_porc_entre_nger5000_riq2.RData")
#load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_var_intersp_indice_media.RData")
#load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_meia_vida.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_indice_dist_riq2.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_porc_entre_nger5000_riq2.RData")

# para analisar no tempo de meia vida
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_25jul16/bat2_porc_entre_meiavida.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/bat3_porc_entre_meiavida.RData")

##########################################################
##########################################################
# Porcentagem de variancia interespecífica do indice de estrategia de vida
##########################################################
##########################################################

##########################################################
######################## Bateria 2 #######################
##########################################################

################# Transformacao dos dados #################
# bat2_var_indice_meia_vida <- c()
# for(i in 1:1000){
#   bat2_var_indice_meia_vida[i] <- bat2_var_indice_media_temporal[i,bat2_meia_vida[i]]
# }
# bat2_var_indice_meia_vida[which(is.na(bat2_var_indice_meia_vida))] <- 0
# bat2_coex <- bat2_var_indice_meia_vida*as.integer(bat2_meia_vida)
bat2_coex <- bat2_porc_entre_nger5000_riq2
bat2_indice_dist <- bat2_indice_dist_riq2

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat2_coex_lin <- lm(bat2_coex~bat2_indice_dist)
bat2_coex_a_lin_start <- coef(bat2_coex_lin)[[2]]
bat2_coex_b_lin_start <- coef(bat2_coex_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat2_coex_mm_linearizada <- lm(bat2_indice_dist/bat2_coex~bat2_indice_dist) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
bat2_coex_a_mm_start <- 1/(coef(bat2_coex_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
bat2_coex_b_mm_start <- (coef(bat2_coex_mm_linearizada)[[1]])*(1/coef(bat2_coex_mm_linearizada)[[2]]) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a, b e c da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat2_coex_a_pot_start <- 1e-17
bat2_coex_b_pot_start <- 2.9

# Valores de start para os coeficientes a, b e c da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat2_coex_a_quadr_start <- -0.00000000000001
bat2_coex_b_quadr_start <- 0.000000003
bat2_coex_c_quadr_start <- 5000

# GAMA

## Nulo
bat2_coex_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
}
bat2_coex_gama_nulo_start <- list(shape=mean(bat2_coex)^2/var(bat2_coex),scale=var(bat2_coex)/mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat2_coex_gama_nulo_mle <- mle2(bat2_coex_gama_nulo, start=bat2_coex_gama_nulo_start)
###### ou:
bat2_coex_gama_nulo_glm <- glm(bat2_coex~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat2_coex_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat2_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat2_coex_gama_shape_dist_lin_start <- list(a=bat2_coex_a_lin_start,b=bat2_coex_b_lin_start,scale=var(bat2_coex)/mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_coex_gama_shape_dist_lin_mle <- mle2(bat2_coex_gama_shape_dist_lin, start=bat2_coex_gama_shape_dist_lin_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_coex_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat2_indice_dist+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
}
bat2_coex_gama_scale_dist_lin_start <- list(a=bat2_coex_a_lin_start,b=bat2_coex_b_lin_start,shape=mean(bat2_coex)^2/var(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_coex_gama_scale_dist_lin_mle <- mle2(bat2_coex_gama_scale_dist_lin, start=bat2_coex_gama_scale_dist_lin_start)
######## ou:
bat2_coex_gama_dist_lin_glm <- glm(bat2_coex~bat2_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre shape
# bat2_coex_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*((bat2_indice_dist)^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat2_coex_gama_shape_dist_pot_start <- list(a=bat2_coex_a_pot_start,b=bat2_coex_b_pot_start,scale=var(bat2_coex)/mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_coex_gama_shape_dist_pot_mle <- mle2(bat2_coex_gama_shape_dist_pot, start=bat2_coex_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre scale
bat2_coex_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado<- (a*(bat2_indice_dist^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
}
bat2_coex_gama_scale_dist_pot_start <- list(a=bat2_coex_a_pot_start,b=bat2_coex_b_pot_start,shape=mean(bat2_coex)^2/var(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_coex_gama_scale_dist_pot_mle <- mle2(bat2_coex_gama_scale_dist_pot, start=bat2_coex_gama_scale_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre shape
# bat2_coex_gama_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(bat2_indice_dist^2) + b*(bat2_indice_dist) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat2_coex_gama_shape_dist_quadr_start <- list(a=bat2_coex_a_quadr_start,b=bat2_coex_b_quadr_start,c=bat2_coex_c_quadr_start,scale=var(bat2_coex)/mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_coex_gama_shape_dist_quadr_mle <- mle2(bat2_coex_gama_shape_dist_quadr, start=bat2_coex_gama_shape_dist_quadr_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_coex_gama_scale_dist_quadr <- function(a,b,c,shape){
  esperancaaoquadrado<- (a*(bat2_indice_dist^2) + b*(bat2_indice_dist) + c)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
}
bat2_coex_gama_scale_dist_quadr_start <- list(a=bat2_coex_a_quadr_start,b=bat2_coex_b_quadr_start,c=bat2_coex_c_quadr_start,shape=mean(bat2_coex)^2/var(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_coex_gama_scale_dist_quadr_mle <- mle2(bat2_coex_gama_scale_dist_quadr, start=bat2_coex_gama_scale_dist_quadr_start,control=list(maxit=1000),method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre shape
# bat2_coex_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat2_indice_dist/(b+bat2_indice_dist))^2 # ambos os lados da funcao foram elevados ao mmado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat2_coex_gama_shape_dist_mm_start <- list(a=bat2_coex_a_mm_start,b=bat2_coex_b_mm_start,scale=var(bat2_coex)/mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_coex_gama_shape_dist_mm_mle <- mle2(bat2_coex_gama_shape_dist_mm, start=bat2_coex_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_coex_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat2_indice_dist/(b+bat2_indice_dist))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
}
bat2_coex_gama_scale_dist_mm_start <- list(a=bat2_coex_a_mm_start,b=bat2_coex_b_mm_start,shape=mean(bat2_coex)^2/var(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_coex_gama_scale_dist_mm_mle <- mle2(bat2_coex_gama_scale_dist_mm, start=bat2_coex_gama_scale_dist_mm_start,method="Nelder-Mead")

### Sobre a variancia

# ## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat2_coex_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat2_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat2_coex_gama_var_shape_dist_lin_start <- list(a=bat2_coex_a_lin_start,b=bat2_coex_b_lin_start,scale=var(bat2_coex)/mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_coex_gama_var_shape_dist_lin_mle <- mle2(bat2_coex_gama_var_shape_dist_lin, start=bat2_coex_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_coex_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat2_indice_dist+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
}
bat2_coex_gama_var_scale_dist_lin_start <- list(a=bat2_coex_a_lin_start,b=bat2_coex_b_lin_start,shape=mean(bat2_coex)^2/var(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_coex_gama_var_scale_dist_lin_mle <- mle2(bat2_coex_gama_var_scale_dist_lin, start=bat2_coex_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre shape
# bat2_coex_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da variancia da distribuicao, que por sua vez eh funcao (potencia) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat2_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat2_coex_gama_var_shape_dist_pot_start <- list(a=bat2_coex_a_pot_start,b=bat2_coex_b_pot_start,scale=var(bat2_coex)/mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_coex_gama_var_shape_dist_pot_mle <- mle2(bat2_coex_gama_var_shape_dist_pot, start=bat2_coex_gama_var_shape_dist_pot_start)

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre scale
bat2_coex_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado<- (a*(bat2_indice_dist^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
}
bat2_coex_gama_var_scale_dist_pot_start <- list(a=bat2_coex_a_pot_start,b=bat2_coex_b_pot_start,shape=mean(bat2_coex)^2/var(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_coex_gama_var_scale_dist_pot_mle <- mle2(bat2_coex_gama_var_scale_dist_pot, start=bat2_coex_gama_var_scale_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat2_coex_gama_var_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat2_indice_dist^2) + b*(bat2_indice_dist) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat2_coex_gama_var_shape_dist_quadr_start <- list(a=bat2_coex_a_quadr_start,b=bat2_coex_b_quadr_start,c=bat2_coex_c_quadr_start,scale=var(bat2_coex)/mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_coex_gama_var_shape_dist_quadr_mle <- mle2(bat2_coex_gama_var_shape_dist_quadr, start=bat2_coex_gama_var_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_coex_gama_var_scale_dist_quadr <- function(a,b,c,shape){
  varianciaaoquadrado<- (a*(bat2_indice_dist^2) + b*(bat2_indice_dist) + c)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
}
bat2_coex_gama_var_scale_dist_quadr_start <- list(a=bat2_coex_a_quadr_start,b=bat2_coex_b_quadr_start,c=bat2_coex_c_quadr_start,shape=mean(bat2_coex)^2/var(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_coex_gama_var_scale_dist_quadr_mle <- mle2(bat2_coex_gama_var_scale_dist_quadr, start=bat2_coex_gama_var_scale_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat2_coex_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat2_indice_dist/(b+bat2_indice_dist))^2 # ambos os lados da funcao foram elevados ao mmado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat2_coex_gama_var_shape_dist_mm_start <- list(a=bat2_coex_a_mm_start,b=bat2_coex_b_mm_start,scale=var(bat2_coex)/mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_coex_gama_var_shape_dist_mm_mle <- mle2(bat2_coex_gama_var_shape_dist_mm, start=bat2_coex_gama_var_shape_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_coex_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat2_indice_dist/(b+bat2_indice_dist))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat2_coex, shape=shape, scale=scale, log=TRUE))
}
bat2_coex_gama_var_scale_dist_mm_start <- list(a=bat2_coex_a_mm_start,b=bat2_coex_b_mm_start,shape=mean(bat2_coex)^2/var(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_coex_gama_var_scale_dist_mm_mle <- mle2(bat2_coex_gama_var_scale_dist_mm, start=bat2_coex_gama_var_scale_dist_mm_start,method="Nelder-Mead")

# NORMAL

## Nulo
bat2_coex_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_nulo_start <- list(mean=mean(bat2_coex),sd=sd(bat2_coex)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat2_coex_norm_nulo_mle <- mle2(bat2_coex_norm_nulo, start=bat2_coex_norm_nulo_start)
########## ou:
bat2_coex_norm_nulo_glm <- glm(bat2_coex~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
bat2_coex_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*bat2_indice_dist+b
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_mean_dist_lin_start <- list(a=bat2_coex_a_lin_start,b=bat2_coex_b_lin_start,sd=sd(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_mean_dist_lin_mle <- mle2(bat2_coex_norm_mean_dist_lin, start=bat2_coex_norm_mean_dist_lin_start)
######### ou:
bat2_coex_norm_dist_lin_glm <- glm(bat2_coex~bat2_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
bat2_coex_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*bat2_indice_dist+b)^2)
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_sd_dist_lin_start <- list(a=bat2_coex_a_lin_start,b=bat2_coex_b_lin_start,mean=mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_sd_dist_lin_mle <- mle2(bat2_coex_norm_sd_dist_lin, start=bat2_coex_norm_sd_dist_lin_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media e sd
bat2_coex_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*bat2_indice_dist+b
  sd <- sqrt((c*bat2_indice_dist+d)^2)
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_mean_sd_dist_lin_start <- list(a=bat2_coex_a_lin_start,b=bat2_coex_b_lin_start,c=bat2_coex_a_lin_start,d=bat2_coex_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_mean_sd_dist_lin_mle <- mle2(bat2_coex_norm_mean_sd_dist_lin, start=bat2_coex_norm_mean_sd_dist_lin_start)


## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre a media
bat2_coex_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(bat2_indice_dist^b)
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_mean_dist_pot_start <- list(a=bat2_coex_a_pot_start,b=bat2_coex_b_pot_start,sd=sd(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_mean_dist_pot_mle <- mle2(bat2_coex_norm_mean_dist_pot, start=bat2_coex_norm_mean_dist_pot_start,method="Nelder-Mead",control=list(maxit=2000))

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre sd
bat2_coex_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(bat2_indice_dist^b))^2)
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_sd_dist_pot_start <- list(a=bat2_coex_a_pot_start,b=bat2_coex_b_pot_start,mean=mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_sd_dist_pot_mle <- mle2(bat2_coex_norm_sd_dist_pot, start=bat2_coex_norm_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre a media e sd
bat2_coex_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(bat2_indice_dist^b)
  sd <- sqrt((c*(bat2_indice_dist^d))^2)
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_mean_sd_dist_pot_start <- list(a=bat2_coex_a_pot_start,b=bat2_coex_b_pot_start,c=bat2_coex_a_pot_start,d=bat2_coex_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_mean_sd_dist_pot_mle <- mle2(bat2_coex_norm_mean_sd_dist_pot, start=bat2_coex_norm_mean_sd_dist_pot_start,control=list(maxit=10000))

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media
bat2_coex_norm_mean_dist_quadr <- function(a,b,c,sd){
  mean <- a*(bat2_indice_dist^2) + b*(bat2_indice_dist) + c
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_mean_dist_quadr_start <- list(a=bat2_coex_a_quadr_start,b=bat2_coex_b_quadr_start,c=bat2_coex_c_quadr_start,sd=sd(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_mean_dist_quadr_mle <- mle2(bat2_coex_norm_mean_dist_quadr, start=bat2_coex_norm_mean_dist_quadr_start)

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre sd
bat2_coex_norm_sd_dist_quadr <- function(a,b,c,mean){
  sd <- sqrt((a*(bat2_indice_dist^2) + b*(bat2_indice_dist) + c)^2)
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_sd_dist_quadr_start <- list(a=bat2_coex_a_quadr_start,b=bat2_coex_b_quadr_start,c=bat2_coex_c_quadr_start,mean=mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_sd_dist_quadr_mle <- mle2(bat2_coex_norm_sd_dist_quadr, start=bat2_coex_norm_sd_dist_quadr_start)

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media e sd
bat2_coex_norm_mean_sd_dist_quadr <- function(a,b,c,d,e,f){
  mean <- a*(bat2_indice_dist^2) + b*(bat2_indice_dist) + c
  sd <- sqrt((d*(bat2_indice_dist^2) + e*(bat2_indice_dist) + f)^2)
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_mean_sd_dist_quadr_start <- list(a=bat2_coex_a_quadr_start,b=bat2_coex_b_quadr_start,c=bat2_coex_c_quadr_start,d=bat2_coex_a_quadr_start,e=bat2_coex_b_quadr_start,f=bat2_coex_c_quadr_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_mean_sd_dist_quadr_mle <- mle2(bat2_coex_norm_mean_sd_dist_quadr, start=bat2_coex_norm_mean_sd_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media
bat2_coex_norm_mean_dist_mm <- function(a,b,sd){
  mean <- (a*bat2_indice_dist/(b+bat2_indice_dist))
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_mean_dist_mm_start <- list(a=bat2_coex_a_mm_start,b=bat2_coex_b_mm_start,sd=sd(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_mean_dist_mm_mle <- mle2(bat2_coex_norm_mean_dist_mm, start=bat2_coex_norm_mean_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre sd
bat2_coex_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*bat2_indice_dist/(b+bat2_indice_dist))^2)
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_sd_dist_mm_start <- list(a=bat2_coex_a_mm_start,b=bat2_coex_b_mm_start,mean=mean(bat2_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_sd_dist_mm_mle <- mle2(bat2_coex_norm_sd_dist_mm, start=bat2_coex_norm_sd_dist_mm_start,control=list(maxit=10000))

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media e sd
bat2_coex_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <-(a*bat2_indice_dist/(b+bat2_indice_dist))
  sd <- sqrt((c*bat2_indice_dist/(d+bat2_indice_dist))^2)
  -sum(dnorm(bat2_coex, mean=mean, sd=sd, log=TRUE))
}
bat2_coex_norm_mean_sd_dist_mm_start <- list(a=bat2_coex_a_mm_start,b=bat2_coex_b_mm_start,c=bat2_coex_a_mm_start,d=bat2_coex_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_coex_norm_mean_sd_dist_mm_mle <- mle2(bat2_coex_norm_mean_sd_dist_mm, start=bat2_coex_norm_mean_sd_dist_mm_start,method="Nelder-Mead",control=list(maxit=1000))

################### SELECAO DE MODELOS ####################
AICtab(bat2_coex_gama_nulo_mle,bat2_coex_gama_scale_dist_lin_mle,bat2_coex_gama_scale_dist_mm_mle,bat2_coex_gama_scale_dist_pot_mle,bat2_coex_gama_scale_dist_quadr_mle,bat2_coex_gama_var_scale_dist_lin_mle,bat2_coex_gama_var_scale_dist_mm_mle,bat2_coex_gama_var_scale_dist_pot_mle,bat2_coex_gama_var_scale_dist_quadr_mle,bat2_coex_norm_nulo_mle,bat2_coex_norm_mean_dist_lin_mle,bat2_coex_norm_sd_dist_lin_mle,bat2_coex_norm_mean_sd_dist_lin_mle,bat2_coex_norm_mean_dist_mm_mle,bat2_coex_norm_sd_dist_mm_mle,bat2_coex_norm_mean_sd_dist_mm_mle,bat2_coex_norm_mean_dist_pot_mle,bat2_coex_norm_sd_dist_pot_mle,bat2_coex_norm_mean_sd_dist_pot_mle,bat2_coex_norm_mean_dist_quadr_mle,bat2_coex_norm_sd_dist_quadr_mle,bat2_coex_norm_mean_sd_dist_quadr_mle,weights=T)
### modelo selecionado: bat2_coex_norm_nulo_mle

##########################################################
######################## Bateria 3 #######################
##########################################################

################# Transformacao dos dados #################
# bat3_var_indice_meia_vida <- c()
# for(i in 1:1000){
#   bat3_var_indice_meia_vida[i] <- bat3_var_indice_media_temporal[i,bat3_meia_vida[i]]
# }
# bat3_var_indice_meia_vida[which(is.na(bat3_var_indice_meia_vida))] <- 0
# bat3_coex <- bat3_var_indice_meia_vida*as.integer(bat3_meia_vida)
bat3_coex <- bat3_porc_entre_nger5000_riq2
bat3_indice_dist <- bat3_indice_dist_riq2

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat3_coex_lin <- lm(bat3_coex~bat3_indice_dist)
bat3_coex_a_lin_start <- coef(bat3_coex_lin)[[2]]
bat3_coex_b_lin_start <- coef(bat3_coex_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat3_coex_mm_linearizada <- lm(bat3_indice_dist/bat3_coex~bat3_indice_dist) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
bat3_coex_a_mm_start <- 1/(coef(bat3_coex_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
bat3_coex_b_mm_start <- (coef(bat3_coex_mm_linearizada)[[1]])*(1/coef(bat3_coex_mm_linearizada)[[2]]) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a, b e c da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat3_coex_a_pot_start <- 5e-18
bat3_coex_b_pot_start <- 2.9

# Valores de start para os coeficientes a, b e c da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat3_coex_a_quadr_start <- 0.00000000000001
bat3_coex_b_quadr_start <- 0.000000003
bat3_coex_c_quadr_start <- 0

# GAMA

## Nulo
bat3_coex_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
}
bat3_coex_gama_nulo_start <- list(shape=mean(bat3_coex)^2/var(bat3_coex),scale=var(bat3_coex)/mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat3_coex_gama_nulo_mle <- mle2(bat3_coex_gama_nulo, start=bat3_coex_gama_nulo_start)
###### ou:
bat3_coex_gama_nulo_glm <- glm(bat3_coex~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat3_coex_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat3_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat3_coex_gama_shape_dist_lin_start <- list(a=bat3_coex_a_lin_start,b=bat3_coex_b_lin_start,scale=var(bat3_coex)/mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_coex_gama_shape_dist_lin_mle <- mle2(bat3_coex_gama_shape_dist_lin, start=bat3_coex_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_coex_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat3_indice_dist+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
}
bat3_coex_gama_scale_dist_lin_start <- list(a=bat3_coex_a_lin_start,b=bat3_coex_b_lin_start,shape=mean(bat3_coex)^2/var(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_coex_gama_scale_dist_lin_mle <- mle2(bat3_coex_gama_scale_dist_lin, start=bat3_coex_gama_scale_dist_lin_start)
######## ou:
bat3_coex_gama_dist_lin_glm <- glm(bat3_coex~bat3_indice_dist,family="Gamma"(link="identity"),start=c(bat3_coex_a_lin_start,0))

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre shape
# bat3_coex_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*((bat3_indice_dist)^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat3_coex_gama_shape_dist_pot_start <- list(a=bat3_coex_a_pot_start,b=bat3_coex_b_pot_start,scale=var(bat3_coex)/mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_coex_gama_shape_dist_pot_mle <- mle2(bat3_coex_gama_shape_dist_pot, start=bat3_coex_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre scale
bat3_coex_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado<- (a*(bat3_indice_dist^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
}
bat3_coex_gama_scale_dist_pot_start <- list(a=bat3_coex_a_pot_start,b=bat3_coex_b_pot_start,shape=mean(bat3_coex)^2/var(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_coex_gama_scale_dist_pot_mle <- mle2(bat3_coex_gama_scale_dist_pot, start=bat3_coex_gama_scale_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre shape
# bat3_coex_gama_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(bat3_indice_dist^2) + b*(bat3_indice_dist) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat3_coex_gama_shape_dist_quadr_start <- list(a=bat3_coex_a_quadr_start,b=bat3_coex_b_quadr_start,c=bat3_coex_c_quadr_start,scale=var(bat3_coex)/mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_coex_gama_shape_dist_quadr_mle <- mle2(bat3_coex_gama_shape_dist_quadr, start=bat3_coex_gama_shape_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_coex_gama_scale_dist_quadr <- function(a,b,c,shape){
  esperancaaoquadrado<- (a*(bat3_indice_dist^2) + b*(bat3_indice_dist) + c)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
}
bat3_coex_gama_scale_dist_quadr_start <- list(a=bat3_coex_a_quadr_start,b=bat3_coex_b_quadr_start,c=bat3_coex_c_quadr_start,shape=mean(bat3_coex)^2/var(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_coex_gama_scale_dist_quadr_mle <- mle2(bat3_coex_gama_scale_dist_quadr, start=bat3_coex_gama_scale_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre shape
# bat3_coex_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat3_indice_dist/(b+bat3_indice_dist))^2 # ambos os lados da funcao foram elevados ao mmado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat3_coex_gama_shape_dist_mm_start <- list(a=bat3_coex_a_mm_start,b=bat3_coex_b_mm_start,scale=var(bat3_coex)/mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_coex_gama_shape_dist_mm_mle <- mle2(bat3_coex_gama_shape_dist_mm, start=bat3_coex_gama_shape_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_coex_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat3_indice_dist/(b+bat3_indice_dist))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
}
bat3_coex_gama_scale_dist_mm_start <- list(a=bat3_coex_a_mm_start,b=bat3_coex_b_mm_start,shape=mean(bat3_coex)^2/var(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_coex_gama_scale_dist_mm_mle <- mle2(bat3_coex_gama_scale_dist_mm, start=bat3_coex_gama_scale_dist_mm_start,method="Nelder-Mead")

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat3_coex_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat3_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat3_coex_gama_var_shape_dist_lin_start <- list(a=bat3_coex_a_lin_start,b=bat3_coex_b_lin_start,scale=var(bat3_coex)/mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_coex_gama_var_shape_dist_lin_mle <- mle2(bat3_coex_gama_var_shape_dist_lin, start=bat3_coex_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_coex_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat3_indice_dist+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
}
bat3_coex_gama_var_scale_dist_lin_start <- list(a=bat3_coex_a_lin_start,b=bat3_coex_b_lin_start,shape=mean(bat3_coex)^2/var(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_coex_gama_var_scale_dist_lin_mle <- mle2(bat3_coex_gama_var_scale_dist_lin, start=bat3_coex_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre shape
# bat3_coex_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat3_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat3_coex_gama_var_shape_dist_pot_start <- list(a=bat3_coex_a_pot_start,b=bat3_coex_b_pot_start,scale=var(bat3_coex)/mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_coex_gama_var_shape_dist_pot_mle <- mle2(bat3_coex_gama_var_shape_dist_pot, start=bat3_coex_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre scale
bat3_coex_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado<- (a*(bat3_indice_dist^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
}
bat3_coex_gama_var_scale_dist_pot_start <- list(a=bat3_coex_a_pot_start,b=bat3_coex_b_pot_start,shape=mean(bat3_coex)^2/var(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_coex_gama_var_scale_dist_pot_mle <- mle2(bat3_coex_gama_var_scale_dist_pot, start=bat3_coex_gama_var_scale_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat3_coex_gama_var_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat3_indice_dist^2) + b*(bat3_indice_dist) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat3_coex_gama_var_shape_dist_quadr_start <- list(a=bat3_coex_a_quadr_start,b=bat3_coex_b_quadr_start,c=bat3_coex_c_quadr_start,scale=var(bat3_coex)/mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_coex_gama_var_shape_dist_quadr_mle <- mle2(bat3_coex_gama_var_shape_dist_quadr, start=bat3_coex_gama_var_shape_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_coex_gama_var_scale_dist_quadr <- function(a,b,c,shape){
  varianciaaoquadrado<- (a*(bat3_indice_dist^2) + b*(bat3_indice_dist) + c)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
}
bat3_coex_gama_var_scale_dist_quadr_start <- list(a=bat3_coex_a_quadr_start,b=bat3_coex_b_quadr_start,c=bat3_coex_c_quadr_start,shape=mean(bat3_coex)^2/var(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_coex_gama_var_scale_dist_quadr_mle <- mle2(bat3_coex_gama_var_scale_dist_quadr, start=bat3_coex_gama_var_scale_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat3_coex_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat3_indice_dist/(b+bat3_indice_dist))^2 # ambos os lados da funcao foram elevados ao mmado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
# }
# bat3_coex_gama_var_shape_dist_mm_start <- list(a=bat3_coex_a_mm_start,b=bat3_coex_b_mm_start,scale=var(bat3_coex)/mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_coex_gama_var_shape_dist_mm_mle <- mle2(bat3_coex_gama_var_shape_dist_mm, start=bat3_coex_gama_var_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_coex_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat3_indice_dist/(b+bat3_indice_dist))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat3_coex, shape=shape, scale=scale, log=TRUE))
}
bat3_coex_gama_var_scale_dist_mm_start <- list(a=bat3_coex_a_mm_start,b=bat3_coex_b_mm_start,shape=mean(bat3_coex)^2/var(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_coex_gama_var_scale_dist_mm_mle <- mle2(bat3_coex_gama_var_scale_dist_mm, start=bat3_coex_gama_var_scale_dist_mm_start,method="Nelder-Mead")

# NORMAL

## Nulo
bat3_coex_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_nulo_start <- list(mean=mean(bat3_coex),sd=sd(bat3_coex)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat3_coex_norm_nulo_mle <- mle2(bat3_coex_norm_nulo, start=bat3_coex_norm_nulo_start)
########## ou:
bat3_coex_norm_nulo_glm <- glm(bat3_coex~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
bat3_coex_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*bat3_indice_dist+b
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_mean_dist_lin_start <- list(a=bat3_coex_a_lin_start,b=bat3_coex_b_lin_start,sd=sd(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_mean_dist_lin_mle <- mle2(bat3_coex_norm_mean_dist_lin, start=bat3_coex_norm_mean_dist_lin_start)
######### ou:
bat3_coex_norm_dist_lin_glm <- glm(bat3_coex~bat3_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
bat3_coex_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*bat3_indice_dist+b)^2)
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_sd_dist_lin_start <- list(a=bat3_coex_a_lin_start,b=bat3_coex_b_lin_start,mean=mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_sd_dist_lin_mle <- mle2(bat3_coex_norm_sd_dist_lin, start=bat3_coex_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media e sd
bat3_coex_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*bat3_indice_dist+b
  sd <- sqrt((c*bat3_indice_dist+d)^2)
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_mean_sd_dist_lin_start <- list(a=bat3_coex_a_lin_start,b=bat3_coex_b_lin_start,c=bat3_coex_a_lin_start,d=bat3_coex_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_mean_sd_dist_lin_mle <- mle2(bat3_coex_norm_mean_sd_dist_lin, start=bat3_coex_norm_mean_sd_dist_lin_start,method="Nelder-Mead")


## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre a media
bat3_coex_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(bat3_indice_dist^b)
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_mean_dist_pot_start <- list(a=bat3_coex_a_pot_start,b=bat3_coex_b_pot_start,sd=sd(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_mean_dist_pot_mle <- mle2(bat3_coex_norm_mean_dist_pot, start=bat3_coex_norm_mean_dist_pot_start)

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre sd
bat3_coex_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(bat3_indice_dist^b))^2)
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_sd_dist_pot_start <- list(a=bat3_coex_a_pot_start,b=bat3_coex_b_pot_start,mean=mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_sd_dist_pot_mle <- mle2(bat3_coex_norm_sd_dist_pot, start=bat3_coex_norm_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre a media e sd
bat3_coex_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(bat3_indice_dist^b)
  sd <- sqrt((c*(bat3_indice_dist^d))^2)
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_mean_sd_dist_pot_start <- list(a=bat3_coex_a_pot_start,b=bat3_coex_b_pot_start,c=bat3_coex_a_pot_start,d=bat3_coex_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_mean_sd_dist_pot_mle <- mle2(bat3_coex_norm_mean_sd_dist_pot, start=bat3_coex_norm_mean_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media
bat3_coex_norm_mean_dist_quadr <- function(a,b,c,sd){
  mean <- a*(bat3_indice_dist^2) + b*(bat3_indice_dist) + c
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_mean_dist_quadr_start <- list(a=bat3_coex_a_quadr_start,b=bat3_coex_b_quadr_start,c=bat3_coex_c_quadr_start,sd=sd(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_mean_dist_quadr_mle <- mle2(bat3_coex_norm_mean_dist_quadr, start=bat3_coex_norm_mean_dist_quadr_start)

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre sd
bat3_coex_norm_sd_dist_quadr <- function(a,b,c,mean){
  sd <- sqrt((a*(bat3_indice_dist^2) + b*(bat3_indice_dist) + c)^2)
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_sd_dist_quadr_start <- list(a=bat3_coex_a_quadr_start,b=bat3_coex_b_quadr_start,c=bat3_coex_c_quadr_start,mean=mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_sd_dist_quadr_mle <- mle2(bat3_coex_norm_sd_dist_quadr, start=bat3_coex_norm_sd_dist_quadr_start)

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media e sd
bat3_coex_norm_mean_sd_dist_quadr <- function(a,b,c,d,e,f){
  mean <- a*(bat3_indice_dist^2) + b*(bat3_indice_dist) + c
  sd <- sqrt((d*(bat3_indice_dist^2) + e*(bat3_indice_dist) + f)^2)
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_mean_sd_dist_quadr_start <- list(a=bat3_coex_a_quadr_start,b=bat3_coex_b_quadr_start,c=bat3_coex_c_quadr_start,d=bat3_coex_a_quadr_start,e=bat3_coex_b_quadr_start,f=bat3_coex_c_quadr_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_mean_sd_dist_quadr_mle <- mle2(bat3_coex_norm_mean_sd_dist_quadr, start=bat3_coex_norm_mean_sd_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media
bat3_coex_norm_mean_dist_mm <- function(a,b,sd){
  mean <- (a*bat3_indice_dist/(b+bat3_indice_dist))
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_mean_dist_mm_start <- list(a=bat3_coex_a_mm_start,b=bat3_coex_b_mm_start,sd=sd(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_mean_dist_mm_mle <- mle2(bat3_coex_norm_mean_dist_mm, start=bat3_coex_norm_mean_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre sd
bat3_coex_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*bat3_indice_dist/(b+bat3_indice_dist))^2)
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_sd_dist_mm_start <- list(a=bat3_coex_a_mm_start,b=bat3_coex_b_mm_start,mean=mean(bat3_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_sd_dist_mm_mle <- mle2(bat3_coex_norm_sd_dist_mm, start=bat3_coex_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media e sd
bat3_coex_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <-(a*bat3_indice_dist/(b+bat3_indice_dist))
  sd <- sqrt((c*bat3_indice_dist/(d+bat3_indice_dist))^2)
  -sum(dnorm(bat3_coex, mean=mean, sd=sd, log=TRUE))
}
bat3_coex_norm_mean_sd_dist_mm_start <- list(a=bat3_coex_a_mm_start,b=bat3_coex_b_mm_start,c=bat3_coex_a_mm_start,d=bat3_coex_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_coex_norm_mean_sd_dist_mm_mle <- mle2(bat3_coex_norm_mean_sd_dist_mm, start=bat3_coex_norm_mean_sd_dist_mm_start,method="Nelder-Mead")

################### SELECAO DE MODELOS ####################
AICtab(bat3_coex_gama_nulo_mle,bat3_coex_gama_scale_dist_lin_mle,bat3_coex_gama_scale_dist_mm_mle,bat3_coex_gama_scale_dist_pot_mle,bat3_coex_gama_scale_dist_quadr_mle,bat3_coex_gama_var_scale_dist_lin_mle,bat3_coex_gama_var_scale_dist_mm_mle,bat3_coex_gama_var_scale_dist_pot_mle,bat3_coex_gama_var_scale_dist_quadr_mle,bat3_coex_norm_nulo_mle,bat3_coex_norm_mean_dist_lin_mle,bat3_coex_norm_sd_dist_lin_mle,bat3_coex_norm_mean_sd_dist_lin_mle,bat3_coex_norm_mean_dist_mm_mle,bat3_coex_norm_sd_dist_mm_mle,bat3_coex_norm_mean_sd_dist_mm_mle,bat3_coex_norm_mean_dist_pot_mle,bat3_coex_norm_sd_dist_pot_mle,bat3_coex_norm_mean_sd_dist_pot_mle,bat3_coex_norm_mean_dist_quadr_mle,bat3_coex_norm_sd_dist_quadr_mle,bat3_coex_norm_mean_sd_dist_quadr_mle,weights=T)
### modelo selecionado: bat3_coex_gama_var_scale_dist_pot_mle

##########################################################
############# CALCULO DO AIC INDIVIDUAL TOTAL ############
##########################################################
aic_mle_soma_individual_coex <- AIC(bat2_coex_norm_nulo_mle) + AIC(bat3_coex_gama_scale_dist_lin_mle) #4462.986




##########################################################
##########################################################
##### Analises das baterias de simulacao EM CONJUNTO #####
##########################################################
##########################################################

geral_coex <- c(bat2_coex,bat3_coex)
geral_indice_dist_coex <- c(bat2_indice_dist,bat3_indice_dist)

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
geral_coex_lin <- lm(geral_coex~geral_indice_dist_coex)
geral_coex_a_lin_start <- coef(geral_coex_lin)[[2]]
geral_coex_b_lin_start <- coef(geral_coex_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
geral_coex_mm_linearizada <- lm(geral_indice_dist_coex/geral_coex~geral_indice_dist_coex) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
geral_coex_a_mm_start <- 1/(coef(geral_coex_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
geral_coex_b_mm_start <- (coef(geral_coex_mm_linearizada)[[1]])*(1/coef(geral_coex_mm_linearizada)[[2]]) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a, b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
geral_coex_a_pot_start <- 5e-18
geral_coex_b_pot_start <- 2.93

# Valores de start para os coeficientes a, b e c da funcao quadratica usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
geral_coex_a_quadr_start <- bat3_coex_a_quadr_start
geral_coex_b_quadr_start <- bat3_coex_a_quadr_start 
geral_coex_c_quadr_start <- 2500

# GAMA

## Nulo
geral_coex_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
}
geral_coex_gama_nulo_start <- list(shape=mean(geral_coex)^2/var(geral_coex),scale=var(geral_coex)/mean(geral_coex)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
geral_coex_gama_nulo_mle <- mle2(geral_coex_gama_nulo, start=geral_coex_gama_nulo_start,method="Nelder-Mead")
###### ou:
geral_coex_gama_nulo_glm <- glm(geral_coex~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# geral_coex_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*geral_indice_dist_coex+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
# }
# geral_coex_gama_shape_dist_lin_start <- list(a=geral_coex_a_lin_start,b=geral_coex_b_lin_start,scale=var(geral_coex)/mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_coex_gama_shape_dist_lin_mle <- mle2(geral_coex_gama_shape_dist_lin, start=geral_coex_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_coex_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*geral_indice_dist_coex+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
}
geral_coex_gama_scale_dist_lin_start <- list(a=geral_coex_a_lin_start,b=geral_coex_b_lin_start,shape=mean(geral_coex)^2/var(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_coex_gama_scale_dist_lin_mle <- mle2(geral_coex_gama_scale_dist_lin, start=geral_coex_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
geral_coex_gama_dist_lin_glm <- glm(geral_coex~geral_indice_dist_coex,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre shape
# geral_coex_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*((geral_indice_dist_coex)^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
# }
# geral_coex_gama_shape_dist_pot_start <- list(a=geral_coex_a_pot_start,b=geral_coex_b_pot_start,scale=var(geral_coex)/mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_coex_gama_shape_dist_pot_mle <- mle2(geral_coex_gama_shape_dist_pot, start=geral_coex_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre scale
geral_coex_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado<- (a*(geral_indice_dist_coex^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
}
geral_coex_gama_scale_dist_pot_start <- list(a=geral_coex_a_pot_start,b=geral_coex_b_pot_start,shape=mean(geral_coex)^2/var(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_coex_gama_scale_dist_pot_mle <- mle2(geral_coex_gama_scale_dist_pot, start=geral_coex_gama_scale_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre shape
# geral_coex_gama_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(geral_indice_dist_coex^2) + b*(geral_indice_dist_coex) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
# }
# geral_coex_gama_shape_dist_quadr_start <- list(a=geral_coex_a_quadr_start,b=geral_coex_b_quadr_start,c=geral_coex_c_quadr_start,scale=var(geral_coex)/mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_coex_gama_shape_dist_quadr_mle <- mle2(geral_coex_gama_shape_dist_quadr, start=geral_coex_gama_shape_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_coex_gama_scale_dist_quadr <- function(a,b,c,shape){
  esperancaaoquadrado<- (a*(geral_indice_dist_coex^2) + b*(geral_indice_dist_coex) + c)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
}
geral_coex_gama_scale_dist_quadr_start <- list(a=geral_coex_a_quadr_start,b=geral_coex_b_quadr_start,c=geral_coex_c_quadr_start,shape=mean(geral_coex)^2/var(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_coex_gama_scale_dist_quadr_mle <- mle2(geral_coex_gama_scale_dist_quadr, start=geral_coex_gama_scale_dist_quadr_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre shape
# geral_coex_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*geral_indice_dist_coex/(b+geral_indice_dist_coex))^2 # ambos os lados da funcao foram elevados ao mmado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
# }
# geral_coex_gama_shape_dist_mm_start <- list(a=geral_coex_a_mm_start,b=geral_coex_b_mm_start,scale=var(geral_coex)/mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_coex_gama_shape_dist_mm_mle <- mle2(geral_coex_gama_shape_dist_mm, start=geral_coex_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_coex_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado<- (a*geral_indice_dist_coex/(b+geral_indice_dist_coex))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
}
geral_coex_gama_scale_dist_mm_start <- list(a=geral_coex_a_mm_start,b=geral_coex_b_mm_start,shape=mean(geral_coex)^2/var(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_coex_gama_scale_dist_mm_mle <- mle2(geral_coex_gama_scale_dist_mm, start=geral_coex_gama_scale_dist_mm_start,method="Nelder-Mead")

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# geral_coex_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*geral_indice_dist_coex+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
# }
# geral_coex_gama_var_shape_dist_lin_start <- list(a=geral_coex_a_lin_start,b=geral_coex_b_lin_start,scale=var(geral_coex)/mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_coex_gama_var_shape_dist_lin_mle <- mle2(geral_coex_gama_var_shape_dist_lin, start=geral_coex_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_coex_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*geral_indice_dist_coex+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
}
geral_coex_gama_var_scale_dist_lin_start <- list(a=geral_coex_a_lin_start,b=geral_coex_b_lin_start,shape=mean(geral_coex)^2/var(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_coex_gama_var_scale_dist_lin_mle <- mle2(geral_coex_gama_var_scale_dist_lin, start=geral_coex_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre shape
# geral_coex_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(geral_indice_dist_coex^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
# }
# geral_coex_gama_var_shape_dist_pot_start <- list(a=geral_coex_a_pot_start,b=geral_coex_b_pot_start,scale=var(geral_coex)/mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_coex_gama_var_shape_dist_pot_mle <- mle2(geral_coex_gama_var_shape_dist_pot, start=geral_coex_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre scale
geral_coex_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado<- (a*(geral_indice_dist_coex^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
}
geral_coex_gama_var_scale_dist_pot_start <- list(a=geral_coex_a_pot_start,b=geral_coex_b_pot_start,shape=mean(geral_coex)^2/var(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_coex_gama_var_scale_dist_pot_mle <- mle2(geral_coex_gama_var_scale_dist_pot, start=geral_coex_gama_var_scale_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# geral_coex_gama_var_shape_dist_quadr <- function(a,b,c,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(geral_indice_dist_coex^2) + b*(geral_indice_dist_coex) + c)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
# }
# geral_coex_gama_var_shape_dist_quadr_start <- list(a=geral_coex_a_quadr_start,b=geral_coex_b_quadr_start,c=geral_coex_c_quadr_start,scale=var(geral_coex)/mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_coex_gama_var_shape_dist_quadr_mle <- mle2(geral_coex_gama_var_shape_dist_quadr, start=geral_coex_gama_var_shape_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_coex_gama_var_scale_dist_quadr <- function(a,b,c,shape){
  varianciaaoquadrado<- (a*(geral_indice_dist_coex^2) + b*(geral_indice_dist_coex) + c)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
}
geral_coex_gama_var_scale_dist_quadr_start <- list(a=geral_coex_a_quadr_start,b=geral_coex_b_quadr_start,c=geral_coex_c_quadr_start,shape=mean(geral_coex)^2/var(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_coex_gama_var_scale_dist_quadr_mle <- mle2(geral_coex_gama_var_scale_dist_quadr, start=geral_coex_gama_var_scale_dist_quadr_start)

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# geral_coex_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*geral_indice_dist_coex/(b+geral_indice_dist_coex))^2 # ambos os lados da funcao foram elevados ao mmado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
# }
# geral_coex_gama_var_shape_dist_mm_start <- list(a=geral_coex_a_mm_start,b=geral_coex_b_mm_start,scale=var(geral_coex)/mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_coex_gama_var_shape_dist_mm_mle <- mle2(geral_coex_gama_var_shape_dist_mm, start=geral_coex_gama_var_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_coex_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado<- (a*geral_indice_dist_coex/(b+geral_indice_dist_coex))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(geral_coex, shape=shape, scale=scale, log=TRUE))
}
geral_coex_gama_var_scale_dist_mm_start <- list(a=geral_coex_a_mm_start,b=geral_coex_b_mm_start,shape=mean(geral_coex)^2/var(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_coex_gama_var_scale_dist_mm_mle <- mle2(geral_coex_gama_var_scale_dist_mm, start=geral_coex_gama_var_scale_dist_mm_start,method="Nelder-Mead")


# NORMAL

## Nulo
geral_coex_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_nulo_start <- list(mean=mean(geral_coex),sd=sd(geral_coex)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
geral_coex_norm_nulo_mle <- mle2(geral_coex_norm_nulo, start=geral_coex_norm_nulo_start)
########## ou:
geral_coex_norm_nulo_glm <- glm(geral_coex~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
geral_coex_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*geral_indice_dist_coex+b
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_mean_dist_lin_start <- list(a=geral_coex_a_lin_start,b=geral_coex_b_lin_start,sd=sd(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_mean_dist_lin_mle <- mle2(geral_coex_norm_mean_dist_lin, start=geral_coex_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
geral_coex_norm_dist_lin_glm <- glm(geral_coex~geral_indice_dist_coex,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
geral_coex_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*geral_indice_dist_coex+b)^2)
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_sd_dist_lin_start <- list(a=geral_coex_a_lin_start,b=geral_coex_b_lin_start,mean=mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_sd_dist_lin_mle <- mle2(geral_coex_norm_sd_dist_lin, start=geral_coex_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media e sd
geral_coex_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*geral_indice_dist_coex+b
  sd <- sqrt((c*geral_indice_dist_coex+d)^2)
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_mean_sd_dist_lin_start <- list(a=geral_coex_a_lin_start,b=geral_coex_b_lin_start,c=geral_coex_a_lin_start,d=geral_coex_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_mean_sd_dist_lin_mle <- mle2(geral_coex_norm_mean_sd_dist_lin, start=geral_coex_norm_mean_sd_dist_lin_start,method="Nelder-Mead")


## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre a media
geral_coex_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(geral_indice_dist_coex^b)
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_mean_dist_pot_start <- list(a=geral_coex_a_pot_start,b=geral_coex_b_pot_start,sd=sd(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_mean_dist_pot_mle <- mle2(geral_coex_norm_mean_dist_pot, start=geral_coex_norm_mean_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre sd
geral_coex_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(geral_indice_dist_coex^b))^2)
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_sd_dist_pot_start <- list(a=geral_coex_a_pot_start,b=geral_coex_b_pot_start,mean=mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_sd_dist_pot_mle <- mle2(geral_coex_norm_sd_dist_pot, start=geral_coex_norm_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao potencia sobre a media e sd
geral_coex_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(geral_indice_dist_coex^b)
  sd <- sqrt((c*(geral_indice_dist_coex^d))^2)
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_mean_sd_dist_pot_start <- list(a=geral_coex_a_pot_start,b=geral_coex_b_pot_start,c=geral_coex_a_pot_start,d=geral_coex_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_mean_sd_dist_pot_mle <- mle2(geral_coex_norm_mean_sd_dist_pot, start=geral_coex_norm_mean_sd_dist_pot_start)

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media
geral_coex_norm_mean_dist_quadr <- function(a,b,c,sd){
  mean <- a*(geral_indice_dist_coex^2) + b*(geral_indice_dist_coex) + c
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_mean_dist_quadr_start <- list(a=geral_coex_a_quadr_start,b=geral_coex_b_quadr_start,c=geral_coex_c_quadr_start,sd=sd(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_mean_dist_quadr_mle <- mle2(geral_coex_norm_mean_dist_quadr, start=geral_coex_norm_mean_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre sd
geral_coex_norm_sd_dist_quadr <- function(a,b,c,mean){
  sd <- sqrt((a*(geral_indice_dist_coex^2) + b*(geral_indice_dist_coex) + c)^2)
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_sd_dist_quadr_start <- list(a=geral_coex_a_quadr_start,b=geral_coex_b_quadr_start,c=geral_coex_c_quadr_start,mean=mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_sd_dist_quadr_mle <- mle2(geral_coex_norm_sd_dist_quadr, start=geral_coex_norm_sd_dist_quadr_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao quadr sobre a media e sd
geral_coex_norm_mean_sd_dist_quadr <- function(a,b,c,d,e,f){
  mean <- a*(geral_indice_dist_coex^2) + b*(geral_indice_dist_coex) + c
  sd <- sqrt((d*(geral_indice_dist_coex^2) + e*(geral_indice_dist_coex) + f)^2)
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_mean_sd_dist_quadr_start <- list(a=geral_coex_a_quadr_start,b=geral_coex_b_quadr_start,c=geral_coex_c_quadr_start,d=geral_coex_a_quadr_start,e=geral_coex_b_quadr_start,f=geral_coex_c_quadr_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_mean_sd_dist_quadr_mle <- mle2(geral_coex_norm_mean_sd_dist_quadr, start=geral_coex_norm_mean_sd_dist_quadr_start,method="Nelder-Mead",control=list(maxit=10000))

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media
geral_coex_norm_mean_dist_mm <- function(a,b,sd){
  mean <- (a*geral_indice_dist_coex/(b+geral_indice_dist_coex))
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_mean_dist_mm_start <- list(a=geral_coex_a_mm_start,b=geral_coex_b_mm_start,sd=sd(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_mean_dist_mm_mle <- mle2(geral_coex_norm_mean_dist_mm, start=geral_coex_norm_mean_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre sd
geral_coex_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*geral_indice_dist_coex/(b+geral_indice_dist_coex))^2)
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_sd_dist_mm_start <- list(a=geral_coex_a_mm_start,b=geral_coex_b_mm_start,mean=mean(geral_coex)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_sd_dist_mm_mle <- mle2(geral_coex_norm_sd_dist_mm, start=geral_coex_norm_sd_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao mm sobre a media e sd
geral_coex_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <-(a*geral_indice_dist_coex/(b+geral_indice_dist_coex))
  sd <- sqrt((c*geral_indice_dist_coex/(d+geral_indice_dist_coex))^2)
  -sum(dnorm(geral_coex, mean=mean, sd=sd, log=TRUE))
}
geral_coex_norm_mean_sd_dist_mm_start <- list(a=geral_coex_a_mm_start,b=geral_coex_b_mm_start,c=geral_coex_a_mm_start,d=geral_coex_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_coex_norm_mean_sd_dist_mm_mle <- mle2(geral_coex_norm_mean_sd_dist_mm, start=geral_coex_norm_mean_sd_dist_mm_start,method="Nelder-Mead")


################### SELECAO DE MODELOS ####################
AICtab(geral_coex_gama_nulo_mle,geral_coex_gama_scale_dist_lin_mle,geral_coex_gama_scale_dist_mm_mle,geral_coex_gama_scale_dist_pot_mle,geral_coex_gama_scale_dist_quadr_mle,geral_coex_gama_var_scale_dist_lin_mle,geral_coex_gama_var_scale_dist_mm_mle,geral_coex_gama_var_scale_dist_pot_mle,geral_coex_gama_var_scale_dist_quadr_mle,geral_coex_norm_nulo_mle,geral_coex_norm_mean_dist_lin_mle,geral_coex_norm_sd_dist_lin_mle,geral_coex_norm_mean_sd_dist_lin_mle,geral_coex_norm_mean_dist_mm_mle,geral_coex_norm_sd_dist_mm_mle,geral_coex_norm_mean_sd_dist_mm_mle,geral_coex_norm_mean_dist_pot_mle,geral_coex_norm_sd_dist_pot_mle,geral_coex_norm_mean_sd_dist_pot_mle,geral_coex_norm_mean_dist_quadr_mle,geral_coex_norm_sd_dist_quadr_mle,geral_coex_norm_mean_sd_dist_quadr_mle,weights=T)
### modelo selecionado: geral_coex_gama_scale_dist_quadr_mle


##########################################################
############# CALCULO DO AIC INDIVIDUAL TOTAL ############
##########################################################
aic_mle_soma_geral_coex <- AIC(geral_coex_gama_scale_dist_quadr_mle) #7459.243



##########################################################
##########################################################
################## COMPARACAO DOS AICS ###################
##########################################################
##########################################################
aics_coex <- c(por_bateria=aic_mle_soma_individual_coex,geral=aic_mle_soma_geral_coex)
min_aic_coex <- min(aics_coex)
daic_coex <- aics_coex - min_aic_coex
sort(daic_coex)






##########################################################
######################## GRAFICOS ########################
##########################################################
##########################################################
hist(geral_coex,breaks=100,las=1,xlab="Variância da estratégia de vida",ylab="Frequência",main="")

par(mar=c(5,5,4,2))
plot(bat2_coex~bat2_indice_dist,pch=20,bty="l",las=1,xlim=c(0,3e5),col=alpha("pink",0.2),ylab="Razão entre variância \ninterespecífica e total",xlab="Índice de distúrbio",ylim=c(0,5300))
points(bat3_coex~bat3_indice_dist,pch=20,col=alpha("lightblue",0.2))

### plotar previstos
pot <- function(x,a,b){a*(x^b)}
#### individual
#bat2_coex_norm_nulo_mle
abline(h=coef(bat2_coex_norm_nulo_mle)[[1]],col="red")
#bat3_coex_gama_scale_dist_lin_mle
curve(pot(x,coef(bat3_coex_gama_var_scale_dist_pot_mle)[[1]],coef(bat3_coex_gama_var_scale_dist_pot_mle)[[2]]),add=T,col="blue")
