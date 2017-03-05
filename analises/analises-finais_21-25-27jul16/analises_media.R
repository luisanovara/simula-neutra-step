################ Carregamento dos pacotes ################
require(ggplot2)
require(bbmle)


################# Carregamento dos dados #################
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_hipercubo/dados_hipercubo_21jul16/dados_arredond_hipercubo_21jul16.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_21jul16/simulacoes_21jul16_output_derivado.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_hipercubo/dados_hipercubo_25jul16/dados_arredond_hipercubo_25jul16.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_25jul16/simulacoes_25jul16_output_derivado.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_hipercubo/dados_hipercubo_27jul16/dados_arredond_hipercubo_27jul16.RData")
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_output/dados_output_27jul16/simulacoes_27jul16_output_derivado.RData")

##########################################################
##########################################################
########## Media do indice de estrategia de vida #########
##########################################################
##########################################################

##########################################################
##########################################################
##### Analises das baterias de simulacao EM SEPARADO #####
##########################################################
##########################################################

##########################################################
######################## Bateria 1 #######################
##########################################################

#################### DADOS ORIGINAIS ######################
################# Transformacao dos dados #################
bat1_indice_dist <- dados3_21jul16[,2]*dados3_21jul16[,3]
bat1_indice_media <- (simulacoes_21jul16_output_derivado[,4]-1)/(20000-1)

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat1_lin <- lm(bat1_indice_media~bat1_indice_dist)
bat1_a_lin_start <- coef(bat1_lin)[[2]]
bat1_b_lin_start <- coef(bat1_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat1_mm_linearizada <- lm(bat1_indice_dist/bat1_indice_media~bat1_indice_dist) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
bat1_a_mm_start <- 1/(coef(bat1_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
bat1_b_mm_start <- (coef(bat1_mm_linearizada)[[1]])*(1/(coef(bat1_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat1_a_pot_start <- 0.0000015
bat1_b_pot_start <- 1

# GAMA

## Nulo
bat1_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat1_gama_nulo_start <- list(shape=mean(bat1_indice_media)^2/var(bat1_indice_media),scale=var(bat1_indice_media)/mean(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat1_gama_nulo_mle <- mle2(bat1_gama_nulo, start=bat1_gama_nulo_start,method="Nelder-Mead")
###### ou:
bat1_gama_nulo_glm <- glm(bat1_indice_media~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat1_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat1_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat1_gama_shape_dist_lin_start <- list(a=bat1_a_lin_start,b=bat1_b_lin_start,scale=var(bat1_indice_media)/mean(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_gama_shape_dist_lin_mle <- mle2(bat1_gama_shape_dist_lin, start=bat1_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat1_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat1_indice_dist+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat1_gama_scale_dist_lin_start <- list(a=bat1_a_lin_start,b=bat1_b_lin_start,shape=mean(bat1_indice_media)^2/var(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_gama_scale_dist_lin_mle <- mle2(bat1_gama_scale_dist_lin, start=bat1_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
bat1_gama_dist_lin_glm <- glm(bat1_indice_media~bat1_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# bat1_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat1_indice_dist/(b+bat1_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat1_gama_shape_dist_mm_start <- list(a=bat1_a_mm_start,b=bat1_b_mm_start,scale=var(bat1_indice_media)/mean(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_gama_shape_dist_mm_mle <- mle2(bat1_gama_shape_dist_mm, start=bat1_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
bat1_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado <- (a*bat1_indice_dist/(b+bat1_indice_dist))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat1_gama_scale_dist_mm_start <- list(a=bat1_a_mm_start,b=bat1_b_mm_start,shape=mean(bat1_indice_media)^2/var(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_gama_scale_dist_mm_mle <- mle2(bat1_gama_scale_dist_mm, start=bat1_gama_scale_dist_mm_start)
######## ou:
bat1_gama_dist_mm_glm <- glm(bat1_indice_dist/bat1_indice_media~bat1_indice_dist,family="Gamma"(link="identity"))


# bat1_gama_dist_mm_nls <- nls(bat1_indice_media ~ a*bat1_indice_dist/(b+bat1_indice_dist),start=list(a=coef(bat1_mm_linearizada)[[2]],b=coef(bat1_mm_linearizada)[[1]]))
# bat1_gama_dist_mm_gnm <- gnm(bat1_indice_media ~ a*bat1_indice_dist/(b+bat1_indice_dist),family="Gamma"(link="identity"))
# MM <- function(resp, conc){
#   list(predictors = list(a = substitute(conc), b = 1),
#        variables = list(substitute(resp), substitute(conc)),
#        term = function(predictors, variables) {
#          pred <- paste("(", predictors[1],"*",variables[2], "/(", predictors[2]," + ", variables[2], "))", sep = "")
#          pred <- paste("(", variables[1], " - ", pred, ")", sep = "")
#        })
# }
# class(MM) <- "nonlin"
# opa <- gnm( ~ -1+MM(bat1_indice_media, bat1_indice_dist),start = c(a=bat1_a_mm_start, b=bat1_b_mm_start), verbose = FALSE,family="Gamma"(link="identity"),iterMax=1000,optim="Nelder-Mead")
# opa


## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# bat1_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(bat1_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat1_gama_shape_dist_pot_start <- list(a=bat1_a_pot_start,b=bat1_b_pot_start,scale=var(bat1_indice_media)/mean(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_gama_shape_dist_pot_mle <- mle2(bat1_gama_shape_dist_pot, start=bat1_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
bat1_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado <- (a*(bat1_indice_dist^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat1_gama_scale_dist_pot_start <- list(a=bat1_a_pot_start,b=bat1_b_pot_start,shape=mean(bat1_indice_media)^2/var(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_gama_scale_dist_pot_mle <- mle2(bat1_gama_scale_dist_pot, start=bat1_gama_scale_dist_pot_start,method="Nelder-Mead")




### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat1_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat1_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat1_gama_var_shape_dist_lin_start <- list(a=bat1_a_lin_start,b=bat1_b_lin_start,scale=var(bat1_indice_media)/mean(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_gama_var_shape_dist_lin_mle <- mle2(bat1_gama_var_shape_dist_lin, start=bat1_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat1_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat1_indice_dist+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat1_gama_var_scale_dist_lin_start <- list(a=bat1_a_lin_start,b=bat1_b_lin_start,shape=mean(bat1_indice_media)^2/var(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_gama_var_scale_dist_lin_mle <- mle2(bat1_gama_var_scale_dist_lin, start=bat1_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# bat1_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat1_indice_dist/(b+bat1_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat1_gama_var_shape_dist_mm_start <- list(a=bat1_a_mm_start,b=bat1_b_mm_start,scale=var(bat1_indice_media)/mean(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_gama_var_shape_dist_mm_mle <- mle2(bat1_gama_var_shape_dist_mm, start=bat1_gama_var_shape_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
bat1_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado <- (a*bat1_indice_dist/(b+bat1_indice_dist))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat1_gama_var_scale_dist_mm_start <- list(a=bat1_a_mm_start,b=bat1_b_mm_start,shape=mean(bat1_indice_media)^2/var(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_gama_var_scale_dist_mm_mle <- mle2(bat1_gama_var_scale_dist_mm, start=bat1_gama_var_scale_dist_mm_start)



## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# bat1_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat1_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat1_gama_var_shape_dist_pot_start <- list(a=bat1_a_pot_start,b=bat1_b_pot_start,scale=var(bat1_indice_media)/mean(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat1_gama_var_shape_dist_pot_mle <- mle2(bat1_gama_var_shape_dist_pot, start=bat1_gama_var_shape_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
bat1_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado <- (a*(bat1_indice_dist^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat1_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat1_gama_var_scale_dist_pot_start <- list(a=bat1_a_pot_start,b=bat1_b_pot_start,shape=mean(bat1_indice_media)^2/var(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat1_gama_var_scale_dist_pot_mle <- mle2(bat1_gama_var_scale_dist_pot, start=bat1_gama_var_scale_dist_pot_start,method="Nelder-Mead")



# NORMAL

## Nulo
bat1_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(bat1_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat1_norm_nulo_start <- list(mean=mean(bat1_indice_media),sd=sd(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat1_norm_nulo_mle <- mle2(bat1_norm_nulo, start=bat1_norm_nulo_start,method="Nelder-Mead")
########## ou:
bat1_norm_nulo_glm <- glm(bat1_indice_media~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
bat1_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*bat1_indice_dist+b
  -sum(dnorm(bat1_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat1_norm_mean_dist_lin_start <- list(a=bat1_a_lin_start,b=bat1_b_lin_start,sd=sd(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_norm_mean_dist_lin_mle <- mle2(bat1_norm_mean_dist_lin, start=bat1_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
bat1_norm_dist_lin_glm <- glm(bat1_indice_media~bat1_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
bat1_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*bat1_indice_dist+b)^2)
  -sum(dnorm(bat1_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat1_norm_sd_dist_lin_start <- list(a=bat1_a_lin_start,b=bat1_b_lin_start,mean=mean(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_norm_sd_dist_lin_mle <- mle2(bat1_norm_sd_dist_lin, start=bat1_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre mean e sd
bat1_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*bat1_indice_dist+b
  sd <- sqrt((c*bat1_indice_dist+d)^2)
  -sum(dnorm(bat1_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat1_norm_mean_sd_dist_lin_start <- list(a=bat1_a_lin_start,b=bat1_b_lin_start,c=bat1_a_lin_start,d=bat1_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_norm_mean_sd_dist_lin_mle <- mle2(bat1_norm_mean_sd_dist_lin, start=bat1_norm_mean_sd_dist_lin_start)

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre a media
bat1_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*bat1_indice_dist/(b+bat1_indice_dist)
  -sum(dnorm(bat1_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat1_norm_mean_dist_mm_start <- list(a=bat1_a_mm_start,b=bat1_b_mm_start,sd=sd(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_norm_mean_dist_mm_mle <- mle2(bat1_norm_mean_dist_mm, start=bat1_norm_mean_dist_mm_start,method="Nelder-Mead")
######## ou:
bat1_norm_dist_mm_glm <- glm(bat1_indice_dist/bat1_indice_media~bat1_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre sd
bat1_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*bat1_indice_dist/(b+bat1_indice_dist))^2)
  -sum(dnorm(bat1_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat1_norm_sd_dist_mm_start <- list(a=bat1_a_mm_start,b=bat1_b_mm_start,mean=mean(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_norm_sd_dist_mm_mle <- mle2(bat1_norm_sd_dist_mm, start=bat1_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre mean e sd
bat1_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*bat1_indice_dist/(b+bat1_indice_dist)
  sd <- sqrt((c*bat1_indice_dist/(d+bat1_indice_dist))^2)
  -sum(dnorm(bat1_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat1_norm_mean_sd_dist_mm_start <- list(a=bat1_a_mm_start,b=bat1_b_mm_start,c=bat1_a_mm_start,d=bat1_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_norm_mean_sd_dist_mm_mle <- mle2(bat1_norm_mean_sd_dist_mm, start=bat1_norm_mean_sd_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre a media
bat1_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(bat1_indice_dist^b)
  -sum(dnorm(bat1_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat1_norm_mean_dist_pot_start <- list(a=bat1_a_pot_start,b=bat1_b_pot_start,sd=sd(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_norm_mean_dist_pot_mle <- mle2(bat1_norm_mean_dist_pot, start=bat1_norm_mean_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre sd
bat1_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(bat1_indice_dist^b))^2)
  -sum(dnorm(bat1_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat1_norm_sd_dist_pot_start <- list(a=bat1_a_pot_start,b=bat1_b_pot_start,mean=mean(bat1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_norm_sd_dist_pot_mle <- mle2(bat1_norm_sd_dist_pot, start=bat1_norm_sd_dist_pot_start)

## Com variavel preditora (disturbio) por meio da funcao potencia sobre mean e sd
bat1_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(bat1_indice_dist^b)
  sd <- sqrt((c*(bat1_indice_dist^d))^2)
  -sum(dnorm(bat1_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat1_norm_mean_sd_dist_pot_start <- list(a=bat1_a_pot_start,b=bat1_b_pot_start,c=bat1_a_pot_start,d=bat1_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat1_norm_mean_sd_dist_pot_mle <- mle2(bat1_norm_mean_sd_dist_pot, start=bat1_norm_mean_sd_dist_pot_start,method="Nelder-Mead")

################### SELECAO DE MODELOS ####################
AICtab(bat1_gama_nulo_mle,bat1_gama_nulo_glm,bat1_gama_scale_dist_lin_mle,bat1_gama_dist_lin_glm,bat1_gama_scale_dist_mm_mle,bat1_gama_dist_mm_glm,bat1_gama_scale_dist_pot_mle,bat1_gama_var_scale_dist_lin_mle,bat1_gama_var_scale_dist_mm_mle,bat1_gama_var_scale_dist_pot_mle,bat1_norm_nulo_mle,bat1_norm_nulo_glm,bat1_norm_mean_dist_lin_mle,bat1_norm_dist_lin_glm,bat1_norm_sd_dist_lin_mle,bat1_norm_mean_sd_dist_lin_mle,bat1_norm_mean_dist_mm_mle,bat1_norm_dist_mm_glm,bat1_norm_sd_dist_mm_mle,bat1_norm_mean_sd_dist_mm_mle,bat1_norm_mean_dist_pot_mle,bat1_norm_sd_dist_pot_mle,bat1_norm_mean_sd_dist_pot_mle)
### modelo selecionado: bat1_gama_scale_dist_lin_mle

##########################################################
######################## Bateria 2 #######################
##########################################################

#################### DADOS ORIGINAIS ######################
################# Transformacao dos dados #################
bat2_indice_dist <- dados3_25jul16[,2]*dados3_25jul16[,3]
bat2_indice_media <- (simulacoes_25jul16_output_derivado[,5]-1)/(20000-1)

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat2_lin <- lm(bat2_indice_media~bat2_indice_dist)
bat2_a_lin_start <- coef(bat2_lin)[[2]]
bat2_b_lin_start <- coef(bat2_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat2_mm_linearizada <- lm(bat2_indice_dist/bat2_indice_media~bat2_indice_dist) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
bat2_a_mm_start <- 1/(coef(bat2_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
bat2_b_mm_start <- (coef(bat2_mm_linearizada)[[1]])*(1/(coef(bat2_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat2_a_pot_start <- 0.0006
bat2_b_pot_start <- 0.6

# GAMA

## Nulo
bat2_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat2_gama_nulo_start <- list(shape=mean(bat2_indice_media)^2/var(bat2_indice_media),scale=var(bat2_indice_media)/mean(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat2_gama_nulo_mle <- mle2(bat2_gama_nulo, start=bat2_gama_nulo_start,method="Nelder-Mead")
###### ou:
bat2_gama_nulo_glm <- glm(bat2_indice_media~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat2_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat2_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat2_gama_shape_dist_lin_start <- list(a=bat2_a_lin_start,b=bat2_b_lin_start,scale=var(bat2_indice_media)/mean(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_gama_shape_dist_lin_mle <- mle2(bat2_gama_shape_dist_lin, start=bat2_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat2_indice_dist+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat2_gama_scale_dist_lin_start <- list(a=bat2_a_lin_start,b=bat2_b_lin_start,shape=mean(bat2_indice_media)^2/var(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_gama_scale_dist_lin_mle <- mle2(bat2_gama_scale_dist_lin, start=bat2_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
bat2_gama_dist_lin_glm <- glm(bat2_indice_media~bat2_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# bat2_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat2_indice_dist/(b+bat2_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat2_gama_shape_dist_mm_start <- list(a=bat2_a_mm_start,b=bat2_b_mm_start,scale=var(bat2_indice_media)/mean(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_gama_shape_dist_mm_mle <- mle2(bat2_gama_shape_dist_mm, start=bat2_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
bat2_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado <- (a*bat2_indice_dist/(b+bat2_indice_dist))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat2_gama_scale_dist_mm_start <- list(a=bat2_a_mm_start,b=bat2_b_mm_start,shape=mean(bat2_indice_media)^2/var(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_gama_scale_dist_mm_mle <- mle2(bat2_gama_scale_dist_mm, start=bat2_gama_scale_dist_mm_start,method="Nelder-Mead")
######## ou:
bat2_gama_dist_mm_glm <- glm(bat2_indice_dist/bat2_indice_media~bat2_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# bat2_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(bat2_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat2_gama_shape_dist_pot_start <- list(a=bat2_a_pot_start,b=bat2_b_pot_start,scale=var(bat2_indice_media)/mean(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_gama_shape_dist_pot_mle <- mle2(bat2_gama_shape_dist_pot, start=bat2_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
bat2_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado <- (a*(bat2_indice_dist^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat2_gama_scale_dist_pot_start <- list(a=bat2_a_pot_start,b=bat2_b_pot_start,shape=mean(bat2_indice_media)^2/var(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_gama_scale_dist_pot_mle <- mle2(bat2_gama_scale_dist_pot, start=bat2_gama_scale_dist_pot_start,method="Nelder-Mead")


### Sobre a variancia

# ## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat2_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat2_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat2_gama_var_shape_dist_lin_start <- list(a=bat2_a_lin_start,b=bat2_b_lin_start,scale=var(bat2_indice_media)/mean(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_gama_var_shape_dist_lin_mle <- mle2(bat2_gama_var_shape_dist_lin, start=bat2_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat2_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat2_indice_dist+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat2_gama_var_scale_dist_lin_start <- list(a=bat2_a_lin_start,b=bat2_b_lin_start,shape=mean(bat2_indice_media)^2/var(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_gama_var_scale_dist_lin_mle <- mle2(bat2_gama_var_scale_dist_lin, start=bat2_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# bat2_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat2_indice_dist/(b+bat2_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat2_gama_var_shape_dist_mm_start <- list(a=bat2_a_mm_start,b=bat2_b_mm_start,scale=var(bat2_indice_media)/mean(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_gama_var_shape_dist_mm_mle <- mle2(bat2_gama_var_shape_dist_mm, start=bat2_gama_var_shape_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
bat2_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado <- (a*bat2_indice_dist/(b+bat2_indice_dist))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat2_gama_var_scale_dist_mm_start <- list(a=bat2_a_mm_start,b=bat2_b_mm_start,shape=mean(bat2_indice_media)^2/var(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_gama_var_scale_dist_mm_mle <- mle2(bat2_gama_var_scale_dist_mm, start=bat2_gama_var_scale_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# bat2_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat2_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat2_gama_var_shape_dist_pot_start <- list(a=bat2_a_pot_start,b=bat2_b_pot_start,scale=var(bat2_indice_media)/mean(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat2_gama_var_shape_dist_pot_mle <- mle2(bat2_gama_var_shape_dist_pot, start=bat2_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
bat2_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado <- (a*(bat2_indice_dist^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat2_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat2_gama_var_scale_dist_pot_start <- list(a=bat2_a_pot_start,b=bat2_b_pot_start,shape=mean(bat2_indice_media)^2/var(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat2_gama_var_scale_dist_pot_mle <- mle2(bat2_gama_var_scale_dist_pot, start=bat2_gama_var_scale_dist_pot_start,method="Nelder-Mead")


# NORMAL

## Nulo
bat2_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(bat2_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat2_norm_nulo_start <- list(mean=mean(bat2_indice_media),sd=sd(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat2_norm_nulo_mle <- mle2(bat2_norm_nulo, start=bat2_norm_nulo_start,method="Nelder-Mead")
########## ou:
bat2_norm_nulo_glm <- glm(bat2_indice_media~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
bat2_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*bat2_indice_dist+b
  -sum(dnorm(bat2_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat2_norm_mean_dist_lin_start <- list(a=bat2_a_lin_start,b=bat2_b_lin_start,sd=sd(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_norm_mean_dist_lin_mle <- mle2(bat2_norm_mean_dist_lin, start=bat2_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
bat2_norm_dist_lin_glm <- glm(bat2_indice_media~bat2_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
bat2_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*bat2_indice_dist+b)^2)
  -sum(dnorm(bat2_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat2_norm_sd_dist_lin_start <- list(a=bat2_a_lin_start,b=bat2_b_lin_start,mean=mean(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_norm_sd_dist_lin_mle <- mle2(bat2_norm_sd_dist_lin, start=bat2_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre mean e sd
bat2_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*bat2_indice_dist+b
  sd <- sqrt((c*bat2_indice_dist+d)^2)
  -sum(dnorm(bat2_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat2_norm_mean_sd_dist_lin_start <- list(a=bat2_a_lin_start,b=bat2_b_lin_start,c=bat2_a_lin_start,d=bat2_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_norm_mean_sd_dist_lin_mle <- mle2(bat2_norm_mean_sd_dist_lin, start=bat2_norm_mean_sd_dist_lin_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre a media
bat2_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*bat2_indice_dist/(b+bat2_indice_dist)
  -sum(dnorm(bat2_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat2_norm_mean_dist_mm_start <- list(a=bat2_a_mm_start,b=bat2_b_mm_start,sd=sd(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_norm_mean_dist_mm_mle <- mle2(bat2_norm_mean_dist_mm, start=bat2_norm_mean_dist_mm_start)
######## ou:
bat2_norm_dist_mm_glm <- glm(bat2_indice_dist/bat2_indice_media~bat2_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre sd
bat2_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*bat2_indice_dist/(b+bat2_indice_dist))^2)
  -sum(dnorm(bat2_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat2_norm_sd_dist_mm_start <- list(a=bat2_a_mm_start,b=bat2_b_mm_start,mean=mean(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_norm_sd_dist_mm_mle <- mle2(bat2_norm_sd_dist_mm, start=bat2_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre mean e sd
bat2_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*bat2_indice_dist/(b+bat2_indice_dist)
  sd <- sqrt((c*bat2_indice_dist/(d+bat2_indice_dist))^2)
  -sum(dnorm(bat2_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat2_norm_mean_sd_dist_mm_start <- list(a=bat2_a_mm_start,b=bat2_b_mm_start,c=bat2_a_mm_start,d=bat2_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_norm_mean_sd_dist_mm_mle <- mle2(bat2_norm_mean_sd_dist_mm, start=bat2_norm_mean_sd_dist_mm_start,method="Nelder-Mead")


## Com variavel preditora (disturbio) por meio da funcao potencia sobre a media
bat2_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(bat2_indice_dist^b)
  -sum(dnorm(bat2_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat2_norm_mean_dist_pot_start <- list(a=bat2_a_pot_start,b=bat2_b_pot_start,sd=sd(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_norm_mean_dist_pot_mle <- mle2(bat2_norm_mean_dist_pot, start=bat2_norm_mean_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre sd
bat2_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(bat2_indice_dist^b))^2)
  -sum(dnorm(bat2_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat2_norm_sd_dist_pot_start <- list(a=bat2_a_pot_start,b=bat2_b_pot_start,mean=mean(bat2_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_norm_sd_dist_pot_mle <- mle2(bat2_norm_sd_dist_pot, start=bat2_norm_sd_dist_pot_start)

## Com variavel preditora (disturbio) por meio da funcao potencia sobre mean e sd
bat2_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(bat2_indice_dist^b)
  sd <- sqrt((c*(bat2_indice_dist^d))^2)
  -sum(dnorm(bat2_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat2_norm_mean_sd_dist_pot_start <- list(a=bat2_a_pot_start,b=bat2_b_pot_start,c=bat2_a_pot_start,d=bat2_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat2_norm_mean_sd_dist_pot_mle <- mle2(bat2_norm_mean_sd_dist_pot, start=bat2_norm_mean_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))


################### SELECAO DE MODELOS ####################
AICtab(bat2_gama_nulo_mle,bat2_gama_nulo_glm,bat2_gama_scale_dist_lin_mle,bat2_gama_dist_lin_glm,bat2_gama_scale_dist_mm_mle,bat2_gama_dist_mm_glm,bat2_gama_scale_dist_pot_mle,bat2_gama_var_scale_dist_lin_mle,bat2_gama_var_scale_dist_mm_mle,bat2_gama_var_scale_dist_pot_mle,bat2_norm_nulo_mle,bat2_norm_nulo_glm,bat2_norm_mean_dist_lin_mle,bat2_norm_dist_lin_glm,bat2_norm_sd_dist_lin_mle,bat2_norm_mean_sd_dist_lin_mle,bat2_norm_mean_dist_mm_mle,bat2_norm_dist_mm_glm,bat2_norm_sd_dist_mm_mle,bat2_norm_mean_sd_dist_mm_mle,bat2_norm_mean_dist_pot_mle,bat2_norm_sd_dist_pot_mle,bat2_norm_mean_sd_dist_pot_mle)
### modelo selecionado: bat2_norm_mean_sd_dist_pot_mle

##########################################################
######################## Bateria 3 #######################
##########################################################

#################### DADOS ORIGINAIS ######################
################# Transformacao dos dados #################
bat3_indice_dist <- dados3_27jul16[,2]*dados3_27jul16[,3]
bat3_indice_media <- (simulacoes_27jul16_output_derivado[,5]-1)/(20000-1)

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat3_lin <- lm(bat3_indice_media~bat3_indice_dist)
bat3_a_lin_start <- coef(bat3_lin)[[2]]
bat3_b_lin_start <- coef(bat3_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat3_mm_linearizada <- lm(bat3_indice_dist/bat3_indice_media~bat3_indice_dist) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
bat3_a_mm_start <- 1/(coef(bat3_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
bat3_b_mm_start <- (coef(bat3_mm_linearizada)[[1]])*(1/(coef(bat3_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
bat3_a_pot_start <- 0.0000015
bat3_b_pot_start <- 1


# GAMA

## Nulo
bat3_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat3_gama_nulo_start <- list(shape=mean(bat3_indice_media)^2/var(bat3_indice_media),scale=var(bat3_indice_media)/mean(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat3_gama_nulo_mle <- mle2(bat3_gama_nulo, start=bat3_gama_nulo_start,method="Nelder-Mead")
###### ou:
bat3_gama_nulo_glm <- glm(bat3_indice_media~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat3_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat3_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat3_gama_shape_dist_lin_start <- list(a=bat3_a_lin_start,b=bat3_b_lin_start,scale=var(bat3_indice_media)/mean(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_gama_shape_dist_lin_mle <- mle2(bat3_gama_shape_dist_lin, start=bat3_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*bat3_indice_dist+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat3_gama_scale_dist_lin_start <- list(a=bat3_a_lin_start,b=bat3_b_lin_start,shape=mean(bat3_indice_media)^2/var(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_gama_scale_dist_lin_mle <- mle2(bat3_gama_scale_dist_lin, start=bat3_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
bat3_gama_dist_lin_glm <- glm(bat3_indice_media~bat3_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# bat3_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*bat3_indice_dist/(b+bat3_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat3_gama_shape_dist_mm_start <- list(a=bat3_a_mm_start,b=bat3_b_mm_start,scale=var(bat3_indice_media)/mean(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_gama_shape_dist_mm_mle <- mle2(bat3_gama_shape_dist_mm, start=bat3_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
bat3_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado <- (a*bat3_indice_dist/(b+bat3_indice_dist))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat3_gama_scale_dist_mm_start <- list(a=bat3_a_mm_start,b=bat3_b_mm_start,shape=mean(bat3_indice_media)^2/var(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_gama_scale_dist_mm_mle <- mle2(bat3_gama_scale_dist_mm, start=bat3_gama_scale_dist_mm_start)
######## ou:
bat3_gama_dist_mm_glm <- glm(bat3_indice_dist/bat3_indice_media~bat3_indice_dist,family="Gamma"(link="identity"))

# ## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# bat3_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(bat3_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat3_gama_shape_dist_pot_start <- list(a=bat3_a_pot_start,b=bat3_b_pot_start,scale=var(bat3_indice_media)/mean(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_gama_shape_dist_pot_mle <- mle2(bat3_gama_shape_dist_pot, start=bat3_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
bat3_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado <- (a*(bat3_indice_dist^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat3_gama_scale_dist_pot_start <- list(a=bat3_a_pot_start,b=bat3_b_pot_start,shape=mean(bat3_indice_media)^2/var(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_gama_scale_dist_pot_mle <- mle2(bat3_gama_scale_dist_pot, start=bat3_gama_scale_dist_pot_start,method="Nelder-Mead")


### Sobre a variancia

# ## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# bat3_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat3_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat3_gama_var_shape_dist_lin_start <- list(a=bat3_a_lin_start,b=bat3_b_lin_start,scale=var(bat3_indice_media)/mean(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_gama_var_shape_dist_lin_mle <- mle2(bat3_gama_var_shape_dist_lin, start=bat3_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
bat3_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*bat3_indice_dist+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat3_gama_var_scale_dist_lin_start <- list(a=bat3_a_lin_start,b=bat3_b_lin_start,shape=mean(bat3_indice_media)^2/var(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_gama_var_scale_dist_lin_mle <- mle2(bat3_gama_var_scale_dist_lin, start=bat3_gama_var_scale_dist_lin_start)

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# bat3_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*bat3_indice_dist/(b+bat3_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat3_gama_var_shape_dist_mm_start <- list(a=bat3_a_mm_start,b=bat3_b_mm_start,scale=var(bat3_indice_media)/mean(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_gama_var_shape_dist_mm_mle <- mle2(bat3_gama_var_shape_dist_mm, start=bat3_gama_var_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
bat3_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado <- (a*bat3_indice_dist/(b+bat3_indice_dist))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat3_gama_var_scale_dist_mm_start <- list(a=bat3_a_mm_start,b=bat3_b_mm_start,shape=mean(bat3_indice_media)^2/var(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_gama_var_scale_dist_mm_mle <- mle2(bat3_gama_var_scale_dist_mm, start=bat3_gama_var_scale_dist_mm_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# bat3_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(bat3_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# bat3_gama_var_shape_dist_pot_start <- list(a=bat3_a_pot_start,b=bat3_b_pot_start,scale=var(bat3_indice_media)/mean(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# bat3_gama_var_shape_dist_pot_mle <- mle2(bat3_gama_var_shape_dist_pot, start=bat3_gama_var_shape_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
bat3_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado <- (a*(bat3_indice_dist^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(bat3_indice_media, shape=shape, scale=scale, log=TRUE))
}
bat3_gama_var_scale_dist_pot_start <- list(a=bat3_a_pot_start,b=bat3_b_pot_start,shape=mean(bat3_indice_media)^2/var(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
bat3_gama_var_scale_dist_pot_mle <- mle2(bat3_gama_var_scale_dist_pot, start=bat3_gama_var_scale_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))


# NORMAL

## Nulo
bat3_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(bat3_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat3_norm_nulo_start <- list(mean=mean(bat3_indice_media),sd=sd(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
bat3_norm_nulo_mle <- mle2(bat3_norm_nulo, start=bat3_norm_nulo_start,method="Nelder-Mead")
########## ou:
bat3_norm_nulo_glm <- glm(bat3_indice_media~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
bat3_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*bat3_indice_dist+b
  -sum(dnorm(bat3_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat3_norm_mean_dist_lin_start <- list(a=bat3_a_lin_start,b=bat3_b_lin_start,sd=sd(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_norm_mean_dist_lin_mle <- mle2(bat3_norm_mean_dist_lin, start=bat3_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
bat3_norm_dist_lin_glm <- glm(bat3_indice_media~bat3_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
bat3_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*bat3_indice_dist+b)^2)
  -sum(dnorm(bat3_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat3_norm_sd_dist_lin_start <- list(a=bat3_a_lin_start,b=bat3_b_lin_start,mean=mean(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_norm_sd_dist_lin_mle <- mle2(bat3_norm_sd_dist_lin, start=bat3_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre mean e sd
bat3_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*bat3_indice_dist+b
  sd <- sqrt((c*bat3_indice_dist+d)^2)
  -sum(dnorm(bat3_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat3_norm_mean_sd_dist_lin_start <- list(a=bat3_a_lin_start,b=bat3_b_lin_start,c=bat3_a_lin_start,d=bat3_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_norm_mean_sd_dist_lin_mle <- mle2(bat3_norm_mean_sd_dist_lin, start=bat3_norm_mean_sd_dist_lin_start)

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre a media
bat3_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*bat3_indice_dist/(b+bat3_indice_dist)
  -sum(dnorm(bat3_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat3_norm_mean_dist_mm_start <- list(a=bat3_a_mm_start,b=bat3_b_mm_start,sd=sd(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_norm_mean_dist_mm_mle <- mle2(bat3_norm_mean_dist_mm, start=bat3_norm_mean_dist_mm_start,method="Nelder-Mead")
######## ou:
bat3_norm_dist_mm_glm <- glm(bat3_indice_dist/bat3_indice_media~bat3_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre sd
bat3_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*bat3_indice_dist/(b+bat3_indice_dist))^2)
  -sum(dnorm(bat3_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat3_norm_sd_dist_mm_start <- list(a=bat3_a_mm_start,b=bat3_b_mm_start,mean=mean(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_norm_sd_dist_mm_mle <- mle2(bat3_norm_sd_dist_mm, start=bat3_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre mean e sd
bat3_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*bat3_indice_dist/(b+bat3_indice_dist)
  sd <- sqrt((c*bat3_indice_dist/(d+bat3_indice_dist))^2)
  -sum(dnorm(bat3_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat3_norm_mean_sd_dist_mm_start <- list(a=bat3_a_mm_start,b=bat3_b_mm_start,c=bat3_a_mm_start,d=bat3_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_norm_mean_sd_dist_mm_mle <- mle2(bat3_norm_mean_sd_dist_mm, start=bat3_norm_mean_sd_dist_mm_start,control=list(maxit=1000),method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre a media
bat3_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(bat3_indice_dist^b)
  -sum(dnorm(bat3_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat3_norm_mean_dist_pot_start <- list(a=bat3_a_pot_start,b=bat3_b_pot_start,sd=sd(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_norm_mean_dist_pot_mle <- mle2(bat3_norm_mean_dist_pot, start=bat3_norm_mean_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre sd
bat3_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(bat3_indice_dist^b))^2)
  -sum(dnorm(bat3_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat3_norm_sd_dist_pot_start <- list(a=bat3_a_pot_start,b=bat3_b_pot_start,mean=mean(bat3_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_norm_sd_dist_pot_mle <- mle2(bat3_norm_sd_dist_pot, start=bat3_norm_sd_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre mean e sd
bat3_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(bat3_indice_dist^b)
  sd <- sqrt((c*(bat3_indice_dist^d))^2)
  -sum(dnorm(bat3_indice_media, mean=mean, sd=sd, log=TRUE))
}
bat3_norm_mean_sd_dist_pot_start <- list(a=bat3_a_pot_start,b=bat3_b_pot_start,c=bat3_a_pot_start,d=bat3_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
bat3_norm_mean_sd_dist_pot_mle <- mle2(bat3_norm_mean_sd_dist_pot, start=bat3_norm_mean_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

################### SELECAO DE MODELOS ####################
AICtab(bat3_gama_nulo_mle,bat3_gama_nulo_glm,bat3_gama_scale_dist_lin_mle,bat3_gama_dist_lin_glm,bat3_gama_scale_dist_mm_mle,bat3_gama_dist_mm_glm,bat3_gama_scale_dist_pot_mle,bat3_gama_var_scale_dist_lin_mle,bat3_gama_var_scale_dist_mm_mle,bat3_gama_var_scale_dist_pot_mle,bat3_norm_nulo_mle,bat3_norm_nulo_glm,bat3_norm_mean_dist_lin_mle,bat3_norm_dist_lin_glm,bat3_norm_sd_dist_lin_mle,bat3_norm_mean_sd_dist_lin_mle,bat3_norm_mean_dist_mm_mle,bat3_norm_dist_mm_glm,bat3_norm_sd_dist_mm_mle,bat3_norm_mean_sd_dist_mm_mle,bat3_norm_mean_dist_pot_mle,bat3_norm_sd_dist_pot_mle,bat3_norm_mean_sd_dist_pot_mle)
### modelo selecionado: bat3_gama_scale_dist_lin_mle

##########################################################
############# CALCULO DO AIC INDIVIDUAL TOTAL ############
##########################################################
aic_mle_soma_individual <- AIC(bat1_gama_scale_dist_lin_mle) + AIC(bat2_norm_mean_sd_dist_pot_mle) + AIC(bat3_gama_scale_dist_lin_mle) #-11597.2




##########################################################
##########################################################
### Analises das baterias de simulacao EM SUBCONJUNTOS ###
###################### por MUTACAO #######################
##########################################################
##########################################################

##########################################################
######################## MUTACAO 0 #######################
##########################################################

mut0_indice_media <- bat2_indice_media
mut0_indice_dist <- bat2_indice_dist

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut0_lin <- lm(mut0_indice_media~mut0_indice_dist)
mut0_a_lin_start <- coef(mut0_lin)[[2]]
mut0_b_lin_start <- coef(mut0_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut0_mm_linearizada <- lm(mut0_indice_dist/mut0_indice_media~mut0_indice_dist) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
mut0_a_mm_start <- 1/(coef(mut0_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
mut0_b_mm_start <- (coef(mut0_mm_linearizada)[[1]])*(1/(coef(mut0_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut0_a_pot_start <- 0.0006
mut0_b_pot_start <- 0.6


# GAMA

## Nulo
mut0_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut0_gama_nulo_start <- list(shape=mean(mut0_indice_media)^2/var(mut0_indice_media),scale=var(mut0_indice_media)/mean(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
mut0_gama_nulo_mle <- mle2(mut0_gama_nulo, start=mut0_gama_nulo_start,method="Nelder-Mead")
###### ou:
mut0_gama_nulo_glm <- glm(mut0_indice_media~1,family="Gamma"(link="identity"))

### Sobre a esperanca

# ## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut0_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*mut0_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut0_gama_shape_dist_lin_start <- list(a=mut0_a_lin_start,b=mut0_b_lin_start,scale=var(mut0_indice_media)/mean(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_gama_shape_dist_lin_mle <- mle2(mut0_gama_shape_dist_lin, start=mut0_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut0_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*mut0_indice_dist+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut0_gama_scale_dist_lin_start <- list(a=mut0_a_lin_start,b=mut0_b_lin_start,shape=mean(mut0_indice_media)^2/var(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_gama_scale_dist_lin_mle <- mle2(mut0_gama_scale_dist_lin, start=mut0_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
mut0_gama_dist_lin_glm <- glm(mut0_indice_media~mut0_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# mut0_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*mut0_indice_dist/(b+mut0_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut0_gama_shape_dist_mm_start <- list(a=mut0_a_mm_start,b=mut0_b_mm_start,scale=var(mut0_indice_media)/mean(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_gama_shape_dist_mm_mle <- mle2(mut0_gama_shape_dist_mm, start=mut0_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
mut0_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado <- (a*mut0_indice_dist/(b+mut0_indice_dist))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut0_gama_scale_dist_mm_start <- list(a=mut0_a_mm_start,b=mut0_b_mm_start,shape=mean(mut0_indice_media)^2/var(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_gama_scale_dist_mm_mle <- mle2(mut0_gama_scale_dist_mm, start=mut0_gama_scale_dist_mm_start,method="Nelder-Mead")
######## ou:
mut0_gama_dist_mm_glm <- glm(mut0_indice_dist/mut0_indice_media~mut0_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# mut0_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(mut0_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut0_gama_shape_dist_pot_start <- list(a=mut0_a_pot_start,b=mut0_b_pot_start,scale=var(mut0_indice_media)/mean(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_gama_shape_dist_pot_mle <- mle2(mut0_gama_shape_dist_pot, start=mut0_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
mut0_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado <- (a*(mut0_indice_dist^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut0_gama_scale_dist_pot_start <- list(a=mut0_a_pot_start,b=mut0_b_pot_start,shape=mean(mut0_indice_media)^2/var(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_gama_scale_dist_pot_mle <- mle2(mut0_gama_scale_dist_pot, start=mut0_gama_scale_dist_pot_start,method="Nelder-Mead")


### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut0_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*mut0_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut0_gama_var_shape_dist_lin_start <- list(a=mut0_a_lin_start,b=mut0_b_lin_start,scale=var(mut0_indice_media)/mean(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_gama_var_shape_dist_lin_mle <- mle2(mut0_gama_var_shape_dist_lin, start=mut0_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut0_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*mut0_indice_dist+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut0_gama_var_scale_dist_lin_start <- list(a=mut0_a_lin_start,b=mut0_b_lin_start,shape=mean(mut0_indice_media)^2/var(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_gama_var_scale_dist_lin_mle <- mle2(mut0_gama_var_scale_dist_lin, start=mut0_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# mut0_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*mut0_indice_dist/(b+mut0_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut0_gama_var_shape_dist_mm_start <- list(a=mut0_a_mm_start,b=mut0_b_mm_start,scale=var(mut0_indice_media)/mean(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_gama_var_shape_dist_mm_mle <- mle2(mut0_gama_var_shape_dist_mm, start=mut0_gama_var_shape_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
mut0_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado <- (a*mut0_indice_dist/(b+mut0_indice_dist))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut0_gama_var_scale_dist_mm_start <- list(a=mut0_a_mm_start,b=mut0_b_mm_start,shape=mean(mut0_indice_media)^2/var(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_gama_var_scale_dist_mm_mle <- mle2(mut0_gama_var_scale_dist_mm, start=mut0_gama_var_scale_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# mut0_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(mut0_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut0_gama_var_shape_dist_pot_start <- list(a=mut0_a_pot_start,b=mut0_b_pot_start,scale=var(mut0_indice_media)/mean(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut0_gama_var_shape_dist_pot_mle <- mle2(mut0_gama_var_shape_dist_pot, start=mut0_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
mut0_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado <- (a*(mut0_indice_dist^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut0_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut0_gama_var_scale_dist_pot_start <- list(a=mut0_a_pot_start,b=mut0_b_pot_start,shape=mean(mut0_indice_media)^2/var(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut0_gama_var_scale_dist_pot_mle <- mle2(mut0_gama_var_scale_dist_pot, start=mut0_gama_var_scale_dist_pot_start,method="Nelder-Mead")


# NORMAL

## Nulo
mut0_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(mut0_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut0_norm_nulo_start <- list(mean=mean(mut0_indice_media),sd=sd(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
mut0_norm_nulo_mle <- mle2(mut0_norm_nulo, start=mut0_norm_nulo_start,method="Nelder-Mead")
########## ou:
mut0_norm_nulo_glm <- glm(mut0_indice_media~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
mut0_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*mut0_indice_dist+b
  -sum(dnorm(mut0_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut0_norm_mean_dist_lin_start <- list(a=mut0_a_lin_start,b=mut0_b_lin_start,sd=sd(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_norm_mean_dist_lin_mle <- mle2(mut0_norm_mean_dist_lin, start=mut0_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
mut0_norm_dist_lin_glm <- glm(mut0_indice_media~mut0_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
mut0_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*mut0_indice_dist+b)^2)
  -sum(dnorm(mut0_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut0_norm_sd_dist_lin_start <- list(a=mut0_a_lin_start,b=mut0_b_lin_start,mean=mean(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_norm_sd_dist_lin_mle <- mle2(mut0_norm_sd_dist_lin, start=mut0_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre mean e sd
mut0_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*mut0_indice_dist+b
  sd <- sqrt((c*mut0_indice_dist+d)^2)
  -sum(dnorm(mut0_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut0_norm_mean_sd_dist_lin_start <- list(a=mut0_a_lin_start,b=mut0_b_lin_start,c=mut0_a_lin_start,d=mut0_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_norm_mean_sd_dist_lin_mle <- mle2(mut0_norm_mean_sd_dist_lin, start=mut0_norm_mean_sd_dist_lin_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre a media
mut0_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*mut0_indice_dist/(b+mut0_indice_dist)
  -sum(dnorm(mut0_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut0_norm_mean_dist_mm_start <- list(a=mut0_a_mm_start,b=mut0_b_mm_start,sd=sd(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_norm_mean_dist_mm_mle <- mle2(mut0_norm_mean_dist_mm, start=mut0_norm_mean_dist_mm_start)
######## ou:
mut0_norm_dist_mm_glm <- glm(mut0_indice_dist/mut0_indice_media~mut0_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre sd
mut0_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*mut0_indice_dist/(b+mut0_indice_dist))^2)
  -sum(dnorm(mut0_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut0_norm_sd_dist_mm_start <- list(a=mut0_a_mm_start,b=mut0_b_mm_start,mean=mean(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_norm_sd_dist_mm_mle <- mle2(mut0_norm_sd_dist_mm, start=mut0_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre mean e sd
mut0_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*mut0_indice_dist/(b+mut0_indice_dist)
  sd <- sqrt((c*mut0_indice_dist/(d+mut0_indice_dist))^2)
  -sum(dnorm(mut0_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut0_norm_mean_sd_dist_mm_start <- list(a=mut0_a_mm_start,b=mut0_b_mm_start,c=mut0_a_mm_start,d=mut0_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_norm_mean_sd_dist_mm_mle <- mle2(mut0_norm_mean_sd_dist_mm, start=mut0_norm_mean_sd_dist_mm_start,method="Nelder-Mead")


## Com variavel preditora (disturbio) por meio da funcao potencia sobre a media
mut0_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(mut0_indice_dist^b)
  -sum(dnorm(mut0_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut0_norm_mean_dist_pot_start <- list(a=mut0_a_pot_start,b=mut0_b_pot_start,sd=sd(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_norm_mean_dist_pot_mle <- mle2(mut0_norm_mean_dist_pot, start=mut0_norm_mean_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre sd
mut0_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(mut0_indice_dist^b))^2)
  -sum(dnorm(mut0_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut0_norm_sd_dist_pot_start <- list(a=mut0_a_pot_start,b=mut0_b_pot_start,mean=mean(mut0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_norm_sd_dist_pot_mle <- mle2(mut0_norm_sd_dist_pot, start=mut0_norm_sd_dist_pot_start)

## Com variavel preditora (disturbio) por meio da funcao potencia sobre mean e sd
mut0_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(mut0_indice_dist^b)
  sd <- sqrt((c*(mut0_indice_dist^d))^2)
  -sum(dnorm(mut0_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut0_norm_mean_sd_dist_pot_start <- list(a=mut0_a_pot_start,b=mut0_b_pot_start,c=mut0_a_pot_start,d=mut0_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut0_norm_mean_sd_dist_pot_mle <- mle2(mut0_norm_mean_sd_dist_pot, start=mut0_norm_mean_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))


################### SELECAO DE MODELOS ####################
AICtab(mut0_gama_nulo_mle,mut0_gama_nulo_glm,mut0_gama_scale_dist_lin_mle,mut0_gama_dist_lin_glm,mut0_gama_scale_dist_mm_mle,mut0_gama_dist_mm_glm,mut0_gama_scale_dist_pot_mle,mut0_gama_var_scale_dist_lin_mle,mut0_gama_var_scale_dist_mm_mle,mut0_gama_var_scale_dist_pot_mle,mut0_norm_nulo_mle,mut0_norm_nulo_glm,mut0_norm_mean_dist_lin_mle,mut0_norm_dist_lin_glm,mut0_norm_sd_dist_lin_mle,mut0_norm_mean_sd_dist_lin_mle,mut0_norm_mean_dist_mm_mle,mut0_norm_dist_mm_glm,mut0_norm_sd_dist_mm_mle,mut0_norm_mean_sd_dist_mm_mle,mut0_norm_mean_dist_pot_mle,mut0_norm_sd_dist_pot_mle,mut0_norm_mean_sd_dist_pot_mle)
### modelo selecionado: mut0_norm_mean_sd_dist_pot_mle

##########################################################
######################## MUTACAO 1 #######################
##########################################################

mut1_indice_media <- c(bat1_indice_media,bat3_indice_media)
mut1_indice_dist <- c(bat1_indice_dist,bat3_indice_dist)

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut1_lin <- lm(mut1_indice_media~mut1_indice_dist)
mut1_a_lin_start <- coef(mut1_lin)[[2]]
mut1_b_lin_start <- coef(mut1_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut1_mm_linearizada <- lm(mut1_indice_dist/mut1_indice_media~mut1_indice_dist) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
mut1_a_mm_start <- 1/(coef(mut1_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
mut1_b_mm_start <- (coef(mut1_mm_linearizada)[[1]])*(1/(coef(mut1_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
mut1_a_pot_start <- 0.0000015
mut1_b_pot_start <- 1


# GAMA

## Nulo
mut1_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut1_gama_nulo_start <- list(shape=mean(mut1_indice_media)^2/var(mut1_indice_media),scale=var(mut1_indice_media)/mean(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
mut1_gama_nulo_mle <- mle2(mut1_gama_nulo, start=mut1_gama_nulo_start,method="Nelder-Mead")
###### ou:
mut1_gama_nulo_glm <- glm(mut1_indice_media~1,family="Gamma"(link="identity"))

### Sobre a esperanca

# ## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut1_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*mut1_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut1_gama_shape_dist_lin_start <- list(a=mut1_a_lin_start,b=mut1_b_lin_start,scale=var(mut1_indice_media)/mean(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_gama_shape_dist_lin_mle <- mle2(mut1_gama_shape_dist_lin, start=mut1_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut1_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*mut1_indice_dist+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut1_gama_scale_dist_lin_start <- list(a=mut1_a_lin_start,b=mut1_b_lin_start,shape=mean(mut1_indice_media)^2/var(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_gama_scale_dist_lin_mle <- mle2(mut1_gama_scale_dist_lin, start=mut1_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
mut1_gama_dist_lin_glm <- glm(mut1_indice_media~mut1_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# mut1_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*mut1_indice_dist/(b+mut1_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut1_gama_shape_dist_mm_start <- list(a=mut1_a_mm_start,b=mut1_b_mm_start,scale=var(mut1_indice_media)/mean(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_gama_shape_dist_mm_mle <- mle2(mut1_gama_shape_dist_mm, start=mut1_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
mut1_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado <- (a*mut1_indice_dist/(b+mut1_indice_dist))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut1_gama_scale_dist_mm_start <- list(a=mut1_a_mm_start,b=mut1_b_mm_start,shape=mean(mut1_indice_media)^2/var(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_gama_scale_dist_mm_mle <- mle2(mut1_gama_scale_dist_mm, start=mut1_gama_scale_dist_mm_start,method="Nelder-Mead",control=list(maxit=1000))
######## ou:
mut1_gama_dist_mm_glm <- glm(mut1_indice_dist/mut1_indice_media~mut1_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# mut1_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(mut1_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut1_gama_shape_dist_pot_start <- list(a=mut1_a_pot_start,b=mut1_b_pot_start,scale=var(mut1_indice_media)/mean(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_gama_shape_dist_pot_mle <- mle2(mut1_gama_shape_dist_pot, start=mut1_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
mut1_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado <- (a*(mut1_indice_dist^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut1_gama_scale_dist_pot_start <- list(a=mut1_a_pot_start,b=mut1_b_pot_start,shape=mean(mut1_indice_media)^2/var(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_gama_scale_dist_pot_mle <- mle2(mut1_gama_scale_dist_pot, start=mut1_gama_scale_dist_pot_start,method="Nelder-Mead")

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# mut1_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*mut1_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut1_gama_var_shape_dist_lin_start <- list(a=mut1_a_lin_start,b=mut1_b_lin_start,scale=var(mut1_indice_media)/mean(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_gama_var_shape_dist_lin_mle <- mle2(mut1_gama_var_shape_dist_lin, start=mut1_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
mut1_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*mut1_indice_dist+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut1_gama_var_scale_dist_lin_start <- list(a=mut1_a_lin_start,b=mut1_b_lin_start,shape=mean(mut1_indice_media)^2/var(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_gama_var_scale_dist_lin_mle <- mle2(mut1_gama_var_scale_dist_lin, start=mut1_gama_var_scale_dist_lin_start)

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# mut1_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*mut1_indice_dist/(b+mut1_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut1_gama_var_shape_dist_mm_start <- list(a=mut1_a_mm_start,b=mut1_b_mm_start,scale=var(mut1_indice_media)/mean(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_gama_var_shape_dist_mm_mle <- mle2(mut1_gama_var_shape_dist_mm, start=mut1_gama_var_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
mut1_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado <- (a*mut1_indice_dist/(b+mut1_indice_dist))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut1_gama_var_scale_dist_mm_start <- list(a=mut1_a_mm_start,b=mut1_b_mm_start,shape=mean(mut1_indice_media)^2/var(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_gama_var_scale_dist_mm_mle <- mle2(mut1_gama_var_scale_dist_mm, start=mut1_gama_var_scale_dist_mm_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# mut1_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(mut1_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# mut1_gama_var_shape_dist_pot_start <- list(a=mut1_a_pot_start,b=mut1_b_pot_start,scale=var(mut1_indice_media)/mean(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# mut1_gama_var_shape_dist_pot_mle <- mle2(mut1_gama_var_shape_dist_pot, start=mut1_gama_var_shape_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
mut1_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado <- (a*(mut1_indice_dist^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(mut1_indice_media, shape=shape, scale=scale, log=TRUE))
}
mut1_gama_var_scale_dist_pot_start <- list(a=mut1_a_pot_start,b=mut1_b_pot_start,shape=mean(mut1_indice_media)^2/var(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
mut1_gama_var_scale_dist_pot_mle <- mle2(mut1_gama_var_scale_dist_pot, start=mut1_gama_var_scale_dist_pot_start,method="Nelder-Mead")


# NORMAL

## Nulo
mut1_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(mut1_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut1_norm_nulo_start <- list(mean=mean(mut1_indice_media),sd=sd(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
mut1_norm_nulo_mle <- mle2(mut1_norm_nulo, start=mut1_norm_nulo_start,method="Nelder-Mead")
########## ou:
mut1_norm_nulo_glm <- glm(mut1_indice_media~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
mut1_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*mut1_indice_dist+b
  -sum(dnorm(mut1_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut1_norm_mean_dist_lin_start <- list(a=mut1_a_lin_start,b=mut1_b_lin_start,sd=sd(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_norm_mean_dist_lin_mle <- mle2(mut1_norm_mean_dist_lin, start=mut1_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
mut1_norm_dist_lin_glm <- glm(mut1_indice_media~mut1_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
mut1_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*mut1_indice_dist+b)^2)
  -sum(dnorm(mut1_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut1_norm_sd_dist_lin_start <- list(a=mut1_a_lin_start,b=mut1_b_lin_start,mean=mean(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_norm_sd_dist_lin_mle <- mle2(mut1_norm_sd_dist_lin, start=mut1_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre mean e sd
mut1_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*mut1_indice_dist+b
  sd <- sqrt((c*mut1_indice_dist+d)^2)
  -sum(dnorm(mut1_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut1_norm_mean_sd_dist_lin_start <- list(a=mut1_a_lin_start,b=mut1_b_lin_start,c=mut1_a_lin_start,d=mut1_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_norm_mean_sd_dist_lin_mle <- mle2(mut1_norm_mean_sd_dist_lin, start=mut1_norm_mean_sd_dist_lin_start)

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre a media
mut1_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*mut1_indice_dist/(b+mut1_indice_dist)
  -sum(dnorm(mut1_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut1_norm_mean_dist_mm_start <- list(a=mut1_a_mm_start,b=mut1_b_mm_start,sd=sd(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_norm_mean_dist_mm_mle <- mle2(mut1_norm_mean_dist_mm, start=mut1_norm_mean_dist_mm_start,method="Nelder-Mead")
######## ou:
mut1_norm_dist_mm_glm <- glm(mut1_indice_dist/mut1_indice_media~mut1_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre sd
mut1_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*mut1_indice_dist/(b+mut1_indice_dist))^2)
  -sum(dnorm(mut1_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut1_norm_sd_dist_mm_start <- list(a=mut1_a_mm_start,b=mut1_b_mm_start,mean=mean(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_norm_sd_dist_mm_mle <- mle2(mut1_norm_sd_dist_mm, start=mut1_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre mean e sd
mut1_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*mut1_indice_dist/(b+mut1_indice_dist)
  sd <- sqrt((c*mut1_indice_dist/(d+mut1_indice_dist))^2)
  -sum(dnorm(mut1_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut1_norm_mean_sd_dist_mm_start <- list(a=mut1_a_mm_start,b=mut1_b_mm_start,c=mut1_a_mm_start,d=mut1_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_norm_mean_sd_dist_mm_mle <- mle2(mut1_norm_mean_sd_dist_mm, start=mut1_norm_mean_sd_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre a media
mut1_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(mut1_indice_dist^b)
  -sum(dnorm(mut1_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut1_norm_mean_dist_pot_start <- list(a=mut1_a_pot_start,b=mut1_b_pot_start,sd=sd(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_norm_mean_dist_pot_mle <- mle2(mut1_norm_mean_dist_pot, start=mut1_norm_mean_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre sd
mut1_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(mut1_indice_dist^b))^2)
  -sum(dnorm(mut1_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut1_norm_sd_dist_pot_start <- list(a=mut1_a_pot_start,b=mut1_b_pot_start,mean=mean(mut1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_norm_sd_dist_pot_mle <- mle2(mut1_norm_sd_dist_pot, start=mut1_norm_sd_dist_pot_start)

## Com variavel preditora (disturbio) por meio da funcao potencia sobre mean e sd
mut1_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(mut1_indice_dist^b)
  sd <- sqrt((c*(mut1_indice_dist^d))^2)
  -sum(dnorm(mut1_indice_media, mean=mean, sd=sd, log=TRUE))
}
mut1_norm_mean_sd_dist_pot_start <- list(a=mut1_a_pot_start,b=mut1_b_pot_start,c=mut1_a_pot_start,d=mut1_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
mut1_norm_mean_sd_dist_pot_mle <- mle2(mut1_norm_mean_sd_dist_pot, start=mut1_norm_mean_sd_dist_pot_start,control=list(maxit=1000))

################### SELECAO DE MODELOS ####################
AICtab(mut1_gama_nulo_mle,mut1_gama_nulo_glm,mut1_gama_scale_dist_lin_mle,mut1_gama_dist_lin_glm,mut1_gama_scale_dist_mm_mle,mut1_gama_dist_mm_glm,mut1_gama_scale_dist_pot_mle,mut1_gama_var_scale_dist_lin_mle,mut1_gama_var_scale_dist_mm_mle,mut1_gama_var_scale_dist_pot_mle,mut1_norm_nulo_mle,mut1_norm_nulo_glm,mut1_norm_mean_dist_lin_mle,mut1_norm_dist_lin_glm,mut1_norm_sd_dist_lin_mle,mut1_norm_mean_sd_dist_lin_mle,mut1_norm_mean_dist_mm_mle,mut1_norm_dist_mm_glm,mut1_norm_sd_dist_mm_mle,mut1_norm_mean_sd_dist_mm_mle,mut1_norm_mean_dist_pot_mle,mut1_norm_sd_dist_pot_mle,mut1_norm_mean_sd_dist_pot_mle)
### modelo selecionado: mut1_gama_scale_dist_lin_mle

##########################################################
######## CALCULO DO AIC SUBCONJUNTO MUTACAO TOTAL ########
##########################################################
aic_mle_soma_sub_mutacao <- AIC(mut0_norm_mean_sd_dist_pot_mle) + AIC(mut1_gama_scale_dist_lin_mle) #-11602.11



##########################################################
##########################################################
### Analises das baterias de simulacao EM SUBCONJUNTOS ###
###################### por RIQUEZA #######################
##########################################################
##########################################################

##########################################################
######################## RIQUEZA 0 #######################
##########################################################

riq0_indice_media <- bat1_indice_media
riq0_indice_dist <- bat1_indice_dist

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq0_lin <- lm(riq0_indice_media~riq0_indice_dist)
riq0_a_lin_start <- coef(riq0_lin)[[2]]
riq0_b_lin_start <- coef(riq0_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq0_mm_linearizada <- lm(riq0_indice_dist/riq0_indice_media~riq0_indice_dist) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
riq0_a_mm_start <- 1/(coef(riq0_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
riq0_b_mm_start <- (coef(riq0_mm_linearizada)[[1]])*(1/(coef(riq0_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao potencia usada para expressar a relacao entre o disturbio e a esperanca/variancia de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq0_a_pot_start <- 0.0000015
riq0_b_pot_start <- 1

# GAMA

## Nulo
riq0_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq0_gama_nulo_start <- list(shape=mean(riq0_indice_media)^2/var(riq0_indice_media),scale=var(riq0_indice_media)/mean(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
riq0_gama_nulo_mle <- mle2(riq0_gama_nulo, start=riq0_gama_nulo_start,method="Nelder-Mead")
###### ou:
riq0_gama_nulo_glm <- glm(riq0_indice_media~1,family="Gamma"(link="identity"))

### Sobre a esperanca

# ## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq0_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*riq0_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq0_gama_shape_dist_lin_start <- list(a=riq0_a_lin_start,b=riq0_b_lin_start,scale=var(riq0_indice_media)/mean(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_gama_shape_dist_lin_mle <- mle2(riq0_gama_shape_dist_lin, start=riq0_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq0_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*riq0_indice_dist+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq0_gama_scale_dist_lin_start <- list(a=riq0_a_lin_start,b=riq0_b_lin_start,shape=mean(riq0_indice_media)^2/var(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_gama_scale_dist_lin_mle <- mle2(riq0_gama_scale_dist_lin, start=riq0_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
riq0_gama_dist_lin_glm <- glm(riq0_indice_media~riq0_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# riq0_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*riq0_indice_dist/(b+riq0_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq0_gama_shape_dist_mm_start <- list(a=riq0_a_mm_start,b=riq0_b_mm_start,scale=var(riq0_indice_media)/mean(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_gama_shape_dist_mm_mle <- mle2(riq0_gama_shape_dist_mm, start=riq0_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
riq0_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado <- (a*riq0_indice_dist/(b+riq0_indice_dist))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq0_gama_scale_dist_mm_start <- list(a=riq0_a_mm_start,b=riq0_b_mm_start,shape=mean(riq0_indice_media)^2/var(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_gama_scale_dist_mm_mle <- mle2(riq0_gama_scale_dist_mm, start=riq0_gama_scale_dist_mm_start)
######## ou:
riq0_gama_dist_mm_glm <- glm(riq0_indice_dist/riq0_indice_media~riq0_indice_dist,family="Gamma"(link="identity"))


# riq0_gama_dist_mm_nls <- nls(riq0_indice_media ~ a*riq0_indice_dist/(b+riq0_indice_dist),start=list(a=coef(riq0_mm_linearizada)[[2]],b=coef(riq0_mm_linearizada)[[1]]))
# riq0_gama_dist_mm_gnm <- gnm(riq0_indice_media ~ a*riq0_indice_dist/(b+riq0_indice_dist),family="Gamma"(link="identity"))
# MM <- function(resp, conc){
#   list(predictors = list(a = substitute(conc), b = 1),
#        variables = list(substitute(resp), substitute(conc)),
#        term = function(predictors, variables) {
#          pred <- paste("(", predictors[1],"*",variables[2], "/(", predictors[2]," + ", variables[2], "))", sep = "")
#          pred <- paste("(", variables[1], " - ", pred, ")", sep = "")
#        })
# }
# class(MM) <- "nonlin"
# opa <- gnm( ~ -1+MM(riq0_indice_media, riq0_indice_dist),start = c(a=riq0_a_mm_start, b=riq0_b_mm_start), verbose = FALSE,family="Gamma"(link="identity"),iterMax=1000,optim="Nelder-Mead")
# opa


## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# riq0_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(riq0_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq0_gama_shape_dist_pot_start <- list(a=riq0_a_pot_start,b=riq0_b_pot_start,scale=var(riq0_indice_media)/mean(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_gama_shape_dist_pot_mle <- mle2(riq0_gama_shape_dist_pot, start=riq0_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
riq0_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado <- (a*(riq0_indice_dist^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq0_gama_scale_dist_pot_start <- list(a=riq0_a_pot_start,b=riq0_b_pot_start,shape=mean(riq0_indice_media)^2/var(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_gama_scale_dist_pot_mle <- mle2(riq0_gama_scale_dist_pot, start=riq0_gama_scale_dist_pot_start,method="Nelder-Mead")




### Sobre a variancia

# ## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq0_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*riq0_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq0_gama_var_shape_dist_lin_start <- list(a=riq0_a_lin_start,b=riq0_b_lin_start,scale=var(riq0_indice_media)/mean(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_gama_var_shape_dist_lin_mle <- mle2(riq0_gama_var_shape_dist_lin, start=riq0_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq0_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*riq0_indice_dist+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq0_gama_var_scale_dist_lin_start <- list(a=riq0_a_lin_start,b=riq0_b_lin_start,shape=mean(riq0_indice_media)^2/var(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_gama_var_scale_dist_lin_mle <- mle2(riq0_gama_var_scale_dist_lin, start=riq0_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# riq0_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*riq0_indice_dist/(b+riq0_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq0_gama_var_shape_dist_mm_start <- list(a=riq0_a_mm_start,b=riq0_b_mm_start,scale=var(riq0_indice_media)/mean(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_gama_var_shape_dist_mm_mle <- mle2(riq0_gama_var_shape_dist_mm, start=riq0_gama_var_shape_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
riq0_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado <- (a*riq0_indice_dist/(b+riq0_indice_dist))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq0_gama_var_scale_dist_mm_start <- list(a=riq0_a_mm_start,b=riq0_b_mm_start,shape=mean(riq0_indice_media)^2/var(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_gama_var_scale_dist_mm_mle <- mle2(riq0_gama_var_scale_dist_mm, start=riq0_gama_var_scale_dist_mm_start)



## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# riq0_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(riq0_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq0_gama_var_shape_dist_pot_start <- list(a=riq0_a_pot_start,b=riq0_b_pot_start,scale=var(riq0_indice_media)/mean(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq0_gama_var_shape_dist_pot_mle <- mle2(riq0_gama_var_shape_dist_pot, start=riq0_gama_var_shape_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
riq0_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado <- (a*(riq0_indice_dist^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq0_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq0_gama_var_scale_dist_pot_start <- list(a=riq0_a_pot_start,b=riq0_b_pot_start,shape=mean(riq0_indice_media)^2/var(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq0_gama_var_scale_dist_pot_mle <- mle2(riq0_gama_var_scale_dist_pot, start=riq0_gama_var_scale_dist_pot_start,method="Nelder-Mead")



# NORMAL

## Nulo
riq0_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(riq0_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq0_norm_nulo_start <- list(mean=mean(riq0_indice_media),sd=sd(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
riq0_norm_nulo_mle <- mle2(riq0_norm_nulo, start=riq0_norm_nulo_start,method="Nelder-Mead")
########## ou:
riq0_norm_nulo_glm <- glm(riq0_indice_media~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
riq0_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*riq0_indice_dist+b
  -sum(dnorm(riq0_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq0_norm_mean_dist_lin_start <- list(a=riq0_a_lin_start,b=riq0_b_lin_start,sd=sd(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_norm_mean_dist_lin_mle <- mle2(riq0_norm_mean_dist_lin, start=riq0_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
riq0_norm_dist_lin_glm <- glm(riq0_indice_media~riq0_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
riq0_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*riq0_indice_dist+b)^2)
  -sum(dnorm(riq0_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq0_norm_sd_dist_lin_start <- list(a=riq0_a_lin_start,b=riq0_b_lin_start,mean=mean(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_norm_sd_dist_lin_mle <- mle2(riq0_norm_sd_dist_lin, start=riq0_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre mean e sd
riq0_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*riq0_indice_dist+b
  sd <- sqrt((c*riq0_indice_dist+d)^2)
  -sum(dnorm(riq0_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq0_norm_mean_sd_dist_lin_start <- list(a=riq0_a_lin_start,b=riq0_b_lin_start,c=riq0_a_lin_start,d=riq0_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_norm_mean_sd_dist_lin_mle <- mle2(riq0_norm_mean_sd_dist_lin, start=riq0_norm_mean_sd_dist_lin_start)

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre a media
riq0_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*riq0_indice_dist/(b+riq0_indice_dist)
  -sum(dnorm(riq0_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq0_norm_mean_dist_mm_start <- list(a=riq0_a_mm_start,b=riq0_b_mm_start,sd=sd(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_norm_mean_dist_mm_mle <- mle2(riq0_norm_mean_dist_mm, start=riq0_norm_mean_dist_mm_start,method="Nelder-Mead")
######## ou:
riq0_norm_dist_mm_glm <- glm(riq0_indice_dist/riq0_indice_media~riq0_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre sd
riq0_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*riq0_indice_dist/(b+riq0_indice_dist))^2)
  -sum(dnorm(riq0_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq0_norm_sd_dist_mm_start <- list(a=riq0_a_mm_start,b=riq0_b_mm_start,mean=mean(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_norm_sd_dist_mm_mle <- mle2(riq0_norm_sd_dist_mm, start=riq0_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre mean e sd
riq0_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*riq0_indice_dist/(b+riq0_indice_dist)
  sd <- sqrt((c*riq0_indice_dist/(d+riq0_indice_dist))^2)
  -sum(dnorm(riq0_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq0_norm_mean_sd_dist_mm_start <- list(a=riq0_a_mm_start,b=riq0_b_mm_start,c=riq0_a_mm_start,d=riq0_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_norm_mean_sd_dist_mm_mle <- mle2(riq0_norm_mean_sd_dist_mm, start=riq0_norm_mean_sd_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre a media
riq0_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(riq0_indice_dist^b)
  -sum(dnorm(riq0_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq0_norm_mean_dist_pot_start <- list(a=riq0_a_pot_start,b=riq0_b_pot_start,sd=sd(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_norm_mean_dist_pot_mle <- mle2(riq0_norm_mean_dist_pot, start=riq0_norm_mean_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre sd
riq0_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(riq0_indice_dist^b))^2)
  -sum(dnorm(riq0_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq0_norm_sd_dist_pot_start <- list(a=riq0_a_pot_start,b=riq0_b_pot_start,mean=mean(riq0_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_norm_sd_dist_pot_mle <- mle2(riq0_norm_sd_dist_pot, start=riq0_norm_sd_dist_pot_start)

## Com variavel preditora (disturbio) por meio da funcao potencia sobre mean e sd
riq0_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(riq0_indice_dist^b)
  sd <- sqrt((c*(riq0_indice_dist^d))^2)
  -sum(dnorm(riq0_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq0_norm_mean_sd_dist_pot_start <- list(a=riq0_a_pot_start,b=riq0_b_pot_start,c=riq0_a_pot_start,d=riq0_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq0_norm_mean_sd_dist_pot_mle <- mle2(riq0_norm_mean_sd_dist_pot, start=riq0_norm_mean_sd_dist_pot_start,method="Nelder-Mead")

################### SELECAO DE MODELOS ####################
AICtab(riq0_gama_nulo_mle,riq0_gama_nulo_glm,riq0_gama_scale_dist_lin_mle,riq0_gama_dist_lin_glm,riq0_gama_scale_dist_mm_mle,riq0_gama_dist_mm_glm,riq0_gama_scale_dist_pot_mle,riq0_gama_var_scale_dist_lin_mle,riq0_gama_var_scale_dist_mm_mle,riq0_gama_var_scale_dist_pot_mle,riq0_norm_nulo_mle,riq0_norm_nulo_glm,riq0_norm_mean_dist_lin_mle,riq0_norm_dist_lin_glm,riq0_norm_sd_dist_lin_mle,riq0_norm_mean_sd_dist_lin_mle,riq0_norm_mean_dist_mm_mle,riq0_norm_dist_mm_glm,riq0_norm_sd_dist_mm_mle,riq0_norm_mean_sd_dist_mm_mle,riq0_norm_mean_dist_pot_mle,riq0_norm_sd_dist_pot_mle,riq0_norm_mean_sd_dist_pot_mle)
### modelo selecionado: riq0_gama_scale_dist_lin_mle

##########################################################
######################## RIQUEZA 1 #######################
##########################################################

riq1_indice_media <- c(bat2_indice_media,bat3_indice_media)
riq1_indice_dist <- c(bat2_indice_dist,bat3_indice_dist)

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq1_lin <- lm(riq1_indice_media~riq1_indice_dist)
riq1_a_lin_start <- coef(riq1_lin)[[2]]
riq1_b_lin_start <- coef(riq1_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq1_mm_linearizada <- lm(riq1_indice_dist/riq1_indice_media~riq1_indice_dist) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
riq1_a_mm_start <- 1/(coef(riq1_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
riq1_b_mm_start <- (coef(riq1_mm_linearizada)[[1]])*(1/(coef(riq1_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
riq1_a_pot_start <- 0.0006
riq1_b_pot_start <- 0.55

# GAMA

## Nulo
riq1_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq1_gama_nulo_start <- list(shape=mean(riq1_indice_media)^2/var(riq1_indice_media),scale=var(riq1_indice_media)/mean(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
riq1_gama_nulo_mle <- mle2(riq1_gama_nulo, start=riq1_gama_nulo_start,method="Nelder-Mead")
###### ou:
riq1_gama_nulo_glm <- glm(riq1_indice_media~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq1_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*riq1_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq1_gama_shape_dist_lin_start <- list(a=riq1_a_lin_start,b=riq1_b_lin_start,scale=var(riq1_indice_media)/mean(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_gama_shape_dist_lin_mle <- mle2(riq1_gama_shape_dist_lin, start=riq1_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq1_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*riq1_indice_dist+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq1_gama_scale_dist_lin_start <- list(a=riq1_a_lin_start,b=riq1_b_lin_start,shape=mean(riq1_indice_media)^2/var(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_gama_scale_dist_lin_mle <- mle2(riq1_gama_scale_dist_lin, start=riq1_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
riq1_gama_dist_lin_glm <- glm(riq1_indice_media~riq1_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# riq1_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*riq1_indice_dist/(b+riq1_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq1_gama_shape_dist_mm_start <- list(a=riq1_a_mm_start,b=riq1_b_mm_start,scale=var(riq1_indice_media)/mean(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_gama_shape_dist_mm_mle <- mle2(riq1_gama_shape_dist_mm, start=riq1_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
riq1_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado <- (a*riq1_indice_dist/(b+riq1_indice_dist))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq1_gama_scale_dist_mm_start <- list(a=riq1_a_mm_start,b=riq1_b_mm_start,shape=mean(riq1_indice_media)^2/var(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_gama_scale_dist_mm_mle <- mle2(riq1_gama_scale_dist_mm, start=riq1_gama_scale_dist_mm_start,method="Nelder-Mead")
######## ou:
riq1_gama_dist_mm_glm <- glm(riq1_indice_dist/riq1_indice_media~riq1_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# riq1_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(riq1_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq1_gama_shape_dist_pot_start <- list(a=riq1_a_pot_start,b=riq1_b_pot_start,scale=var(riq1_indice_media)/mean(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_gama_shape_dist_pot_mle <- mle2(riq1_gama_shape_dist_pot, start=riq1_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
riq1_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado <- (a*(riq1_indice_dist^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq1_gama_scale_dist_pot_start <- list(a=riq1_a_pot_start,b=riq1_b_pot_start,shape=mean(riq1_indice_media)^2/var(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_gama_scale_dist_pot_mle <- mle2(riq1_gama_scale_dist_pot, start=riq1_gama_scale_dist_pot_start,method="Nelder-Mead")


### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# riq1_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*riq1_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq1_gama_var_shape_dist_lin_start <- list(a=riq1_a_lin_start,b=riq1_b_lin_start,scale=var(riq1_indice_media)/mean(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_gama_var_shape_dist_lin_mle <- mle2(riq1_gama_var_shape_dist_lin, start=riq1_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
riq1_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*riq1_indice_dist+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq1_gama_var_scale_dist_lin_start <- list(a=riq1_a_lin_start,b=riq1_b_lin_start,shape=mean(riq1_indice_media)^2/var(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_gama_var_scale_dist_lin_mle <- mle2(riq1_gama_var_scale_dist_lin, start=riq1_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# riq1_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*riq1_indice_dist/(b+riq1_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq1_gama_var_shape_dist_mm_start <- list(a=riq1_a_mm_start,b=riq1_b_mm_start,scale=var(riq1_indice_media)/mean(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_gama_var_shape_dist_mm_mle <- mle2(riq1_gama_var_shape_dist_mm, start=riq1_gama_var_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
riq1_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado <- (a*riq1_indice_dist/(b+riq1_indice_dist))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq1_gama_var_scale_dist_mm_start <- list(a=riq1_a_mm_start,b=riq1_b_mm_start,shape=mean(riq1_indice_media)^2/var(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_gama_var_scale_dist_mm_mle <- mle2(riq1_gama_var_scale_dist_mm, start=riq1_gama_var_scale_dist_mm_start,method="Nelder-Mead")

# ## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# riq1_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(riq1_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# riq1_gama_var_shape_dist_pot_start <- list(a=riq1_a_pot_start,b=riq1_b_pot_start,scale=var(riq1_indice_media)/mean(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# riq1_gama_var_shape_dist_pot_mle <- mle2(riq1_gama_var_shape_dist_pot, start=riq1_gama_var_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
riq1_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado <- (a*(riq1_indice_dist^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(riq1_indice_media, shape=shape, scale=scale, log=TRUE))
}
riq1_gama_var_scale_dist_pot_start <- list(a=riq1_a_pot_start,b=riq1_b_pot_start,shape=mean(riq1_indice_media)^2/var(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
riq1_gama_var_scale_dist_pot_mle <- mle2(riq1_gama_var_scale_dist_pot, start=riq1_gama_var_scale_dist_pot_start,method="Nelder-Mead")


# NORMAL

## Nulo
riq1_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(riq1_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq1_norm_nulo_start <- list(mean=mean(riq1_indice_media),sd=sd(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
riq1_norm_nulo_mle <- mle2(riq1_norm_nulo, start=riq1_norm_nulo_start,method="Nelder-Mead")
########## ou:
riq1_norm_nulo_glm <- glm(riq1_indice_media~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
riq1_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*riq1_indice_dist+b
  -sum(dnorm(riq1_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq1_norm_mean_dist_lin_start <- list(a=riq1_a_lin_start,b=riq1_b_lin_start,sd=sd(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_norm_mean_dist_lin_mle <- mle2(riq1_norm_mean_dist_lin, start=riq1_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
riq1_norm_dist_lin_glm <- glm(riq1_indice_media~riq1_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
riq1_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*riq1_indice_dist+b)^2)
  -sum(dnorm(riq1_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq1_norm_sd_dist_lin_start <- list(a=riq1_a_lin_start,b=riq1_b_lin_start,mean=mean(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_norm_sd_dist_lin_mle <- mle2(riq1_norm_sd_dist_lin, start=riq1_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre mean e sd
riq1_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*riq1_indice_dist+b
  sd <- sqrt((c*riq1_indice_dist+d)^2)
  -sum(dnorm(riq1_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq1_norm_mean_sd_dist_lin_start <- list(a=riq1_a_lin_start,b=riq1_b_lin_start,c=riq1_a_lin_start,d=riq1_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_norm_mean_sd_dist_lin_mle <- mle2(riq1_norm_mean_sd_dist_lin, start=riq1_norm_mean_sd_dist_lin_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre a media
riq1_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*riq1_indice_dist/(b+riq1_indice_dist)
  -sum(dnorm(riq1_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq1_norm_mean_dist_mm_start <- list(a=riq1_a_mm_start,b=riq1_b_mm_start,sd=sd(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_norm_mean_dist_mm_mle <- mle2(riq1_norm_mean_dist_mm, start=riq1_norm_mean_dist_mm_start)
######## ou:
riq1_norm_dist_mm_glm <- glm(riq1_indice_dist/riq1_indice_media~riq1_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre sd
riq1_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*riq1_indice_dist/(b+riq1_indice_dist))^2)
  -sum(dnorm(riq1_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq1_norm_sd_dist_mm_start <- list(a=riq1_a_mm_start,b=riq1_b_mm_start,mean=mean(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_norm_sd_dist_mm_mle <- mle2(riq1_norm_sd_dist_mm, start=riq1_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre mean e sd
riq1_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*riq1_indice_dist/(b+riq1_indice_dist)
  sd <- sqrt((c*riq1_indice_dist/(d+riq1_indice_dist))^2)
  -sum(dnorm(riq1_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq1_norm_mean_sd_dist_mm_start <- list(a=riq1_a_mm_start,b=riq1_b_mm_start,c=riq1_a_mm_start,d=riq1_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_norm_mean_sd_dist_mm_mle <- mle2(riq1_norm_mean_sd_dist_mm, start=riq1_norm_mean_sd_dist_mm_start,control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre a media
riq1_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(riq1_indice_dist^b)
  -sum(dnorm(riq1_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq1_norm_mean_dist_pot_start <- list(a=riq1_a_pot_start,b=riq1_b_pot_start,sd=sd(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_norm_mean_dist_pot_mle <- mle2(riq1_norm_mean_dist_pot, start=riq1_norm_mean_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre sd
riq1_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(riq1_indice_dist^b))^2)
  -sum(dnorm(riq1_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq1_norm_sd_dist_pot_start <- list(a=riq1_a_pot_start,b=riq1_b_pot_start,mean=mean(riq1_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_norm_sd_dist_pot_mle <- mle2(riq1_norm_sd_dist_pot, start=riq1_norm_sd_dist_pot_start)

## Com variavel preditora (disturbio) por meio da funcao potencia sobre mean e sd
riq1_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(riq1_indice_dist^b)
  sd <- sqrt((c*(riq1_indice_dist^d))^2)
  -sum(dnorm(riq1_indice_media, mean=mean, sd=sd, log=TRUE))
}
riq1_norm_mean_sd_dist_pot_start <- list(a=riq1_a_pot_start,b=riq1_b_pot_start,c=riq1_a_pot_start,d=riq1_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
riq1_norm_mean_sd_dist_pot_mle <- mle2(riq1_norm_mean_sd_dist_pot, start=riq1_norm_mean_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

################### SELECAO DE MODELOS ####################
AICtab(riq1_gama_nulo_mle,riq1_gama_nulo_glm,riq1_gama_scale_dist_lin_mle,riq1_gama_dist_lin_glm,riq1_gama_scale_dist_mm_mle,riq1_gama_dist_mm_glm,riq1_gama_scale_dist_pot_mle,riq1_gama_var_scale_dist_lin_mle,riq1_gama_var_scale_dist_mm_mle,riq1_gama_var_scale_dist_pot_mle,riq1_norm_nulo_mle,riq1_norm_nulo_glm,riq1_norm_mean_dist_lin_mle,riq1_norm_dist_lin_glm,riq1_norm_sd_dist_lin_mle,riq1_norm_mean_sd_dist_lin_mle,riq1_norm_mean_dist_mm_mle,riq1_norm_dist_mm_glm,riq1_norm_sd_dist_mm_mle,riq1_norm_mean_sd_dist_mm_mle,riq1_norm_mean_dist_pot_mle,riq1_norm_sd_dist_pot_mle,riq1_norm_mean_sd_dist_pot_mle,weights=T)
### modelo selecionado: riq1_gama_scale_dist_pot_mle 

##########################################################
######## CALCULO DO AIC SUBCONJUNTO MUTACAO TOTAL ########
##########################################################
aic_mle_soma_sub_riqueza <- AIC(riq0_gama_scale_dist_lin_mle) + AIC(riq1_gama_scale_dist_pot_mle) #-7253.413





##########################################################
##########################################################
##### Analises das baterias de simulacao EM CONJUNTO #####
##########################################################
##########################################################

geral_indice_media <- c(bat1_indice_media,bat2_indice_media,bat3_indice_media)
geral_indice_dist <- c(bat1_indice_dist,bat2_indice_dist,bat3_indice_dist)

################## Construcao de modelos ##################

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
geral_lin <- lm(geral_indice_media~geral_indice_dist)
geral_a_lin_start <- coef(geral_lin)[[2]]
geral_b_lin_start <- coef(geral_lin)[[1]]

# Valores de start para os coeficientes a e b da funcao Michaelis-Menten usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
geral_mm_linearizada <- lm(geral_indice_dist/geral_indice_media~geral_indice_dist) # para linearizar a funcao michaelis-menten, eh necessario colocar ambos os lados da funcao como denominador da variavel independente x...
geral_a_mm_start <- 1/(coef(geral_mm_linearizada)[[2]]) # ...dessa forma, o coeficiente a da michaelis-menten original eh equivalente ao inverso da inclinacao da michaelis-menten linearizada...
geral_b_mm_start <- (coef(geral_mm_linearizada)[[1]])*(1/(coef(geral_mm_linearizada)[[2]])) # ...e o coeficiente b da michaelis-menten original eh equivalente ao produto do intercepto da michaelis-menten linearizada pelo inverso da inclinacao da michaelis-menten linearizada.

# Valores de start para os coeficientes a e b da funcao linear usada para expressar a relacao entre o disturbio e a esperanca de todas as distribuicoes nos modelos em que a variavel preditora foi incluida
geral_a_pot_start <- 0.00062
geral_b_pot_start <- 0.54

# GAMA

## Nulo
geral_gama_nulo <- function(shape,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
}
geral_gama_nulo_start <- list(shape=mean(geral_indice_media)^2/var(geral_indice_media),scale=var(geral_indice_media)/mean(geral_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
geral_gama_nulo_mle <- mle2(geral_gama_nulo, start=geral_gama_nulo_start,method="Nelder-Mead")
###### ou:
geral_gama_nulo_glm <- glm(geral_indice_media~1,family="Gamma"(link="identity"))

### Sobre a esperanca

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# geral_gama_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*geral_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# geral_gama_shape_dist_lin_start <- list(a=geral_a_lin_start,b=geral_b_lin_start,scale=var(geral_indice_media)/mean(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_gama_shape_dist_lin_mle <- mle2(geral_gama_shape_dist_lin, start=geral_gama_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_gama_scale_dist_lin <- function(a,b,shape){
  esperancaaoquadrado<- (a*geral_indice_dist+b)^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
}
geral_gama_scale_dist_lin_start <- list(a=geral_a_lin_start,b=geral_b_lin_start,shape=mean(geral_indice_media)^2/var(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_gama_scale_dist_lin_mle <- mle2(geral_gama_scale_dist_lin, start=geral_gama_scale_dist_lin_start,method="Nelder-Mead")
######## ou:
geral_gama_dist_lin_glm <- glm(geral_indice_media~geral_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# geral_gama_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*geral_indice_dist/(b+geral_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# geral_gama_shape_dist_mm_start <- list(a=geral_a_mm_start,b=geral_b_mm_start,scale=var(geral_indice_media)/mean(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_gama_shape_dist_mm_mle <- mle2(geral_gama_shape_dist_mm, start=geral_gama_shape_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
geral_gama_scale_dist_mm <- function(a,b,shape){
  esperancaaoquadrado <- (a*geral_indice_dist/(b+geral_indice_dist))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
}
geral_gama_scale_dist_mm_start <- list(a=geral_a_mm_start,b=geral_b_mm_start,shape=mean(geral_indice_media)^2/var(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_gama_scale_dist_mm_mle <- mle2(geral_gama_scale_dist_mm, start=geral_gama_scale_dist_mm_start,method="Nelder-Mead")
######## ou:
geral_gama_dist_mm_glm <- glm(geral_indice_dist/geral_indice_media~geral_indice_dist,family="Gamma"(link="identity"))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# geral_gama_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   esperancaaoquadrado<- (a*(geral_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(esperancaaoquadrado)/scale
#   -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# geral_gama_shape_dist_pot_start <- list(a=geral_a_pot_start,b=geral_b_pot_start,scale=var(geral_indice_media)/mean(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_gama_shape_dist_pot_mle <- mle2(geral_gama_shape_dist_pot, start=geral_gama_shape_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
geral_gama_scale_dist_pot <- function(a,b,shape){
  esperancaaoquadrado <- (a*(geral_indice_dist^b))^2
  scale <- sqrt(esperancaaoquadrado)/shape
  -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
}
geral_gama_scale_dist_pot_start <- list(a=geral_a_pot_start,b=geral_b_pot_start,shape=mean(geral_indice_media)^2/var(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_gama_scale_dist_pot_mle <- mle2(geral_gama_scale_dist_pot, start=geral_gama_scale_dist_pot_start,method="Nelder-Mead")

### Sobre a variancia

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre shape
# geral_gama_var_shape_dist_lin <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*geral_indice_dist+b)^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# geral_gama_var_shape_dist_lin_start <- list(a=geral_a_lin_start,b=geral_b_lin_start,scale=var(geral_indice_media)/mean(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_gama_var_shape_dist_lin_mle <- mle2(geral_gama_var_shape_dist_lin, start=geral_gama_var_shape_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre scale
geral_gama_var_scale_dist_lin <- function(a,b,shape){
  varianciaaoquadrado<- (a*geral_indice_dist+b)^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
}
geral_gama_var_scale_dist_lin_start <- list(a=geral_a_lin_start,b=geral_b_lin_start,shape=mean(geral_indice_media)^2/var(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_gama_var_scale_dist_lin_mle <- mle2(geral_gama_var_scale_dist_lin, start=geral_gama_var_scale_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre shape
# geral_gama_var_shape_dist_mm <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*geral_indice_dist/(b+geral_indice_dist))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# geral_gama_var_shape_dist_mm_start <- list(a=geral_a_mm_start,b=geral_b_mm_start,scale=var(geral_indice_media)/mean(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_gama_var_shape_dist_mm_mle <- mle2(geral_gama_var_shape_dist_mm, start=geral_gama_var_shape_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre scale
geral_gama_var_scale_dist_mm <- function(a,b,shape){
  varianciaaoquadrado <- (a*geral_indice_dist/(b+geral_indice_dist))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
}
geral_gama_var_scale_dist_mm_start <- list(a=geral_a_mm_start,b=geral_b_mm_start,shape=mean(geral_indice_media)^2/var(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_gama_var_scale_dist_mm_mle <- mle2(geral_gama_var_scale_dist_mm, start=geral_gama_var_scale_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre shape
# geral_gama_var_shape_dist_pot <- function(a,b,scale){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que o parametro scale eh constante e o parametro shape eh funcao da esperanca da distribuicao, que por sua vez eh funcao (michaelis-menten) do indice de disturbio
#   varianciaaoquadrado<- (a*(geral_indice_dist^b))^2 # ambos os lados da funcao foram elevados ao quadrado para evitar valores negativos de shape na formula abaixo
#   shape <- sqrt(varianciaaoquadrado)/(scale^2)
#   -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
# }
# geral_gama_var_shape_dist_pot_start <- list(a=geral_a_pot_start,b=geral_b_pot_start,scale=var(geral_indice_media)/mean(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
# geral_gama_var_shape_dist_pot_mle <- mle2(geral_gama_var_shape_dist_pot, start=geral_gama_var_shape_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

## Com variavel preditora (disturbio) por meio da funcao potencia sobre scale
geral_gama_var_scale_dist_pot <- function(a,b,shape){
  varianciaaoquadrado <- (a*(geral_indice_dist^b))^2
  scale <- sqrt(sqrt(varianciaaoquadrado)/shape)
  -sum(dgamma(geral_indice_media, shape=shape, scale=scale, log=TRUE))
}
geral_gama_var_scale_dist_pot_start <- list(a=geral_a_pot_start,b=geral_b_pot_start,shape=mean(geral_indice_media)^2/var(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de scale foi calculado a partir do metodo dos momentos
geral_gama_var_scale_dist_pot_mle <- mle2(geral_gama_var_scale_dist_pot, start=geral_gama_var_scale_dist_pot_start,method="Nelder-Mead")


# NORMAL

## Nulo
geral_norm_nulo <- function(mean,sd){ # funcao que encontra a soma dos valores de log-verossimilhanca negativa referentes aos dados observados, dado que os parametros da distribuicao sao constantes
  -sum(dnorm(geral_indice_media, mean=mean, sd=sd, log=TRUE))
}
geral_norm_nulo_start <- list(mean=mean(geral_indice_media),sd=sd(geral_indice_media)) # valores que serao utilizados como start na funcao mle2, calculados a partir do metodo dos momentos
geral_norm_nulo_mle <- mle2(geral_norm_nulo, start=geral_norm_nulo_start,method="Nelder-Mead")
########## ou:
geral_norm_nulo_glm <- glm(geral_indice_media~1,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre a media
geral_norm_mean_dist_lin <- function(a,b,sd){
  mean <- a*geral_indice_dist+b
  -sum(dnorm(geral_indice_media, mean=mean, sd=sd, log=TRUE))
}
geral_norm_mean_dist_lin_start <- list(a=geral_a_lin_start,b=geral_b_lin_start,sd=sd(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_norm_mean_dist_lin_mle <- mle2(geral_norm_mean_dist_lin, start=geral_norm_mean_dist_lin_start,method="Nelder-Mead")
######### ou:
geral_norm_dist_lin_glm <- glm(geral_indice_media~geral_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre sd
geral_norm_sd_dist_lin <- function(a,b,mean){
  sd <- sqrt((a*geral_indice_dist+b)^2)
  -sum(dnorm(geral_indice_media, mean=mean, sd=sd, log=TRUE))
}
geral_norm_sd_dist_lin_start <- list(a=geral_a_lin_start,b=geral_b_lin_start,mean=mean(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_norm_sd_dist_lin_mle <- mle2(geral_norm_sd_dist_lin, start=geral_norm_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio de uma funcao linear sobre mean e sd
geral_norm_mean_sd_dist_lin <- function(a,b,c,d){
  mean <- a*geral_indice_dist+b
  sd <- sqrt((c*geral_indice_dist+d)^2)
  -sum(dnorm(geral_indice_media, mean=mean, sd=sd, log=TRUE))
}
geral_norm_mean_sd_dist_lin_start <- list(a=geral_a_lin_start,b=geral_b_lin_start,c=geral_a_lin_start,d=geral_b_lin_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_norm_mean_sd_dist_lin_mle <- mle2(geral_norm_mean_sd_dist_lin, start=geral_norm_mean_sd_dist_lin_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da Michaelis-Menten sobre a media
geral_norm_mean_dist_mm <- function(a,b,sd){
  mean <- a*geral_indice_dist/(b+geral_indice_dist)
  -sum(dnorm(geral_indice_media, mean=mean, sd=sd, log=TRUE))
}
geral_norm_mean_dist_mm_start <- list(a=geral_a_mm_start,b=geral_b_mm_start,sd=sd(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_norm_mean_dist_mm_mle <- mle2(geral_norm_mean_dist_mm, start=geral_norm_mean_dist_mm_start)
######## ou:
geral_norm_dist_mm_glm <- glm(geral_indice_dist/geral_indice_media~geral_indice_dist,family="gaussian")

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre sd
geral_norm_sd_dist_mm <- function(a,b,mean){
  sd <- sqrt((a*geral_indice_dist/(b+geral_indice_dist))^2)
  -sum(dnorm(geral_indice_media, mean=mean, sd=sd, log=TRUE))
}
geral_norm_sd_dist_mm_start <- list(a=geral_a_mm_start,b=geral_b_mm_start,mean=mean(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_norm_sd_dist_mm_mle <- mle2(geral_norm_sd_dist_mm, start=geral_norm_sd_dist_mm_start)

## Com variavel preditora (disturbio) por meio da funcao Michaelis-Menten sobre mean e sd
geral_norm_mean_sd_dist_mm <- function(a,b,c,d){
  mean <- a*geral_indice_dist/(b+geral_indice_dist)
  sd <- sqrt((c*geral_indice_dist/(d+geral_indice_dist))^2)
  -sum(dnorm(geral_indice_media, mean=mean, sd=sd, log=TRUE))
}
geral_norm_mean_sd_dist_mm_start <- list(a=geral_a_mm_start,b=geral_b_mm_start,c=geral_a_mm_start,d=geral_b_mm_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_norm_mean_sd_dist_mm_mle <- mle2(geral_norm_mean_sd_dist_mm, start=geral_norm_mean_sd_dist_mm_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre a media
geral_norm_mean_dist_pot <- function(a,b,sd){
  mean <- a*(geral_indice_dist^b)
  -sum(dnorm(geral_indice_media, mean=mean, sd=sd, log=TRUE))
}
geral_norm_mean_dist_pot_start <- list(a=geral_a_pot_start,b=geral_b_pot_start,sd=sd(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_norm_mean_dist_pot_mle <- mle2(geral_norm_mean_dist_pot, start=geral_norm_mean_dist_pot_start,method="Nelder-Mead")

## Com variavel preditora (disturbio) por meio da funcao potencia sobre sd
geral_norm_sd_dist_pot <- function(a,b,mean){
  sd <- sqrt((a*(geral_indice_dist^b))^2)
  -sum(dnorm(geral_indice_media, mean=mean, sd=sd, log=TRUE))
}
geral_norm_sd_dist_pot_start <- list(a=geral_a_pot_start,b=geral_b_pot_start,mean=mean(geral_indice_media)) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_norm_sd_dist_pot_mle <- mle2(geral_norm_sd_dist_pot, start=geral_norm_sd_dist_pot_start)

## Com variavel preditora (disturbio) por meio da funcao potencia sobre mean e sd
geral_norm_mean_sd_dist_pot <- function(a,b,c,d){
  mean <- a*(geral_indice_dist^b)
  sd <- sqrt((c*(geral_indice_dist^d))^2)
  -sum(dnorm(geral_indice_media, mean=mean, sd=sd, log=TRUE))
}
geral_norm_mean_sd_dist_pot_start <- list(a=geral_a_pot_start,b=geral_b_pot_start,c=geral_a_pot_start,d=geral_b_pot_start) # valores que serao utilizados como start na funcao mle2. O valor de sd foi calculado a partir do metodo dos momentos
geral_norm_mean_sd_dist_pot_mle <- mle2(geral_norm_mean_sd_dist_pot, start=geral_norm_mean_sd_dist_pot_start,method="Nelder-Mead",control=list(maxit=1000))

################### SELECAO DE MODELOS ####################
AICtab(geral_gama_nulo_mle,geral_gama_nulo_glm,geral_gama_scale_dist_lin_mle,geral_gama_dist_lin_glm,geral_gama_scale_dist_mm_mle,geral_gama_dist_mm_glm,geral_gama_scale_dist_pot_mle,geral_gama_var_scale_dist_lin_mle,geral_gama_var_scale_dist_mm_mle,geral_gama_var_scale_dist_pot_mle,geral_norm_nulo_mle,geral_norm_nulo_glm,geral_norm_mean_dist_lin_mle,geral_norm_dist_lin_glm,geral_norm_sd_dist_lin_mle,geral_norm_mean_sd_dist_lin_mle,geral_norm_mean_dist_mm_mle,geral_norm_dist_mm_glm,geral_norm_sd_dist_mm_mle,geral_norm_mean_sd_dist_mm_mle,geral_norm_mean_dist_pot_mle,geral_norm_sd_dist_pot_mle,geral_norm_mean_sd_dist_pot_mle)
### modelo selecionado: geral_gama_var_scale_dist_lin_mle

##########################################################
################## CALCULO DO AIC GERAL ##################
##########################################################
aic_mle_geral <- AIC(geral_gama_var_scale_dist_lin_mle) #-5395.671





##########################################################
##########################################################
################## COMPARACAO DOS AICS ###################
##########################################################
##########################################################
aics <- c(por_bateria=aic_mle_soma_individual,por_mutacao=aic_mle_soma_sub_mutacao,por_riqueza=aic_mle_soma_sub_riqueza,geral=aic_mle_geral)
min_aic <- min(aics)
daic <- aics - min_aic
sort(daic)





##########################################################
##########################################################
######################## GRAFICOS ########################
##########################################################
##########################################################
hist(geral_indice_media,breaks=100,las=1,xlab="Índice de estratégia de vida",ylab="Frequência",main="",xlim=c(0,1))

par(mar=c(5,5,4,2))
plot(bat1_indice_dist,bat1_indice_media,pch=20,col=alpha("dimgray",0.2),xlim=c(0,3e5),ylim=c(0,1),xlab="Índice de distúrbio",ylab="Média do índice \nde estratégia de vida",bty="l",las=1)
par(new=T)
plot(bat2_indice_dist,bat2_indice_media,pch=20,col=alpha("pink",0.2),xlim=c(0,3e5),ylim=c(0,1),xlab="",ylab="",axes=F)
par(new=T)
plot(bat3_indice_dist,bat3_indice_media,pch=20,col=alpha("lightblue",0.2),xlim=c(0,3e5),ylim=c(0,1),xlab="",ylab="",axes=F)

### plotar previstos
lin <- function(x,a,b) {a*x+b}
pot <- function(x,a,b) {a*(x^b)}
#### por mutacao
#mut0_norm_mean_sd_dist_mm_mle
curve(pot(x,coef(mut0_norm_mean_sd_dist_pot_mle)[[1]],coef(mut0_norm_mean_sd_dist_pot_mle)[[2]]),add=T,col="red")
#mut1_gama_scale_dist_lin_mle
curve(lin(x,coef(mut1_gama_scale_dist_lin_mle)[[1]],coef(mut1_gama_scale_dist_lin_mle)[[2]]),add=T,col="darkblue")
###### aprender a plotar as variancias!

par(mar=c(5,5,4,2))
plot(bat1_indice_dist,bat1_indice_media,pch=20,col=alpha("dimgray",0.5),xlim=c(0,3e5),ylim=c(0,1),xlab="Índice de distúrbio",ylab="Média do índice \nde estratégia de vida",bty="l",las=1)
par(new=T)
plot(bat2_indice_dist,bat2_indice_media,pch=20,col=alpha("dimgray",0.5),xlim=c(0,3e5),ylim=c(0,1),xlab="",ylab="",axes=F)
par(new=T)
plot(bat3_indice_dist,bat3_indice_media,pch=20,col=alpha("dimgray",0.5),xlim=c(0,3e5),ylim=c(0,1),xlab="",ylab="",axes=F)
