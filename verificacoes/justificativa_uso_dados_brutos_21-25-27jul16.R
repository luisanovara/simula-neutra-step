##########################################################
##########################################################
###### Formalizacao do porque usar os dados brutos #######
##########################################################
##########################################################

##########################################################
### Demonstrar que nao existe interacao significativa entre o disturbio e as coisas que eu quero ignorar na determinacao da variavel-resposta ou que os valores dos coeficientes sao baixos

nclasses <- cut(dados3_21jul16[,1],breaks=4,labels=1:4)
plot(bat1_indice_media~bat1_indice_dist,col=as.integer(nclasses),ylim=c(0,1))

nclasses <- cut(dados3_25jul16[,1],breaks=4,labels=1:4)
plot(bat2_indice_media~bat2_indice_dist,col=as.integer(nclasses),ylim=c(0,1))

nclasses <- cut(dados3_27jul16[,1],breaks=4,labels=1:4)
plot(bat3_indice_media~bat3_indice_dist,col=as.integer(nclasses),ylim=c(0,1))

#### Padronizar variaveis preditoras

bat1_indice_dist_padr <- (bat1_indice_dist - mean(bat1_indice_dist))/sd(bat1_indice_dist)
bat1_indice_media_inicial_padr <- (dados3_21jul16[,1] - mean(dados3_21jul16[,1]))/sd(dados3_21jul16[,1])
bat2_indice_dist_padr <- (bat2_indice_dist - mean(bat2_indice_dist))/sd(bat2_indice_dist)
bat2_riqueza_inicial_padr <- (dados3_25jul16[,1] - mean(dados3_25jul16[,1]))/sd(dados3_25jul16[,1])
bat3_indice_dist_padr <- (bat3_indice_dist - mean(bat3_indice_dist))/sd(bat3_indice_dist)
bat3_riqueza_inicial_padr <- (dados3_27jul16[,1] - mean(dados3_27jul16[,1]))/sd(dados3_27jul16[,1])

#### Media
bat1_mod <- lm(bat1_indice_media~bat1_indice_dist_padr)
bat1_mod_ad <- lm(bat1_indice_media~bat1_indice_media_inicial_padr+bat1_indice_dist_padr)
bat1_mod_int <- lm(bat1_indice_media~bat1_indice_media_inicial_padr*bat1_indice_dist_padr)
AICtab(bat1_mod,bat1_mod_ad,bat1_mod_int)
summary(bat1_mod_int)
confint(bat1_mod_int)
# viajando
plot(bat1_indice_media~bat1_indice_dist_padr)
plot(bat1_indice_media~bat1_indice_media_inicial_padr)
modelo_nulo1 <- lm(bat1_indice_media~1)
modelo_efeito1 <- lm(bat1_indice_media~bat1_indice_media_inicial_padr)
AICtab(modelo_nulo1,modelo_efeito1)

bat2_mod <- lm(bat2_indice_media~bat2_indice_dist_padr)
bat2_mod_ad <- lm(bat2_indice_media~bat2_riqueza_inicial_padr+bat2_indice_dist_padr)
bat2_mod_int <- lm(bat2_indice_media~bat2_riqueza_inicial_padr*bat2_indice_dist_padr)
AICtab(bat2_mod,bat2_mod_ad,bat2_mod_int) # ok :)
summary(bat2_mod_int)
confint(bat2_mod_int)
# viajando
plot(bat2_indice_media~bat2_indice_dist_padr)
plot(bat2_indice_media~bat2_riqueza_inicial_padr)
modelo_nulo2 <- lm(bat2_indice_media~1)
modelo_efeito2 <- lm(bat2_indice_media~bat2_riqueza_inicial_padr)
AICtab(modelo_nulo2,modelo_efeito2)

bat3_mod <- lm(bat3_indice_media~bat3_indice_dist_padr)
bat3_mod_ad <- lm(bat3_indice_media~bat3_riqueza_inicial_padr+bat3_indice_dist_padr)
bat3_mod_int <- lm(bat3_indice_media~bat3_riqueza_inicial_padr*bat3_indice_dist_padr)
AICtab(bat3_mod,bat3_mod_ad,bat3_mod_int)
summary(bat3_mod_int)
confint(bat3_mod_int)
# viajando
plot(bat3_indice_media~bat3_indice_dist_padr)
plot(bat3_indice_media~bat3_riqueza_inicial_padr)
modelo_nulo3 <- lm(bat3_indice_media~1)
modelo_efeito3 <- lm(bat3_indice_media~bat3_riqueza_inicial_padr)
AICtab(modelo_nulo3,modelo_efeito3)


#fazendo do jeito do pi e juntando as baterias
geral_indice_media <- c(bat1_indice_media,bat2_indice_media,bat3_indice_media)
geral_desinteresse <- c(bat1_indice_media_inicial_padr,bat2_riqueza_inicial_padr,bat3_riqueza_inicial_padr)
geral_dist_padr <- c(bat1_indice_dist_padr,bat2_indice_dist_padr,bat3_indice_dist_padr)
geral_mod <- lm(geral_indice_media~geral_desinteresse)
geral_mod_ad <- lm(geral_indice_media~geral_desinteresse+geral_dist_padr)
geral_mod_int <- lm(geral_indice_media~geral_desinteresse*geral_dist_padr)
AICtab(geral_mod,geral_mod_ad,geral_mod_int)
summary(geral_mod_int)
confint(geral_mod_int)


#### Variancia

bat1_mod_var <- lm(bat1_indice_var~bat1_indice_dist_padr)
bat1_mod_ad_var <- lm(bat1_indice_var~bat1_indice_media_inicial_padr+bat1_indice_dist_padr)
bat1_mod_int_var <- lm(bat1_indice_var~bat1_indice_media_inicial_padr*bat1_indice_dist_padr)
AICtab(bat1_mod_var,bat1_mod_ad_var,bat1_mod_int_var) # ok :)
summary(bat1_mod_int_var)
confint(bat1_mod_int_var)

bat2_mod_var <- lm(bat2_indice_var~bat2_indice_dist_padr)
bat2_mod_ad_var <- lm(bat2_indice_var~bat2_riqueza_inicial_padr+bat2_indice_dist_padr)
bat2_mod_int_var <- lm(bat2_indice_var~bat2_riqueza_inicial_padr*bat2_indice_dist_padr)
AICtab(bat2_mod_var,bat2_mod_ad_var,bat2_mod_int_var) # ok 
summary(bat2_mod_int_var)
confint(bat2_mod_int_var)

bat3_mod_var <- lm(bat3_indice_var~bat3_indice_dist_padr)
bat3_mod_ad_var <- lm(bat3_indice_var~bat3_riqueza_inicial_padr+bat3_indice_dist_padr)
bat3_mod_int_var <- lm(bat3_indice_var~bat3_riqueza_inicial_padr*bat3_indice_dist_padr)
AICtab(bat3_mod_var,bat3_mod_ad_var,bat3_mod_int_var) # ok :)
summary(bat3_mod_int_var)
confint(bat3_mod_int_var)

geral_indice_var <- c(bat1_indice_var,bat2_indice_var,bat3_indice_var)
geral_desinteresse <- c(bat1_indice_media_inicial_padr,bat2_riqueza_inicial_padr,bat3_riqueza_inicial_padr)
geral_dist_padr <- c(bat1_indice_dist_padr,bat2_indice_dist_padr,bat3_indice_dist_padr)
geral_mod_var <- lm(geral_indice_var~geral_desinteresse)
geral_mod_ad_var <- lm(geral_indice_var~geral_desinteresse+geral_dist_padr)
geral_mod_int_var <- lm(geral_indice_var~geral_desinteresse*geral_dist_padr)
AICtab(geral_mod_var,geral_mod_ad_var,geral_mod_int_var)
summary(geral_mod_int_var)
confint(geral_mod_int_var)



###### Variancia inter
geral_indice_coex <- c(bat2_coex,bat3_coex)
geral_desinteresse_coex <- c(bat2_riqueza_inicial_padr[which(simulacoes_25jul16_output_derivado[,2]==2)],bat3_riqueza_inicial_padr[which(simulacoes_27jul16_output_derivado[,2]==2)])
geral_dist_padr_coex <- c(bat2_indice_dist_padr,bat3_indice_dist_padr)
geral_mod_coex <- lm(geral_indice_coex~geral_desinteresse_coex)
geral_mod_ad_coex <- lm(geral_indice_coex~geral_desinteresse_coex+geral_dist_padr_coex)
geral_mod_int_coex <- lm(geral_indice_coex~geral_desinteresse_coex*geral_dist_padr_coex)
AICtab(geral_mod_coex,geral_mod_ad_coex,geral_mod_int_coex)
summary(geral_mod_int_coex)
confint(geral_mod_int_coex)

bat2_mod_coex <- lm(bat2_coex~bat2_riqueza_inicial_padr[which(simulacoes_25jul16_output_derivado[,2]==2)])
bat2_mod_ad_coex <- lm(bat2_coex~bat2_riqueza_inicial_padr[which(simulacoes_25jul16_output_derivado[,2]==2)]+bat2_indice_dist_padr)
bat2_mod_int_coex <- lm(bat2_coex~bat2_riqueza_inicial_padr[which(simulacoes_25jul16_output_derivado[,2]==2)]*bat2_indice_dist_padr)
AICtab(bat2_mod_coex,bat2_mod_ad_coex,bat2_mod_int_coex)
summary(bat2_mod_int_coex)


########## Tentando plotar variancias

## tentativa de plotar confidence intervals pra bateria 2 individual
preddat <- data.frame(x = seq(min(bat2_indice_dist), max(bat2_indice_dist), length = 1000))
preds <- predict(bat2_res_norm_dist_lin_glm, newdata = preddat, se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
fit2 <- mod$family$linkinv(fit)
upr2 <- mod$family$linkinv(upr)
lwr2 <- mod$family$linkinv(lwr)
lines(bat2_indice_dist,upr,col="black",lty=5)
lines(bat2_indice_dist,lwr,col="black")