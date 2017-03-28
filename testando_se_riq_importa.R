# testando se... 

load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

## ... a riqueza entre 5-500 importa (apenas dentro dos cenarios 2 e 3)...
### ...para a media
media_mod1 <- lm(dados$media[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000])
media_mod2 <- lm(dados$media[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]+dados$riqueza_inicial_scale[1001:3000])
media_mod3 <- lm(dados$media[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]*dados$riqueza_inicial_scale[1001:3000])
### ...para o ss total
ss_total_mod1 <- lm(dados$ss_total[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000])
ss_total_mod2 <- lm(dados$ss_total[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]+dados$riqueza_inicial_scale[1001:3000])
ss_total_mod3 <- lm(dados$ss_total[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]*dados$riqueza_inicial_scale[1001:3000])
### ...para o ss inter
ss_inter_mod1 <- lm(dados$ss_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000])
ss_inter_mod2 <- lm(dados$ss_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]+dados$riqueza_inicial_scale[1001:3000])
ss_inter_mod3 <- lm(dados$ss_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]*dados$riqueza_inicial_scale[1001:3000])
### ...para a var inter
var_inter_mod1 <- lm(dados$var_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000])
var_inter_mod2 <- lm(dados$var_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]+dados$riqueza_inicial_scale[1001:3000])
var_inter_mod3 <- lm(dados$var_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]*dados$riqueza_inicial_scale[1001:3000])
### AIC
require(bbmle)
AICtab(media_mod1, media_mod2, media_mod3) # nao, nao importa
AICtab(ss_total_mod1, ss_total_mod2, ss_total_mod3) # da pra falar que nao, nao importa
AICtab(ss_inter_mod1, ss_inter_mod2, ss_inter_mod3) # sim, importa!
AICtab(var_inter_mod1, var_inter_mod2, var_inter_mod3) # sim, importa!


## ... a riqueza final importa (apenas dentro dos cenarios 2 e 3)...
### ...para a media
media_mod4 <- lm(dados$media[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000])
media_mod5 <- lm(dados$media[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]+dados$riqueza_final[1001:3000])
media_mod6 <- lm(dados$media[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]*dados$riqueza_final[1001:3000])
### ...para o ss total
ss_total_mod4 <- lm(dados$ss_total[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000])
ss_total_mod5 <- lm(dados$ss_total[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]+dados$riqueza_final[1001:3000])
ss_total_mod6 <- lm(dados$ss_total[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]*dados$riqueza_final[1001:3000])
### ...para o ss inter
ss_inter_mod4 <- lm(dados$ss_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000])
ss_inter_mod5 <- lm(dados$ss_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]+dados$riqueza_final[1001:3000])
ss_inter_mod6 <- lm(dados$ss_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]*dados$riqueza_final[1001:3000])
### ...para a var inter
var_inter_mod4 <- lm(dados$var_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000])
var_inter_mod5 <- lm(dados$var_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]+dados$riqueza_final_scale[1001:3000])
var_inter_mod6 <- lm(dados$var_inter[1001:3000]~dados$dist_indice_scale[1001:3000]*dados$bateria[1001:3000]*dados$riqueza_final_scale[1001:3000])
### AIC
require(bbmle)
AICtab(media_mod4, media_mod5, media_mod6) # sim, importa
AICtab(ss_total_mod4, ss_total_mod5, ss_total_mod6) # sim, importa
AICtab(ss_inter_mod4, ss_inter_mod5, ss_inter_mod6) # sim, importa
AICtab(var_inter_mod4, var_inter_mod5, var_inter_mod6) # sim, importa


## ... a estrategia de vida inicial importa (apenas para o cenario 1)...
### ...para a media
media_mod7 <- lm(dados$media[1:1000]~dados$dist_indice_scale[1:1000])
media_mod8 <- lm(dados$media[1:1000]~dados$dist_indice_scale[1:1000]+dados$estrat_inicial_scale[1:1000])
media_mod9 <- lm(dados$media[1:1000]~dados$dist_indice_scale[1:1000]*dados$estrat_inicial_scale[1:1000])
### ...para o ss total
ss_total_mod7 <- lm(dados$ss_total[1:1000]~dados$dist_indice_scale[1:1000])
ss_total_mod8 <- lm(dados$ss_total[1:1000]~dados$dist_indice_scale[1:1000]+dados$estrat_inicial_scale[1:1000])
ss_total_mod9 <- lm(dados$ss_total[1:1000]~dados$dist_indice_scale[1:1000]*dados$estrat_inicial_scale[1:1000])
### AIC
require(bbmle)
AICtab(media_mod7, media_mod8, media_mod9) # sim, importa
AICtab(ss_total_mod7, ss_total_mod8, ss_total_mod9) # nao, nao importa
