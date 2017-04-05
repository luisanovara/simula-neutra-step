# MODELOS REAL OFICIAL (?)
load("~/Documents/LABTROP_LET/R/dados_mestrado/dados_mestrado.RData")

#### SEPARANDO DIST EM FREQ E INT

############################ MEDIA ############################

# modelo mais complexo
complexo_media <- lm(media~mutacao+riqueza_inicial_fator+mutacao:riqueza_inicial_fator+dist_freq_scale+dist_freq_scale:mutacao+dist_freq_scale:riqueza_inicial_fator+dist_freq_scale:riqueza_inicial_fator:mutacao+dist_int_scale+dist_int_scale:mutacao+dist_int_scale:riqueza_inicial_fator+dist_int_scale:riqueza_inicial_fator:mutacao+dist_int_scale:dist_freq_scale+dist_int_scale:dist_freq_scale:mutacao+dist_int_scale:dist_freq_scale:riqueza_inicial_fator+dist_int_scale:dist_freq_scale:mutacao:riqueza_inicial_fator,data=dados,na.action = "na.fail")

nulo_media <-lm(media~mutacao+riqueza_inicial_fator+
            mutacao:riqueza_inicial_fator,data=dados,na.action = "na.fail")

dist_media <- lm(media~dist_int_scale+dist_freq_scale,data=dados)
dist_interacao_media <- lm(media~dist_int_scale*dist_freq_scale,data=dados)
dist_freq_media <- lm(media~dist_freq_scale,data=dados)
dist_int_media <- lm(media~dist_int_scale,data=dados)

plot(lm(dados$media[dados$bateria==1]~dados$dist_freq_scale[dados$bateria==1]*dados$dist_int_scale[dados$bateria==1]))
plot(glm(I(dados$media[dados$bateria==1]/20000)~dados$dist_freq_scale[dados$bateria==1]*dados$dist_int_scale[dados$bateria==1],family="binomial"(link="logit")))
lm(dados$media[dados$bateria==2]~dados$dist_freq_scale[dados$bateria==2]*dados$dist_int_scale[dados$bateria==2])
glm(I(dados$media[dados$bateria==2]/20000)~dados$dist_freq_scale[dados$bateria==2]*dados$dist_int_scale[dados$bateria==2],family="binomial"(link="logit"))

#install.packages("MuMIn")
library(MuMIn)
tab_modelo_media <- dredge(complexo_media,rank = "AIC", fixed = c("mutacao","riqueza_inicial_fator","mutacao:riqueza_inicial_fator"))
lista_mod_media <- get.models(tab_modelo_media, subset = NA)
length(lista_mod_media)
lista_mod_media[[1]]

mod_selecionado <- lm(media~mutacao+riqueza_inicial_fator+mutacao:riqueza_inicial_fator+dist_freq_scale+dist_freq_scale:mutacao+dist_freq_scale:riqueza_inicial_fator+dist_int_scale+dist_int_scale:mutacao+dist_int_scale:riqueza_inicial_fator+dist_int_scale:dist_freq_scale+dist_int_scale:dist_freq_scale:mutacao+dist_int_scale:dist_freq_scale:riqueza_inicial_fator,data=dados,na.action = "na.fail")
plot(mod_selecionado)

mod_selecionado_gama_inverse <- glm(media~mutacao+riqueza_inicial_fator+mutacao:riqueza_inicial_fator+dist_freq_scale+dist_freq_scale:mutacao+dist_freq_scale:riqueza_inicial_fator+dist_int_scale+dist_int_scale:mutacao+dist_int_scale:riqueza_inicial_fator+dist_int_scale:dist_freq_scale+dist_int_scale:dist_freq_scale:mutacao+dist_int_scale:dist_freq_scale:riqueza_inicial_fator,data=dados,na.action = "na.fail",family="Gamma"(link="inverse"))
#mod_selecionado_gama_identity <- glm(media~mutacao+riqueza_inicial_fator+mutacao:riqueza_inicial_fator+dist_freq_scale+dist_freq_scale:mutacao+dist_freq_scale:riqueza_inicial_fator+dist_int_scale+dist_int_scale:mutacao+dist_int_scale:riqueza_inicial_fator+dist_int_scale:dist_freq_scale+dist_int_scale:dist_freq_scale:mutacao+dist_int_scale:dist_freq_scale:riqueza_inicial_fator,data=dados,na.action = "na.fail",family="Gamma"(link="identity"))
mod_selecionado_binomial_logit <- glm(I(media/20000)~mutacao+riqueza_inicial_fator+mutacao:riqueza_inicial_fator+dist_freq_scale+dist_freq_scale:mutacao+dist_freq_scale:riqueza_inicial_fator+dist_int_scale+dist_int_scale:mutacao+dist_int_scale:riqueza_inicial_fator+dist_int_scale:dist_freq_scale+dist_int_scale:dist_freq_scale:mutacao+dist_int_scale:dist_freq_scale:riqueza_inicial_fator,data=dados,na.action = "na.fail",family="binomial"(link="logit"))
plot(mod_selecionado_binomial_logit)

mod_selecionado_comquadrado <- lm(media~mutacao+riqueza_inicial_fator+mutacao:riqueza_inicial_fator+dist_freq_scale+dist_freq_scale:mutacao+dist_freq_scale:riqueza_inicial_fator+dist_int_scale+dist_int_scale:mutacao+dist_int_scale:riqueza_inicial_fator+dist_int_scale:dist_freq_scale+dist_int_scale:dist_freq_scale:mutacao+dist_int_scale:dist_freq_scale:riqueza_inicial_fator+I(dist_int_scale^2)+I(dist_freq_scale^2),data=dados,na.action = "na.fail")


AICtab(mod_selecionado_binomial_probit,mod_selecionado_binomial_logit,mod_selecionado_gama_inverse,mod_selecionado,mod_selecionado_comquadrado )

complexo_media_sem_intercepto <- lm(media~mutacao+riqueza_inicial_fator+mutacao:riqueza_inicial_fator+dist_freq_scale+dist_freq_scale:mutacao+dist_freq_scale:riqueza_inicial_fator+dist_freq_scale:riqueza_inicial_fator:mutacao+dist_int_scale+dist_int_scale:mutacao+dist_int_scale:riqueza_inicial_fator+dist_int_scale:riqueza_inicial_fator:mutacao+dist_int_scale:dist_freq_scale+dist_int_scale:dist_freq_scale:mutacao+dist_int_scale:dist_freq_scale:riqueza_inicial_fator+dist_int_scale:dist_freq_scale:mutacao:riqueza_inicial_fator-1,data=dados[1001:4000,],na.action = "na.fail")
tab_modelo_media_sem_intercepto <- dredge(complexo_media_sem_intercepto,rank = "AIC", fixed = c("mutacao","riqueza_inicial_fator","mutacao:riqueza_inicial_fator"))
lista_mod_media_sem_intercepto <- get.models(tab_modelo_media_sem_intercepto, subset = NA)
length(lista_mod_media_sem_intercepto)
lista_mod_media_sem_intercepto[[1]]

############################ VAR_TOTAL ############################
colnames(dados)

# modelo mais complexo
complexo_var_total <- lm(var_total~mutacao+
                           riqueza_inicial_fator+
                           mutacao:riqueza_inicial_fator+
                           dist_freq_scale+
                           dist_freq_scale:mutacao+
                           dist_freq_scale:riqueza_inicial_fator+
                           dist_freq_scale:riqueza_inicial_fator:mutacao+
                           dist_int_scale+
                           dist_int_scale:mutacao+
                           dist_int_scale:riqueza_inicial_fator+
                           dist_int_scale:riqueza_inicial_fator:mutacao+
                           dist_int_scale:dist_freq_scale+
                           dist_int_scale:dist_freq_scale:mutacao+
                           dist_int_scale:dist_freq_scale:riqueza_inicial_fator+
                           dist_int_scale:dist_freq_scale:mutacao:riqueza_inicial_fator+
                           dist_int_scale:dist_int_scale+
                           dist_freq_scale:dist_freq_scale,data=dados,na.action = "na.fail")

nulo_var_total <-lm(var_total~mutacao+riqueza_inicial_fator+
           mutacao:riqueza_inicial_fator,data=dados,na.action = "na.fail")

tab_modelo_var_total <- dredge(complexo_var_total,rank = "AIC", fixed = c("mutacao","riqueza_inicial_fator","mutacao:riqueza_inicial_fator"))
lista_mod_var_total <- get.models(tab_modelo_var_total, subset = NA)
length(lista_mod_var_total)
lista_mod_var_total[[1]]
plot(lista_mod_var_total[[1]])


############################ var total sem as comunidades espremidas ############################
summary(sqrt(dados$var_total[dados$media[dados$bateria==1|dados$bateria==3]<=12000 & dados$media[dados$bateria==1|dados$bateria==3]>=8000])) #conclusao: 50 eh uma boa medida para o dp medio das comunidades centrais
dados_truncados <- dados[-(dados$media<=3*50|dados$media>(20000-(3*50))),]

# modelo mais complexo
complexo_var_total_truncados <- lm(var_total~mutacao+
                           riqueza_inicial_fator+
                           mutacao:riqueza_inicial_fator+
                           dist_freq_scale+
                           dist_freq_scale:mutacao+
                           dist_freq_scale:riqueza_inicial_fator+
                           dist_freq_scale:riqueza_inicial_fator:mutacao+
                           dist_int_scale+
                           dist_int_scale:mutacao+
                           dist_int_scale:riqueza_inicial_fator+
                           dist_int_scale:riqueza_inicial_fator:mutacao+
                           dist_int_scale:dist_freq_scale+
                           dist_int_scale:dist_freq_scale:mutacao+
                           dist_int_scale:dist_freq_scale:riqueza_inicial_fator+
                           dist_int_scale:dist_freq_scale:mutacao:riqueza_inicial_fator+
                           dist_int_scale:dist_int_scale+
                           dist_freq_scale:dist_freq_scale,data=dados_truncados,na.action = "na.fail")

nulo_var_total_truncados <-lm(var_total~mutacao+riqueza_inicial_fator+
                      mutacao:riqueza_inicial_fator,data=dados_truncados,na.action = "na.fail")

tab_modelo_var_total_truncados <- dredge(complexo_var_total_truncados,rank = "AIC", fixed = c("mutacao","riqueza_inicial_fator","mutacao:riqueza_inicial_fator"))
lista_mod_var_total_truncados <- get.models(tab_modelo_var_total_truncados, subset = NA)
length(lista_mod_var_total_truncados)
plot(lista_mod_var_total_truncados[[1]])

############################ VAR INTER ############################
colnames(dados)
dados$var_inter[2001:4000][is.na(dados$var_inter[2001:4000])] <- 0

# modelo mais complexo
complexo_var_inter <- lm(var_inter~mutacao+
                           #riqueza_inicial_fator+
                           #mutacao:riqueza_inicial_fator+
                           dist_freq_scale+
                           dist_freq_scale:mutacao+
                           #dist_freq_scale:riqueza_inicial_fator+
                           #dist_freq_scale:riqueza_inicial_fator:mutacao+
                           dist_int_scale+
                           dist_int_scale:mutacao+
                           #dist_int_scale:riqueza_inicial_fator+
                           #dist_int_scale:riqueza_inicial_fator:mutacao+
                           dist_int_scale:dist_freq_scale+
                           dist_int_scale:dist_freq_scale:mutacao+
                           #dist_int_scale:dist_freq_scale:riqueza_inicial_fator+
                           #dist_int_scale:dist_freq_scale:mutacao:riqueza_inicial_fator+
                           dist_int_scale:dist_int_scale+
                           dist_freq_scale:dist_freq_scale,data=dados[2001:4000,],na.action = "na.fail")

nulo_var_inter <-lm(var_inter~mutacao,data=dados[2001:4000,],na.action = "na.fail")

tab_modelo_var_inter <- dredge(complexo_var_inter,rank = "AIC", fixed = c("mutacao"))
lista_mod_var_inter <- get.models(tab_modelo_var_inter, subset = NA)
length(lista_mod_var_inter)
plot(lista_mod_var_inter[[1]])

#### bat 1
lm()
