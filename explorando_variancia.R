# comunidade com 10 especies com amplitude de 1 a 91
com10 <- rep(seq(1,100,10),each=10)
com10_sstotal<- sum((com10-mean(com10))^2)
com10_gltotal <- 99
com10_vartotal<- com10_sstotal/com10_gltotal
com10_var <- var(com10)
com10_coef_var <- sqrt(var(com10))/mean(com10)
com10_medias_grupos_replicadas <- rep(as.vector(tapply(com10,as.factor(com10),mean)),each=10)
com10_ssintra <- sum((com10-com10_medias_grupos_replicadas)^2)
com10_glintra <- com10_gltotal - 9
com10_varintra <- com10_ssintra/com10_glintra
com10_ssentre <- com10_sstotal-com10_ssintra
com10_glentre <-com10_gltotal-com10_glintra
com10_varentre <- com10_ssentre/com10_glentre
com10_varentre_relativa <- com10_varentre/com10_var

# comunidade com 2 especies com amplitude de 1 a 91
com2 <- c(rep(1,50),rep(91,50))
com2_sstotal<- sum((com2-mean(com2))^2)
com2_gltotal <- 99
com2_vartotal<- com2_sstotal/com2_gltotal
com2_var <- var(com2)
com2_coef_var <- sqrt(var(com2))/mean(com2)
com2_medias_grupos_replicadas <- rep(as.vector(tapply(com2,as.factor(com2),mean)),each=50)
com2_ssintra <- sum((com2-com2_medias_grupos_replicadas)^2)
com2_glintra <- com2_gltotal - 1
com2_varintra <- com2_ssintra/com2_glintra
com2_ssentre <- com2_sstotal-com2_ssintra
com2_glentre <-com2_gltotal-com2_glintra
com2_varentre <- com2_ssentre/com2_glentre
com2_varentre_relativa <- com2_varentre/com2_var

# comunidade com 3 especies com amplitude de 1 a 91
com3 <- c(rep(1,33),rep(46,34),rep(91,33))
com3_sstotal<- sum((com3-mean(com3))^2)
com3_gltotal <- 99
com3_vartotal<- com3_sstotal/com3_gltotal
com3_var <- var(com3)
com3_coef_var <- sqrt(var(com3))/mean(com3)
com3_medias_grupos_replicadas <- rep(as.vector(tapply(com3,as.factor(com3),mean)),times=c(33,34,33))
com3_ssintra <- sum((com3-com3_medias_grupos_replicadas)^2)
com3_glintra <- com3_gltotal - 2
com3_varintra <- com3_ssintra/com3_glintra
com3_ssentre <- com3_sstotal-com3_ssintra
com3_glentre <-com3_gltotal-com3_glintra
com3_varentre <- com3_ssentre/com3_glentre
com3_varentre_relativa <- com3_varentre/com3_var

# comunidade com 8 especies sorteadas da amplitude de 1 a 91
sample(c(1,11,21,31,41,51,61,71,81,91),8) # 1, 21, 31, 41, 51, 61, 81, 91
com8_sort <- c(rep(1,12),rep(21,13),rep(31,12),rep(41,13),rep(51,12),rep(61,13),rep(81,12),rep(91,13))
com8_sort_sstotal<- sum((com8_sort-mean(com8_sort))^2)
com8_sort_gltotal <- 99
com8_sort_vartotal<- com8_sort_sstotal/com8_sort_gltotal
com8_sort_var <- var(com8_sort)
com8_sort_coef_var <- sqrt(var(com8_sort))/mean(com8_sort)
com8_sort_medias_grupos_replicadas <- rep(as.vector(tapply(com8_sort,as.factor(com8_sort),mean)),times=c(12,13,12,13,12,13,12,13))
com8_sort_ssintra <- sum((com8_sort-com8_sort_medias_grupos_replicadas)^2)
com8_sort_glintra <- com8_sort_gltotal - 7
com8_sort_varintra <- com8_sort_ssintra/com8_sort_glintra
com8_sort_ssentre <- com8_sort_sstotal-com8_sort_ssintra
com8_sort_glentre <-com8_sort_gltotal-com8_sort_glintra
com8_sort_varentre <- com8_sort_ssentre/com8_sort_glentre
com8_sort_varentre_relativa <- com8_sort_varentre/com8_sort_var

# comunidade com 3 especies sorteadas da amplitude de 1 a 91
sample(c(1,11,21,31,41,51,61,71,81,91),3) # 1, 61, 81
com3_sort <- c(rep(1,33),rep(61,34),rep(81,33))
com3_sort_sstotal<- sum((com3_sort-mean(com3_sort))^2)
com3_sort_gltotal <- 99
com3_sort_vartotal<- com3_sort_sstotal/com3_sort_gltotal
com3_sort_var <- var(com3_sort)
com3_sort_coef_var <- sqrt(var(com3_sort))/mean(com3_sort)
com3_sort_medias_grupos_replicadas <- rep(as.vector(tapply(com3_sort,as.factor(com3_sort),mean)),times=c(33,34,33))
com3_sort_ssintra <- sum((com3_sort-com3_sort_medias_grupos_replicadas)^2)
com3_sort_glintra <- com3_sort_gltotal - 2
com3_sort_varintra <- com3_sort_ssintra/com3_sort_glintra
com3_sort_ssentre <- com3_sort_sstotal-com3_sort_ssintra
com3_sort_glentre <-com3_sort_gltotal-com3_sort_glintra
com3_sort_varentre <- com3_sort_ssentre/com3_sort_glentre
com3_sort_varentre_relativa <- com3_sort_varentre/com3_sort_var

# comunidade com 3 especies sorteadas da amplitude de 1 a 91... 2
sample(c(1,11,21,31,41,51,61,71,81,91),3) # 51, 71, 91
com3_linha_sort <- c(rep(51,33),rep(71,34),rep(91,33))
com3_linha_sort_sstotal<- sum((com3_linha_sort-mean(com3_linha_sort))^2)
com3_linha_sort_gltotal <- 99
com3_linha_sort_vartotal<- com3_linha_sort_sstotal/com3_linha_sort_gltotal
com3_linha_sort_var <- var(com3_linha_sort)
com3_linha_sort_coef_var <- sqrt(var(com3_linha_sort))/mean(com3_linha_sort)
com3_linha_sort_medias_grupos_replicadas <- rep(as.vector(tapply(com3_linha_sort,as.factor(com3_linha_sort),mean)),times=c(33,34,33))
com3_linha_sort_ssintra <- sum((com3_linha_sort-com3_linha_sort_medias_grupos_replicadas)^2)
com3_linha_sort_glintra <- com3_linha_sort_gltotal - 2
com3_linha_sort_varintra <- com3_linha_sort_ssintra/com3_linha_sort_glintra
com3_linha_sort_ssentre <- com3_linha_sort_sstotal-com3_linha_sort_ssintra
com3_linha_sort_glentre <-com3_linha_sort_gltotal-com3_linha_sort_glintra
com3_linha_sort_varentre <- com3_linha_sort_ssentre/com3_linha_sort_glentre
com3_linha_sort_varentre_relativa <- com3_linha_sort_varentre/com3_linha_sort_var

# comunidade com 2 especies sorteadas da amplitude de 1 a 91
sample(c(1,11,21,31,41,51,61,71,81,91),2) # 11, 51
com2_sort <- c(rep(11,50),rep(51,50))
com2_sort_sstotal<- sum((com2_sort-mean(com2_sort))^2)
com2_sort_gltotal <- 99
com2_sort_vartotal<- com2_sort_sstotal/com2_sort_gltotal
com2_sort_var <- var(com2_sort)
com2_sort_coef_var <- sqrt(var(com2_sort))/mean(com2_sort)
com2_sort_medias_grupos_replicadas <- rep(as.vector(tapply(com2_sort,as.factor(com2_sort),mean)),times=c(50,50))
com2_sort_ssintra <- sum((com2_sort-com2_sort_medias_grupos_replicadas)^2)
com2_sort_glintra <- com2_sort_gltotal - 1
com2_sort_varintra <- com2_sort_ssintra/com2_sort_glintra
com2_sort_ssentre <- com2_sort_sstotal-com2_sort_ssintra
com2_sort_glentre <-com2_sort_gltotal-com2_sort_glintra
com2_sort_varentre <- com2_sort_ssentre/com2_sort_glentre
com2_sort_varentre_relativa <- com2_sort_varentre/com2_sort_var

# comunidade com 2 especies sorteadas da amplitude de 1 a 91.... 2
sample(c(1,11,21,31,41,51,61,71,81,91),2) # 1, 51
com2_linha_sort <- c(rep(1,50),rep(51,50))
com2_linha_sort_sstotal<- sum((com2_linha_sort-mean(com2_linha_sort))^2)
com2_linha_sort_gltotal <- 99
com2_linha_sort_vartotal<- com2_linha_sort_sstotal/com2_linha_sort_gltotal
com2_linha_sort_var <- var(com2_linha_sort)
com2_linha_sort_coef_var <- sqrt(var(com2_linha_sort))/mean(com2_linha_sort)
com2_linha_sort_medias_grupos_replicadas <- rep(as.vector(tapply(com2_linha_sort,as.factor(com2_linha_sort),mean)),times=c(50,50))
com2_linha_sort_ssintra <- sum((com2_linha_sort-com2_linha_sort_medias_grupos_replicadas)^2)
com2_linha_sort_glintra <- com2_linha_sort_gltotal - 1
com2_linha_sort_varintra <- com2_linha_sort_ssintra/com2_linha_sort_glintra
com2_linha_sort_ssentre <- com2_linha_sort_sstotal-com2_linha_sort_ssintra
com2_linha_sort_glentre <-com2_linha_sort_gltotal-com2_linha_sort_glintra
com2_linha_sort_varentre <- com2_linha_sort_ssentre/com2_linha_sort_glentre
com2_linha_sort_varentre_relativa <- com2_linha_sort_varentre/com2_linha_sort_var

### A variancia interespecifica relativa eh sempre igual quando o numero de especies eh o mesmo (mesmo que a variancia interespecifica "absoluta" seja diferente), ja a variancia interespecifica "absoluta", para um mesmo numero de especies, eh maior quanto maior a distancia entre as especies. Quanto maior o numero de especies, menor a variancia inter relativa. A variancia interespecifica "absoluta" tambem parece diminuir quando se aumenta o numero de especies, ainda que haja variacoes.
# Logo, me parece melhor nao usar a variancia inter relativa, e sim a variancia inter absoluta, para que seja possivel visualizar o efeito da maior ou menor distancia entre elas. E para nao haver influencia do numero de especies, podemos acrescentar riqueza final da comunidade como uma covariavel.