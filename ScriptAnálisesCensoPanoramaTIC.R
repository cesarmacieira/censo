####===============================================
#### Trabalho Censo - Construção do banco de dados
####===============================================
####=============================
#### Preparando o R para análise
####=============================
rm(list=ls(all=T))#Limpar ambiente/histórico
tryCatch({setwd("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/censo")},
         error = function(e) { setwd("D:/NESCON/Trabalho - Censo/censo") })

####=================================
#### Instalando e carregando pacotes
####=================================
if(!require(openxlsx)){ install.packages('openxlsx'); require(openxlsx)}#Ler e exportar excel
if(!require(purrr)){ install.packages('purrr'); require(purrr)}#Programação funcional
if(!require(tidyverse)){ install.packages('tidyverse'); require(tidyverse)}#Manipulação de dados
if(!require(stringi)){ install.packages('stringi'); require(stringi)}
if(!require(haven)){ install.packages("haven"); require(haven)}

####=========
#### Funções
####=========
TabelaGLMLogistica = function(modelo,casasdecimaisExpB=F){
  options(OutDec=",")
  if(casasdecimaisExpB == F){
    Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                        "β" = summary(modelo)$coefficients[,1],
                        "Exp β" = exp(summary(modelo)$coefficients[,1]),
                        "Alteração" = (exp(summary(modelo)$coefficients[,1]) - 1),
                        "I.C." = paste0("[",round(exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2]),3),"; ",
                                        round(exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2]),3),"]"),
                        "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2])-1)*100,2),"%; ",
                                                    +                       round((exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2])-1)*100,2),"%]"),
                        "Valor-p" = round(summary(modelo)$coefficients[,4],4))
  }else{
    Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                        "β" = summary(modelo)$coefficients[,1],
                        "Exp β" = round(exp(summary(modelo)$coefficients[,1]),casasdecimaisExpB),
                        "Alteração" = (exp(summary(modelo)$coefficients[,1]) - 1),
                        "I.C." = paste0("[",round(exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2]),casasdecimaisExpB),"; ",
                                        round(exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2]),casasdecimaisExpB),"]"),
                        "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2])-1)*100,casasdecimaisExpB),"%; ",
                                                    round((exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2])-1)*100,casasdecimaisExpB),"%]"),
                        "Valor-p" = round(summary(modelo)$coefficients[,4],4))
  }
  return(Tabela)
}

DescritivaCat = function(x){
  tabela = cbind(table(x), prop.table(table(x)))
  colnames(tabela) = c('Freq. Absoluta (N)', 'Freq. Relativa (%)')
  return(tabela)
}

DescritivaNum = function(x, more = F) {
  stats = list();
  clean.x = x[!is.na(x)]
  stats$N_validos = round(length(clean.x),3)
  stats$Média = round(mean(clean.x),3)
  stats$Var = round(var(clean.x),3)
  stats$D.P = round(sd(clean.x),3)
  stats$Mín. = round(min(clean.x),3)
  stats$Q1 = round(fivenum(clean.x)[2],3)
  stats$Q2 = round(fivenum(clean.x)[3],3)
  stats$Q3 = round(fivenum(clean.x)[4],3)
  stats$Máx. = round(max(clean.x),3)
  t1 = unlist(stats)
  names(t1) = c('N','Média','Variância','D.P.','Mínimo','1ºQ','2ºQ','3ºQ','Máximo')
  t1
}

basic.stats = function(x, more = F) {
  stats = list()
  clean.x = x[!is.na(x)]
  stats$N_validos = round(length(clean.x),3)
  stats$Média = round(mean(clean.x),3)
  stats$Var = round(var(clean.x),3)
  stats$D.P = round(sd(clean.x),3)
  stats$E.P = round(sd(clean.x)/sqrt(length(clean.x)),3)
  stats$Min = round(min(clean.x),3)
  stats$Q1 = round(fivenum(clean.x)[2],3)
  stats$Q2 = round(fivenum(clean.x)[3],3)
  stats$Q3 = round(fivenum(clean.x)[4],3)
  stats$Max = round(max(clean.x),3)
  t1 = unlist(stats)
  names(t1) = c('N válidos', 'Média', 'Variância', 'D.P.', 'E.P.', 'Mínimo', '1ºQ', '2ºQ', '3ºQ', 'Máximo')
  t1
}

DescritivaNumMais2Grupos = function(y, z, more = F){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  colnames(tab)= c('N válidos', 'Média', 'Variância', 'D.P.', 'E.P.', 'Mínimo', '1ºQ', '2ºQ', '3ºQ', 'Máximo')
  rownames(tab)= levels(factor(z))
  tab
}

QuiQuadrado_Fisher = function(x, y, type.sum, teste){
  t0 = table(x, y)
  if(type.sum==2) {
    t1 = prop.table(t0, 2)
  } else {
    t1 = prop.table(t0, 1)
  }
  colnames(t0) = paste0('X', 1:dim(t0)[2])
  colnames(t1) = paste0('X', 1:dim(t1)[2])
  t2_aux = cbind(t0, t1)
  t3 = t2_aux[, order(colnames(t2_aux))]
  colnames(t3) = c(rep(c('N', '%'), dim(t3)[2]/2))
  if(teste=='chisq') {
    Valor_p = chisq.test(t0)$p.value
  }
  if(teste=='fisher') {
    Valor_p = fisher.test(t0)$p.value
  } 
  if(teste=='chisq.simulate'){
    Valor_p = chisq.test(t0, simulate.p.value=TRUE, B=10000)$p.value
  }
  t4 = cbind(t3, Valor_p)
  return(t4)
}

QuiQuadrado_Fisher = function(x, y, type.sum, teste){
  t0 = table(x, y)
  t0_filtered = t0[rowSums(t0) > 0, colSums(t0) > 0, drop = FALSE]
  if(type.sum == 2) {
    t1 = prop.table(t0, 2)
  } else {
    t1 = prop.table(t0, 1)
  }
  colnames(t0) = paste0('X', 1:dim(t0)[2])
  colnames(t1) = paste0('X', 1:dim(t1)[2])
  t2_aux = cbind(t0, t1)
  t3 = t2_aux[, order(colnames(t2_aux))]
  colnames(t3) = c(rep(c('N', '%'), dim(t3)[2]/2))
  if(nrow(t0_filtered) > 0 && ncol(t0_filtered) > 0) {
    if(teste == 'chisq') {
      Valor_p = chisq.test(t0_filtered)$p.value
    }
    if(teste == 'fisher') {
      Valor_p = fisher.test(t0_filtered)$p.value
    } 
    if(teste == 'chisq.simulate'){
      Valor_p = chisq.test(t0_filtered, simulate.p.value=TRUE, B=10000)$p.value
    }
  }
  t4 = cbind(t3, Valor_p)
  return(t4)
}

KruskalTeste = function(y, z, more = F){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  p_valor = rep(kruskal.test(y~factor(z))$p.value, length(levels(factor(z))))
  tab = cbind(tab, p_valor)
  colnames(tab)= c('N válidos', 'Média', 'Variância', 'D.P.', 'E.P.', 'Mínimo', '1ºQ', '2ºQ', '3ºQ', 'Máximo', 'Valor-p')
  rownames(tab)= levels(factor(z))
  if(!require(PMCMRplus)){ install.packages('PMCMRplus'); require(PMCMRplus) }
  #CM = posthoc.kruskal.nemenyi.test(y ~ factor(z), dist='Chisq')$p.value
  CM = kwAllPairsNemenyiTest(y ~ factor(z), dist='Chisquare')$p.value
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

FriedmanTeste = function(y, z, id, more = F){
  dados = data.frame(y = y, grupos = z, id = id)
  dados_agg = dados %>% select(y,grupos,id) %>% group_by(grupos,id) %>%
    summarize(media = mean(y, na.rm = TRUE))
  tab = matrix(NA, length(levels(factor(dados_agg$grupos))), 10)
  for(i in 1:length(levels(factor(dados_agg$grupos)))){ 
    desc = tapply(dados_agg$media, factor(dados_agg$grupos),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  p_valor = rep(friedman.test(media ~ grupos | id, data = dados_agg)$p.value, length(levels(factor(dados_agg$grupos))))
  tab = cbind(tab, p_valor)
  colnames(tab)= c('N válidos', 'Média', 'Variância', 'D.P.', 'E.P.', 'Mínimo', '1ºQ', '2ºQ', '3ºQ', 'Máximo', 'Valor-p')
  rownames(tab)= levels(factor(dados_agg$grupos))
  dados_CM = dados_agg %>% na.omit()
  if(!require(PMCMRplus)){ install.packages('PMCMRplus'); require(PMCMRplus) }
  #CM = pairwise.wilcox.test(dados_CM$media, factor(dados_CM$grupos), p.adjust.method = 'bonferroni')$p.value
  CM = frdAllPairsConoverTest(y = dados_CM$media, groups = dados_CM$grupos, 
                              blocks = dados_CM$id, p.adjust.method = 'none')$p.value
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

MannWhitney = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = wilcox.test(y ~ x, exact=FALSE)$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c('N válidos', 'Média', 'Variância', 'D.P.', 'E.P.', 'Mínimo', '1ºQ', '2ºQ', '3ºQ', 'Máximo','Valor-p')
  return(tab)
}

WilcoxonDependente = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = wilcox.test(y ~ x, exact=FALSE, paired = TRUE, alternative = 'two.sided')$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c('N válidos', 'Média', 'Variância', 'D.P.', 'E.P.', 'Mínimo', '1ºQ', '2ºQ', '3ºQ', 'Máximo','Valor-p')
  return(tab)
}

AnovaIndepTeste = function(y, z, CM_teste = 'bonferroni', more = FALSE){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  anova_result = summary(aov(y ~ factor(z)))
  p_valor_anova = anova_result[[1]]$'Pr(>F)'[1]
  #CM = pairwise.t.test(y, factor(z), p.adjust.method = 'bonferroni')$p.value
  if(CM_teste == 'tukey') {
    CM = TukeyHSD(aov(y ~ factor(z)))$`factor(z)`
  } else if(CM_teste == 'bonferroni') {
    if(!require(PMCMRplus)){ 
      install.packages('PMCMRplus')
      require(PMCMRplus) 
    }
    CM = pairwise.t.test(y, factor(z), p.adjust.method = 'bonferroni')$p.value
  }
  tab = cbind(tab, p_valor_anova)
  colnames(tab)= c('N válidos', 'Média', 'Variância', 'D.P.', 'E.P.', 'Mínimo', '1ºQ', '2ºQ', '3ºQ', 'Máximo', 'Valor-p_ANOVA')
  rownames(tab)= levels(factor(z))
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

AnovaDepTeste = function(y, z, unid_amostral, CM_teste = 'tukey', more = FALSE){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  anova_result = aov(y ~ factor(z) + Error(factor(unid_amostral)), data = data.frame(y, z, unid_amostral))
  p_valor_anova = summary(anova_result)[[1]]$'Pr(>F)'[1]
  #CM = pairwise.t.test(y, factor(z), p.adjust.method = 'bonferroni')$p.value
  if(CM_teste == 'tukey') {
    CM = TukeyHSD(aov(y ~ factor(z)))$`factor(z)`
  } else if(CM_teste == 'bonferroni') {
    if(!require(PMCMRplus)){install.packages('PMCMRplus'); require(PMCMRplus)}
    CM = pairwise.t.test(y, factor(z), p.adjust.method = 'bonferroni')$p.value
  }
  tab = cbind(tab, p_valor_anova)
  colnames(tab)= c('N válidos', 'Média', 'Variância', 'D.P.', 'E.P.', 'Mínimo', '1ºQ', '2ºQ', '3ºQ', 'Máximo', 'Valor-p_ANOVA')
  rownames(tab)= levels(factor(z))
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

TesteTpareado = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = t.test(y ~ x, exact = FALSE, paired = TRUE, alternative = 'two.sided')$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c('N válidos', 'Média', 'Variância', 'D.P.', 'E.P.', 'Mínimo', '1ºQ', '2ºQ', '3ºQ', 'Máximo', 'Valor-p')
  return(tab)
}

TesteTindep = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = t.test(y ~ x, exact = FALSE)$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c('N válidos', 'Média', 'Variância', 'D.P.', 'E.P.', 'Mínimo', '1ºQ', '2ºQ', '3ºQ', 'Máximo','Valor-p')
  return(tab)
}

TesteDeNormalidade = function(x){
  if(!require(dgof)){ install.packages('dgof'); require(dgof)}#Teste de Kolmogorov-Smirnov
  if(!require(nortest)){ install.packages('nortest'); require(nortest)}#Anderson-Darling
  AndersonDarling = round(ad.test(x)$p.value,3)
  KolmogorovSmirnov = round(ks.test(x, 'pnorm', mean(x, na.rm = T), sd(x, na.rm = T))$p.value,3)
  Lilliefors = round(lillie.test(x)$p.value,3)
  CramerVonMises = round(cvm.test(x)$p.value,3)
  if(length(x) > 5000){
    ShapiroWilk = 'N > 5000'
    ShapiroFrancia = 'N > 5000'
  }else{
    ShapiroWilk = shapiro.test(x)$p.value
    ShapiroFrancia = sf.test(x)$p.value   
  }
  tabela = cbind(AndersonDarling,KolmogorovSmirnov,Lilliefors,CramerVonMises,
                 ShapiroWilk,ShapiroFrancia)
  colnames(tabela) = c('Anderson-Darling','Kolmogorov-Smirnov','Lilliefors','Cramer Von Mises','Shapiro-Wilk','Shapiro Francia')
  #row.names(tabela) = x
  return(tabela)
}

####=============================
#### Carregando o banco de dados 
####=============================
dados_originais = tryCatch({arrow::read_parquet("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/censo/44938_censo2024 06_08_25.parquet")},
                           error = function(e) {arrow::read_parquet("D:/NESCON/Trabalho - Censo/censo/44938_censo2024 06_08_25.parquet")})
#write.xlsx(dados %>% as.data.frame(), 'dados Censo 07-08-2025.xlsx')
dados_originais = dados_originais %>% filter(V362 != '.')

dados_rm = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/censo/Variáveis de interesse - Sem IPEA.xlsx", sheet = 2)},
                 error = function(e) {read.xlsx("D:/NESCON/Trabalho - Censo/censo/Variáveis de interesse - Sem IPEA.xlsx", sheet = 2)}) %>% 
  distinct(.keep_all = TRUE)

cob_esf = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/censo/Cobertura ESF.xlsx", sheet = 1)},
                    error = function(e) {read.xlsx("D:/NESCON/Trabalho - Censo/censo/Cobertura ESF.xlsx", sheet = 1)}) 
leitos = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/censo/Número de leitos.xlsx", sheet = 1)},
                  error = function(e) {read.xlsx("D:/NESCON/Trabalho - Censo/censo/Número de leitos.xlsx", sheet = 1)}) 
planos_saude = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/censo/Planos de saúde.xlsx", sheet = 1)},
                        error = function(e) {read.xlsx("D:/NESCON/Trabalho - Censo/censo/Planos de saúde.xlsx", sheet = 1)}) 
populacao = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/censo/População.xlsx", sheet = 1)},
                     error = function(e) {read.xlsx("D:/NESCON/Trabalho - Censo/censo/População.xlsx", sheet = 1)}) 
ivs = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/censo/IVS.xlsx", sheet = 1)},
               error = function(e) {read.xlsx("D:/NESCON/Trabalho - Censo/censo/IVS.xlsx", sheet = 1)}) 
gini = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/censo/Gini.xlsx", sheet = 1)},
               error = function(e) {read.xlsx("D:/NESCON/Trabalho - Censo/censo/Gini.xlsx", sheet = 1)}) 

####=====================
#### Tratamento de dados
####=====================
dados_originais = dados_originais %>% rename(ivs_censo = ivs)
df = as.data.frame(lapply(dados_originais, function(col) { 
  attributes(col) <- NULL 
  return(col)}), stringsAsFactors = FALSE)
df1 = df %>% mutate(across(everything(), ~ na_if(as.character(.), 'NA'))) 
df2 = df1 %>% 
  mutate(V321 = as.numeric(V321),
         V322 = as.numeric(V322),
         V323 = as.numeric(V323),
         V324 = as.numeric(V324),
         V325 = as.numeric(V325),
         V326 = as.numeric(V326))

df2$Regiao = 
  factor(case_when(df2$V17 == "MG" ~ "Sudeste", df2$V17 == "SP" ~ "Sudeste", 
                   df2$V17 == "RJ" ~ "Sudeste", df2$V17 == "ES" ~ "Sudeste", 
                   df2$V17 == "AM" ~ "Norte", df2$V17 == "AC" ~ "Norte", 
                   df2$V17 == "RO" ~ "Norte", df2$V17 == "RR" ~ "Norte", 
                   df2$V17 == "AP" ~ "Norte", df2$V17 == "PA" ~ "Norte", 
                   df2$V17 == "TO" ~ "Norte", df2$V17 == "MA" ~ "Nordeste", 
                   df2$V17 == "PI" ~ "Nordeste", df2$V17 == "RN" ~ "Nordeste", 
                   df2$V17 == "CE" ~ "Nordeste", df2$V17 == "BA" ~ "Nordeste", 
                   df2$V17 == "PE" ~ "Nordeste", df2$V17 == "AL" ~ "Nordeste", 
                   df2$V17 == "SE" ~ "Nordeste", df2$V17 == "PB" ~ "Nordeste", 
                   df2$V17 == "GO" ~ "Centro-Oeste", df2$V17 == "MT" ~ "Centro-Oeste", 
                   df2$V17 == "MS" ~ "Centro-Oeste", df2$V17 == "DF" ~ "Centro-Oeste", 
                   df2$V17 == "SC" ~ "Sul", df2$V17 == "PR" ~ "Sul", 
                   df2$V17 == "RS" ~ "Sul"), c('Norte','Nordeste','Sudeste','Sul','Centro-Oeste'))

df2$V7esf = as.numeric(df2$V7esf)
df2$V7eap = as.numeric(df2$V7eap)
df2 = df2 %>% mutate(V7esf_eap = rowSums(across(c(V7esf, V7eap)), na.rm = TRUE))
df2$V7esf_eap_cat = ifelse(df2$V7esf_eap >= 7,'7 ou mais',df2$V7esf_eap)

df2$V321cat = factor(case_when( (df2$V7esf_eap <= 1) & (df2$V321 >= 0  & df2$V321 <= 1) ~ 'Péssimo',
                                  (df2$V7esf_eap <= 1) & (df2$V321 >= 2  & df2$V321 <= 4) ~ 'Ruim',
                                  (df2$V7esf_eap <= 1) & (df2$V321 >= 5  & df2$V321 <= 7) ~ 'Regular',
                                  (df2$V7esf_eap <= 1) & (df2$V321 >= 8  & df2$V321 <= 9) ~ 'Bom',
                                  (df2$V7esf_eap <= 1) & (df2$V321 >= 10) ~ 'Ótimo',
                                  
                                  (df2$V7esf_eap == 2) & (df2$V321 >= 0  & df2$V321 <= 2) ~ 'Péssimo',
                                  (df2$V7esf_eap == 2) & (df2$V321 >= 3  & df2$V321 <= 7) ~ 'Ruim',
                                  (df2$V7esf_eap == 2) & (df2$V321 >= 8  & df2$V321 <= 10) ~ 'Regular',
                                  (df2$V7esf_eap == 2) & (df2$V321 >= 11  & df2$V321 <= 14) ~ 'Bom',
                                  (df2$V7esf_eap == 2) & (df2$V321 >= 15) ~ 'Ótimo',
                                  
                                  (df2$V7esf_eap == 3) & (df2$V321 >= 0  & df2$V321 <= 3) ~ 'Péssimo',
                                  (df2$V7esf_eap == 3) & (df2$V321 >= 4  & df2$V321 <= 10) ~ 'Ruim',
                                  (df2$V7esf_eap == 3) & (df2$V321 == 11) ~ 'Regular',
                                  (df2$V7esf_eap == 3) & (df2$V321 >= 12  & df2$V321 <= 20) ~ 'Bom',
                                  (df2$V7esf_eap == 3) & (df2$V321 >= 21) ~ 'Ótimo',
                                  
                                  
                                  (df2$V7esf_eap == 4) & (df2$V321 >= 0  & df2$V321 <= 4) ~ 'Péssimo',
                                  (df2$V7esf_eap == 4) & (df2$V321 >= 5  & df2$V321 <= 13) ~ 'Ruim',
                                  (df2$V7esf_eap == 4) & (df2$V321 == 14) ~ 'Regular',
                                  (df2$V7esf_eap == 4) & (df2$V321 >= 15  & df2$V321 <= 26) ~ 'Bom',
                                  (df2$V7esf_eap == 4) & (df2$V321 >= 27) ~ 'Ótimo',
                                  
                                  
                                  (df2$V7esf_eap == 5) & (df2$V321 >= 0  & df2$V321 <= 5) ~ 'Péssimo',
                                  (df2$V7esf_eap == 5) & (df2$V321 >= 6  & df2$V321 <= 16) ~ 'Ruim',
                                  (df2$V7esf_eap == 5) & (df2$V321 == 17) ~ 'Regular',
                                  (df2$V7esf_eap == 5) & (df2$V321 >= 18  & df2$V321 <= 32) ~ 'Bom',
                                  (df2$V7esf_eap == 5) & (df2$V321 >= 33) ~ 'Ótimo',
                                  
                                  (df2$V7esf_eap == 6) & (df2$V321 >= 0  & df2$V321 <= 6) ~ 'Péssimo',
                                  (df2$V7esf_eap == 6) & (df2$V321 >= 7  & df2$V321 <= 19) ~ 'Ruim',
                                  (df2$V7esf_eap == 6) & (df2$V321 == 20) ~ 'Regular',
                                  (df2$V7esf_eap == 6) & (df2$V321 >= 21  & df2$V321 <= 38) ~ 'Bom',
                                  (df2$V7esf_eap == 6) & (df2$V321 >= 39) ~ 'Ótimo',
                                  
                                  (df2$V7esf_eap >= 7) & (df2$V321 >= 0  & df2$V321 <= 7) ~ 'Péssimo',
                                  (df2$V7esf_eap >= 7) & (df2$V321 >= 8  & df2$V321 <= 22) ~ 'Ruim',
                                  (df2$V7esf_eap >= 7) & (df2$V321 == 23) ~ 'Regular',
                                  (df2$V7esf_eap >= 7) & (df2$V321 >= 24  & df2$V321 <= 45) ~ 'Bom',
                                  (df2$V7esf_eap >= 7) & (df2$V321 >= 45) ~ 'Ótimo'), c('Péssimo','Ruim','Regular','Bom','Ótimo'))

df2$V325cat = case_when(df2$V325 == 0 ~ 'Ausente',
                          df2$V325 >= 1 ~ 'Presente')

df2$V324cat = case_when(df2$V324 == 0 ~ 'Ausente',
                          df2$V324 >= 1 ~ 'Presente')

df2$V86 = as.numeric(df2$V86)
df2$V323 = as.numeric(df2$V323)
df2$V86cat = factor(case_when(df2$V86 >= 0 & df2$V86 <= 3 ~ "0 a 3",
                                df2$V86 >= 4 & df2$V86 <= 7 ~ "4 a 7",
                                df2$V86 >= 8 & df2$V86 <= 11 ~ "8 a 11",
                                df2$V86 >= 12 & df2$V86 <= 15 ~ "12 a 15",
                                df2$V86 >= 16 & df2$V86 <= 19 ~ "16 a 19",
                                df2$V86 >= 20 & df2$V86 <= 23 ~ "20 a 23",
                                df2$V86 >= 24 & df2$V86 <= 27 ~ "24 a 27",
                                df2$V86 >= 28 ~ "28 ou mais"), c("0 a 3","4 a 7","8 a 11","12 a 15","16 a 19","20 a 23","24 a 27","28 ou mais"))

df2$V323cat = factor(case_when( (df2$V86cat == "0 a 3") & (df2$V323 == 0) ~ 'Péssimo',
                                  (df2$V86cat == "0 a 3") & (df2$V323 == 1) ~ 'Ruim',
                                  (df2$V86cat == "0 a 3") & (df2$V323 == 2) ~ 'Regular',
                                  (df2$V86cat == "0 a 3") & (df2$V323 == 3) ~ 'Bom',
                                  (df2$V86cat == "0 a 3") & (df2$V323 >= 4) ~ 'Ótimo',
                                  
                                  (df2$V86cat == "4 a 7") & (df2$V323 == 0) ~ 'Péssimo',
                                  (df2$V86cat == "4 a 7") & (df2$V323 == 1) ~ 'Ruim',
                                  (df2$V86cat == "4 a 7") & (df2$V323 == 2) ~ 'Regular',
                                  (df2$V86cat == "4 a 7") & (df2$V323 == 3) ~ 'Bom',
                                  (df2$V86cat == "4 a 7") & (df2$V323 >= 4) ~ 'Ótimo',
                                  
                                  (df2$V86cat == "8 a 11") & (df2$V323 == 0) ~ 'Péssimo',
                                  (df2$V86cat == "8 a 11") & (df2$V323 >= 1  & df2$V323 <= 3) ~ 'Ruim',
                                  (df2$V86cat == "8 a 11") & (df2$V323 == 4) ~ 'Regular',
                                  (df2$V86cat == "8 a 11") & (df2$V323 >= 5  & df2$V323 <= 7) ~ 'Bom',
                                  (df2$V86cat == "8 a 11") & (df2$V323 >= 8) ~ 'Ótimo',
                                  
                                  (df2$V86cat == "12 a 15") & (df2$V323 >= 0  & df2$V323 <= 2) ~ 'Péssimo',
                                  (df2$V86cat == "12 a 15") & (df2$V323 >= 3  & df2$V323 <= 5) ~ 'Ruim',
                                  (df2$V86cat == "12 a 15") & (df2$V323 >= 6  & df2$V323 <= 7) ~ 'Regular',
                                  (df2$V86cat == "12 a 15") & (df2$V323 >= 8  & df2$V323 <= 10) ~ 'Bom',
                                  
                                  (df2$V86cat == "16 a 19") & (df2$V323 >= 0  & df2$V323 <= 3) ~ 'Péssimo',
                                  (df2$V86cat == "16 a 19") & (df2$V323 >= 4  & df2$V323 <= 7) ~ 'Ruim',
                                  (df2$V86cat == "16 a 19") & (df2$V323 >= 8  & df2$V323 <= 10) ~ 'Regular',
                                  (df2$V86cat == "16 a 19") & (df2$V323 >= 11  & df2$V323 <= 14) ~ 'Bom',
                                  (df2$V86cat == "16 a 19") & (df2$V323 >= 15) ~ 'Ótimo',
                                  
                                  (df2$V86cat == "20 a 23") & (df2$V323 >= 0  & df2$V323 <= 4) ~ 'Péssimo',
                                  (df2$V86cat == "20 a 23") & (df2$V323 >= 5  & df2$V323 <= 9) ~ 'Ruim',
                                  (df2$V86cat == "20 a 23") & (df2$V323 >= 10  & df2$V323 <= 14) ~ 'Regular',
                                  (df2$V86cat == "20 a 23") & (df2$V323 >= 15  & df2$V323 <= 18) ~ 'Bom',
                                  (df2$V86cat == "20 a 23") & (df2$V323 >= 19) ~ 'Ótimo',
                                  
                                  (df2$V86cat == "24 a 27") & (df2$V323 >= 0  & df2$V323 <= 5) ~ 'Péssimo',
                                  (df2$V86cat == "24 a 27") & (df2$V323 >= 6  & df2$V323 <= 11) ~ 'Ruim',
                                  (df2$V86cat == "24 a 27") & (df2$V323 >= 12  & df2$V323 <= 16) ~ 'Regular',
                                  (df2$V86cat == "24 a 27") & (df2$V323 >= 17  & df2$V323 <= 21) ~ 'Bom',
                                  (df2$V86cat == "24 a 27") & (df2$V323 >= 22) ~ 'Ótimo',
                                  
                                  (df2$V86cat == "28 ou mais") & (df2$V323 >= 0  & df2$V323 <= 6) ~ 'Péssimo',
                                  (df2$V86cat == "28 ou mais") & (df2$V323 >= 7  & df2$V323 <= 13) ~ 'Ruim',
                                  (df2$V86cat == "28 ou mais") & (df2$V323 >= 14  & df2$V323 <= 16) ~ 'Regular',
                                  (df2$V86cat == "28 ou mais") & (df2$V323 >= 17  & df2$V323 <= 24) ~ 'Bom',
                                  (df2$V86cat == "28 ou mais") & (df2$V323 >= 25) ~ 'Ótimo'), c('Péssimo','Ruim','Regular','Bom','Ótimo'))

df2 = df2 %>% mutate(V2537_num = case_when(V2537 == 'Não' ~ 0,V2537 == 'Sim' ~ 1),
                         V2577_num = case_when(V2567 == 'Não' ~ 0,V2567 == 'Sim' ~ 1),
                         V2537_V2567 = rowSums(across(c(V2537_num, V2577_num)), na.rm = TRUE),
                         V2537_V2567_num = ifelse(V2537_V2567 >= 1, 1, 0),
                         V2564_num = case_when(V2564 == 'Não' ~ 0,V2564 == 'Sim' ~ 1),
                         V2565_num = case_when(V2565 == 'Não' ~ 0,V2565 == 'Sim' ~ 1),
                         V2537_V2567_V2564_V2565 = rowSums(across(c(V2537_V2567_num, V2564_num,V2565_num)), na.rm = TRUE),
                         V2537_V2567_V2564_V2565_cat = factor(case_when(V2537_V2567_V2564_V2565 == 0 ~ 'Péssimo/Ruim',
                                                                        V2537_V2567_V2564_V2565 == 1 ~ 'Regular',
                                                                        V2537_V2567_V2564_V2565 == 2 ~ 'Bom',
                                                                        V2537_V2567_V2564_V2565 == 3 ~ 'Ótimo'),c('Péssimo/Ruim','Regular','Bom','Ótimo')))

df2 = df2 %>% mutate(
  V341_num = case_when(V341 == 'Não' ~ 0, V341 == 'Sim' ~ 2),
  V342_num = case_when(V342 == 'Não' ~ 0, V342 == 'Sim' ~ 1),
  V343_num = case_when(V343 == 'Não' ~ 0, V343 == 'Sim' ~ 1),
  V344_num = case_when(V344 == 'Não' ~ 0, V344 == 'Sim' ~ 2),
  V345_num = case_when(V345 == 'Não' ~ 0, V345 == 'Sim' ~ 2),
  V346_num = case_when(V346 == 'Não' ~ 0, V346 == 'Sim' ~ 1),
  V347_num = case_when(V347 == 'Não' ~ 1, V347 == 'Sim' ~ 0),
  V34_num = rowSums(across(c(V341_num, V342_num, V343_num, V344_num, V345_num, V346_num, V347_num)), na.rm = TRUE),
  V34_num_cat = factor(case_when(V34_num <= 1 ~ 'Péssimo',
                                 V34_num >= 2 & V34_num <= 3 ~ 'Ruim',
                                 V34_num >= 4 & V34_num <= 6 ~ 'Regular',
                                 V34_num >= 7 & V34_num <= 8 ~ 'Bom',
                                 V34_num >= 9 ~ 'Ótimo'), levels = c('Péssimo', 'Ruim', 'Regular', 'Bom', 'Ótimo')))

df2 = df2 %>% mutate(V361_num = case_when(V361 == 'Não' ~ 0,V361 == 'Sim' ~ 1),
                         V364_num = case_when(V364 == 'Não' ~ 0,V364 == 'Sim' ~ 1),
                         V361_V364 = rowSums(across(c(V361_num, V364_num)), na.rm = TRUE))

df2 = df2 %>% mutate(V3711_num = case_when(V3711 == 'Não' ~ 0,V3711 == 'Sim' ~ 2),
                         V3712_num = case_when(V3712 == 'Não' ~ 0,V3712 == 'Sim' ~ 1),
                         V3713_num = case_when(V3713 == 'Não' ~ 0,V3713 == 'Sim' ~ 1),
                         V3714_num = case_when(V3714 == 'Não' ~ 0,V3714 == 'Sim' ~ 1),
                         V3715_num = case_when(V3715 == 'Não' ~ 0,V3715 == 'Sim' ~ 1),
                         V3716_num = case_when(V3716 == 'Não' ~ 0,V3716 == 'Sim' ~ 1),
                         V3711_V3712_V3713_V3714_V3715_V3716 = rowSums(across(c(V3711_num,V3712_num,V3713_num,V3714_num,V3715_num,V3716_num)), na.rm = TRUE))
df2$V3711_V3712_V3713_V3714_V3715_V3716cat = ifelse((df2$V37 == 'Não' & df2$V3711_V3712_V3713_V3714_V3715_V3716 != 0), 0, df2$V3711_V3712_V3713_V3714_V3715_V3716)
df2$V3711_V3712_V3713_V3714_V3715_V3716cat5 = 
  factor(case_when(df2$V3711_V3712_V3713_V3714_V3715_V3716 == 0 ~ 'Péssimo',
                   df2$V3711_V3712_V3713_V3714_V3715_V3716 == 1 | df2$V3711_V3712_V3713_V3714_V3715_V3716 == 2 ~ 'Ruim',
                   df2$V3711_V3712_V3713_V3714_V3715_V3716 == 3 | df2$V3711_V3712_V3713_V3714_V3715_V3716 == 4 ~ 'Regular',
                   df2$V3711_V3712_V3713_V3714_V3715_V3716 == 5 | df2$V3711_V3712_V3713_V3714_V3715_V3716 == 6 ~ 'Bom',
                   df2$V3711_V3712_V3713_V3714_V3715_V3716 == 7 ~ 'Ótimo'), c('Péssimo','Ruim','Regular','Bom','Ótimo'))

df2 = df2 %>% mutate(V3741_num = case_when(V3741 == 'Não' ~ 0,V3741 == 'Sim' ~ 1),
                         V3742_num = case_when(V3742 == 'Não' ~ 0,V3742 == 'Sim' ~ 1),
                         V3743_num = case_when(V3743 == 'Não' ~ 0,V3743 == 'Sim' ~ 1),
                         V3744_num = case_when(V3744 == 'Não' ~ 0,V3744 == 'Sim' ~ 1),
                         V3745_num = case_when(V3745 == 'Não' ~ 0,V3745 == 'Sim' ~ 1),
                         V3746_num = case_when(V3746 == 'Não' ~ 0,V3746 == 'Sim' ~ 1),
                         V3747_num = case_when(V3747 == 'Não' ~ 0,V3747 == 'Sim' ~ 1),
                         V3748_num = case_when(V3748 == 'Não' ~ 0,V3748 == 'Sim' ~ 1),
                         V3749_num = case_when(V3749 == 'Não' ~ 0,V3749 == 'Sim' ~ 1),
                         V37410_num = case_when(V37410 == 'Não' ~ 0,V37410 == 'Sim' ~ 1),
                         V374_num = rowSums(across(c(V3741_num,V3742_num,V3743_num,V3744_num,V3745_num,V3746_num,
                                                     V3747_num,V3748_num,V3749_num,V37410_num)), na.rm = TRUE))
df2$V374_cat = ifelse((df2$V37 == 'Não' & df2$V374_num != 0), 0, df2$V374_num)
df2$V374_cat = as.numeric(df2$V374_cat)
df2$V374_cat5 = 
  factor(case_when(df2$V374_cat == 0 ~ 'Péssimo/Ruim',
                   df2$V374_cat >= 1 & df2$V374_cat <= 2 ~ 'Regular',
                   df2$V374_cat >= 3 & df2$V374_cat <= 7 ~ 'Bom',
                   df2$V374_cat >= 8 ~ 'Ótimo'), c('Péssimo/Ruim','Regular','Bom','Ótimo'))

df2 = df2 %>% mutate(V911_num = case_when(V911 == 'Não' ~ 0,V911 == 'Sim' ~ 1),
                         V912_num = case_when(V912 == 'Não' ~ 0,V912 == 'Sim' ~ 1),
                         V913_num = case_when(V913 == 'Não' ~ 0,V913 == 'Sim' ~ 2),
                         V914_num = case_when(V914 == 'Não' ~ 0,V914 == 'Sim' ~ 1),
                         V915_num = case_when(V915 == 'Não' ~ 0,V915 == 'Sim' ~ 2),
                         V916_num = case_when(V916 == 'Não' ~ 0,V916 == 'Sim' ~ 2),
                         V917_num = case_when(V917 == 'Não' ~ 0,V917 == 'Sim' ~ 1),
                         V918_num = case_when(V918 == 'Não' ~ 0,V918 == 'Sim' ~ 2),
                         V919_num = case_when(V919 == 'Não' ~ 0,V919 == 'Sim' ~ 2),
                         
                         V91_num = rowSums(across(c(V911_num,V912_num,V913_num,V914_num,V915_num,V916_num,
                                                    V917_num,V918_num,V919_num)), na.rm = TRUE),
                         V91_num_cat = factor(case_when(V91_num <= 1 ~ 'Péssimo',
                                                        V91_num >= 2 & V91_num <= 4 ~ 'Ruim',
                                                        V91_num >= 5 & V91_num <= 8 ~ 'Regular',
                                                        V91_num >= 9 & V91_num <= 11 ~ 'Bom',
                                                        V91_num >= 12 ~ 'Ótimo'), levels = c('Péssimo', 'Ruim', 'Regular', 'Bom', 'Ótimo')))

df2 = df2 %>% mutate(V1031_num = case_when(V1031 == 'Não' ~ 0,V1031 == 'Sim' ~ 1),
                         V1032_num = case_when(V1032 == 'Não' ~ 0,V1032 == 'Sim' ~ 1),
                         V1033_num = case_when(V1033 == 'Não' ~ 0,V1033 == 'Sim' ~ 2),
                         V1034_num = case_when(V1034 == 'Não' ~ 0,V1034 == 'Sim' ~ 2),
                         V1035_num = case_when(v1035 == 'Não' ~ 0,v1035 == 'Sim' ~ 2),
                         V1036_num = case_when(v1036 == 'Não' ~ 0,v1036 == 'Sim' ~ 2),
                         V1037_num = case_when(v1037 == 'Não' ~ 0,v1037 == 'Sim' ~ 1), 
                         V103_num = rowSums(across(c(V1031_num,V1032_num,V1033_num,V1034_num,V1035_num,V1036_num,V1037_num)), na.rm = TRUE),
                         V103_num_imp = ifelse(is.na(V103_num) == TRUE, 0, V103_num),
                         V103_num_cat = factor(case_when(V103_num_imp <= 1 ~ 'Péssimo',
                                                         V103_num_imp >= 2 & V103_num_imp <= 3 ~ 'Ruim',
                                                         V103_num_imp >= 4 & V103_num_imp <= 6 ~ 'Regular',
                                                         V103_num_imp >= 7 & V103_num_imp <= 8 ~ 'Bom',
                                                         V103_num_imp >= 9 ~ 'Ótimo'), levels = c('Péssimo', 'Ruim', 'Regular', 'Bom', 'Ótimo')))

df2 = df2 %>% mutate(v1063_num = case_when(v1063 == 'Não' ~ 0,v1063 == 'Sim' ~ 1),
                         v1064_num = case_when(v1064 == 'Não' ~ 0,v1064 == 'Sim' ~ 1),
                         v1065_num = case_when(v1065 == 'Não' ~ 0,v1065 == 'Sim' ~ 1),
                         v1066_num = case_when(v1066 == 'Não' ~ 0,v1066 == 'Sim' ~ 1),
                         v1065_v1064_v1065_v1066 = rowSums(across(c(v1063_num,v1064_num,v1065_num,v1066_num)), na.rm = TRUE))
df2$v1065_v1064_v1065_v1066cat = 
  factor(case_when(df2$v1065_v1064_v1065_v1066 == 0 ~ 'Péssimo/Ruim',
                   df2$v1065_v1064_v1065_v1066 == 1 ~ 'Regular',
                   df2$v1065_v1064_v1065_v1066 == 2 | df2$v1065_v1064_v1065_v1066 == 3 ~ 'Bom',
                   df2$v1065_v1064_v1065_v1066 == 4 ~ 'Ótimo'), c('Péssimo/Ruim','Regular','Bom','Ótimo'))

df2$v12026_cat = ifelse(df2$v120 == 'Não', 'Não', df2$v12026)
df2$v12026_cat_imp = ifelse(is.na(df2$v12026_cat) == TRUE, 'Não', df2$v12026_cat)
df2 = df2 %>% mutate(v1263_num = case_when(v1263 == 'Não' ~ 0,v1263 == 'Sim' ~ 1),
                         v1264_num = case_when(v1264 == 'Não' ~ 0,v1264 == 'Sim' ~ 1),
                         v1265_num = case_when(v1263 == 'Não' ~ 0,v1265 == 'Sim' ~ 1),
                         v1266_num = case_when(v1263 == 'Não' ~ 0,v1266 == 'Sim' ~ 1),
                         v1267_num = case_when(v1264 == 'Não' ~ 0,v1267 == 'Sim' ~ 1),
                         v1268_num = case_when(v1263 == 'Não' ~ 0,v1268 == 'Sim' ~ 1),
                         v1269_num = case_when(v1263 == 'Não' ~ 0,v1269 == 'Sim' ~ 1),
                         v126_num = rowSums(across(c(v1263_num,v1264_num,v1265_num,v1266_num,v1267_num,v1268_num,v1269_num)), na.rm = TRUE))
df2$v126_cat = 
  factor(case_when(df2$v126_num == 0 | df2$v12616 == 'Sim' ~ 'Péssimo',
                   df2$v126_num == 1 | df2$v126_num == 2 ~ 'Ruim',
                   df2$v126_num == 3 | df2$v126_num == 4 ~ 'Regular',
                   df2$v126_num == 5 | df2$v126_num == 6 ~ 'Bom',
                   df2$v126_num >= 7 ~ 'Ótimo'), c('Péssimo','Ruim','Regular','Bom','Ótimo'))
df2$V33 = factor(df2$V33, c('Não tem acesso à internet',
                                'Possui acesso à Internet, mas funciona de maneira inadequada (quedas e instabilidades frequentes)',
                                'Possui acesso à Internet adequado para a execução das atividades'))

numerico = function(x){
  return(case_when(x == 'Péssimo' ~ 1,
                   x == 'Ruim' ~ 2,
                   x == 'Regular' ~ 3,
                   x == 'Bom' ~ 4,
                   x == 'Ótimo' ~ 5))
}

df2 = df2 %>% mutate(V321cat_num = numerico(V321cat),
                         V325cat_num = case_when(V325cat == 'Ausente' ~ 1, V325cat == 'Presente' ~ 5),
                         V324cat_num = case_when(V324cat == 'Ausente' ~ 1, V324cat == 'Presente' ~ 5),
                         V323cat_num = numerico(V323cat),
                         V2537_V2567_V2564_V2565_cat_num = case_when(V2537_V2567_V2564_V2565_cat == 'Péssimo/Ruim' ~ 1,
                                                                     V2537_V2567_V2564_V2565_cat == 'Regular' ~ 3,
                                                                     V2537_V2567_V2564_V2565_cat == 'Bom' ~ 4,
                                                                     V2537_V2567_V2564_V2565_cat == 'Ótimo' ~ 5),
                         V34_cat_num = numerico(V34_num_cat),
                         V358_num = case_when(V358 == 'Não' ~ 1, V358 == 'Sim' ~ 5),
                         V361_V364_num = case_when(V361_V364 == 2 ~ 5, V361_V364 == 1 ~ 3, V361_V364 == 0 ~ 1),
                         V37_num = case_when(V37 == 'Não' ~ 1, V37 == 'Sim' ~ 5),
                         V3711_V3712_V3713_V3714_V3715_V3716cat5_num = numerico(V3711_V3712_V3713_V3714_V3715_V3716cat5),
                         V374_cat5_num = case_when(V374_cat5 == 'Péssimo/Ruim' ~ 1,
                                                   V374_cat5 == 'Regular' ~ 3,
                                                   V374_cat5 == 'Bom' ~ 4,
                                                   V374_cat5 == 'Ótimo' ~ 5),
                         V91cat_num = numerico(V91_num_cat),
                         V103cat_num = numerico(V103_num_cat), 
                         v1065_v1064_v1065_v1066cat_num = case_when(v1065_v1064_v1065_v1066cat == 'Péssimo/Ruim' ~ 1,
                                                                    v1065_v1064_v1065_v1066cat == 'Regular' ~ 3,
                                                                    v1065_v1064_v1065_v1066cat == 'Bom' ~ 4,
                                                                    v1065_v1064_v1065_v1066cat == 'Ótimo' ~ 5),
                         v12026_num =  case_when(v12026_cat_imp == 'Não' ~ 1, v12026_cat_imp == 'Sim' ~ 5),
                         v126_cat_num = numerico(v126_cat),
                         V33_num = case_when(V33 == 'Possui acesso à Internet adequado para a execução das atividades' ~ 5,
                                             V33 == 'Possui acesso à Internet, mas funciona de maneira inadequada (quedas e instabilidades frequentes)' ~ 3,
                                             V33 == 'Não tem acesso à internet' ~ 1),
                         Indicador_TIC = rowSums(across(c(V321cat_num,V325cat_num,V324cat_num,V323cat_num,V2537_V2567_V2564_V2565_cat_num,
                                                      V34_cat_num,V358_num,V361_V364_num,V37_num,V3711_V3712_V3713_V3714_V3715_V3716cat5_num,V374_cat5_num,
                                                      V91cat_num,V103cat_num,v1065_v1064_v1065_v1066cat_num,v12026_num,v126_cat_num,V33_num)), na.rm = TRUE),
                         Indicador_TIC_cat = factor(case_when(Indicador_TIC <= 25 ~ 'Péssimo',
                                                          Indicador_TIC > 25 & Indicador_TIC <= 35 ~ 'Ruim',
                                                          Indicador_TIC > 35 & Indicador_TIC <= 45 ~ 'Regular',
                                                          Indicador_TIC > 45 & Indicador_TIC <= 60 ~ 'Bom',
                                                          Indicador_TIC > 60 ~ 'Ótimo'), c('Péssimo','Ruim','Regular','Bom','Ótimo')
                         )
)

df2$V323_bin = ifelse(df2$V323 > 0, 1, df2$V323)
df2$V321_bin = ifelse(df2$V321 > 0, 1, df2$V321)

criar_indicador = function(df, variaveis, nome_indicador) {
  alvo_num = unique(vapply(variaveis, function(v) if (grepl("num$", v)) v else paste0(v, "num"), character(1)))
  recode_vars = unique(variaveis[!grepl("num$", variaveis) & !paste0(variaveis, "num") %in% names(df)])
  if (length(recode_vars) > 0) {
    df = df %>% mutate(across(all_of(recode_vars), ~recode(.,"Sim"=1,"Não"=0,.default=NA_real_), .names="{.col}num"))
  }
  cols_present = intersect(alvo_num, names(df))
  message(sprintf("Indicador '%s': %d questões usadas.", nome_indicador, length(cols_present)))  
  df %>% mutate(!!nome_indicador := rowSums(across(any_of(alvo_num)), na.rm=TRUE))
}

df2$porte_populacional = factor(df2$porte_populacional,
                                c("Pequeno Porte I","Pequeno Porte II","Médio Porte","Grande Porte","Metrópole"))

####===============================
#### Junção com dados demográficos
####===============================
leitos_pop = left_join(leitos %>% rename(NLeitos = Quantidade.existente), 
                       populacao %>% select('COD-6D',POPULAÇÃO.ESTIMADA), by = c("COD-6D"="COD-6D")) %>% 
  mutate(PercLeitos = (NLeitos/POPULAÇÃO.ESTIMADA)*100000)

planos_saude_pop = left_join(planos_saude, 
                             populacao %>% select('COD-6D',POPULAÇÃO.ESTIMADA), by = c("IBGE"="COD-6D")) %>% 
  mutate(PercPlanosSaude = Planos.de.saúde/POPULAÇÃO.ESTIMADA)

cob_esf_pop = left_join(cob_esf %>% select(Município,Qt..eSF), 
                        populacao %>% select('COD-6D',POPULAÇÃO.ESTIMADA), by = c("Município"="COD-6D")) %>% 
  mutate(CoberturaESF = (Qt..eSF*3500)/POPULAÇÃO.ESTIMADA)

df3 = left_join(df2, populacao %>% select('COD-6D',POPULAÇÃO.ESTIMADA), by = c("CO_MUNICIPIO_IBGE"="COD-6D"))
df4 = left_join(df3, leitos_pop %>% select(PercLeitos,"COD-6D"), by = c("CO_MUNICIPIO_IBGE"="COD-6D"))
df5 = left_join(df4, planos_saude_pop %>% select(PercPlanosSaude,IBGE), by = c("CO_MUNICIPIO_IBGE"="IBGE"))
df6 = left_join(df5, ivs, by = c("CO_MUNICIPIO_IBGE"="IBGE"))
df7 = left_join(df6, gini %>% mutate(IBGE = as.character(IBGE)), by = c("CO_MUNICIPIO_IBGE"="IBGE"))
dados = left_join(df7, cob_esf_pop %>% select(CoberturaESF,"Município"), by = c("CO_MUNICIPIO_IBGE"="Município"))

####==============
#### Saúde sexual - 19 questões
####==============
vars_saude_sexual = c("V471","V472","V473","V474","V475","V476","V477",
                      "V478","V479","V4710","V4711","V4712","V4713",
                      "V4714","V4715","V4716","V4717","V4718","V4719")
dados1 = criar_indicador(dados, vars_saude_sexual, "IndSaudeSexual")

####==================================
#### Atenção ao Pré-Natal e puerpério - 36 questões
####==================================
vars_pre_natal = c("V481","V482","V483","V484","V485","V486","V487","V488","V489",
                   "V4810","V4811","V4812","V4813","V4814","V4815","V4816","V4817",
                   "V4818","V4819","V4820","V4821","V4822","V4823","V4824","V4825",
                   "V4826","V4827","V4828","V4829","V4830","V4831","v441",#V49,
                   "V50","V51","V52","V53")
dados2 = criar_indicador(dados1, vars_pre_natal, "IndPreNatal")

####=================
#### Saúde da mulher - 8 questões
####=================
dados2$V55num = case_when(dados2$V55 == 'Não' ~ 0,
                          dados2$V55 == 'Sim, para colo do útero' | dados2$V55 == 'Sim, para mama' ~ 0.5,
                          dados2$V55 == 'Sim, para ambos' ~ 1)
vars_saude_mulher = c("V55num","V561","V562","V563","V564","V565","V57","V58")
dados3 = criar_indicador(dados2, vars_saude_mulher, "IndSaudeMulher")

####==================
#### Saúde da criança - 18 questões
####==================
vars_saude_crianca = c("V591","V592","V593","V594","V595","V596","V597",
                       "V598","V599","V5910","V601","V602","V603","V604",
                       "V605","V606","V607","V608")
dados4 = criar_indicador(dados3, vars_saude_crianca, "IndSaudeCrianca")

####=======================
#### Atenção ao hipertenso - 21 questões
####=======================
vars_hipertenso = c("V611","V612","V613","V614","V615","V616","V617",
                    "V618","V619","V6110","V6111","V6112","V6113",
                    "V6114","V6115","V6116","V6117","V6118","V6119",
                    "V6120","V6121")
dados5 = criar_indicador(dados4, vars_hipertenso, "IndHipertenso")

####======================
#### Atenção ao diabético - 24 questões
####======================
vars_diabetico = c("V621","V622","V623","V624","V625","V626","V627",
                   "V628","V629","V6210","V6211","V6212","V6213",
                   "V6214","V6215","V6216","V6217","V6218","V6219",
                   "V6220","V6221","V6222","V6223","V6224")
dados6 = criar_indicador(dados5, vars_diabetico, "IndDiabetico")

####=====================
#### Atenção à obesidade - 15 questões
####=====================
vars_obesidade = c("V631","V632","V633","V634","V635","V636","V637",
                   "V638","V639","V6310","V6311","V6312","V6313",
                   "V6314","V6315")
dados7 = criar_indicador(dados6, vars_obesidade, "IndObesidade")

####==================================
#### Atenção à tuberculose hanseníase - 28 questões
####==================================
vars_tuberculose_hanseniase = c("V641","V642","V643","V644","V645",
                                "V646","V647","V648","V649","V6410",
                                "V6411","V6412","V6413","V6414","V6415",
                                "V651","V652","V653","V654","V655",
                                "V656","V657","V658","V659","V6510",
                                "V6511","V6512","V6513")
dados8 = criar_indicador(dados7, vars_tuberculose_hanseniase, "IndTuberculoseHanseniase")

####===================================
#### Atenção ao sofrimento psicológico - 10 questões
####===================================
vars_sofrimento_psi = c("V661","V662","V663","V664","V665","V666","V667","V668","V669","V6610")
dados9 = criar_indicador(dados8, vars_sofrimento_psi, "IndSofrimentoPsi")

####======================================
#### Atenção à pessoa vítima de violência - 11 questões
####======================================
vars_violencia = c("V671","V672","V673","V674","V675","V676","V677","V678","V679","V6710","V6711")
dados10 = criar_indicador(dados9, vars_violencia, "IndViolencia")

####========================
#### Atenção à pessoa idosa - 13 questões
####========================
vars_idosa = c("V681","V682","V683","V684","V685","V686","V687","V688","V689","V6810","V6811","V6812","V6813")
dados11 = criar_indicador(dados10, vars_idosa, "IndIdosa")

####================
#### Saúde do homem - 9 questões
####================
vars_saude_homem = c("V691","V692","V693","V694","V695","V696","V697","V698","V699")
dados12 = criar_indicador(dados11, vars_saude_homem, "IndSaudeHomem")

####=============================
#### Atenção às pessoas acamados - 25 questões
####=============================
vars_acamadas = c("V701","V702","V703","V704","V705","V706","V707","V708","V709","V7010","V7011","V7012",
                  "V7013","V7014","V7015","V7016","V7017","V7018","V7019","V7020","V7021","V7022",
                  "V7023","V7024","V7025")
dados13 = criar_indicador(dados12, vars_acamadas, "IndPessoasAcamadas")

####====================
#### Ações de vacinação - 22 questões
####====================
dados13$v642num = case_when(dados13$v642 == 'Não' ~ 0,dados13$v642 == 'Sim' ~ 1)
dados13$V7111num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V7111 == 'Não' ~ 0,dados13$V7111 == 'Sim' ~ 1)
dados13$V7112num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V7112 == 'Não' ~ 0,dados13$V7112 == 'Sim' ~ 1)
dados13$V7113num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V7113 == 'Não' ~ 0,dados13$V7113 == 'Sim' ~ 1)
dados13$V7114num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V7114 == 'Não' ~ 0,dados13$V7114 == 'Sim' ~ 1)
dados13$V7115num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V7115 == 'Não' ~ 0,dados13$V7115 == 'Sim' ~ 1)
dados13$V7116num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V7116 == 'Não' ~ 0,dados13$V7116 == 'Sim' ~ 1)
dados13$V7117num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V7117 == 'Não' ~ 0,dados13$V7117 == 'Sim' ~ 1)
dados13$V7118num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V7118 == 'Não' ~ 0,dados13$V7118 == 'Sim' ~ 1)
dados13$V7119num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V7119 == 'Não' ~ 0,dados13$V7119 == 'Sim' ~ 1)
dados13$V71110num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V71110 == 'Não' ~ 0,dados13$V71110 == 'Sim' ~ 1)
dados13$V71111num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V71111 == 'Não' ~ 0,dados13$V71111 == 'Sim' ~ 1)
dados13$V71112num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V71112 == 'Não' ~ 0,dados13$V71112 == 'Sim' ~ 1)
dados13$V71113num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V71113 == 'Não' ~ 0,dados13$V71113 == 'Sim' ~ 1)
dados13$V71114num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V71114 == 'Não' ~ 0,dados13$V71114 == 'Sim' ~ 1)
dados13$V71115num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V71115 == 'Não' ~ 0,dados13$V71115 == 'Sim' ~ 1)
dados13$V71116num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V71116 == 'Não' ~ 0,dados13$V71116 == 'Sim' ~ 1)
dados13$V71117num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V71117 == 'Não' ~ 0,dados13$V71117 == 'Sim' ~ 1)
dados13$V71118num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V71118 == 'Não' ~ 0,dados13$V71118 == 'Sim' ~ 1)
dados13$V712num = case_when(dados13$v642 == 'Não' ~ 0,dados13$V712 == 'Não' ~ 0,dados13$V712 == 'Sim' ~ 1)
dados13$v662num = case_when(dados13$v662 == 'Não' ~ 0,dados13$v662 == 'Sim' ~ 1)
dados13$v663num = case_when(dados13$v663 == 'Não' ~ 0,dados13$v663 == 'Sim' ~ 1)
dados13$v664num = case_when(dados13$v664 == 'Não' ~ 0,dados13$v664 == 'Sim' ~ 1)
dados13$v665num = case_when(dados13$v665 == 'Não' ~ 0,dados13$v665 == 'Sim' ~ 1)
dados13$v666num = case_when(dados13$v666 == 'Não' ~ 0,dados13$v666 == 'Sim' ~ 1)
dados13$v667num = case_when(dados13$v667 == 'Não' ~ 0,dados13$v667 == 'Sim' ~ 1)
dados13$v668num = case_when(dados13$v668 == 'Não' ~ 0,dados13$v668 == 'Sim' ~ 1)
dados13$V728num = case_when(dados13$V728 == 'Não' ~ 0,dados13$V728 == 'Sim' ~ 1)
dados13$V729num = case_when(dados13$V729 == 'Não' ~ 0,dados13$V729 == 'Sim' ~ 1)
dados13$V7210num = case_when(dados13$V7210 == 'Não' ~ 0,dados13$V7210 == 'Sim' ~ 1)

#Removidas: V7113, V7116, V7119, V71112, V71115, V71116, V71117, V71118

vars_acoes_vacinacao = c("v642num","V7111num","V7112num","V7114num","V7115num",
                         "V7117num","V7118num","V71110num",
                         "V71111num","V71113num","V71114num",
                         "V712num",
                         "v662num","v663num","v664num","v665num","v666num","v667num","v668num","V728num","V729num","V7210num")

dados13 %>% select(v642,V7111,V7112,V7113,V7114,V7115) %>% map(DescritivaCat)
dados13 %>% select(v642num,V7111num,V7112num,V7113num,V7114num,V7115num) %>% map(DescritivaCat)

dados13 %>% select(V7116,V7117,V7118,V7119,V71110) %>% map(DescritivaCat)
dados13 %>% select(V7116num,V7117num,V7118num,V7119num,V71110num) %>% map(DescritivaCat)

dados13 %>% select(V71111,V71112,V71113,V71114,V71115) %>% map(DescritivaCat)
dados13 %>% select(V71111num,V71112num,V71113num,V71114num,V71115num) %>% map(DescritivaCat)

dados13 %>% select(V71116,V71117,V71118) %>% map(DescritivaCat)
dados13 %>% select(V71116num,V71117num,V71118num) %>% map(DescritivaCat)

dados13 %>% select(V712,v662,v663,v664,v665,v666,v667,v668,V728,V729,V7210) %>% map(DescritivaCat)
dados13 %>% select(V712num,v662num,v663num,v664num,v665num,v666num,v667num,v668num,V728num,V729num,V7210num) %>% map(DescritivaCat)

dados14 = criar_indicador(dados13, vars_acoes_vacinacao, "IndAcoesVacinacao")
dados14 %>% select(vars_acoes_vacinacao, "IndAcoesVacinacao") %>% as.data.frame()
DescritivaCat(dados14$IndAcoesVacinacao)

####========================================
#### Práticas integrativas e complementares
####========================================
# vars_praticas_int_comp = c("V674","V675","V676","V677","V678","V741","V742","V743","V744","V745",
#                            "V746","V747","V748","V749","V7410","V7411","V7412","V7413","V7414",
#                            "V7415","V7416","V7417","V7418","V7419","V7420","V7421","V7422",
#                            "V7423","V7424","V7425","V7426")
# dados15 = criar_indicador(dados14, vars_praticas_int_comp, "IndPraticasIntComp")
dados15 = dados14

####=====================================
#### Atendimento à urgência e emergência - 33 questões
####=====================================
dados15$v701num = case_when(dados15$v701 == 'Nunca' ~ 0,
                            dados15$v701 == 'Raramente' ~ 1,
                            dados15$v701 == 'Às vezes' ~ 1,
                            dados15$v701 == 'Quase Sempre' ~ 1,
                            dados15$v701 == 'Sempre' ~ 1)
vars_atendimento_urg_emerg = c("v701num","V7511","V7512","V7513","V7514","V7515","V7516","V7517","V7518",
                               "V7519","V75110","V75111","V75112","V75113","V75114","V75115","V75116","V75117",
                               "V75118","V75119","V75120","V761","V762","V763","V764","V765","V766","V767",
                               "V768","V769","V7610","V7611","V7612")
dados16 = criar_indicador(dados15, vars_atendimento_urg_emerg, "IndAtendimentoUrgEmerg")

####=============================================
#### Atendimento programado e demanda espontânea - 15 questões
####=============================================
vars_atendimento_prog_demanda_esp = c("V102","V1031","V1032","V1033","V1034","v1035","v1036","v1037",
                                      "v1038","v1041","v1042","v1043","v1044","v1045","v1046")
dados17 = criar_indicador(dados16, vars_atendimento_prog_demanda_esp, "IndAtendimentoProgDemandaEsp")

####==================================
#### Atendimento à demanda espontânea - 1 questão
####==================================
dados17$v105num = case_when(dados17$v105 == 'Não são reservadas vagas de consultas para atendimento à demanda espontânea.' ~ 0,
                            dados17$v105 == 'Sim, apenas em um turno' ~ 0.5,
                            dados17$v105 == 'Sim, para todos os turnos de funcionamento da UBS' ~ 1)
dados17$IndAtendimentoDemandaEsp = dados17$v105num
dados18 = dados17

####===================
#### Integração da APS - 29 questões
####===================
dados18$v107num = case_when(dados18$v107 == 'Nunca' ~ 0,
                            dados18$v107 == 'Raramente' ~ 0.25,
                            dados18$v107 == 'Algumas vezes' ~ 0.5,
                            dados18$v107 == 'Quase sempre' ~ 0.75,
                            dados18$v107 == 'Sempre' ~ 1)
dados18$v108_01num = case_when(dados18$v108_01 == 'Nunca' ~ 0,
                               dados18$v108_01 == 'Raramente' ~ 0.25,
                               dados18$v108_01 == 'Algumas vezes' ~ 0.5,
                               dados18$v108_01 == 'Quase sempre' ~ 0.75,
                               dados18$v108_01 == 'Sempre' ~ 1)
vars_integracao_aps = c("v1061","v1062","v1063","v1064","v1065","v1066","v1067","v1068",
                        "v1069","v10610","v10611","v107num","v108_01num","v1091","v1092","v1093",
                        "v1094","v1095","v1101","v1102","v1103","v1104","v1105","v1106",
                        "v1107","v1108","v1109","v11010","v11011")
dados19 = criar_indicador(dados18, vars_integracao_aps, "IndIntegracaoAPS")

####========================
#### Regulação Assistencial - 5 questões
####========================
vars_regulacao_assistencial = c("v111","v112","v113","v114","v115_01")
dados20 = criar_indicador(dados19, vars_regulacao_assistencial, "IndRegulacaoAssistencial")

####=======================
#### Cuidado compartilhado - 53 questões
####=======================
dados20$v1201num = case_when(dados20$v120 == 'Não' ~ 0,
                             dados20$v1201 == 'Sem periodicidade definida' ~ 0.25,
                             dados20$v1201 == 'Mensal' ~ 0.5,
                             dados20$v1201 == 'Quinzenal' ~ 0.75,
                             dados20$v1201 == 'Semanal' ~ 1)
vars_cuidado_compartilhado = c("v1171","v1172","v1173","v1174","v1175","v1176","v1177","v1178","v1179","v11710",
                               "v11711","v11712","v11713","v11714","v11715","v11716","v11717","v11718","v11719",
                               "v1181","v1182","v1183","v1184",
                               "v1191","v1192","v1193","v1194","v1195","v1196","v1197","v1198","v1199","v11910",
                               "v11911","v11912","v11913","v11914","v11915","v11916","v11917","v11918","v11919","v11920","v11921","v11922",
                               "v120",
                               "v1201num","v12022","v12023","v12024","v12025","v12026","v121")
dados21 = criar_indicador(dados20, vars_cuidado_compartilhado, "IndCuidadoCompartilhado")

####=================================
#### Categorizar indicadores por UBS
####=================================
add_num = function(vars) {
  ifelse(grepl("num$", vars), vars, paste0(vars, "num"))
}

dados21_ind_ubs = dados21 %>% 
  # select(CO_MUNICIPIO_IBGE,V16,V17,
  #        c(vars_saude_sexual, add_num(vars_saude_sexual)), IndSaudeSexual,
  #        c(vars_pre_natal, add_num(vars_pre_natal)), IndPreNatal,
  #        c(vars_saude_mulher, add_num(vars_saude_mulher)), IndSaudeMulher,
  #        c(vars_saude_crianca, add_num(vars_saude_crianca)), IndSaudeCrianca,
  #        c(vars_hipertenso, add_num(vars_hipertenso)), IndHipertenso,
  #        c(vars_diabetico, add_num(vars_diabetico)), IndDiabetico,
  #        c(vars_obesidade, add_num(vars_obesidade)), IndObesidade,
  #        c(vars_tuberculose_hanseniase, add_num(vars_tuberculose_hanseniase)), IndTuberculoseHanseniase,
  #        c(vars_sofrimento_psi, add_num(vars_sofrimento_psi)), IndSofrimentoPsi,
  #        c(vars_violencia, add_num(vars_violencia)), IndViolencia,
  #        c(vars_idosa, add_num(vars_idosa)), IndIdosa,
  #        c(vars_saude_homem, add_num(vars_saude_homem)), IndSaudeHomem,
  #        c(vars_acamadas, add_num(vars_acamadas)), IndPessoasAcamadas,
  #        c(vars_acoes_vacinacao, add_num(vars_acoes_vacinacao)), IndAcoesVacinacao,
  #        #c(vars_praticas_int_comp, add_num(vars_praticas_int_comp)), IndPraticasIntComp,
  #        c(vars_atendimento_urg_emerg, add_num(vars_atendimento_urg_emerg)), IndAtendimentoUrgEmerg,
  #        c(vars_atendimento_prog_demanda_esp, add_num(vars_atendimento_prog_demanda_esp)), IndAtendimentoProgDemandaEsp,
  #        v105, v105num, IndAtendimentoDemandaEsp,
  #        c(vars_integracao_aps, add_num(vars_integracao_aps)), IndIntegracaoAPS,
  #        c(vars_regulacao_assistencial, add_num(vars_regulacao_assistencial)), IndRegulacaoAssistencial,
  #        c(vars_cuidado_compartilhado, add_num(vars_cuidado_compartilhado)), IndCuidadoCompartilhado) %>%
  mutate(Indicador_Panorama = rowSums(across(c(IndSaudeSexual, IndPreNatal, IndSaudeMulher, IndSaudeCrianca,
                                           IndHipertenso, IndDiabetico, IndObesidade, IndTuberculoseHanseniase,
                                           IndSofrimentoPsi, IndViolencia, IndIdosa, IndSaudeHomem,
                                           IndPessoasAcamadas, IndAcoesVacinacao, #IndPraticasIntComp,
                                           IndAtendimentoUrgEmerg)), na.rm = TRUE),
         Regiao = case_when(V17 %in% c("AC","AM","AP","PA","RO","RR","TO") ~ "Norte",
                            V17 %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE") ~ "Nordeste",
                            V17 %in% c("DF","GO","MT","MS") ~ "Centro-Oeste",
                            V17 %in% c("ES","MG","RJ","SP") ~ "Sudeste",
                            V17 %in% c("PR","RS","SC") ~ "Sul",
                            TRUE ~ NA_character_)) %>% as.data.frame() 
  # group_by(CO_MUNICIPIO_IBGE, Regiao, V16, V17) %>%
  # summarise(IndSaudeSexual = mean(IndSaudeSexual, na.rm = TRUE),
  #           IndPreNatal = mean(IndPreNatal, na.rm = TRUE),
  #           IndSaudeMulher = mean(IndSaudeMulher, na.rm = TRUE),
  #           IndSaudeCrianca = mean(IndSaudeCrianca, na.rm = TRUE),
  #           IndHipertenso = mean(IndHipertenso, na.rm = TRUE),
  #           IndDiabetico = mean(IndDiabetico, na.rm = TRUE),
  #           IndObesidade = mean(IndObesidade, na.rm = TRUE),
  #           IndTuberculoseHanseniase = mean(IndTuberculoseHanseniase, na.rm = TRUE),
  #           IndSofrimentoPsi = mean(IndSofrimentoPsi, na.rm = TRUE),
  #           IndViolencia = mean(IndViolencia, na.rm = TRUE),
  #           IndIdosa = mean(IndIdosa, na.rm = TRUE),
  #           IndSaudeHomem = mean(IndSaudeHomem, na.rm = TRUE),
  #           IndPessoasAcamadas = mean(IndPessoasAcamadas, na.rm = TRUE),
  #           IndAcoesVacinacao = mean(IndAcoesVacinacao, na.rm = TRUE),
  #           #IndPraticasIntComp = mean(IndPraticasIntComp, na.rm = TRUE),
  #           IndAtendimentoUrgEmerg = mean(IndAtendimentoUrgEmerg, na.rm = TRUE),
  #           IndAtendimentoProgDemandaEsp = mean(IndAtendimentoProgDemandaEsp, na.rm = TRUE),
  #           IndAtendimentoDemandaEsp = mean(IndAtendimentoDemandaEsp, na.rm = TRUE),
  #           IndIntegracaoAPS = mean(IndIntegracaoAPS, na.rm = TRUE),
  #           IndRegulacaoAssistencial = mean(IndRegulacaoAssistencial, na.rm = TRUE),
  #           IndCuidadoCompartilhado = mean(IndCuidadoCompartilhado, na.rm = TRUE),
  #           Indicador_Panorama = mean(Indicador_Panorama, na.rm = TRUE))
#write.xlsx(dados21_ind %>% as.data.frame(),'Dados com indicadores agrupados por municípios.xlsx')

DescritivaCat(dados21_ind_ubs$IndSaudeSexual)
DescritivaNum(dados21_ind_ubs$IndSaudeSexual)

DescritivaCat(dados21_ind_ubs$IndPreNatal)
DescritivaNum(dados21_ind_ubs$IndPreNatal)

DescritivaCat(dados21_ind_ubs$IndSaudeMulher)
DescritivaNum(dados21_ind_ubs$IndSaudeMulher)

DescritivaCat(dados21_ind_ubs$IndSaudeCrianca)
DescritivaNum(dados21_ind_ubs$IndSaudeCrianca)

DescritivaCat(dados21_ind_ubs$IndHipertenso)
DescritivaNum(dados21_ind_ubs$IndHipertenso)

DescritivaCat(dados21_ind_ubs$IndDiabetico)
DescritivaNum(dados21_ind_ubs$IndDiabetico)

DescritivaCat(dados21_ind_ubs$IndObesidade)
DescritivaNum(dados21_ind_ubs$IndObesidade)

DescritivaCat(dados21_ind_ubs$IndTuberculoseHanseniase)
DescritivaNum(dados21_ind_ubs$IndTuberculoseHanseniase)

DescritivaCat(dados21_ind_ubs$IndSofrimentoPsi)
DescritivaNum(dados21_ind_ubs$IndSofrimentoPsi)

DescritivaCat(dados21_ind_ubs$IndViolencia)
DescritivaNum(dados21_ind_ubs$IndViolencia)

DescritivaCat(dados21_ind_ubs$IndIdosa)
DescritivaNum(dados21_ind_ubs$IndIdosa)

DescritivaCat(dados21_ind_ubs$IndSaudeHomem)
DescritivaNum(dados21_ind_ubs$IndSaudeHomem)

DescritivaCat(dados21_ind_ubs$IndPessoasAcamadas)
DescritivaNum(dados21_ind_ubs$IndPessoasAcamadas)

DescritivaCat(dados21_ind_ubs$IndAcoesVacinacao)
DescritivaNum(dados21_ind_ubs$IndAcoesVacinacao)

#write.xlsx(DescritivaCat(dados21_ind_ubs$Indicador_Panorama) %>% as.data.frame(),"Descritiva Indicador Geral.xlsx",rowNames = T)
DescritivaNum(dados21_ind_ubs$Indicador_Panorama)

dados21_ind_ubs$IndSaudeSexual_cat = factor(case_when(
  dados21_ind_ubs$IndSaudeSexual <= 3.000000 ~ "Péssimo",
  dados21_ind_ubs$IndSaudeSexual >  3.000000 & dados21_ind_ubs$IndSaudeSexual <=  7.000000 ~ "Ruim",
  dados21_ind_ubs$IndSaudeSexual >  7.000000 & dados21_ind_ubs$IndSaudeSexual <= 13.000000 ~ "Regular",
  dados21_ind_ubs$IndSaudeSexual > 13.000000 & dados21_ind_ubs$IndSaudeSexual <= 16.000000 ~ "Bom",
  dados21_ind_ubs$IndSaudeSexual > 16.000000 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndPreNatal_cat = factor(case_when(
  dados21_ind_ubs$IndPreNatal <= 5.684211 ~ "Péssimo",
  dados21_ind_ubs$IndPreNatal >  5.684211 & dados21_ind_ubs$IndPreNatal <= 13.263158 ~ "Ruim",
  dados21_ind_ubs$IndPreNatal > 13.263158 & dados21_ind_ubs$IndPreNatal <= 24.631579 ~ "Regular",
  dados21_ind_ubs$IndPreNatal > 24.631579 & dados21_ind_ubs$IndPreNatal <= 30.315789 ~ "Bom",
  dados21_ind_ubs$IndPreNatal > 30.315789 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndSaudeMulher_cat = factor(case_when(
  dados21_ind_ubs$IndSaudeMulher <= 1.263158 ~ "Péssimo",
  dados21_ind_ubs$IndSaudeMulher >  1.263158 & dados21_ind_ubs$IndSaudeMulher <= 2.947368 ~ "Ruim",
  dados21_ind_ubs$IndSaudeMulher >  2.947368 & dados21_ind_ubs$IndSaudeMulher <= 5.473684 ~ "Regular",
  dados21_ind_ubs$IndSaudeMulher >  5.473684 & dados21_ind_ubs$IndSaudeMulher <= 6.736842 ~ "Bom",
  dados21_ind_ubs$IndSaudeMulher >  6.736842 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndSaudeCrianca_cat = factor(case_when(
  dados21_ind_ubs$IndSaudeCrianca <= 2.842105 ~ "Péssimo",
  dados21_ind_ubs$IndSaudeCrianca >  2.842105 & dados21_ind_ubs$IndSaudeCrianca <=  6.631579 ~ "Ruim",
  dados21_ind_ubs$IndSaudeCrianca >  6.631579 & dados21_ind_ubs$IndSaudeCrianca <= 12.315789 ~ "Regular",
  dados21_ind_ubs$IndSaudeCrianca > 12.315789 & dados21_ind_ubs$IndSaudeCrianca <= 15.157895 ~ "Bom",
  dados21_ind_ubs$IndSaudeCrianca > 15.157895 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndHipertenso_cat = factor(case_when(
  dados21_ind_ubs$IndHipertenso <= 3.315789 ~ "Péssimo",
  dados21_ind_ubs$IndHipertenso >  3.315789 & dados21_ind_ubs$IndHipertenso <=  7.736842 ~ "Ruim",
  dados21_ind_ubs$IndHipertenso >  7.736842 & dados21_ind_ubs$IndHipertenso <= 14.368421 ~ "Regular",
  dados21_ind_ubs$IndHipertenso > 14.368421 & dados21_ind_ubs$IndHipertenso <= 17.684211 ~ "Bom",
  dados21_ind_ubs$IndHipertenso > 17.684211 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndDiabetico_cat = factor(case_when(
  dados21_ind_ubs$IndDiabetico <= 3.789474 ~ "Péssimo",
  dados21_ind_ubs$IndDiabetico >  3.789474 & dados21_ind_ubs$IndDiabetico <=  8.842105 ~ "Ruim",
  dados21_ind_ubs$IndDiabetico >  8.842105 & dados21_ind_ubs$IndDiabetico <= 16.421053 ~ "Regular",
  dados21_ind_ubs$IndDiabetico > 16.421053 & dados21_ind_ubs$IndDiabetico <= 20.210526 ~ "Bom",
  dados21_ind_ubs$IndDiabetico > 20.210526 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndObesidade_cat = factor(case_when(
  dados21_ind_ubs$IndObesidade <= 2.368421 ~ "Péssimo",
  dados21_ind_ubs$IndObesidade >  2.368421 & dados21_ind_ubs$IndObesidade <=  5.526316 ~ "Ruim",
  dados21_ind_ubs$IndObesidade >  5.526316 & dados21_ind_ubs$IndObesidade <= 10.263158 ~ "Regular",
  dados21_ind_ubs$IndObesidade > 10.263158 & dados21_ind_ubs$IndObesidade <= 12.631579 ~ "Bom",
  dados21_ind_ubs$IndObesidade > 12.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndTuberculoseHanseniase_cat = factor(case_when(
  dados21_ind_ubs$IndTuberculoseHanseniase <=  4.421053 ~ "Péssimo",
  dados21_ind_ubs$IndTuberculoseHanseniase >   4.421053 & dados21_ind_ubs$IndTuberculoseHanseniase <= 10.315789 ~ "Ruim",
  dados21_ind_ubs$IndTuberculoseHanseniase >  10.315789 & dados21_ind_ubs$IndTuberculoseHanseniase <= 19.157895 ~ "Regular",
  dados21_ind_ubs$IndTuberculoseHanseniase >  19.157895 & dados21_ind_ubs$IndTuberculoseHanseniase <= 23.578947 ~ "Bom",
  dados21_ind_ubs$IndTuberculoseHanseniase >  23.578947 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndSofrimentoPsi_cat = factor(case_when(
  dados21_ind_ubs$IndSofrimentoPsi <= 1.578947 ~ "Péssimo",
  dados21_ind_ubs$IndSofrimentoPsi >  1.578947 & dados21_ind_ubs$IndSofrimentoPsi <= 3.684211 ~ "Ruim",
  dados21_ind_ubs$IndSofrimentoPsi >  3.684211 & dados21_ind_ubs$IndSofrimentoPsi <= 6.842105 ~ "Regular",
  dados21_ind_ubs$IndSofrimentoPsi >  6.842105 & dados21_ind_ubs$IndSofrimentoPsi <= 8.421053 ~ "Bom",
  dados21_ind_ubs$IndSofrimentoPsi >  8.421053 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndViolencia_cat = factor(case_when(
  dados21_ind_ubs$IndViolencia <= 1.736842 ~ "Péssimo",
  dados21_ind_ubs$IndViolencia >  1.736842 & dados21_ind_ubs$IndViolencia <= 4.052632 ~ "Ruim",
  dados21_ind_ubs$IndViolencia >  4.052632 & dados21_ind_ubs$IndViolencia <= 7.526316 ~ "Regular",
  dados21_ind_ubs$IndViolencia >  7.526316 & dados21_ind_ubs$IndViolencia <= 9.263158 ~ "Bom",
  dados21_ind_ubs$IndViolencia >  9.263158 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndIdosa_cat = factor(case_when(
  dados21_ind_ubs$IndIdosa <= 2.052632 ~ "Péssimo",
  dados21_ind_ubs$IndIdosa >  2.052632 & dados21_ind_ubs$IndIdosa <= 4.789474 ~ "Ruim",
  dados21_ind_ubs$IndIdosa >  4.789474 & dados21_ind_ubs$IndIdosa <= 8.894737 ~ "Regular",
  dados21_ind_ubs$IndIdosa >  8.894737 & dados21_ind_ubs$IndIdosa <= 10.947368 ~ "Bom",
  dados21_ind_ubs$IndIdosa > 10.947368 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndSaudeHomem_cat = factor(case_when(
  dados21_ind_ubs$IndSaudeHomem <= 1.421053 ~ "Péssimo",
  dados21_ind_ubs$IndSaudeHomem >  1.421053 & dados21_ind_ubs$IndSaudeHomem <= 3.315789 ~ "Ruim",
  dados21_ind_ubs$IndSaudeHomem >  3.315789 & dados21_ind_ubs$IndSaudeHomem <= 6.157895 ~ "Regular",
  dados21_ind_ubs$IndSaudeHomem >  6.157895 & dados21_ind_ubs$IndSaudeHomem <= 7.578947 ~ "Bom",
  dados21_ind_ubs$IndSaudeHomem >  7.578947 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndPessoasAcamadas_cat = factor(case_when(
  dados21_ind_ubs$IndPessoasAcamadas <=  3.947368 ~ "Péssimo",
  dados21_ind_ubs$IndPessoasAcamadas >   3.947368 & dados21_ind_ubs$IndPessoasAcamadas <=  9.210526 ~ "Ruim",
  dados21_ind_ubs$IndPessoasAcamadas >   9.210526 & dados21_ind_ubs$IndPessoasAcamadas <= 17.105263 ~ "Regular",
  dados21_ind_ubs$IndPessoasAcamadas >  17.105263 & dados21_ind_ubs$IndPessoasAcamadas <= 21.052632 ~ "Bom",
  dados21_ind_ubs$IndPessoasAcamadas >  21.052632 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndAcoesVacinacao_cat = factor(case_when(
  dados21_ind_ubs$IndAcoesVacinacao <=  3.473684 ~ "Péssimo",
  dados21_ind_ubs$IndAcoesVacinacao >   3.473684 & dados21_ind_ubs$IndAcoesVacinacao <=  8.105263 ~ "Ruim",
  dados21_ind_ubs$IndAcoesVacinacao >   8.105263 & dados21_ind_ubs$IndAcoesVacinacao <= 15.052632 ~ "Regular",
  dados21_ind_ubs$IndAcoesVacinacao >  15.052632 & dados21_ind_ubs$IndAcoesVacinacao <= 18.526316 ~ "Bom",
  dados21_ind_ubs$IndAcoesVacinacao >  18.526316 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

# dados21_ind_ubs$IndPraticasIntComp_cat = factor(case_when(
#   dados21_ind_ubs$IndPraticasIntComp <= 3 ~ "Péssimo",
#   dados21_ind_ubs$IndPraticasIntComp > 3 & dados21_ind_ubs$IndPraticasIntComp <= 8 ~ "Ruim",
#   dados21_ind_ubs$IndPraticasIntComp > 8 & dados21_ind_ubs$IndPraticasIntComp <= 14 ~ "Regular",
#   dados21_ind_ubs$IndPraticasIntComp > 14 & dados21_ind_ubs$IndPraticasIntComp <= 18 ~ "Bom",
#   dados21_ind_ubs$IndPraticasIntComp > 18 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndAtendimentoUrgEmerg_cat = factor(case_when(
  dados21_ind_ubs$IndAtendimentoUrgEmerg <=  5.210526 ~ "Péssimo",
  dados21_ind_ubs$IndAtendimentoUrgEmerg >   5.210526 & dados21_ind_ubs$IndAtendimentoUrgEmerg <= 12.157895 ~ "Ruim",
  dados21_ind_ubs$IndAtendimentoUrgEmerg >  12.157895 & dados21_ind_ubs$IndAtendimentoUrgEmerg <= 22.578947 ~ "Regular",
  dados21_ind_ubs$IndAtendimentoUrgEmerg >  22.578947 & dados21_ind_ubs$IndAtendimentoUrgEmerg <= 27.789474 ~ "Bom",
  dados21_ind_ubs$IndAtendimentoUrgEmerg >  27.789474 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndAtendimentoProgDemandaEsp_cat = factor(case_when(
  dados21_ind_ubs$IndAtendimentoProgDemandaEsp <=  2.368421 ~ "Péssimo",
  dados21_ind_ubs$IndAtendimentoProgDemandaEsp >   2.368421 & dados21_ind_ubs$IndAtendimentoProgDemandaEsp <=  5.526316 ~ "Ruim",
  dados21_ind_ubs$IndAtendimentoProgDemandaEsp >   5.526316 & dados21_ind_ubs$IndAtendimentoProgDemandaEsp <= 10.263158 ~ "Regular",
  dados21_ind_ubs$IndAtendimentoProgDemandaEsp >  10.263158 & dados21_ind_ubs$IndAtendimentoProgDemandaEsp <= 12.631579 ~ "Bom",
  dados21_ind_ubs$IndAtendimentoProgDemandaEsp >  12.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndAtendimentoDemandaEsp_cat <- factor(case_when(
  dados21_ind_ubs$IndAtendimentoDemandaEsp <= 0.157895 ~ "Péssimo",
  dados21_ind_ubs$IndAtendimentoDemandaEsp >  0.157895 & dados21_ind_ubs$IndAtendimentoDemandaEsp <= 0.368421 ~ "Ruim",
  dados21_ind_ubs$IndAtendimentoDemandaEsp >  0.368421 & dados21_ind_ubs$IndAtendimentoDemandaEsp <= 0.684211 ~ "Regular",
  dados21_ind_ubs$IndAtendimentoDemandaEsp >  0.684211 & dados21_ind_ubs$IndAtendimentoDemandaEsp <= 0.842105 ~ "Bom",
  dados21_ind_ubs$IndAtendimentoDemandaEsp >  0.842105 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndIntegracaoAPS_cat = factor(case_when(
  dados21_ind_ubs$IndIntegracaoAPS <=  4.578947 ~ "Péssimo",
  dados21_ind_ubs$IndIntegracaoAPS >   4.578947 & dados21_ind_ubs$IndIntegracaoAPS <= 10.684211 ~ "Ruim",
  dados21_ind_ubs$IndIntegracaoAPS >  10.684211 & dados21_ind_ubs$IndIntegracaoAPS <= 19.842105 ~ "Regular",
  dados21_ind_ubs$IndIntegracaoAPS >  19.842105 & dados21_ind_ubs$IndIntegracaoAPS <= 24.421053 ~ "Bom",
  dados21_ind_ubs$IndIntegracaoAPS >  24.421053 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndRegulacaoAssistencial_cat = factor(case_when(
  dados21_ind_ubs$IndRegulacaoAssistencial <= 0.789474 ~ "Péssimo",
  dados21_ind_ubs$IndRegulacaoAssistencial >  0.789474 & dados21_ind_ubs$IndRegulacaoAssistencial <= 1.842105 ~ "Ruim",
  dados21_ind_ubs$IndRegulacaoAssistencial >  1.842105 & dados21_ind_ubs$IndRegulacaoAssistencial <= 3.421053 ~ "Regular",
  dados21_ind_ubs$IndRegulacaoAssistencial >  3.421053 & dados21_ind_ubs$IndRegulacaoAssistencial <= 4.210526 ~ "Bom",
  dados21_ind_ubs$IndRegulacaoAssistencial >  4.210526 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_ubs$IndCuidadoCompartilhado_cat = factor(case_when(
  dados21_ind_ubs$IndCuidadoCompartilhado <=  8.368421 ~ "Péssimo",
  dados21_ind_ubs$IndCuidadoCompartilhado >   8.368421 & dados21_ind_ubs$IndCuidadoCompartilhado <= 19.526316 ~ "Ruim",
  dados21_ind_ubs$IndCuidadoCompartilhado >  19.526316 & dados21_ind_ubs$IndCuidadoCompartilhado <= 36.263158 ~ "Regular",
  dados21_ind_ubs$IndCuidadoCompartilhado >  36.263158 & dados21_ind_ubs$IndCuidadoCompartilhado <= 44.631579 ~ "Bom",
  dados21_ind_ubs$IndCuidadoCompartilhado >  44.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

#Até urgência e emergência
dados21_ind_ubs$Indicador_Panorama_cat = factor(case_when(
  dados21_ind_ubs$Indicador_Panorama <= 49.26 ~ "Péssimo",
  dados21_ind_ubs$Indicador_Panorama > 49.26 & dados21_ind_ubs$Indicador_Panorama <= 114.95 ~ "Ruim",
  dados21_ind_ubs$Indicador_Panorama > 114.95 & dados21_ind_ubs$Indicador_Panorama <= 213.47 ~ "Regular",
  dados21_ind_ubs$Indicador_Panorama > 213.47 & dados21_ind_ubs$Indicador_Panorama <= 262.74 ~ "Bom",
  dados21_ind_ubs$Indicador_Panorama > 262.74 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))
#write.xlsx(dados21_ind_ubs %>% as.data.frame(),'Dados com indicadores categorizados agrupados por ubs.xlsx')

####=============
#### Comparações
####=============
DescritivaCat(dados21_ind_ubs$Indicador_TIC_cat)
# write.xlsx(DescritivaCat(dados21_ind_ubs$Indicador_TIC) %>% as.data.frame(), 'Tabela 18.xlsx', rowNames = T)
# write.xlsx(DescritivaCat(dados21_ind_ubs$Indicador_TIC_cat) %>% as.data.frame(), 'Tabela 19.xlsx', rowNames = T)

#write.xlsx(dados21_ind_ubs %>% as.data.frame(),'Dados censo com indicadores UBS.xlsx')

Tabela1 = rbind(
  QuiQuadrado_Fisher(dados21_ind_ubs$Indicador_TIC_cat,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$V351,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V352,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V353,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V354,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V355,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V356,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V357,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V358,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$V361,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V362,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V363,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V364,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V365,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V366,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V367,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V368,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V369,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3610,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3611,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3612,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$v304,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$v305,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3613,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3614,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3615,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3616,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3617,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3618,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$V3621,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3622,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3623,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3624,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3625,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3626,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3627,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$V37,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3711,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3712,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3713,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3714,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3715,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3716,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$V372,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V373,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3741,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3742,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3743,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3744,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3745,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3746,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3747,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3748,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3749,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V37410,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V37411,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V325,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$porte_populacional,dados21_ind_ubs$Indicador_Panorama_cat,'1','chisq')
  )
#write.xlsx(Tabela1 %>% as.data.frame(), 'Tabela 1.xlsx', rowNames = T)

dados21_ind_ubs %>% select(PercLeitos,PercPlanosSaude,ivs,Gini,CoberturaESF) %>% 
  map(TesteDeNormalidade)

Tabela2 = rbind(KruskalTeste(dados21_ind_ubs$PercLeitos,dados21_ind_ubs$Indicador_Panorama_cat)$tabela,
                KruskalTeste(dados21_ind_ubs$PercPlanosSaude,dados21_ind_ubs$Indicador_Panorama_cat)$tabela,
                KruskalTeste(dados21_ind_ubs$ivs,dados21_ind_ubs$Indicador_Panorama_cat)$tabela,
                KruskalTeste(dados21_ind_ubs$Gini,dados21_ind_ubs$Indicador_Panorama_cat)$tabela,
                KruskalTeste(dados21_ind_ubs$CoberturaESF,dados21_ind_ubs$Indicador_Panorama_cat)$tabela)
#write.xlsx(Tabela2 %>% as.data.frame(), 'Tabela 2.xlsx', rowNames = T)

Tabela3 = rbind(
  QuiQuadrado_Fisher(dados21_ind_ubs$Indicador_Panorama_cat,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$V351,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V352,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V353,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V354,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V355,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V356,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V357,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V358,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$V361,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V362,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V363,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V364,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V365,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V366,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V367,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V368,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V369,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3610,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3611,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3612,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$v304,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$v305,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3613,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3614,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3615,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3616,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3617,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3618,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$V3621,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3622,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3623,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3624,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3625,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3626,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3627,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$V37,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3711,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3712,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3713,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3714,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3715,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3716,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$V372,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V373,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3741,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3742,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3743,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3744,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3745,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3746,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3747,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3748,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V3749,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V37410,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V37411,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  QuiQuadrado_Fisher(dados21_ind_ubs$V325,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq'),
  
  QuiQuadrado_Fisher(dados21_ind_ubs$porte_populacional,dados21_ind_ubs$Indicador_TIC_cat,'1','chisq')
)
#write.xlsx(Tabela3 %>% as.data.frame(), 'Tabela 3.xlsx', rowNames = T)

Tabela4 = rbind(KruskalTeste(dados21_ind_ubs$PercLeitos,dados21_ind_ubs$Indicador_TIC_cat)$tabela,
                KruskalTeste(dados21_ind_ubs$PercPlanosSaude,dados21_ind_ubs$Indicador_TIC_cat)$tabela,
                KruskalTeste(dados21_ind_ubs$ivs,dados21_ind_ubs$Indicador_TIC_cat)$tabela,
                KruskalTeste(dados21_ind_ubs$Gini,dados21_ind_ubs$Indicador_TIC_cat)$tabela,
                KruskalTeste(dados21_ind_ubs$CoberturaESF,dados21_ind_ubs$Indicador_TIC_cat)$tabela)
#write.xlsx(Tabela4 %>% as.data.frame(), 'Tabela 4.xlsx', rowNames = T)

####===============
#### Modelos - TIC
####===============
DescritivaCat(dados21_ind_ubs$Indicador_TIC_cat)
DescritivaNum(dados21_ind_ubs$Indicador_TIC)
hist(dados21_ind_ubs$Indicador_TIC)
vars_numericas = dados21_ind_ubs[, c("PercLeitos", "PercPlanosSaude", "ivs", "Gini", "CoberturaESF")]
vars_numericas = na.omit(vars_numericas)
round(cor(vars_numericas, method = "pearson"), 3)


# mod_TIC_uni1 = glm(Indicador_TIC ~ porte_populacional, data = dados21_ind_ubs, family = Gamma(link = 'log'))
# mod_TIC_uni2 = glm(Indicador_TIC ~ PercLeitos, data = dados21_ind_ubs, family = Gamma(link = 'log'))
# mod_TIC_uni3 = glm(Indicador_TIC ~ PercPlanosSaude, data = dados21_ind_ubs, family = Gamma(link = 'log'))
# mod_TIC_uni4 = glm(Indicador_TIC ~ I(ivs/100), data = dados21_ind_ubs, family = Gamma(link = 'log'))
# mod_TIC_uni5 = glm(Indicador_TIC ~ I(Gini/100), data = dados21_ind_ubs, family = Gamma(link = 'log'))
# mod_TIC_uni6 = glm(Indicador_TIC ~ CoberturaESF, data = dados21_ind_ubs, family = Gamma(link = 'log'))
# mod_TIC_uni7 = glm(Indicador_TIC ~ Indicador_Panorama_cat, data = dados21_ind_ubs, family = Gamma(link = 'log'))
# summary(mod_TIC_uni1)
# summary(mod_TIC_uni2)
# summary(mod_TIC_uni3)
# summary(mod_TIC_uni4)
# summary(mod_TIC_uni5)
# summary(mod_TIC_uni6)
# summary(mod_TIC_uni7)
# mod_TIC_multi_gamma = glm(Indicador_TIC ~ porte_populacional + PercLeitos + PercPlanosSaude + I(ivs/100) + 
#                       I(Gini/100) + CoberturaESF + Indicador_Panorama_cat, 
#                     data = dados21_ind_ubs, family = Gamma(link = 'log'))
# summary(mod_TIC_multi_gamma)

TabelaRegressaoLinear = function(modelo, casasdecimaisB = 3){
  options(OutDec = ",")
  coef = summary(modelo)$coefficients
  ICinf = coef[,1] - 1.96 * coef[,2]
  ICsup = coef[,1] + 1.96 * coef[,2]
  
  Tabela = data.frame(
    "Variáveis" = rownames(coef),
    "β" = round(coef[,1], casasdecimaisB),
    "Erro Padrão" = round(coef[,2], casasdecimaisB),
    "IC 95%" = paste0("[", round(ICinf, casasdecimaisB), "; ", round(ICsup, casasdecimaisB), "]"),
    "Valor-p" = round(coef[,4], 5)
  )
  return(Tabela)
}

TabelaMultinomialOrdinal = function(modelo, casasdecimaisExpB = 3){
  options(OutDec = ",")
  coef = summary(modelo)$coefficients
  
  # cálculo de p-valor manual para polr ou multinom
  if(ncol(coef) == 3){
    p = 2 * pnorm(abs(coef[, "t value"]), lower.tail = FALSE)
  } else {
    p = coef[,4]
  }
  
  ICinf = exp(coef[,1] - 1.96 * coef[,2])
  ICsup = exp(coef[,1] + 1.96 * coef[,2])
  
  Tabela = data.frame(
    "Variáveis" = rownames(coef),
    "β" = round(coef[,1], casasdecimaisExpB),
    "Exp(β)" = round(exp(coef[,1]), casasdecimaisExpB),
    "Alteração (%)" = round((exp(coef[,1]) - 1) * 100, 2),
    "IC (β) 95%" = paste0("[", round(ICinf, casasdecimaisExpB), "; ", round(ICsup, casasdecimaisExpB), "]"),
    "Valor-p" = round(p, 5)
  )
  Tabela = Tabela[!grepl("\\|", Tabela$Variáveis), ]
  return(Tabela)
}

mod_TIC_uni1 = lm(Indicador_TIC ~ porte_populacional, data = dados21_ind_ubs)
mod_TIC_uni2 = lm(Indicador_TIC ~ PercLeitos, data = dados21_ind_ubs)
mod_TIC_uni3 = lm(Indicador_TIC ~ PercPlanosSaude, data = dados21_ind_ubs)
mod_TIC_uni4 = lm(Indicador_TIC ~ ivs, data = dados21_ind_ubs)
mod_TIC_uni5 = lm(Indicador_TIC ~ Gini, data = dados21_ind_ubs)
mod_TIC_uni6 = lm(Indicador_TIC ~ CoberturaESF, data = dados21_ind_ubs)
mod_TIC_uni7 = lm(Indicador_TIC ~ Indicador_Panorama_cat, data = dados21_ind_ubs)
# write.xlsx(rbind(TabelaRegressaoLinear(mod_TIC_uni1),TabelaRegressaoLinear(mod_TIC_uni2),
#                  TabelaRegressaoLinear(mod_TIC_uni3),TabelaRegressaoLinear(mod_TIC_uni4),
#                  TabelaRegressaoLinear(mod_TIC_uni5),TabelaRegressaoLinear(mod_TIC_uni6),
#                  TabelaRegressaoLinear(mod_TIC_uni7)) %>% as.data.frame(), "Tabela 5.xlsx", rowNames = F)

mod_TIC_multi = lm(Indicador_TIC ~ porte_populacional + PercLeitos + PercPlanosSaude + ivs + 
                     Gini + #CoberturaESF + 
                     Indicador_Panorama_cat, 
                   data = dados21_ind_ubs)
summary(mod_TIC_multi)
#write.xlsx(rbind(TabelaRegressaoLinear(mod_TIC_multi)) %>% as.data.frame(), "Tabela 5.1.xlsx", rowNames = F)
car::vif(mod_TIC_multi)

mod_TIC_cat_uni1 = MASS::polr(Indicador_TIC_cat ~ porte_populacional, data = dados21_ind_ubs, Hess = TRUE)
mod_TIC_cat_uni2 = MASS::polr(Indicador_TIC_cat ~ PercLeitos, data = dados21_ind_ubs, Hess = TRUE)
mod_TIC_cat_uni3 = MASS::polr(Indicador_TIC_cat ~ PercPlanosSaude, data = dados21_ind_ubs, Hess = TRUE)
mod_TIC_cat_uni4 = MASS::polr(Indicador_TIC_cat ~ ivs, data = dados21_ind_ubs, Hess = TRUE)
mod_TIC_cat_uni5 = MASS::polr(Indicador_TIC_cat ~ Gini, data = dados21_ind_ubs, Hess = TRUE)
mod_TIC_cat_uni6 = MASS::polr(Indicador_TIC_cat ~ CoberturaESF, data = dados21_ind_ubs, Hess = TRUE)
mod_TIC_cat_uni7 = MASS::polr(Indicador_TIC_cat ~ Indicador_Panorama_cat, data = dados21_ind_ubs, Hess = TRUE)
# write.xlsx(rbind(TabelaMultinomialOrdinal(mod_TIC_cat_uni1),TabelaMultinomialOrdinal(mod_TIC_cat_uni2),
#                  TabelaMultinomialOrdinal(mod_TIC_cat_uni3),TabelaMultinomialOrdinal(mod_TIC_cat_uni4),
#                  TabelaMultinomialOrdinal(mod_TIC_cat_uni5),TabelaMultinomialOrdinal(mod_TIC_cat_uni6),
#                  TabelaMultinomialOrdinal(mod_TIC_cat_uni7)) %>% as.data.frame(), "Tabela 6.xlsx", rowNames = F)

mod_TIC_cat_multi = MASS::polr(Indicador_TIC_cat ~ porte_populacional + PercLeitos + PercPlanosSaude +
                     ivs + Gini + #CoberturaESF + 
                       Indicador_Panorama_cat,
                   data = dados21_ind_ubs, Hess = TRUE)
write.xlsx(rbind(TabelaMultinomialOrdinal(mod_TIC_cat_multi)) %>% as.data.frame(), "Tabela 6.1.xlsx", rowNames = F)

summary(mod_TIC_cat_multi)
ctable = coef(summary(mod_TIC_cat_multi))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable = cbind(ctable, "p value" = p)
round(ctable %>% as.data.frame() %>% select('p value'),5)

####========================================
#### Categorizar indicadores por municípios
####========================================
dados21_ind_mun = dados21 %>% 
  # select(CO_MUNICIPIO_IBGE,V16,V17,
  #        c(vars_saude_sexual, add_num(vars_saude_sexual)), IndSaudeSexual,
  #        c(vars_pre_natal, add_num(vars_pre_natal)), IndPreNatal,
  #        c(vars_saude_mulher, add_num(vars_saude_mulher)), IndSaudeMulher,
  #        c(vars_saude_crianca, add_num(vars_saude_crianca)), IndSaudeCrianca,
  #        c(vars_hipertenso, add_num(vars_hipertenso)), IndHipertenso,
  #        c(vars_diabetico, add_num(vars_diabetico)), IndDiabetico,
  #        c(vars_obesidade, add_num(vars_obesidade)), IndObesidade,
  #        c(vars_tuberculose_hanseniase, add_num(vars_tuberculose_hanseniase)), IndTuberculoseHanseniase,
  #        c(vars_sofrimento_psi, add_num(vars_sofrimento_psi)), IndSofrimentoPsi,
  #        c(vars_violencia, add_num(vars_violencia)), IndViolencia,
  #        c(vars_idosa, add_num(vars_idosa)), IndIdosa,
  #        c(vars_saude_homem, add_num(vars_saude_homem)), IndSaudeHomem,
  #        c(vars_acamadas, add_num(vars_acamadas)), IndPessoasAcamadas,
  #        c(vars_acoes_vacinacao, add_num(vars_acoes_vacinacao)), IndAcoesVacinacao,
  #        #c(vars_praticas_int_comp, add_num(vars_praticas_int_comp)), IndPraticasIntComp,
  #        c(vars_atendimento_urg_emerg, add_num(vars_atendimento_urg_emerg)), IndAtendimentoUrgEmerg,
  #        c(vars_atendimento_prog_demanda_esp, add_num(vars_atendimento_prog_demanda_esp)), IndAtendimentoProgDemandaEsp,
  #        v105, v105num, IndAtendimentoDemandaEsp,
  #        c(vars_integracao_aps, add_num(vars_integracao_aps)), IndIntegracaoAPS,
  #        c(vars_regulacao_assistencial, add_num(vars_regulacao_assistencial)), IndRegulacaoAssistencial,
  #        c(vars_cuidado_compartilhado, add_num(vars_cuidado_compartilhado)), IndCuidadoCompartilhado) %>%
  mutate(Indicador_Panorama = rowSums(across(c(IndSaudeSexual, IndPreNatal, IndSaudeMulher, IndSaudeCrianca,
                                           IndHipertenso, IndDiabetico, IndObesidade, IndTuberculoseHanseniase,
                                           IndSofrimentoPsi, IndViolencia, IndIdosa, IndSaudeHomem,
                                           IndPessoasAcamadas, IndAcoesVacinacao, #IndPraticasIntComp,
                                           IndAtendimentoUrgEmerg)), na.rm = TRUE),
         Regiao = case_when(V17 %in% c("AC","AM","AP","PA","RO","RR","TO") ~ "Norte",
                            V17 %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE") ~ "Nordeste",
                            V17 %in% c("DF","GO","MT","MS") ~ "Centro-Oeste",
                            V17 %in% c("ES","MG","RJ","SP") ~ "Sudeste",
                            V17 %in% c("PR","RS","SC") ~ "Sul",
                            TRUE ~ NA_character_)) %>% 
  group_by(CO_MUNICIPIO_IBGE, Regiao, V16, V17) %>%
  summarise(IndSaudeSexual = mean(IndSaudeSexual, na.rm = TRUE),
            IndPreNatal = mean(IndPreNatal, na.rm = TRUE),
            IndSaudeMulher = mean(IndSaudeMulher, na.rm = TRUE),
            IndSaudeCrianca = mean(IndSaudeCrianca, na.rm = TRUE),
            IndHipertenso = mean(IndHipertenso, na.rm = TRUE),
            IndDiabetico = mean(IndDiabetico, na.rm = TRUE),
            IndObesidade = mean(IndObesidade, na.rm = TRUE),
            IndTuberculoseHanseniase = mean(IndTuberculoseHanseniase, na.rm = TRUE),
            IndSofrimentoPsi = mean(IndSofrimentoPsi, na.rm = TRUE),
            IndViolencia = mean(IndViolencia, na.rm = TRUE),
            IndIdosa = mean(IndIdosa, na.rm = TRUE),
            IndSaudeHomem = mean(IndSaudeHomem, na.rm = TRUE),
            IndPessoasAcamadas = mean(IndPessoasAcamadas, na.rm = TRUE),
            IndAcoesVacinacao = mean(IndAcoesVacinacao, na.rm = TRUE),
            #IndPraticasIntComp = mean(IndPraticasIntComp, na.rm = TRUE),
            IndAtendimentoUrgEmerg = mean(IndAtendimentoUrgEmerg, na.rm = TRUE),
            IndAtendimentoProgDemandaEsp = mean(IndAtendimentoProgDemandaEsp, na.rm = TRUE),
            IndAtendimentoDemandaEsp = mean(IndAtendimentoDemandaEsp, na.rm = TRUE),
            IndIntegracaoAPS = mean(IndIntegracaoAPS, na.rm = TRUE),
            IndRegulacaoAssistencial = mean(IndRegulacaoAssistencial, na.rm = TRUE),
            IndCuidadoCompartilhado = mean(IndCuidadoCompartilhado, na.rm = TRUE),
            Indicador_Panorama = mean(Indicador_Panorama, na.rm = TRUE)) %>% as.data.frame()
#write.xlsx(dados21_ind %>% as.data.frame(),'Dados com indicadores agrupados por municípios.xlsx')

dados21_ind_mun$IndSaudeSexual_cat = factor(case_when(
  dados21_ind_mun$IndSaudeSexual <= 3.000000 ~ "Péssimo",
  dados21_ind_mun$IndSaudeSexual >  3.000000 & dados21_ind_mun$IndSaudeSexual <=  7.000000 ~ "Ruim",
  dados21_ind_mun$IndSaudeSexual >  7.000000 & dados21_ind_mun$IndSaudeSexual <= 13.000000 ~ "Regular",
  dados21_ind_mun$IndSaudeSexual > 13.000000 & dados21_ind_mun$IndSaudeSexual <= 16.000000 ~ "Bom",
  dados21_ind_mun$IndSaudeSexual > 16.000000 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndPreNatal_cat = factor(case_when(
  dados21_ind_mun$IndPreNatal <= 5.684211 ~ "Péssimo",
  dados21_ind_mun$IndPreNatal >  5.684211 & dados21_ind_mun$IndPreNatal <= 13.263158 ~ "Ruim",
  dados21_ind_mun$IndPreNatal > 13.263158 & dados21_ind_mun$IndPreNatal <= 24.631579 ~ "Regular",
  dados21_ind_mun$IndPreNatal > 24.631579 & dados21_ind_mun$IndPreNatal <= 30.315789 ~ "Bom",
  dados21_ind_mun$IndPreNatal > 30.315789 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndSaudeMulher_cat = factor(case_when(
  dados21_ind_mun$IndSaudeMulher <= 1.263158 ~ "Péssimo",
  dados21_ind_mun$IndSaudeMulher >  1.263158 & dados21_ind_mun$IndSaudeMulher <= 2.947368 ~ "Ruim",
  dados21_ind_mun$IndSaudeMulher >  2.947368 & dados21_ind_mun$IndSaudeMulher <= 5.473684 ~ "Regular",
  dados21_ind_mun$IndSaudeMulher >  5.473684 & dados21_ind_mun$IndSaudeMulher <= 6.736842 ~ "Bom",
  dados21_ind_mun$IndSaudeMulher >  6.736842 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndSaudeCrianca_cat = factor(case_when(
  dados21_ind_mun$IndSaudeCrianca <= 2.842105 ~ "Péssimo",
  dados21_ind_mun$IndSaudeCrianca >  2.842105 & dados21_ind_mun$IndSaudeCrianca <=  6.631579 ~ "Ruim",
  dados21_ind_mun$IndSaudeCrianca >  6.631579 & dados21_ind_mun$IndSaudeCrianca <= 12.315789 ~ "Regular",
  dados21_ind_mun$IndSaudeCrianca > 12.315789 & dados21_ind_mun$IndSaudeCrianca <= 15.157895 ~ "Bom",
  dados21_ind_mun$IndSaudeCrianca > 15.157895 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndHipertenso_cat = factor(case_when(
  dados21_ind_mun$IndHipertenso <= 3.315789 ~ "Péssimo",
  dados21_ind_mun$IndHipertenso >  3.315789 & dados21_ind_mun$IndHipertenso <=  7.736842 ~ "Ruim",
  dados21_ind_mun$IndHipertenso >  7.736842 & dados21_ind_mun$IndHipertenso <= 14.368421 ~ "Regular",
  dados21_ind_mun$IndHipertenso > 14.368421 & dados21_ind_mun$IndHipertenso <= 17.684211 ~ "Bom",
  dados21_ind_mun$IndHipertenso > 17.684211 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndDiabetico_cat = factor(case_when(
  dados21_ind_mun$IndDiabetico <= 3.789474 ~ "Péssimo",
  dados21_ind_mun$IndDiabetico >  3.789474 & dados21_ind_mun$IndDiabetico <=  8.842105 ~ "Ruim",
  dados21_ind_mun$IndDiabetico >  8.842105 & dados21_ind_mun$IndDiabetico <= 16.421053 ~ "Regular",
  dados21_ind_mun$IndDiabetico > 16.421053 & dados21_ind_mun$IndDiabetico <= 20.210526 ~ "Bom",
  dados21_ind_mun$IndDiabetico > 20.210526 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndObesidade_cat = factor(case_when(
  dados21_ind_mun$IndObesidade <= 2.368421 ~ "Péssimo",
  dados21_ind_mun$IndObesidade >  2.368421 & dados21_ind_mun$IndObesidade <=  5.526316 ~ "Ruim",
  dados21_ind_mun$IndObesidade >  5.526316 & dados21_ind_mun$IndObesidade <= 10.263158 ~ "Regular",
  dados21_ind_mun$IndObesidade > 10.263158 & dados21_ind_mun$IndObesidade <= 12.631579 ~ "Bom",
  dados21_ind_mun$IndObesidade > 12.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndTuberculoseHanseniase_cat = factor(case_when(
  dados21_ind_mun$IndTuberculoseHanseniase <=  4.421053 ~ "Péssimo",
  dados21_ind_mun$IndTuberculoseHanseniase >   4.421053 & dados21_ind_mun$IndTuberculoseHanseniase <= 10.315789 ~ "Ruim",
  dados21_ind_mun$IndTuberculoseHanseniase >  10.315789 & dados21_ind_mun$IndTuberculoseHanseniase <= 19.157895 ~ "Regular",
  dados21_ind_mun$IndTuberculoseHanseniase >  19.157895 & dados21_ind_mun$IndTuberculoseHanseniase <= 23.578947 ~ "Bom",
  dados21_ind_mun$IndTuberculoseHanseniase >  23.578947 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndSofrimentoPsi_cat = factor(case_when(
  dados21_ind_mun$IndSofrimentoPsi <= 1.578947 ~ "Péssimo",
  dados21_ind_mun$IndSofrimentoPsi >  1.578947 & dados21_ind_mun$IndSofrimentoPsi <= 3.684211 ~ "Ruim",
  dados21_ind_mun$IndSofrimentoPsi >  3.684211 & dados21_ind_mun$IndSofrimentoPsi <= 6.842105 ~ "Regular",
  dados21_ind_mun$IndSofrimentoPsi >  6.842105 & dados21_ind_mun$IndSofrimentoPsi <= 8.421053 ~ "Bom",
  dados21_ind_mun$IndSofrimentoPsi >  8.421053 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndViolencia_cat = factor(case_when(
  dados21_ind_mun$IndViolencia <= 1.736842 ~ "Péssimo",
  dados21_ind_mun$IndViolencia >  1.736842 & dados21_ind_mun$IndViolencia <= 4.052632 ~ "Ruim",
  dados21_ind_mun$IndViolencia >  4.052632 & dados21_ind_mun$IndViolencia <= 7.526316 ~ "Regular",
  dados21_ind_mun$IndViolencia >  7.526316 & dados21_ind_mun$IndViolencia <= 9.263158 ~ "Bom",
  dados21_ind_mun$IndViolencia >  9.263158 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndIdosa_cat = factor(case_when(
  dados21_ind_mun$IndIdosa <= 2.052632 ~ "Péssimo",
  dados21_ind_mun$IndIdosa >  2.052632 & dados21_ind_mun$IndIdosa <= 4.789474 ~ "Ruim",
  dados21_ind_mun$IndIdosa >  4.789474 & dados21_ind_mun$IndIdosa <= 8.894737 ~ "Regular",
  dados21_ind_mun$IndIdosa >  8.894737 & dados21_ind_mun$IndIdosa <= 10.947368 ~ "Bom",
  dados21_ind_mun$IndIdosa > 10.947368 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndSaudeHomem_cat = factor(case_when(
  dados21_ind_mun$IndSaudeHomem <= 1.421053 ~ "Péssimo",
  dados21_ind_mun$IndSaudeHomem >  1.421053 & dados21_ind_mun$IndSaudeHomem <= 3.315789 ~ "Ruim",
  dados21_ind_mun$IndSaudeHomem >  3.315789 & dados21_ind_mun$IndSaudeHomem <= 6.157895 ~ "Regular",
  dados21_ind_mun$IndSaudeHomem >  6.157895 & dados21_ind_mun$IndSaudeHomem <= 7.578947 ~ "Bom",
  dados21_ind_mun$IndSaudeHomem >  7.578947 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndPessoasAcamadas_cat = factor(case_when(
  dados21_ind_mun$IndPessoasAcamadas <=  3.947368 ~ "Péssimo",
  dados21_ind_mun$IndPessoasAcamadas >   3.947368 & dados21_ind_mun$IndPessoasAcamadas <=  9.210526 ~ "Ruim",
  dados21_ind_mun$IndPessoasAcamadas >   9.210526 & dados21_ind_mun$IndPessoasAcamadas <= 17.105263 ~ "Regular",
  dados21_ind_mun$IndPessoasAcamadas >  17.105263 & dados21_ind_mun$IndPessoasAcamadas <= 21.052632 ~ "Bom",
  dados21_ind_mun$IndPessoasAcamadas >  21.052632 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndAcoesVacinacao_cat = factor(case_when(
  dados21_ind_mun$IndAcoesVacinacao <=  3.473684 ~ "Péssimo",
  dados21_ind_mun$IndAcoesVacinacao >   3.473684 & dados21_ind_mun$IndAcoesVacinacao <=  8.105263 ~ "Ruim",
  dados21_ind_mun$IndAcoesVacinacao >   8.105263 & dados21_ind_mun$IndAcoesVacinacao <= 15.052632 ~ "Regular",
  dados21_ind_mun$IndAcoesVacinacao >  15.052632 & dados21_ind_mun$IndAcoesVacinacao <= 18.526316 ~ "Bom",
  dados21_ind_mun$IndAcoesVacinacao >  18.526316 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

# dados21_ind_mun$IndPraticasIntComp_cat = factor(case_when(
#   dados21_ind_mun$IndPraticasIntComp <= 3 ~ "Péssimo",
#   dados21_ind_mun$IndPraticasIntComp > 3 & dados21_ind_mun$IndPraticasIntComp <= 8 ~ "Ruim",
#   dados21_ind_mun$IndPraticasIntComp > 8 & dados21_ind_mun$IndPraticasIntComp <= 14 ~ "Regular",
#   dados21_ind_mun$IndPraticasIntComp > 14 & dados21_ind_mun$IndPraticasIntComp <= 18 ~ "Bom",
#   dados21_ind_mun$IndPraticasIntComp > 18 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndAtendimentoUrgEmerg_cat = factor(case_when(
  dados21_ind_mun$IndAtendimentoUrgEmerg <=  5.210526 ~ "Péssimo",
  dados21_ind_mun$IndAtendimentoUrgEmerg >   5.210526 & dados21_ind_mun$IndAtendimentoUrgEmerg <= 12.157895 ~ "Ruim",
  dados21_ind_mun$IndAtendimentoUrgEmerg >  12.157895 & dados21_ind_mun$IndAtendimentoUrgEmerg <= 22.578947 ~ "Regular",
  dados21_ind_mun$IndAtendimentoUrgEmerg >  22.578947 & dados21_ind_mun$IndAtendimentoUrgEmerg <= 27.789474 ~ "Bom",
  dados21_ind_mun$IndAtendimentoUrgEmerg >  27.789474 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndAtendimentoProgDemandaEsp_cat = factor(case_when(
  dados21_ind_mun$IndAtendimentoProgDemandaEsp <=  2.368421 ~ "Péssimo",
  dados21_ind_mun$IndAtendimentoProgDemandaEsp >   2.368421 & dados21_ind_mun$IndAtendimentoProgDemandaEsp <=  5.526316 ~ "Ruim",
  dados21_ind_mun$IndAtendimentoProgDemandaEsp >   5.526316 & dados21_ind_mun$IndAtendimentoProgDemandaEsp <= 10.263158 ~ "Regular",
  dados21_ind_mun$IndAtendimentoProgDemandaEsp >  10.263158 & dados21_ind_mun$IndAtendimentoProgDemandaEsp <= 12.631579 ~ "Bom",
  dados21_ind_mun$IndAtendimentoProgDemandaEsp >  12.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndAtendimentoDemandaEsp_cat <- factor(case_when(
  dados21_ind_mun$IndAtendimentoDemandaEsp <= 0.157895 ~ "Péssimo",
  dados21_ind_mun$IndAtendimentoDemandaEsp >  0.157895 & dados21_ind_mun$IndAtendimentoDemandaEsp <= 0.368421 ~ "Ruim",
  dados21_ind_mun$IndAtendimentoDemandaEsp >  0.368421 & dados21_ind_mun$IndAtendimentoDemandaEsp <= 0.684211 ~ "Regular",
  dados21_ind_mun$IndAtendimentoDemandaEsp >  0.684211 & dados21_ind_mun$IndAtendimentoDemandaEsp <= 0.842105 ~ "Bom",
  dados21_ind_mun$IndAtendimentoDemandaEsp >  0.842105 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndIntegracaoAPS_cat = factor(case_when(
  dados21_ind_mun$IndIntegracaoAPS <=  4.578947 ~ "Péssimo",
  dados21_ind_mun$IndIntegracaoAPS >   4.578947 & dados21_ind_mun$IndIntegracaoAPS <= 10.684211 ~ "Ruim",
  dados21_ind_mun$IndIntegracaoAPS >  10.684211 & dados21_ind_mun$IndIntegracaoAPS <= 19.842105 ~ "Regular",
  dados21_ind_mun$IndIntegracaoAPS >  19.842105 & dados21_ind_mun$IndIntegracaoAPS <= 24.421053 ~ "Bom",
  dados21_ind_mun$IndIntegracaoAPS >  24.421053 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndRegulacaoAssistencial_cat = factor(case_when(
  dados21_ind_mun$IndRegulacaoAssistencial <= 0.789474 ~ "Péssimo",
  dados21_ind_mun$IndRegulacaoAssistencial >  0.789474 & dados21_ind_mun$IndRegulacaoAssistencial <= 1.842105 ~ "Ruim",
  dados21_ind_mun$IndRegulacaoAssistencial >  1.842105 & dados21_ind_mun$IndRegulacaoAssistencial <= 3.421053 ~ "Regular",
  dados21_ind_mun$IndRegulacaoAssistencial >  3.421053 & dados21_ind_mun$IndRegulacaoAssistencial <= 4.210526 ~ "Bom",
  dados21_ind_mun$IndRegulacaoAssistencial >  4.210526 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_mun$IndCuidadoCompartilhado_cat = factor(case_when(
  dados21_ind_mun$IndCuidadoCompartilhado <=  8.368421 ~ "Péssimo",
  dados21_ind_mun$IndCuidadoCompartilhado >   8.368421 & dados21_ind_mun$IndCuidadoCompartilhado <= 19.526316 ~ "Ruim",
  dados21_ind_mun$IndCuidadoCompartilhado >  19.526316 & dados21_ind_mun$IndCuidadoCompartilhado <= 36.263158 ~ "Regular",
  dados21_ind_mun$IndCuidadoCompartilhado >  36.263158 & dados21_ind_mun$IndCuidadoCompartilhado <= 44.631579 ~ "Bom",
  dados21_ind_mun$IndCuidadoCompartilhado >  44.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

#Até urgência e emergência
dados21_ind_mun$Indicador_Panorama_cat = factor(case_when(
  dados21_ind_mun$Indicador_Panorama <= 49.26 ~ "Péssimo",
  dados21_ind_mun$Indicador_Panorama > 49.26 & dados21_ind_mun$Indicador_Panorama <= 114.95 ~ "Ruim",
  dados21_ind_mun$Indicador_Panorama > 114.95 & dados21_ind_mun$Indicador_Panorama <= 213.47 ~ "Regular",
  dados21_ind_mun$Indicador_Panorama > 213.47 & dados21_ind_mun$Indicador_Panorama <= 262.74 ~ "Bom",
  dados21_ind_mun$Indicador_Panorama > 262.74 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))
#write.xlsx(dados21_ind_mun %>% as.data.frame(),'Dados com indicadores categorizados agrupados por município.xlsx')

####==================================================
#### Categorizar indicadores por região metropolitana
####==================================================
dados_rm$CODIGO_sem_ultimo = as.numeric(str_sub(as.character(dados_rm$CODIGO), 1, -2))
dados21_ind_rm = left_join(dados21 %>% as.data.frame(),
                           dados_rm %>% select(REGIÃO.METROPOLITANA,CODIGO_sem_ultimo), 
                           by = c('CO_MUNICIPIO_IBGE'='CODIGO_sem_ultimo')) %>% filter(is.na(REGIÃO.METROPOLITANA) == FALSE)

dados_ind_rm_agg_rm = dados21_ind_rm %>% 
  # select(V16,V17,REGIÃO.METROPOLITANA,
  #        c(vars_saude_sexual, add_num(vars_saude_sexual)), IndSaudeSexual,
  #        c(vars_pre_natal, add_num(vars_pre_natal)), IndPreNatal,
  #        c(vars_saude_mulher, add_num(vars_saude_mulher)), IndSaudeMulher,
  #        c(vars_saude_crianca, add_num(vars_saude_crianca)), IndSaudeCrianca,
  #        c(vars_hipertenso, add_num(vars_hipertenso)), IndHipertenso,
  #        c(vars_diabetico, add_num(vars_diabetico)), IndDiabetico,
  #        c(vars_obesidade, add_num(vars_obesidade)), IndObesidade,
  #        c(vars_tuberculose_hanseniase, add_num(vars_tuberculose_hanseniase)), IndTuberculoseHanseniase,
  #        c(vars_sofrimento_psi, add_num(vars_sofrimento_psi)), IndSofrimentoPsi,
  #        c(vars_violencia, add_num(vars_violencia)), IndViolencia,
  #        c(vars_idosa, add_num(vars_idosa)), IndIdosa,
  #        c(vars_saude_homem, add_num(vars_saude_homem)), IndSaudeHomem,
  #        c(vars_acamadas, add_num(vars_acamadas)), IndPessoasAcamadas,
  #        c(vars_acoes_vacinacao, add_num(vars_acoes_vacinacao)), IndAcoesVacinacao,
  #        #c(vars_praticas_int_comp, add_num(vars_praticas_int_comp)), IndPraticasIntComp,
  #        c(vars_atendimento_urg_emerg, add_num(vars_atendimento_urg_emerg)), IndAtendimentoUrgEmerg,
  #        c(vars_atendimento_prog_demanda_esp, add_num(vars_atendimento_prog_demanda_esp)), IndAtendimentoProgDemandaEsp,
  #        v105, v105num, IndAtendimentoDemandaEsp,
  #        c(vars_integracao_aps, add_num(vars_integracao_aps)), IndIntegracaoAPS,
  #        c(vars_regulacao_assistencial, add_num(vars_regulacao_assistencial)), IndRegulacaoAssistencial,
  #        c(vars_cuidado_compartilhado, add_num(vars_cuidado_compartilhado)), IndCuidadoCompartilhado) %>%
  mutate(Indicador_Panorama = rowSums(across(c(IndSaudeSexual, IndPreNatal, IndSaudeMulher, IndSaudeCrianca,
                                           IndHipertenso, IndDiabetico, IndObesidade, IndTuberculoseHanseniase,
                                           IndSofrimentoPsi, IndViolencia, IndIdosa, IndSaudeHomem,
                                           IndPessoasAcamadas, IndAcoesVacinacao, #IndPraticasIntComp,
                                           IndAtendimentoUrgEmerg)), na.rm = TRUE),
         Regiao = case_when(V17 %in% c("AC","AM","AP","PA","RO","RR","TO") ~ "Norte",
                            V17 %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE") ~ "Nordeste",
                            V17 %in% c("DF","GO","MT","MS") ~ "Centro-Oeste",
                            V17 %in% c("ES","MG","RJ","SP") ~ "Sudeste",
                            V17 %in% c("PR","RS","SC") ~ "Sul",
                            TRUE ~ NA_character_)) %>%
  group_by(REGIÃO.METROPOLITANA,V17,Regiao) %>%
  summarise(IndSaudeSexual = mean(IndSaudeSexual, na.rm = TRUE),
            IndPreNatal = mean(IndPreNatal, na.rm = TRUE),
            IndSaudeMulher = mean(IndSaudeMulher, na.rm = TRUE),
            IndSaudeCrianca = mean(IndSaudeCrianca, na.rm = TRUE),
            IndHipertenso = mean(IndHipertenso, na.rm = TRUE),
            IndDiabetico = mean(IndDiabetico, na.rm = TRUE),
            IndObesidade = mean(IndObesidade, na.rm = TRUE),
            IndTuberculoseHanseniase = mean(IndTuberculoseHanseniase, na.rm = TRUE),
            IndSofrimentoPsi = mean(IndSofrimentoPsi, na.rm = TRUE),
            IndViolencia = mean(IndViolencia, na.rm = TRUE),
            IndIdosa = mean(IndIdosa, na.rm = TRUE),
            IndSaudeHomem = mean(IndSaudeHomem, na.rm = TRUE),
            IndPessoasAcamadas = mean(IndPessoasAcamadas, na.rm = TRUE),
            IndAcoesVacinacao = mean(IndAcoesVacinacao, na.rm = TRUE),
            #IndPraticasIntComp = mean(IndPraticasIntComp, na.rm = TRUE),
            IndAtendimentoUrgEmerg = mean(IndAtendimentoUrgEmerg, na.rm = TRUE),
            IndAtendimentoProgDemandaEsp = mean(IndAtendimentoProgDemandaEsp, na.rm = TRUE),
            IndAtendimentoDemandaEsp = mean(IndAtendimentoDemandaEsp, na.rm = TRUE),
            IndIntegracaoAPS = mean(IndIntegracaoAPS, na.rm = TRUE),
            IndRegulacaoAssistencial = mean(IndRegulacaoAssistencial, na.rm = TRUE),
            IndCuidadoCompartilhado = mean(IndCuidadoCompartilhado, na.rm = TRUE),
            Indicador_Panorama = mean(Indicador_Panorama, na.rm = TRUE)) %>% as.data.frame()

dados_ind_rm_agg_rm$IndSaudeSexual_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndSaudeSexual <= 3.000000 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndSaudeSexual >  3.000000 & dados_ind_rm_agg_rm$IndSaudeSexual <=  7.000000 ~ "Ruim",
  dados_ind_rm_agg_rm$IndSaudeSexual >  7.000000 & dados_ind_rm_agg_rm$IndSaudeSexual <= 13.000000 ~ "Regular",
  dados_ind_rm_agg_rm$IndSaudeSexual > 13.000000 & dados_ind_rm_agg_rm$IndSaudeSexual <= 16.000000 ~ "Bom",
  dados_ind_rm_agg_rm$IndSaudeSexual > 16.000000 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndPreNatal_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndPreNatal <= 5.684211 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndPreNatal >  5.684211 & dados_ind_rm_agg_rm$IndPreNatal <= 13.263158 ~ "Ruim",
  dados_ind_rm_agg_rm$IndPreNatal > 13.263158 & dados_ind_rm_agg_rm$IndPreNatal <= 24.631579 ~ "Regular",
  dados_ind_rm_agg_rm$IndPreNatal > 24.631579 & dados_ind_rm_agg_rm$IndPreNatal <= 30.315789 ~ "Bom",
  dados_ind_rm_agg_rm$IndPreNatal > 30.315789 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndSaudeMulher_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndSaudeMulher <= 1.263158 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndSaudeMulher >  1.263158 & dados_ind_rm_agg_rm$IndSaudeMulher <= 2.947368 ~ "Ruim",
  dados_ind_rm_agg_rm$IndSaudeMulher >  2.947368 & dados_ind_rm_agg_rm$IndSaudeMulher <= 5.473684 ~ "Regular",
  dados_ind_rm_agg_rm$IndSaudeMulher >  5.473684 & dados_ind_rm_agg_rm$IndSaudeMulher <= 6.736842 ~ "Bom",
  dados_ind_rm_agg_rm$IndSaudeMulher >  6.736842 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndSaudeCrianca_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndSaudeCrianca <= 2.842105 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndSaudeCrianca >  2.842105 & dados_ind_rm_agg_rm$IndSaudeCrianca <=  6.631579 ~ "Ruim",
  dados_ind_rm_agg_rm$IndSaudeCrianca >  6.631579 & dados_ind_rm_agg_rm$IndSaudeCrianca <= 12.315789 ~ "Regular",
  dados_ind_rm_agg_rm$IndSaudeCrianca > 12.315789 & dados_ind_rm_agg_rm$IndSaudeCrianca <= 15.157895 ~ "Bom",
  dados_ind_rm_agg_rm$IndSaudeCrianca > 15.157895 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndHipertenso_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndHipertenso <= 3.315789 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndHipertenso >  3.315789 & dados_ind_rm_agg_rm$IndHipertenso <=  7.736842 ~ "Ruim",
  dados_ind_rm_agg_rm$IndHipertenso >  7.736842 & dados_ind_rm_agg_rm$IndHipertenso <= 14.368421 ~ "Regular",
  dados_ind_rm_agg_rm$IndHipertenso > 14.368421 & dados_ind_rm_agg_rm$IndHipertenso <= 17.684211 ~ "Bom",
  dados_ind_rm_agg_rm$IndHipertenso > 17.684211 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndDiabetico_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndDiabetico <= 3.789474 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndDiabetico >  3.789474 & dados_ind_rm_agg_rm$IndDiabetico <=  8.842105 ~ "Ruim",
  dados_ind_rm_agg_rm$IndDiabetico >  8.842105 & dados_ind_rm_agg_rm$IndDiabetico <= 16.421053 ~ "Regular",
  dados_ind_rm_agg_rm$IndDiabetico > 16.421053 & dados_ind_rm_agg_rm$IndDiabetico <= 20.210526 ~ "Bom",
  dados_ind_rm_agg_rm$IndDiabetico > 20.210526 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndObesidade_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndObesidade <= 2.368421 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndObesidade >  2.368421 & dados_ind_rm_agg_rm$IndObesidade <=  5.526316 ~ "Ruim",
  dados_ind_rm_agg_rm$IndObesidade >  5.526316 & dados_ind_rm_agg_rm$IndObesidade <= 10.263158 ~ "Regular",
  dados_ind_rm_agg_rm$IndObesidade > 10.263158 & dados_ind_rm_agg_rm$IndObesidade <= 12.631579 ~ "Bom",
  dados_ind_rm_agg_rm$IndObesidade > 12.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndTuberculoseHanseniase_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndTuberculoseHanseniase <=  4.421053 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndTuberculoseHanseniase >   4.421053 & dados_ind_rm_agg_rm$IndTuberculoseHanseniase <= 10.315789 ~ "Ruim",
  dados_ind_rm_agg_rm$IndTuberculoseHanseniase >  10.315789 & dados_ind_rm_agg_rm$IndTuberculoseHanseniase <= 19.157895 ~ "Regular",
  dados_ind_rm_agg_rm$IndTuberculoseHanseniase >  19.157895 & dados_ind_rm_agg_rm$IndTuberculoseHanseniase <= 23.578947 ~ "Bom",
  dados_ind_rm_agg_rm$IndTuberculoseHanseniase >  23.578947 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndSofrimentoPsi_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndSofrimentoPsi <= 1.578947 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndSofrimentoPsi >  1.578947 & dados_ind_rm_agg_rm$IndSofrimentoPsi <= 3.684211 ~ "Ruim",
  dados_ind_rm_agg_rm$IndSofrimentoPsi >  3.684211 & dados_ind_rm_agg_rm$IndSofrimentoPsi <= 6.842105 ~ "Regular",
  dados_ind_rm_agg_rm$IndSofrimentoPsi >  6.842105 & dados_ind_rm_agg_rm$IndSofrimentoPsi <= 8.421053 ~ "Bom",
  dados_ind_rm_agg_rm$IndSofrimentoPsi >  8.421053 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndViolencia_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndViolencia <= 1.736842 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndViolencia >  1.736842 & dados_ind_rm_agg_rm$IndViolencia <= 4.052632 ~ "Ruim",
  dados_ind_rm_agg_rm$IndViolencia >  4.052632 & dados_ind_rm_agg_rm$IndViolencia <= 7.526316 ~ "Regular",
  dados_ind_rm_agg_rm$IndViolencia >  7.526316 & dados_ind_rm_agg_rm$IndViolencia <= 9.263158 ~ "Bom",
  dados_ind_rm_agg_rm$IndViolencia >  9.263158 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndIdosa_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndIdosa <= 2.052632 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndIdosa >  2.052632 & dados_ind_rm_agg_rm$IndIdosa <= 4.789474 ~ "Ruim",
  dados_ind_rm_agg_rm$IndIdosa >  4.789474 & dados_ind_rm_agg_rm$IndIdosa <= 8.894737 ~ "Regular",
  dados_ind_rm_agg_rm$IndIdosa >  8.894737 & dados_ind_rm_agg_rm$IndIdosa <= 10.947368 ~ "Bom",
  dados_ind_rm_agg_rm$IndIdosa > 10.947368 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndSaudeHomem_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndSaudeHomem <= 1.421053 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndSaudeHomem >  1.421053 & dados_ind_rm_agg_rm$IndSaudeHomem <= 3.315789 ~ "Ruim",
  dados_ind_rm_agg_rm$IndSaudeHomem >  3.315789 & dados_ind_rm_agg_rm$IndSaudeHomem <= 6.157895 ~ "Regular",
  dados_ind_rm_agg_rm$IndSaudeHomem >  6.157895 & dados_ind_rm_agg_rm$IndSaudeHomem <= 7.578947 ~ "Bom",
  dados_ind_rm_agg_rm$IndSaudeHomem >  7.578947 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndPessoasAcamadas_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndPessoasAcamadas <=  3.947368 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndPessoasAcamadas >   3.947368 & dados_ind_rm_agg_rm$IndPessoasAcamadas <=  9.210526 ~ "Ruim",
  dados_ind_rm_agg_rm$IndPessoasAcamadas >   9.210526 & dados_ind_rm_agg_rm$IndPessoasAcamadas <= 17.105263 ~ "Regular",
  dados_ind_rm_agg_rm$IndPessoasAcamadas >  17.105263 & dados_ind_rm_agg_rm$IndPessoasAcamadas <= 21.052632 ~ "Bom",
  dados_ind_rm_agg_rm$IndPessoasAcamadas >  21.052632 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndAcoesVacinacao_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndAcoesVacinacao <=  3.473684 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndAcoesVacinacao >   3.473684 & dados_ind_rm_agg_rm$IndAcoesVacinacao <=  8.105263 ~ "Ruim",
  dados_ind_rm_agg_rm$IndAcoesVacinacao >   8.105263 & dados_ind_rm_agg_rm$IndAcoesVacinacao <= 15.052632 ~ "Regular",
  dados_ind_rm_agg_rm$IndAcoesVacinacao >  15.052632 & dados_ind_rm_agg_rm$IndAcoesVacinacao <= 18.526316 ~ "Bom",
  dados_ind_rm_agg_rm$IndAcoesVacinacao >  18.526316 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

# dados_ind_rm_agg_rm$IndPraticasIntComp_cat = factor(case_when(
#   dados_ind_rm_agg_rm$IndPraticasIntComp <= 3 ~ "Péssimo",
#   dados_ind_rm_agg_rm$IndPraticasIntComp > 3 & dados_ind_rm_agg_rm$IndPraticasIntComp <= 8 ~ "Ruim",
#   dados_ind_rm_agg_rm$IndPraticasIntComp > 8 & dados_ind_rm_agg_rm$IndPraticasIntComp <= 14 ~ "Regular",
#   dados_ind_rm_agg_rm$IndPraticasIntComp > 14 & dados_ind_rm_agg_rm$IndPraticasIntComp <= 18 ~ "Bom",
#   dados_ind_rm_agg_rm$IndPraticasIntComp > 18 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndAtendimentoUrgEmerg_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndAtendimentoUrgEmerg <=  5.210526 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndAtendimentoUrgEmerg >   5.210526 & dados_ind_rm_agg_rm$IndAtendimentoUrgEmerg <= 12.157895 ~ "Ruim",
  dados_ind_rm_agg_rm$IndAtendimentoUrgEmerg >  12.157895 & dados_ind_rm_agg_rm$IndAtendimentoUrgEmerg <= 22.578947 ~ "Regular",
  dados_ind_rm_agg_rm$IndAtendimentoUrgEmerg >  22.578947 & dados_ind_rm_agg_rm$IndAtendimentoUrgEmerg <= 27.789474 ~ "Bom",
  dados_ind_rm_agg_rm$IndAtendimentoUrgEmerg >  27.789474 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndAtendimentoProgDemandaEsp_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndAtendimentoProgDemandaEsp <=  2.368421 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndAtendimentoProgDemandaEsp >   2.368421 & dados_ind_rm_agg_rm$IndAtendimentoProgDemandaEsp <=  5.526316 ~ "Ruim",
  dados_ind_rm_agg_rm$IndAtendimentoProgDemandaEsp >   5.526316 & dados_ind_rm_agg_rm$IndAtendimentoProgDemandaEsp <= 10.263158 ~ "Regular",
  dados_ind_rm_agg_rm$IndAtendimentoProgDemandaEsp >  10.263158 & dados_ind_rm_agg_rm$IndAtendimentoProgDemandaEsp <= 12.631579 ~ "Bom",
  dados_ind_rm_agg_rm$IndAtendimentoProgDemandaEsp >  12.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndAtendimentoDemandaEsp_cat <- factor(case_when(
  dados_ind_rm_agg_rm$IndAtendimentoDemandaEsp <= 0.157895 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndAtendimentoDemandaEsp >  0.157895 & dados_ind_rm_agg_rm$IndAtendimentoDemandaEsp <= 0.368421 ~ "Ruim",
  dados_ind_rm_agg_rm$IndAtendimentoDemandaEsp >  0.368421 & dados_ind_rm_agg_rm$IndAtendimentoDemandaEsp <= 0.684211 ~ "Regular",
  dados_ind_rm_agg_rm$IndAtendimentoDemandaEsp >  0.684211 & dados_ind_rm_agg_rm$IndAtendimentoDemandaEsp <= 0.842105 ~ "Bom",
  dados_ind_rm_agg_rm$IndAtendimentoDemandaEsp >  0.842105 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndIntegracaoAPS_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndIntegracaoAPS <=  4.578947 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndIntegracaoAPS >   4.578947 & dados_ind_rm_agg_rm$IndIntegracaoAPS <= 10.684211 ~ "Ruim",
  dados_ind_rm_agg_rm$IndIntegracaoAPS >  10.684211 & dados_ind_rm_agg_rm$IndIntegracaoAPS <= 19.842105 ~ "Regular",
  dados_ind_rm_agg_rm$IndIntegracaoAPS >  19.842105 & dados_ind_rm_agg_rm$IndIntegracaoAPS <= 24.421053 ~ "Bom",
  dados_ind_rm_agg_rm$IndIntegracaoAPS >  24.421053 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndRegulacaoAssistencial_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndRegulacaoAssistencial <= 0.789474 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndRegulacaoAssistencial >  0.789474 & dados_ind_rm_agg_rm$IndRegulacaoAssistencial <= 1.842105 ~ "Ruim",
  dados_ind_rm_agg_rm$IndRegulacaoAssistencial >  1.842105 & dados_ind_rm_agg_rm$IndRegulacaoAssistencial <= 3.421053 ~ "Regular",
  dados_ind_rm_agg_rm$IndRegulacaoAssistencial >  3.421053 & dados_ind_rm_agg_rm$IndRegulacaoAssistencial <= 4.210526 ~ "Bom",
  dados_ind_rm_agg_rm$IndRegulacaoAssistencial >  4.210526 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados_ind_rm_agg_rm$IndCuidadoCompartilhado_cat = factor(case_when(
  dados_ind_rm_agg_rm$IndCuidadoCompartilhado <=  8.368421 ~ "Péssimo",
  dados_ind_rm_agg_rm$IndCuidadoCompartilhado >   8.368421 & dados_ind_rm_agg_rm$IndCuidadoCompartilhado <= 19.526316 ~ "Ruim",
  dados_ind_rm_agg_rm$IndCuidadoCompartilhado >  19.526316 & dados_ind_rm_agg_rm$IndCuidadoCompartilhado <= 36.263158 ~ "Regular",
  dados_ind_rm_agg_rm$IndCuidadoCompartilhado >  36.263158 & dados_ind_rm_agg_rm$IndCuidadoCompartilhado <= 44.631579 ~ "Bom",
  dados_ind_rm_agg_rm$IndCuidadoCompartilhado >  44.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

#Até urgência e emergência
dados_ind_rm_agg_rm$Indicador_Panorama_cat = factor(case_when(
  dados_ind_rm_agg_rm$Indicador_Panorama <= 49.26 ~ "Péssimo",
  dados_ind_rm_agg_rm$Indicador_Panorama > 49.26 & dados_ind_rm_agg_rm$Indicador_Panorama <= 114.95 ~ "Ruim",
  dados_ind_rm_agg_rm$Indicador_Panorama > 114.95 & dados_ind_rm_agg_rm$Indicador_Panorama <= 213.47 ~ "Regular",
  dados_ind_rm_agg_rm$Indicador_Panorama > 213.47 & dados_ind_rm_agg_rm$Indicador_Panorama <= 262.74 ~ "Bom",
  dados_ind_rm_agg_rm$Indicador_Panorama > 262.74 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))
#write.xlsx(dados_ind_rm_agg_rm %>% as.data.frame(),'Dados com indicadores categorizados agrupados por região metropolitana.xlsx')

####====================================
#### Categorizar indicadores por Estado
####====================================
dados21_ind_estado = dados21 %>% 
  # select(CO_MUNICIPIO_IBGE,V16,V17,
  #        c(vars_saude_sexual, add_num(vars_saude_sexual)), IndSaudeSexual,
  #        c(vars_pre_natal, add_num(vars_pre_natal)), IndPreNatal,
  #        c(vars_saude_mulher, add_num(vars_saude_mulher)), IndSaudeMulher,
  #        c(vars_saude_crianca, add_num(vars_saude_crianca)), IndSaudeCrianca,
  #        c(vars_hipertenso, add_num(vars_hipertenso)), IndHipertenso,
  #        c(vars_diabetico, add_num(vars_diabetico)), IndDiabetico,
  #        c(vars_obesidade, add_num(vars_obesidade)), IndObesidade,
  #        c(vars_tuberculose_hanseniase, add_num(vars_tuberculose_hanseniase)), IndTuberculoseHanseniase,
  #        c(vars_sofrimento_psi, add_num(vars_sofrimento_psi)), IndSofrimentoPsi,
  #        c(vars_violencia, add_num(vars_violencia)), IndViolencia,
  #        c(vars_idosa, add_num(vars_idosa)), IndIdosa,
  #        c(vars_saude_homem, add_num(vars_saude_homem)), IndSaudeHomem,
  #        c(vars_acamadas, add_num(vars_acamadas)), IndPessoasAcamadas,
  #        c(vars_acoes_vacinacao, add_num(vars_acoes_vacinacao)), IndAcoesVacinacao,
  #        #c(vars_praticas_int_comp, add_num(vars_praticas_int_comp)), IndPraticasIntComp,
  #        c(vars_atendimento_urg_emerg, add_num(vars_atendimento_urg_emerg)), IndAtendimentoUrgEmerg,
  #        c(vars_atendimento_prog_demanda_esp, add_num(vars_atendimento_prog_demanda_esp)), IndAtendimentoProgDemandaEsp,
  #        v105, v105num, IndAtendimentoDemandaEsp,
  #        c(vars_integracao_aps, add_num(vars_integracao_aps)), IndIntegracaoAPS,
  #        c(vars_regulacao_assistencial, add_num(vars_regulacao_assistencial)), IndRegulacaoAssistencial,
  #        c(vars_cuidado_compartilhado, add_num(vars_cuidado_compartilhado)), IndCuidadoCompartilhado) %>%
  mutate(Indicador_Panorama = rowSums(across(c(IndSaudeSexual, IndPreNatal, IndSaudeMulher, IndSaudeCrianca,
                                           IndHipertenso, IndDiabetico, IndObesidade, IndTuberculoseHanseniase,
                                           IndSofrimentoPsi, IndViolencia, IndIdosa, IndSaudeHomem,
                                           IndPessoasAcamadas, IndAcoesVacinacao, #IndPraticasIntComp,
                                           IndAtendimentoUrgEmerg)), na.rm = TRUE),
         Regiao = case_when(V17 %in% c("AC","AM","AP","PA","RO","RR","TO") ~ "Norte",
                            V17 %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE") ~ "Nordeste",
                            V17 %in% c("DF","GO","MT","MS") ~ "Centro-Oeste",
                            V17 %in% c("ES","MG","RJ","SP") ~ "Sudeste",
                            V17 %in% c("PR","RS","SC") ~ "Sul",
                            TRUE ~ NA_character_)) %>% 
  group_by(Regiao, V17) %>%
  summarise(IndSaudeSexual = mean(IndSaudeSexual, na.rm = TRUE),
            IndPreNatal = mean(IndPreNatal, na.rm = TRUE),
            IndSaudeMulher = mean(IndSaudeMulher, na.rm = TRUE),
            IndSaudeCrianca = mean(IndSaudeCrianca, na.rm = TRUE),
            IndHipertenso = mean(IndHipertenso, na.rm = TRUE),
            IndDiabetico = mean(IndDiabetico, na.rm = TRUE),
            IndObesidade = mean(IndObesidade, na.rm = TRUE),
            IndTuberculoseHanseniase = mean(IndTuberculoseHanseniase, na.rm = TRUE),
            IndSofrimentoPsi = mean(IndSofrimentoPsi, na.rm = TRUE),
            IndViolencia = mean(IndViolencia, na.rm = TRUE),
            IndIdosa = mean(IndIdosa, na.rm = TRUE),
            IndSaudeHomem = mean(IndSaudeHomem, na.rm = TRUE),
            IndPessoasAcamadas = mean(IndPessoasAcamadas, na.rm = TRUE),
            IndAcoesVacinacao = mean(IndAcoesVacinacao, na.rm = TRUE),
            #IndPraticasIntComp = mean(IndPraticasIntComp, na.rm = TRUE),
            IndAtendimentoUrgEmerg = mean(IndAtendimentoUrgEmerg, na.rm = TRUE),
            IndAtendimentoProgDemandaEsp = mean(IndAtendimentoProgDemandaEsp, na.rm = TRUE),
            IndAtendimentoDemandaEsp = mean(IndAtendimentoDemandaEsp, na.rm = TRUE),
            IndIntegracaoAPS = mean(IndIntegracaoAPS, na.rm = TRUE),
            IndRegulacaoAssistencial = mean(IndRegulacaoAssistencial, na.rm = TRUE),
            IndCuidadoCompartilhado = mean(IndCuidadoCompartilhado, na.rm = TRUE),
            Indicador_Panorama = mean(Indicador_Panorama, na.rm = TRUE)) %>% as.data.frame()
#write.xlsx(dados21_ind %>% as.data.frame(),'Dados com indicadores agrupados por municípios.xlsx')

dados21_ind_estado$IndSaudeSexual_cat = factor(case_when(
  dados21_ind_estado$IndSaudeSexual <= 3.000000 ~ "Péssimo",
  dados21_ind_estado$IndSaudeSexual >  3.000000 & dados21_ind_estado$IndSaudeSexual <=  7.000000 ~ "Ruim",
  dados21_ind_estado$IndSaudeSexual >  7.000000 & dados21_ind_estado$IndSaudeSexual <= 13.000000 ~ "Regular",
  dados21_ind_estado$IndSaudeSexual > 13.000000 & dados21_ind_estado$IndSaudeSexual <= 16.000000 ~ "Bom",
  dados21_ind_estado$IndSaudeSexual > 16.000000 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndPreNatal_cat = factor(case_when(
  dados21_ind_estado$IndPreNatal <= 5.684211 ~ "Péssimo",
  dados21_ind_estado$IndPreNatal >  5.684211 & dados21_ind_estado$IndPreNatal <= 13.263158 ~ "Ruim",
  dados21_ind_estado$IndPreNatal > 13.263158 & dados21_ind_estado$IndPreNatal <= 24.631579 ~ "Regular",
  dados21_ind_estado$IndPreNatal > 24.631579 & dados21_ind_estado$IndPreNatal <= 30.315789 ~ "Bom",
  dados21_ind_estado$IndPreNatal > 30.315789 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndSaudeMulher_cat = factor(case_when(
  dados21_ind_estado$IndSaudeMulher <= 1.263158 ~ "Péssimo",
  dados21_ind_estado$IndSaudeMulher >  1.263158 & dados21_ind_estado$IndSaudeMulher <= 2.947368 ~ "Ruim",
  dados21_ind_estado$IndSaudeMulher >  2.947368 & dados21_ind_estado$IndSaudeMulher <= 5.473684 ~ "Regular",
  dados21_ind_estado$IndSaudeMulher >  5.473684 & dados21_ind_estado$IndSaudeMulher <= 6.736842 ~ "Bom",
  dados21_ind_estado$IndSaudeMulher >  6.736842 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndSaudeCrianca_cat = factor(case_when(
  dados21_ind_estado$IndSaudeCrianca <= 2.842105 ~ "Péssimo",
  dados21_ind_estado$IndSaudeCrianca >  2.842105 & dados21_ind_estado$IndSaudeCrianca <=  6.631579 ~ "Ruim",
  dados21_ind_estado$IndSaudeCrianca >  6.631579 & dados21_ind_estado$IndSaudeCrianca <= 12.315789 ~ "Regular",
  dados21_ind_estado$IndSaudeCrianca > 12.315789 & dados21_ind_estado$IndSaudeCrianca <= 15.157895 ~ "Bom",
  dados21_ind_estado$IndSaudeCrianca > 15.157895 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndHipertenso_cat = factor(case_when(
  dados21_ind_estado$IndHipertenso <= 3.315789 ~ "Péssimo",
  dados21_ind_estado$IndHipertenso >  3.315789 & dados21_ind_estado$IndHipertenso <=  7.736842 ~ "Ruim",
  dados21_ind_estado$IndHipertenso >  7.736842 & dados21_ind_estado$IndHipertenso <= 14.368421 ~ "Regular",
  dados21_ind_estado$IndHipertenso > 14.368421 & dados21_ind_estado$IndHipertenso <= 17.684211 ~ "Bom",
  dados21_ind_estado$IndHipertenso > 17.684211 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndDiabetico_cat = factor(case_when(
  dados21_ind_estado$IndDiabetico <= 3.789474 ~ "Péssimo",
  dados21_ind_estado$IndDiabetico >  3.789474 & dados21_ind_estado$IndDiabetico <=  8.842105 ~ "Ruim",
  dados21_ind_estado$IndDiabetico >  8.842105 & dados21_ind_estado$IndDiabetico <= 16.421053 ~ "Regular",
  dados21_ind_estado$IndDiabetico > 16.421053 & dados21_ind_estado$IndDiabetico <= 20.210526 ~ "Bom",
  dados21_ind_estado$IndDiabetico > 20.210526 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndObesidade_cat = factor(case_when(
  dados21_ind_estado$IndObesidade <= 2.368421 ~ "Péssimo",
  dados21_ind_estado$IndObesidade >  2.368421 & dados21_ind_estado$IndObesidade <=  5.526316 ~ "Ruim",
  dados21_ind_estado$IndObesidade >  5.526316 & dados21_ind_estado$IndObesidade <= 10.263158 ~ "Regular",
  dados21_ind_estado$IndObesidade > 10.263158 & dados21_ind_estado$IndObesidade <= 12.631579 ~ "Bom",
  dados21_ind_estado$IndObesidade > 12.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndTuberculoseHanseniase_cat = factor(case_when(
  dados21_ind_estado$IndTuberculoseHanseniase <=  4.421053 ~ "Péssimo",
  dados21_ind_estado$IndTuberculoseHanseniase >   4.421053 & dados21_ind_estado$IndTuberculoseHanseniase <= 10.315789 ~ "Ruim",
  dados21_ind_estado$IndTuberculoseHanseniase >  10.315789 & dados21_ind_estado$IndTuberculoseHanseniase <= 19.157895 ~ "Regular",
  dados21_ind_estado$IndTuberculoseHanseniase >  19.157895 & dados21_ind_estado$IndTuberculoseHanseniase <= 23.578947 ~ "Bom",
  dados21_ind_estado$IndTuberculoseHanseniase >  23.578947 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndSofrimentoPsi_cat = factor(case_when(
  dados21_ind_estado$IndSofrimentoPsi <= 1.578947 ~ "Péssimo",
  dados21_ind_estado$IndSofrimentoPsi >  1.578947 & dados21_ind_estado$IndSofrimentoPsi <= 3.684211 ~ "Ruim",
  dados21_ind_estado$IndSofrimentoPsi >  3.684211 & dados21_ind_estado$IndSofrimentoPsi <= 6.842105 ~ "Regular",
  dados21_ind_estado$IndSofrimentoPsi >  6.842105 & dados21_ind_estado$IndSofrimentoPsi <= 8.421053 ~ "Bom",
  dados21_ind_estado$IndSofrimentoPsi >  8.421053 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndViolencia_cat = factor(case_when(
  dados21_ind_estado$IndViolencia <= 1.736842 ~ "Péssimo",
  dados21_ind_estado$IndViolencia >  1.736842 & dados21_ind_estado$IndViolencia <= 4.052632 ~ "Ruim",
  dados21_ind_estado$IndViolencia >  4.052632 & dados21_ind_estado$IndViolencia <= 7.526316 ~ "Regular",
  dados21_ind_estado$IndViolencia >  7.526316 & dados21_ind_estado$IndViolencia <= 9.263158 ~ "Bom",
  dados21_ind_estado$IndViolencia >  9.263158 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndIdosa_cat = factor(case_when(
  dados21_ind_estado$IndIdosa <= 2.052632 ~ "Péssimo",
  dados21_ind_estado$IndIdosa >  2.052632 & dados21_ind_estado$IndIdosa <= 4.789474 ~ "Ruim",
  dados21_ind_estado$IndIdosa >  4.789474 & dados21_ind_estado$IndIdosa <= 8.894737 ~ "Regular",
  dados21_ind_estado$IndIdosa >  8.894737 & dados21_ind_estado$IndIdosa <= 10.947368 ~ "Bom",
  dados21_ind_estado$IndIdosa > 10.947368 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndSaudeHomem_cat = factor(case_when(
  dados21_ind_estado$IndSaudeHomem <= 1.421053 ~ "Péssimo",
  dados21_ind_estado$IndSaudeHomem >  1.421053 & dados21_ind_estado$IndSaudeHomem <= 3.315789 ~ "Ruim",
  dados21_ind_estado$IndSaudeHomem >  3.315789 & dados21_ind_estado$IndSaudeHomem <= 6.157895 ~ "Regular",
  dados21_ind_estado$IndSaudeHomem >  6.157895 & dados21_ind_estado$IndSaudeHomem <= 7.578947 ~ "Bom",
  dados21_ind_estado$IndSaudeHomem >  7.578947 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndPessoasAcamadas_cat = factor(case_when(
  dados21_ind_estado$IndPessoasAcamadas <=  3.947368 ~ "Péssimo",
  dados21_ind_estado$IndPessoasAcamadas >   3.947368 & dados21_ind_estado$IndPessoasAcamadas <=  9.210526 ~ "Ruim",
  dados21_ind_estado$IndPessoasAcamadas >   9.210526 & dados21_ind_estado$IndPessoasAcamadas <= 17.105263 ~ "Regular",
  dados21_ind_estado$IndPessoasAcamadas >  17.105263 & dados21_ind_estado$IndPessoasAcamadas <= 21.052632 ~ "Bom",
  dados21_ind_estado$IndPessoasAcamadas >  21.052632 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndAcoesVacinacao_cat = factor(case_when(
  dados21_ind_estado$IndAcoesVacinacao <=  3.473684 ~ "Péssimo",
  dados21_ind_estado$IndAcoesVacinacao >   3.473684 & dados21_ind_estado$IndAcoesVacinacao <=  8.105263 ~ "Ruim",
  dados21_ind_estado$IndAcoesVacinacao >   8.105263 & dados21_ind_estado$IndAcoesVacinacao <= 15.052632 ~ "Regular",
  dados21_ind_estado$IndAcoesVacinacao >  15.052632 & dados21_ind_estado$IndAcoesVacinacao <= 18.526316 ~ "Bom",
  dados21_ind_estado$IndAcoesVacinacao >  18.526316 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

# dados21_ind_estado$IndPraticasIntComp_cat = factor(case_when(
#   dados21_ind_estado$IndPraticasIntComp <= 3 ~ "Péssimo",
#   dados21_ind_estado$IndPraticasIntComp > 3 & dados21_ind_estado$IndPraticasIntComp <= 8 ~ "Ruim",
#   dados21_ind_estado$IndPraticasIntComp > 8 & dados21_ind_estado$IndPraticasIntComp <= 14 ~ "Regular",
#   dados21_ind_estado$IndPraticasIntComp > 14 & dados21_ind_estado$IndPraticasIntComp <= 18 ~ "Bom",
#   dados21_ind_estado$IndPraticasIntComp > 18 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndAtendimentoUrgEmerg_cat = factor(case_when(
  dados21_ind_estado$IndAtendimentoUrgEmerg <=  5.210526 ~ "Péssimo",
  dados21_ind_estado$IndAtendimentoUrgEmerg >   5.210526 & dados21_ind_estado$IndAtendimentoUrgEmerg <= 12.157895 ~ "Ruim",
  dados21_ind_estado$IndAtendimentoUrgEmerg >  12.157895 & dados21_ind_estado$IndAtendimentoUrgEmerg <= 22.578947 ~ "Regular",
  dados21_ind_estado$IndAtendimentoUrgEmerg >  22.578947 & dados21_ind_estado$IndAtendimentoUrgEmerg <= 27.789474 ~ "Bom",
  dados21_ind_estado$IndAtendimentoUrgEmerg >  27.789474 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndAtendimentoProgDemandaEsp_cat = factor(case_when(
  dados21_ind_estado$IndAtendimentoProgDemandaEsp <=  2.368421 ~ "Péssimo",
  dados21_ind_estado$IndAtendimentoProgDemandaEsp >   2.368421 & dados21_ind_estado$IndAtendimentoProgDemandaEsp <=  5.526316 ~ "Ruim",
  dados21_ind_estado$IndAtendimentoProgDemandaEsp >   5.526316 & dados21_ind_estado$IndAtendimentoProgDemandaEsp <= 10.263158 ~ "Regular",
  dados21_ind_estado$IndAtendimentoProgDemandaEsp >  10.263158 & dados21_ind_estado$IndAtendimentoProgDemandaEsp <= 12.631579 ~ "Bom",
  dados21_ind_estado$IndAtendimentoProgDemandaEsp >  12.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndAtendimentoDemandaEsp_cat <- factor(case_when(
  dados21_ind_estado$IndAtendimentoDemandaEsp <= 0.157895 ~ "Péssimo",
  dados21_ind_estado$IndAtendimentoDemandaEsp >  0.157895 & dados21_ind_estado$IndAtendimentoDemandaEsp <= 0.368421 ~ "Ruim",
  dados21_ind_estado$IndAtendimentoDemandaEsp >  0.368421 & dados21_ind_estado$IndAtendimentoDemandaEsp <= 0.684211 ~ "Regular",
  dados21_ind_estado$IndAtendimentoDemandaEsp >  0.684211 & dados21_ind_estado$IndAtendimentoDemandaEsp <= 0.842105 ~ "Bom",
  dados21_ind_estado$IndAtendimentoDemandaEsp >  0.842105 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndIntegracaoAPS_cat = factor(case_when(
  dados21_ind_estado$IndIntegracaoAPS <=  4.578947 ~ "Péssimo",
  dados21_ind_estado$IndIntegracaoAPS >   4.578947 & dados21_ind_estado$IndIntegracaoAPS <= 10.684211 ~ "Ruim",
  dados21_ind_estado$IndIntegracaoAPS >  10.684211 & dados21_ind_estado$IndIntegracaoAPS <= 19.842105 ~ "Regular",
  dados21_ind_estado$IndIntegracaoAPS >  19.842105 & dados21_ind_estado$IndIntegracaoAPS <= 24.421053 ~ "Bom",
  dados21_ind_estado$IndIntegracaoAPS >  24.421053 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndRegulacaoAssistencial_cat = factor(case_when(
  dados21_ind_estado$IndRegulacaoAssistencial <= 0.789474 ~ "Péssimo",
  dados21_ind_estado$IndRegulacaoAssistencial >  0.789474 & dados21_ind_estado$IndRegulacaoAssistencial <= 1.842105 ~ "Ruim",
  dados21_ind_estado$IndRegulacaoAssistencial >  1.842105 & dados21_ind_estado$IndRegulacaoAssistencial <= 3.421053 ~ "Regular",
  dados21_ind_estado$IndRegulacaoAssistencial >  3.421053 & dados21_ind_estado$IndRegulacaoAssistencial <= 4.210526 ~ "Bom",
  dados21_ind_estado$IndRegulacaoAssistencial >  4.210526 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_estado$IndCuidadoCompartilhado_cat = factor(case_when(
  dados21_ind_estado$IndCuidadoCompartilhado <=  8.368421 ~ "Péssimo",
  dados21_ind_estado$IndCuidadoCompartilhado >   8.368421 & dados21_ind_estado$IndCuidadoCompartilhado <= 19.526316 ~ "Ruim",
  dados21_ind_estado$IndCuidadoCompartilhado >  19.526316 & dados21_ind_estado$IndCuidadoCompartilhado <= 36.263158 ~ "Regular",
  dados21_ind_estado$IndCuidadoCompartilhado >  36.263158 & dados21_ind_estado$IndCuidadoCompartilhado <= 44.631579 ~ "Bom",
  dados21_ind_estado$IndCuidadoCompartilhado >  44.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

#Até urgência e emergência
dados21_ind_estado$Indicador_Panorama_cat = factor(case_when(
  dados21_ind_estado$Indicador_Panorama <= 49.26 ~ "Péssimo",
  dados21_ind_estado$Indicador_Panorama > 49.26 & dados21_ind_estado$Indicador_Panorama <= 114.95 ~ "Ruim",
  dados21_ind_estado$Indicador_Panorama > 114.95 & dados21_ind_estado$Indicador_Panorama <= 213.47 ~ "Regular",
  dados21_ind_estado$Indicador_Panorama > 213.47 & dados21_ind_estado$Indicador_Panorama <= 262.74 ~ "Bom",
  dados21_ind_estado$Indicador_Panorama > 262.74 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))
#write.xlsx(dados21_ind_estado %>% as.data.frame(),'Dados com indicadores categorizados agrupados por estado.xlsx')

####====================================
#### Categorizar indicadores por região
####====================================
dados21_ind_regiao = dados21 %>% 
  # select(CO_MUNICIPIO_IBGE,V16,V17,
  #        c(vars_saude_sexual, add_num(vars_saude_sexual)), IndSaudeSexual,
  #        c(vars_pre_natal, add_num(vars_pre_natal)), IndPreNatal,
  #        c(vars_saude_mulher, add_num(vars_saude_mulher)), IndSaudeMulher,
  #        c(vars_saude_crianca, add_num(vars_saude_crianca)), IndSaudeCrianca,
  #        c(vars_hipertenso, add_num(vars_hipertenso)), IndHipertenso,
  #        c(vars_diabetico, add_num(vars_diabetico)), IndDiabetico,
  #        c(vars_obesidade, add_num(vars_obesidade)), IndObesidade,
  #        c(vars_tuberculose_hanseniase, add_num(vars_tuberculose_hanseniase)), IndTuberculoseHanseniase,
  #        c(vars_sofrimento_psi, add_num(vars_sofrimento_psi)), IndSofrimentoPsi,
  #        c(vars_violencia, add_num(vars_violencia)), IndViolencia,
  #        c(vars_idosa, add_num(vars_idosa)), IndIdosa,
  #        c(vars_saude_homem, add_num(vars_saude_homem)), IndSaudeHomem,
  #        c(vars_acamadas, add_num(vars_acamadas)), IndPessoasAcamadas,
  #        c(vars_acoes_vacinacao, add_num(vars_acoes_vacinacao)), IndAcoesVacinacao,
  #        #c(vars_praticas_int_comp, add_num(vars_praticas_int_comp)), IndPraticasIntComp,
  #        c(vars_atendimento_urg_emerg, add_num(vars_atendimento_urg_emerg)), IndAtendimentoUrgEmerg,
  #        c(vars_atendimento_prog_demanda_esp, add_num(vars_atendimento_prog_demanda_esp)), IndAtendimentoProgDemandaEsp,
  #        v105, v105num, IndAtendimentoDemandaEsp,
  #        c(vars_integracao_aps, add_num(vars_integracao_aps)), IndIntegracaoAPS,
  #        c(vars_regulacao_assistencial, add_num(vars_regulacao_assistencial)), IndRegulacaoAssistencial,
  #        c(vars_cuidado_compartilhado, add_num(vars_cuidado_compartilhado)), IndCuidadoCompartilhado) %>%
  mutate(Indicador_Panorama = rowSums(across(c(IndSaudeSexual, IndPreNatal, IndSaudeMulher, IndSaudeCrianca,
                                           IndHipertenso, IndDiabetico, IndObesidade, IndTuberculoseHanseniase,
                                           IndSofrimentoPsi, IndViolencia, IndIdosa, IndSaudeHomem,
                                           IndPessoasAcamadas, IndAcoesVacinacao, #IndPraticasIntComp,
                                           IndAtendimentoUrgEmerg)), na.rm = TRUE),
         Regiao = case_when(V17 %in% c("AC","AM","AP","PA","RO","RR","TO") ~ "Norte",
                            V17 %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE") ~ "Nordeste",
                            V17 %in% c("DF","GO","MT","MS") ~ "Centro-Oeste",
                            V17 %in% c("ES","MG","RJ","SP") ~ "Sudeste",
                            V17 %in% c("PR","RS","SC") ~ "Sul",
                            TRUE ~ NA_character_)) %>% 
  group_by(Regiao) %>%
  summarise(IndSaudeSexual = mean(IndSaudeSexual, na.rm = TRUE),
            IndPreNatal = mean(IndPreNatal, na.rm = TRUE),
            IndSaudeMulher = mean(IndSaudeMulher, na.rm = TRUE),
            IndSaudeCrianca = mean(IndSaudeCrianca, na.rm = TRUE),
            IndHipertenso = mean(IndHipertenso, na.rm = TRUE),
            IndDiabetico = mean(IndDiabetico, na.rm = TRUE),
            IndObesidade = mean(IndObesidade, na.rm = TRUE),
            IndTuberculoseHanseniase = mean(IndTuberculoseHanseniase, na.rm = TRUE),
            IndSofrimentoPsi = mean(IndSofrimentoPsi, na.rm = TRUE),
            IndViolencia = mean(IndViolencia, na.rm = TRUE),
            IndIdosa = mean(IndIdosa, na.rm = TRUE),
            IndSaudeHomem = mean(IndSaudeHomem, na.rm = TRUE),
            IndPessoasAcamadas = mean(IndPessoasAcamadas, na.rm = TRUE),
            IndAcoesVacinacao = mean(IndAcoesVacinacao, na.rm = TRUE),
            #IndPraticasIntComp = mean(IndPraticasIntComp, na.rm = TRUE),
            IndAtendimentoUrgEmerg = mean(IndAtendimentoUrgEmerg, na.rm = TRUE),
            IndAtendimentoProgDemandaEsp = mean(IndAtendimentoProgDemandaEsp, na.rm = TRUE),
            IndAtendimentoDemandaEsp = mean(IndAtendimentoDemandaEsp, na.rm = TRUE),
            IndIntegracaoAPS = mean(IndIntegracaoAPS, na.rm = TRUE),
            IndRegulacaoAssistencial = mean(IndRegulacaoAssistencial, na.rm = TRUE),
            IndCuidadoCompartilhado = mean(IndCuidadoCompartilhado, na.rm = TRUE),
            Indicador_Panorama = mean(Indicador_Panorama, na.rm = TRUE)) %>% as.data.frame()
#write.xlsx(dados21_ind %>% as.data.frame(),'Dados com indicadores agrupados por municípios.xlsx')

dados21_ind_regiao$IndSaudeSexual_cat = factor(case_when(
  dados21_ind_regiao$IndSaudeSexual <= 3.000000 ~ "Péssimo",
  dados21_ind_regiao$IndSaudeSexual >  3.000000 & dados21_ind_regiao$IndSaudeSexual <=  7.000000 ~ "Ruim",
  dados21_ind_regiao$IndSaudeSexual >  7.000000 & dados21_ind_regiao$IndSaudeSexual <= 13.000000 ~ "Regular",
  dados21_ind_regiao$IndSaudeSexual > 13.000000 & dados21_ind_regiao$IndSaudeSexual <= 16.000000 ~ "Bom",
  dados21_ind_regiao$IndSaudeSexual > 16.000000 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndPreNatal_cat = factor(case_when(
  dados21_ind_regiao$IndPreNatal <= 5.684211 ~ "Péssimo",
  dados21_ind_regiao$IndPreNatal >  5.684211 & dados21_ind_regiao$IndPreNatal <= 13.263158 ~ "Ruim",
  dados21_ind_regiao$IndPreNatal > 13.263158 & dados21_ind_regiao$IndPreNatal <= 24.631579 ~ "Regular",
  dados21_ind_regiao$IndPreNatal > 24.631579 & dados21_ind_regiao$IndPreNatal <= 30.315789 ~ "Bom",
  dados21_ind_regiao$IndPreNatal > 30.315789 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndSaudeMulher_cat = factor(case_when(
  dados21_ind_regiao$IndSaudeMulher <= 1.263158 ~ "Péssimo",
  dados21_ind_regiao$IndSaudeMulher >  1.263158 & dados21_ind_regiao$IndSaudeMulher <= 2.947368 ~ "Ruim",
  dados21_ind_regiao$IndSaudeMulher >  2.947368 & dados21_ind_regiao$IndSaudeMulher <= 5.473684 ~ "Regular",
  dados21_ind_regiao$IndSaudeMulher >  5.473684 & dados21_ind_regiao$IndSaudeMulher <= 6.736842 ~ "Bom",
  dados21_ind_regiao$IndSaudeMulher >  6.736842 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndSaudeCrianca_cat = factor(case_when(
  dados21_ind_regiao$IndSaudeCrianca <= 2.842105 ~ "Péssimo",
  dados21_ind_regiao$IndSaudeCrianca >  2.842105 & dados21_ind_regiao$IndSaudeCrianca <=  6.631579 ~ "Ruim",
  dados21_ind_regiao$IndSaudeCrianca >  6.631579 & dados21_ind_regiao$IndSaudeCrianca <= 12.315789 ~ "Regular",
  dados21_ind_regiao$IndSaudeCrianca > 12.315789 & dados21_ind_regiao$IndSaudeCrianca <= 15.157895 ~ "Bom",
  dados21_ind_regiao$IndSaudeCrianca > 15.157895 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndHipertenso_cat = factor(case_when(
  dados21_ind_regiao$IndHipertenso <= 3.315789 ~ "Péssimo",
  dados21_ind_regiao$IndHipertenso >  3.315789 & dados21_ind_regiao$IndHipertenso <=  7.736842 ~ "Ruim",
  dados21_ind_regiao$IndHipertenso >  7.736842 & dados21_ind_regiao$IndHipertenso <= 14.368421 ~ "Regular",
  dados21_ind_regiao$IndHipertenso > 14.368421 & dados21_ind_regiao$IndHipertenso <= 17.684211 ~ "Bom",
  dados21_ind_regiao$IndHipertenso > 17.684211 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndDiabetico_cat = factor(case_when(
  dados21_ind_regiao$IndDiabetico <= 3.789474 ~ "Péssimo",
  dados21_ind_regiao$IndDiabetico >  3.789474 & dados21_ind_regiao$IndDiabetico <=  8.842105 ~ "Ruim",
  dados21_ind_regiao$IndDiabetico >  8.842105 & dados21_ind_regiao$IndDiabetico <= 16.421053 ~ "Regular",
  dados21_ind_regiao$IndDiabetico > 16.421053 & dados21_ind_regiao$IndDiabetico <= 20.210526 ~ "Bom",
  dados21_ind_regiao$IndDiabetico > 20.210526 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndObesidade_cat = factor(case_when(
  dados21_ind_regiao$IndObesidade <= 2.368421 ~ "Péssimo",
  dados21_ind_regiao$IndObesidade >  2.368421 & dados21_ind_regiao$IndObesidade <=  5.526316 ~ "Ruim",
  dados21_ind_regiao$IndObesidade >  5.526316 & dados21_ind_regiao$IndObesidade <= 10.263158 ~ "Regular",
  dados21_ind_regiao$IndObesidade > 10.263158 & dados21_ind_regiao$IndObesidade <= 12.631579 ~ "Bom",
  dados21_ind_regiao$IndObesidade > 12.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndTuberculoseHanseniase_cat = factor(case_when(
  dados21_ind_regiao$IndTuberculoseHanseniase <=  4.421053 ~ "Péssimo",
  dados21_ind_regiao$IndTuberculoseHanseniase >   4.421053 & dados21_ind_regiao$IndTuberculoseHanseniase <= 10.315789 ~ "Ruim",
  dados21_ind_regiao$IndTuberculoseHanseniase >  10.315789 & dados21_ind_regiao$IndTuberculoseHanseniase <= 19.157895 ~ "Regular",
  dados21_ind_regiao$IndTuberculoseHanseniase >  19.157895 & dados21_ind_regiao$IndTuberculoseHanseniase <= 23.578947 ~ "Bom",
  dados21_ind_regiao$IndTuberculoseHanseniase >  23.578947 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndSofrimentoPsi_cat = factor(case_when(
  dados21_ind_regiao$IndSofrimentoPsi <= 1.578947 ~ "Péssimo",
  dados21_ind_regiao$IndSofrimentoPsi >  1.578947 & dados21_ind_regiao$IndSofrimentoPsi <= 3.684211 ~ "Ruim",
  dados21_ind_regiao$IndSofrimentoPsi >  3.684211 & dados21_ind_regiao$IndSofrimentoPsi <= 6.842105 ~ "Regular",
  dados21_ind_regiao$IndSofrimentoPsi >  6.842105 & dados21_ind_regiao$IndSofrimentoPsi <= 8.421053 ~ "Bom",
  dados21_ind_regiao$IndSofrimentoPsi >  8.421053 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndViolencia_cat = factor(case_when(
  dados21_ind_regiao$IndViolencia <= 1.736842 ~ "Péssimo",
  dados21_ind_regiao$IndViolencia >  1.736842 & dados21_ind_regiao$IndViolencia <= 4.052632 ~ "Ruim",
  dados21_ind_regiao$IndViolencia >  4.052632 & dados21_ind_regiao$IndViolencia <= 7.526316 ~ "Regular",
  dados21_ind_regiao$IndViolencia >  7.526316 & dados21_ind_regiao$IndViolencia <= 9.263158 ~ "Bom",
  dados21_ind_regiao$IndViolencia >  9.263158 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndIdosa_cat = factor(case_when(
  dados21_ind_regiao$IndIdosa <= 2.052632 ~ "Péssimo",
  dados21_ind_regiao$IndIdosa >  2.052632 & dados21_ind_regiao$IndIdosa <= 4.789474 ~ "Ruim",
  dados21_ind_regiao$IndIdosa >  4.789474 & dados21_ind_regiao$IndIdosa <= 8.894737 ~ "Regular",
  dados21_ind_regiao$IndIdosa >  8.894737 & dados21_ind_regiao$IndIdosa <= 10.947368 ~ "Bom",
  dados21_ind_regiao$IndIdosa > 10.947368 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndSaudeHomem_cat = factor(case_when(
  dados21_ind_regiao$IndSaudeHomem <= 1.421053 ~ "Péssimo",
  dados21_ind_regiao$IndSaudeHomem >  1.421053 & dados21_ind_regiao$IndSaudeHomem <= 3.315789 ~ "Ruim",
  dados21_ind_regiao$IndSaudeHomem >  3.315789 & dados21_ind_regiao$IndSaudeHomem <= 6.157895 ~ "Regular",
  dados21_ind_regiao$IndSaudeHomem >  6.157895 & dados21_ind_regiao$IndSaudeHomem <= 7.578947 ~ "Bom",
  dados21_ind_regiao$IndSaudeHomem >  7.578947 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndPessoasAcamadas_cat = factor(case_when(
  dados21_ind_regiao$IndPessoasAcamadas <=  3.947368 ~ "Péssimo",
  dados21_ind_regiao$IndPessoasAcamadas >   3.947368 & dados21_ind_regiao$IndPessoasAcamadas <=  9.210526 ~ "Ruim",
  dados21_ind_regiao$IndPessoasAcamadas >   9.210526 & dados21_ind_regiao$IndPessoasAcamadas <= 17.105263 ~ "Regular",
  dados21_ind_regiao$IndPessoasAcamadas >  17.105263 & dados21_ind_regiao$IndPessoasAcamadas <= 21.052632 ~ "Bom",
  dados21_ind_regiao$IndPessoasAcamadas >  21.052632 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndAcoesVacinacao_cat = factor(case_when(
  dados21_ind_regiao$IndAcoesVacinacao <=  3.473684 ~ "Péssimo",
  dados21_ind_regiao$IndAcoesVacinacao >   3.473684 & dados21_ind_regiao$IndAcoesVacinacao <=  8.105263 ~ "Ruim",
  dados21_ind_regiao$IndAcoesVacinacao >   8.105263 & dados21_ind_regiao$IndAcoesVacinacao <= 15.052632 ~ "Regular",
  dados21_ind_regiao$IndAcoesVacinacao >  15.052632 & dados21_ind_regiao$IndAcoesVacinacao <= 18.526316 ~ "Bom",
  dados21_ind_regiao$IndAcoesVacinacao >  18.526316 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

# dados21_ind_regiao$IndPraticasIntComp_cat = factor(case_when(
#   dados21_ind_regiao$IndPraticasIntComp <= 3 ~ "Péssimo",
#   dados21_ind_regiao$IndPraticasIntComp > 3 & dados21_ind_regiao$IndPraticasIntComp <= 8 ~ "Ruim",
#   dados21_ind_regiao$IndPraticasIntComp > 8 & dados21_ind_regiao$IndPraticasIntComp <= 14 ~ "Regular",
#   dados21_ind_regiao$IndPraticasIntComp > 14 & dados21_ind_regiao$IndPraticasIntComp <= 18 ~ "Bom",
#   dados21_ind_regiao$IndPraticasIntComp > 18 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndAtendimentoUrgEmerg_cat = factor(case_when(
  dados21_ind_regiao$IndAtendimentoUrgEmerg <=  5.210526 ~ "Péssimo",
  dados21_ind_regiao$IndAtendimentoUrgEmerg >   5.210526 & dados21_ind_regiao$IndAtendimentoUrgEmerg <= 12.157895 ~ "Ruim",
  dados21_ind_regiao$IndAtendimentoUrgEmerg >  12.157895 & dados21_ind_regiao$IndAtendimentoUrgEmerg <= 22.578947 ~ "Regular",
  dados21_ind_regiao$IndAtendimentoUrgEmerg >  22.578947 & dados21_ind_regiao$IndAtendimentoUrgEmerg <= 27.789474 ~ "Bom",
  dados21_ind_regiao$IndAtendimentoUrgEmerg >  27.789474 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndAtendimentoProgDemandaEsp_cat = factor(case_when(
  dados21_ind_regiao$IndAtendimentoProgDemandaEsp <=  2.368421 ~ "Péssimo",
  dados21_ind_regiao$IndAtendimentoProgDemandaEsp >   2.368421 & dados21_ind_regiao$IndAtendimentoProgDemandaEsp <=  5.526316 ~ "Ruim",
  dados21_ind_regiao$IndAtendimentoProgDemandaEsp >   5.526316 & dados21_ind_regiao$IndAtendimentoProgDemandaEsp <= 10.263158 ~ "Regular",
  dados21_ind_regiao$IndAtendimentoProgDemandaEsp >  10.263158 & dados21_ind_regiao$IndAtendimentoProgDemandaEsp <= 12.631579 ~ "Bom",
  dados21_ind_regiao$IndAtendimentoProgDemandaEsp >  12.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndAtendimentoDemandaEsp_cat <- factor(case_when(
  dados21_ind_regiao$IndAtendimentoDemandaEsp <= 0.157895 ~ "Péssimo",
  dados21_ind_regiao$IndAtendimentoDemandaEsp >  0.157895 & dados21_ind_regiao$IndAtendimentoDemandaEsp <= 0.368421 ~ "Ruim",
  dados21_ind_regiao$IndAtendimentoDemandaEsp >  0.368421 & dados21_ind_regiao$IndAtendimentoDemandaEsp <= 0.684211 ~ "Regular",
  dados21_ind_regiao$IndAtendimentoDemandaEsp >  0.684211 & dados21_ind_regiao$IndAtendimentoDemandaEsp <= 0.842105 ~ "Bom",
  dados21_ind_regiao$IndAtendimentoDemandaEsp >  0.842105 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndIntegracaoAPS_cat = factor(case_when(
  dados21_ind_regiao$IndIntegracaoAPS <=  4.578947 ~ "Péssimo",
  dados21_ind_regiao$IndIntegracaoAPS >   4.578947 & dados21_ind_regiao$IndIntegracaoAPS <= 10.684211 ~ "Ruim",
  dados21_ind_regiao$IndIntegracaoAPS >  10.684211 & dados21_ind_regiao$IndIntegracaoAPS <= 19.842105 ~ "Regular",
  dados21_ind_regiao$IndIntegracaoAPS >  19.842105 & dados21_ind_regiao$IndIntegracaoAPS <= 24.421053 ~ "Bom",
  dados21_ind_regiao$IndIntegracaoAPS >  24.421053 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndRegulacaoAssistencial_cat = factor(case_when(
  dados21_ind_regiao$IndRegulacaoAssistencial <= 0.789474 ~ "Péssimo",
  dados21_ind_regiao$IndRegulacaoAssistencial >  0.789474 & dados21_ind_regiao$IndRegulacaoAssistencial <= 1.842105 ~ "Ruim",
  dados21_ind_regiao$IndRegulacaoAssistencial >  1.842105 & dados21_ind_regiao$IndRegulacaoAssistencial <= 3.421053 ~ "Regular",
  dados21_ind_regiao$IndRegulacaoAssistencial >  3.421053 & dados21_ind_regiao$IndRegulacaoAssistencial <= 4.210526 ~ "Bom",
  dados21_ind_regiao$IndRegulacaoAssistencial >  4.210526 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

dados21_ind_regiao$IndCuidadoCompartilhado_cat = factor(case_when(
  dados21_ind_regiao$IndCuidadoCompartilhado <=  8.368421 ~ "Péssimo",
  dados21_ind_regiao$IndCuidadoCompartilhado >   8.368421 & dados21_ind_regiao$IndCuidadoCompartilhado <= 19.526316 ~ "Ruim",
  dados21_ind_regiao$IndCuidadoCompartilhado >  19.526316 & dados21_ind_regiao$IndCuidadoCompartilhado <= 36.263158 ~ "Regular",
  dados21_ind_regiao$IndCuidadoCompartilhado >  36.263158 & dados21_ind_regiao$IndCuidadoCompartilhado <= 44.631579 ~ "Bom",
  dados21_ind_regiao$IndCuidadoCompartilhado >  44.631579 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))

#Até urgência e emergência
dados21_ind_regiao$Indicador_Panorama_cat = factor(case_when(
  dados21_ind_regiao$Indicador_Panorama <= 49.26 ~ "Péssimo",
  dados21_ind_regiao$Indicador_Panorama > 49.26 & dados21_ind_regiao$Indicador_Panorama <= 114.95 ~ "Ruim",
  dados21_ind_regiao$Indicador_Panorama > 114.95 & dados21_ind_regiao$Indicador_Panorama <= 213.47 ~ "Regular",
  dados21_ind_regiao$Indicador_Panorama > 213.47 & dados21_ind_regiao$Indicador_Panorama <= 262.74 ~ "Bom",
  dados21_ind_regiao$Indicador_Panorama > 262.74 ~ "Ótimo"), c("Péssimo","Ruim","Regular","Bom","Ótimo"))
#write.xlsx(dados21_ind_regiao %>% as.data.frame(),'Dados com indicadores categorizados agrupados por região.xlsx')

####==========
#### Análises
####==========
####=====
#### UBS
####=====
Tabela1 = do.call(rbind,dados21_ind_ubs %>% select(IndSaudeSexual_cat, IndPreNatal_cat, IndSaudeMulher_cat, IndSaudeCrianca_cat,
                                                   IndHipertenso_cat, IndDiabetico_cat, IndObesidade_cat, IndTuberculoseHanseniase_cat,
                                                   IndSofrimentoPsi_cat, IndViolencia_cat, IndIdosa_cat, IndSaudeHomem_cat,
                                                   IndPessoasAcamadas_cat, IndAcoesVacinacao_cat, #IndPraticasIntComp_cat,
                                                   IndAtendimentoUrgEmerg_cat, IndAtendimentoProgDemandaEsp_cat,
                                                   IndAtendimentoDemandaEsp_cat, IndIntegracaoAPS_cat, IndRegulacaoAssistencial_cat,
                                                   IndCuidadoCompartilhado_cat, Indicador_Panorama_cat) %>% map(DescritivaCat))
#write.xlsx(Tabela1 %>% as.data.frame(),"Tabela 1.xlsx", rowNames = T)

Tabela2 = do.call(rbind,lapply(c('IndSaudeSexual_cat', 'IndPreNatal_cat', 'IndSaudeMulher_cat', 'IndSaudeCrianca_cat',
                                 'IndHipertenso_cat', 'IndDiabetico_cat', 'IndObesidade_cat', 'IndTuberculoseHanseniase_cat',
                                 'IndSofrimentoPsi_cat', 'IndViolencia_cat', 'IndIdosa_cat', 'IndSaudeHomem_cat',
                                 'IndPessoasAcamadas_cat', 'IndAcoesVacinacao_cat', #'IndPraticasIntComp_cat',
                                 'IndAtendimentoUrgEmerg_cat', 'IndAtendimentoProgDemandaEsp_cat',
                                 'IndAtendimentoDemandaEsp_cat', 'IndIntegracaoAPS_cat', 'IndRegulacaoAssistencial_cat',
                                 'IndCuidadoCompartilhado_cat', 'Indicador_Panorama_cat'), function(v) {
                                   QuiQuadrado_Fisher(dados21_ind_ubs[[v]], dados21_ind_ubs$Regiao, '2', 'chisq.simulate')}))
#write.xlsx(Tabela2 %>% as.data.frame(),"Tabela 2.xlsx", rowNames = T)

Tabela3 = do.call(rbind,lapply(c('IndSaudeSexual_cat', 'IndPreNatal_cat', 'IndSaudeMulher_cat', 'IndSaudeCrianca_cat',
                                 'IndHipertenso_cat', 'IndDiabetico_cat', 'IndObesidade_cat', 'IndTuberculoseHanseniase_cat',
                                 'IndSofrimentoPsi_cat', 'IndViolencia_cat', 'IndIdosa_cat', 'IndSaudeHomem_cat',
                                 'IndPessoasAcamadas_cat', 'IndAcoesVacinacao_cat', #'IndPraticasIntComp_cat',
                                 'IndAtendimentoUrgEmerg_cat', 'IndAtendimentoProgDemandaEsp_cat',
                                 'IndAtendimentoDemandaEsp_cat', 'IndIntegracaoAPS_cat', 'IndRegulacaoAssistencial_cat',
                                 'IndCuidadoCompartilhado_cat', 'Indicador_Panorama_cat'), function(v) {
                                   QuiQuadrado_Fisher(dados21_ind_ubs[[v]], dados21_ind_ubs$V17, '2', 'chisq.simulate')}))
#write.xlsx(Tabela3 %>% as.data.frame(),"Tabela 3.xlsx", rowNames = T)

####============
#### Municípios
####============
Tabela4 = do.call(rbind,dados21_ind_mun %>% select(IndSaudeSexual_cat, IndPreNatal_cat, IndSaudeMulher_cat, IndSaudeCrianca_cat,
                                                   IndHipertenso_cat, IndDiabetico_cat, IndObesidade_cat, IndTuberculoseHanseniase_cat,
                                                   IndSofrimentoPsi_cat, IndViolencia_cat, IndIdosa_cat, IndSaudeHomem_cat,
                                                   IndPessoasAcamadas_cat, IndAcoesVacinacao_cat, #IndPraticasIntComp_cat,
                                                   IndAtendimentoUrgEmerg_cat, IndAtendimentoProgDemandaEsp_cat,
                                                   IndAtendimentoDemandaEsp_cat, IndIntegracaoAPS_cat, IndRegulacaoAssistencial_cat,
                                                   IndCuidadoCompartilhado_cat, Indicador_Panorama_cat) %>% map(DescritivaCat))
#write.xlsx(Tabela4 %>% as.data.frame(),"Tabela 4.xlsx", rowNames = T)

Tabela5 = do.call(rbind,lapply(c('IndSaudeSexual_cat', 'IndPreNatal_cat', 'IndSaudeMulher_cat', 'IndSaudeCrianca_cat',
                                 'IndHipertenso_cat', 'IndDiabetico_cat', 'IndObesidade_cat', 'IndTuberculoseHanseniase_cat',
                                 'IndSofrimentoPsi_cat', 'IndViolencia_cat', 'IndIdosa_cat', 'IndSaudeHomem_cat',
                                 'IndPessoasAcamadas_cat', 'IndAcoesVacinacao_cat', #'IndPraticasIntComp_cat',
                                 'IndAtendimentoUrgEmerg_cat', 'IndAtendimentoProgDemandaEsp_cat',
                                 'IndAtendimentoDemandaEsp_cat', 'IndIntegracaoAPS_cat', 'IndRegulacaoAssistencial_cat',
                                 'IndCuidadoCompartilhado_cat', 'Indicador_Panorama_cat'), function(v) {
                                   QuiQuadrado_Fisher(dados21_ind_mun[[v]], dados21_ind_mun$Regiao, '2', 'chisq.simulate')}))
#write.xlsx(Tabela5 %>% as.data.frame(),"Tabela 5.xlsx", rowNames = T)

Tabela6 = do.call(rbind,lapply(c('IndSaudeSexual_cat', 'IndPreNatal_cat', 'IndSaudeMulher_cat', 'IndSaudeCrianca_cat',
                                 'IndHipertenso_cat', 'IndDiabetico_cat', 'IndObesidade_cat', 'IndTuberculoseHanseniase_cat',
                                 'IndSofrimentoPsi_cat', 'IndViolencia_cat', 'IndIdosa_cat', 'IndSaudeHomem_cat',
                                 'IndPessoasAcamadas_cat', 'IndAcoesVacinacao_cat', #'IndPraticasIntComp_cat',
                                 'IndAtendimentoUrgEmerg_cat', 'IndAtendimentoProgDemandaEsp_cat',
                                 'IndAtendimentoDemandaEsp_cat', 'IndIntegracaoAPS_cat', 'IndRegulacaoAssistencial_cat',
                                 'IndCuidadoCompartilhado_cat', 'Indicador_Panorama_cat'), function(v) {
                                   QuiQuadrado_Fisher(dados21_ind_mun[[v]], dados21_ind_mun$V17, '2', 'chisq.simulate')}))
#write.xlsx(Tabela6 %>% as.data.frame(),"Tabela 6.xlsx", rowNames = T)

####======================
#### Região metropolitana
####======================
Tabela7 = do.call(rbind,dados_ind_rm_agg_rm %>% select(IndSaudeSexual_cat, IndPreNatal_cat, IndSaudeMulher_cat, IndSaudeCrianca_cat,
                                                   IndHipertenso_cat, IndDiabetico_cat, IndObesidade_cat, IndTuberculoseHanseniase_cat,
                                                   IndSofrimentoPsi_cat, IndViolencia_cat, IndIdosa_cat, IndSaudeHomem_cat,
                                                   IndPessoasAcamadas_cat, IndAcoesVacinacao_cat, #IndPraticasIntComp_cat,
                                                   IndAtendimentoUrgEmerg_cat, IndAtendimentoProgDemandaEsp_cat,
                                                   IndAtendimentoDemandaEsp_cat, IndIntegracaoAPS_cat, IndRegulacaoAssistencial_cat,
                                                   IndCuidadoCompartilhado_cat, Indicador_Panorama_cat) %>% map(DescritivaCat))
#write.xlsx(Tabela7 %>% as.data.frame(),"Tabela 7.xlsx", rowNames = T)

Tabela8 = do.call(rbind,lapply(c('IndSaudeSexual_cat', 'IndPreNatal_cat', 'IndSaudeMulher_cat', 'IndSaudeCrianca_cat',
                                 'IndHipertenso_cat', 'IndDiabetico_cat', 'IndObesidade_cat', 'IndTuberculoseHanseniase_cat',
                                 'IndSofrimentoPsi_cat', 'IndViolencia_cat', 'IndIdosa_cat', 'IndSaudeHomem_cat',
                                 'IndPessoasAcamadas_cat', 'IndAcoesVacinacao_cat', #'IndPraticasIntComp_cat',
                                 'IndAtendimentoUrgEmerg_cat', 'IndAtendimentoProgDemandaEsp_cat',
                                 'IndAtendimentoDemandaEsp_cat', 'IndIntegracaoAPS_cat', 'IndRegulacaoAssistencial_cat',
                                 'IndCuidadoCompartilhado_cat', 'Indicador_Panorama_cat'), function(v) {
                                   QuiQuadrado_Fisher(dados_ind_rm_agg_rm[[v]], dados_ind_rm_agg_rm$Regiao, '2', 'chisq.simulate')}))
#write.xlsx(Tabela8 %>% as.data.frame(),"Tabela 8.xlsx", rowNames = T)

Tabela9 = do.call(rbind,lapply(c('IndSaudeSexual_cat', 'IndPreNatal_cat', 'IndSaudeMulher_cat', 'IndSaudeCrianca_cat',
                                 'IndHipertenso_cat', 'IndDiabetico_cat', 'IndObesidade_cat', 'IndTuberculoseHanseniase_cat',
                                 'IndSofrimentoPsi_cat', 'IndViolencia_cat', 'IndIdosa_cat', 'IndSaudeHomem_cat',
                                 'IndPessoasAcamadas_cat', 'IndAcoesVacinacao_cat', #'IndPraticasIntComp_cat',
                                 'IndAtendimentoUrgEmerg_cat', 'IndAtendimentoProgDemandaEsp_cat',
                                 'IndAtendimentoDemandaEsp_cat', 'IndIntegracaoAPS_cat', 'IndRegulacaoAssistencial_cat',
                                 'IndCuidadoCompartilhado_cat', 'Indicador_Panorama_cat'), function(v) {
                                   QuiQuadrado_Fisher(dados_ind_rm_agg_rm[[v]], dados_ind_rm_agg_rm$V17, '2', 'chisq.simulate')}))
#write.xlsx(Tabela9 %>% as.data.frame(),"Tabela 9.xlsx", rowNames = T)

####=========
#### Estados
####=========
Tabela10 = do.call(rbind,dados21_ind_estado %>% select(IndSaudeSexual_cat, IndPreNatal_cat, IndSaudeMulher_cat, IndSaudeCrianca_cat,
                                                       IndHipertenso_cat, IndDiabetico_cat, IndObesidade_cat, IndTuberculoseHanseniase_cat,
                                                       IndSofrimentoPsi_cat, IndViolencia_cat, IndIdosa_cat, IndSaudeHomem_cat,
                                                       IndPessoasAcamadas_cat, IndAcoesVacinacao_cat, #IndPraticasIntComp_cat,
                                                       IndAtendimentoUrgEmerg_cat, IndAtendimentoProgDemandaEsp_cat,
                                                       IndAtendimentoDemandaEsp_cat, IndIntegracaoAPS_cat, IndRegulacaoAssistencial_cat,
                                                       IndCuidadoCompartilhado_cat, Indicador_Panorama_cat) %>% map(DescritivaCat))
write.xlsx(Tabela10 %>% as.data.frame(),"Tabela 10.xlsx", rowNames = T)

Tabela11 = do.call(rbind,lapply(c('IndSaudeSexual_cat', 'IndPreNatal_cat', 'IndSaudeMulher_cat', 'IndSaudeCrianca_cat',
                                  'IndHipertenso_cat', 'IndDiabetico_cat', 'IndObesidade_cat', 'IndTuberculoseHanseniase_cat',
                                  'IndSofrimentoPsi_cat', 'IndViolencia_cat', 'IndIdosa_cat', 'IndSaudeHomem_cat',
                                  'IndPessoasAcamadas_cat', 'IndAcoesVacinacao_cat', #'IndPraticasIntComp_cat',
                                  'IndAtendimentoUrgEmerg_cat', 'IndAtendimentoProgDemandaEsp_cat',
                                  'IndAtendimentoDemandaEsp_cat', 'IndIntegracaoAPS_cat', 'IndRegulacaoAssistencial_cat',
                                  'IndCuidadoCompartilhado_cat', 'Indicador_Panorama_cat'), function(v) {
                                    QuiQuadrado_Fisher(dados21_ind_estado[[v]], dados21_ind_estado$Regiao, '2', 'chisq.simulate')}))
write.xlsx(Tabela11 %>% as.data.frame(),"Tabela 11.xlsx", rowNames = T)

####========
#### Região
####========
Tabela12 = do.call(rbind,dados21_ind_regiao %>% select(IndSaudeSexual_cat, IndPreNatal_cat, IndSaudeMulher_cat, IndSaudeCrianca_cat,
                                                       IndHipertenso_cat, IndDiabetico_cat, IndObesidade_cat, IndTuberculoseHanseniase_cat,
                                                       IndSofrimentoPsi_cat, IndViolencia_cat, IndIdosa_cat, IndSaudeHomem_cat,
                                                       IndPessoasAcamadas_cat, IndAcoesVacinacao_cat, #IndPraticasIntComp_cat,
                                                       IndAtendimentoUrgEmerg_cat, IndAtendimentoProgDemandaEsp_cat,
                                                       IndAtendimentoDemandaEsp_cat, IndIntegracaoAPS_cat, IndRegulacaoAssistencial_cat,
                                                       IndCuidadoCompartilhado_cat, Indicador_Panorama_cat) %>% map(DescritivaCat))
write.xlsx(Tabela12 %>% as.data.frame(),"Tabela 12.xlsx", rowNames = T)

####========
#### Extras
####========
DescritivaCat(dados21_num_agg$Regiao)
DescritivaCat(dados21_num_agg$V17)
dados21_num_agg%>% head

dados21_ind %>% select(IndSaudeSexual, IndPreNatal, IndSaudeMulher, IndSaudeCrianca,
                       IndHipertenso, IndDiabetico, IndObesidade, IndTuberculoseHanseniase,
                       IndSofrimentoPsi, IndViolencia, IndIdosa, IndSaudeHomem,
                       IndPessoasAcamadas, IndAcoesVacinacao, #IndPraticasIntComp,
                       IndAtendimentoUrgEmerg, IndAtendimentoProgDemandaEsp,
                       IndAtendimentoDemandaEsp, IndIntegracaoAPS, IndRegulacaoAssistencial,
                       IndCuidadoCompartilhado) %>% map(DescritivaCat)

DescritivaCat(dados21_num_agg$IndSaudeSexual_cat)
DescritivaCat(dados21_num_agg$IndPreNatal_cat)
DescritivaCat(dados21_num_agg$IndSaudeMulher_cat)
DescritivaCat(dados21_num_agg$IndSaudeCrianca_cat)


hist(dados21_num_agg$IndSaudeCrianca)
DescritivaCat(dados21_num_agg$IndSaudeCrianca)
DescritivaCat(dados21_ind$IndSaudeCrianca)
DescritivaCat(dados21_num_agg$IndSaudeMulher)

DescritivaNum(dados21_num_agg$IndSaudeMulher)
DescritivaNum(dados21_num_agg$IndSaudeCrianca)
DescritivaNum(dados21_num_agg$IndHipertenso)

dados21_num_agg %>% head

# dados1 = dados_originais %>% filter(P12_1 == 'De 0 a até 1 ano e meio')
# dados2 = dados_originais %>% filter(P12_2 == 'Mais de 1 ano e meio até 5 anos')
# dados3 = dados_originais %>% filter(P12_3 == 'Mais de 5 até 17 anos')
# write.xlsx(dados1 %>% as.data.frame(),'Dados De 0 a até 1 ano e meio.xlsx')
# write.xlsx(dados2 %>% as.data.frame(),'Mais de 1 ano e meio até 5 anos.xlsx')
# write.xlsx(dados3 %>% as.data.frame(),'Mais de 5 até 17 anos.xlsx')
dados = dados_originais %>% filter(P12_1 == 'De 0 a até 1 ano e meio' | P12_2 == 'Mais de 1 ano e meio até 5 anos' | P12_3 == 'Mais de 5 até 17 anos')
# write.xlsx(dados %>% as.data.frame(),'Dados com filtro.xlsx')

dados[dados == 'N/A'] = NA
dados[dados == 'Muitas Vezes'] = 'Muitas vezes'
dados[dados == 'Prefiro não declarar'] = NA
dados[dados == 'Prefiro não responder'] = NA
dados[dados == 'Prefiro não responder'] = NA
dados[dados == 'Não se aplica'] = NA

dados$faixa_etaria = case_when(dados$P6 < 30 ~ '18 a 29 anos',
                               dados$P6 >= 30 & dados$P6 < 40 ~ '30 a 39 anos',
                               dados$P6 >= 40 & dados$P6 < 50 ~ '40 a 49 anos',
                               dados$P6 >= 50 & dados$P6 < 60 ~ '50 a 59 anos',
                               dados$P6 >= 60 & dados$P6 < 70 ~ '60 a 69 anos',
                               dados$P6 >= 70 ~ '70 anos ou mais')

dados1 = dados %>%
  mutate(
    P22a = fct_relevel(P22a, 'Não', 'Sim'),
    P22b = fct_relevel(P22b, 'Não', 'Sim'),
    P22c = fct_relevel(P22c, 'Não', 'Sim'),
    P3   = fct_relevel(P3, 'Área ribeirinha/quilombola/assentamento rural/indígena', 'Rural', 'Urbana'),
    P5   = fct_relevel(P5, 'Amarela', 'Branca', 'Indígena', 'Parda', 'Preta'),
    P7 = factor(dados$P7, c('Sem instrução formal','Ensino fundamental incompleto','Ensino fundamental completo',
                            'Ensino médio incompleto','Ensino médio completo','Superior incompleto','Superior completo','Pós-graduação')),
    P10 = factor(dados$P10, c('Não tem renda','Até 1 salário-mínimo (Até R$1.320,00)','Mais de 1 até 2 SM (R$1.320,01 a R$2.640,00)',
                              'Mais de 2 a 3 SM (R$2.640,01 a R$ 3.960,00)','Mais de 3 a 5 SM (R$3.960,01 a R$6.600,00)',
                              'Mais de 5 a 10 SM (R$6.600,01 a R$13.200,00)','Mais de 10 SM (R$13.200,01 ou mais)')),
    P11  = fct_relevel(P11, 'Não, nenhuma tem plano de saúde privado',
                       'Sim, apenas algumas pessoas têm plano de saúde privado',
                       'Sim, todas têm plano de saúde privado'),
    P15  = fct_relevel(P15, 'Não', 'Sim'),
    P16  = fct_relevel(P16, 'Não', 'Sim'),
    P17  = fct_relevel(P17, 'Não', 'Sim'),
    P18  = fct_relevel(P18, 'Não', 'Sim'))

ordem_likert = c('Discordo totalmente','Discordo','Nem concordo e nem discordo','Concordo',
                 'Concordo totalmente')

dados1 = dados1 %>% mutate(across(starts_with('P14_'),~ factor(as.character(.x), levels = ordem_likert)))

dados1$P22a_P22b = case_when(dados1$P22a == "Sim" | dados1$P22b == "Sim" ~ "Sim",
                             dados1$P22a == "Não" & dados1$P22b == "Não" ~ "Não",
                             (dados1$P22a == "Não" & is.na(dados1$P22b)) | (is.na(dados1$P22a) & dados1$P22b == "Não") ~ "Não",
                             is.na(dados1$P22a) & is.na(dados1$P22b) ~ NA_character_,
                             TRUE ~ NA_character_)

####====================
#### Análise descritiva
####====================
Tabela1 = do.call(rbind,dados1 %>% select(P12_1,P12_2,P12_3) %>% map(DescritivaCat))
Tabela2 = do.call(rbind,dados1 %>% select(P22a,P22b,P22c,P3,P4,P5,P6,faixa_etaria,P7,P10,P11,P15,P16,P17,P18) %>% map(DescritivaCat))
Tabela3 = do.call(rbind,dados1 %>% select(P14_1,P14_2,P14_3,P14_4,P14_5,P14_6,P14_7,P14_8,P14_9,P14_10,P14_11,
                                          P14_12,P14_13,P14_14,P14_15) %>% map(DescritivaCat))
# write.xlsx(Tabela1 %>% as.data.frame(), 'Tabela 1.xlsx', rowNames = T)
# write.xlsx(Tabela2 %>% as.data.frame(), 'Tabela 2.xlsx', rowNames = T)
# write.xlsx(Tabela3 %>% as.data.frame(), 'Tabela 3.xlsx', rowNames = T)

####=============
#### Comparações
####=============
QuiQuadrado_Fisher(dados1$P1,dados1$P22a,'1','chisq.simulate')
QuiQuadrado_Fisher(dados1$P1,dados1$P22b,'1','chisq.simulate')
QuiQuadrado_Fisher(dados1$P1,dados1$P22c,'1','chisq.simulate')
QuiQuadrado_Fisher(dados1$P1,dados1$P22a_P22b,'1','chisq.simulate')

# Desfecho: P22a, P22b e P22c
Tabela4 = rbind(QuiQuadrado_Fisher(dados1$P3,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P4,dados1$P22a,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P5,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$faixa_etaria,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P7,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P10,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P11,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P15,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P16,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P17,dados1$P22a,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P18,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_1,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_2,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_3,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_4,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_5,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_6,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_7,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_8,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_9,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_10,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_11,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_12,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_13,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_14,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_15,dados1$P22a,'1','fisher'))

Tabela5 = rbind(QuiQuadrado_Fisher(dados1$P3,dados1$P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P4,dados1$P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P5,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$faixa_etaria,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P7,dados1$P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P10,dados1$P22b,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados1$P11,dados1$P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P15,dados1$P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P16,dados1$P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P17,dados1$P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P18,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_1,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_2,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_3,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_4,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_5,dados1$P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P14_6,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_7,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_8,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_9,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_10,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_11,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_12,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_13,dados1$P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P14_14,dados1$P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_15,dados1$P22b,'1','fisher'))

Tabela6 = rbind(QuiQuadrado_Fisher(dados1$P3,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P4,dados1$P22c,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P5,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$faixa_etaria,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P7,dados1$P22c,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados1$P10,dados1$P22c,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados1$P11,dados1$P22c,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P15,dados1$P22c,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P16,dados1$P22c,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P17,dados1$P22c,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P18,dados1$P22c,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P14_1,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_2,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_3,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_4,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_5,dados1$P22c,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P14_6,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_7,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_8,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_9,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_10,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_11,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_12,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_13,dados1$P22c,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P14_14,dados1$P22c,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_15,dados1$P22c,'1','fisher'))
# write.xlsx(Tabela4 %>% as.data.frame(), 'Tabela 4.xlsx', rowNames = T)
# write.xlsx(Tabela5 %>% as.data.frame(), 'Tabela 5.xlsx', rowNames = T)
# write.xlsx(Tabela6 %>% as.data.frame(), 'Tabela 6.xlsx', rowNames = T)

Tabela4.1 = rbind(QuiQuadrado_Fisher(dados1$P3,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P4,dados1$P22a,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P5,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$faixa_etaria,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P7,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P10,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P11,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P15,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P16,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P17,dados1$P22a,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P18,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_1,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_2,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_3,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_4,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_5,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_6,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_7,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_8,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_9,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_10,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_11,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_12,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_13,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_14,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_15,dados1$P22a,'2','fisher'))

Tabela5.1 = rbind(QuiQuadrado_Fisher(dados1$P3,dados1$P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P4,dados1$P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P5,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$faixa_etaria,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P7,dados1$P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P10,dados1$P22b,'2','chisq.simulate'),
                  QuiQuadrado_Fisher(dados1$P11,dados1$P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P15,dados1$P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P16,dados1$P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P17,dados1$P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P18,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_1,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_2,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_3,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_4,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_5,dados1$P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P14_6,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_7,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_8,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_9,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_10,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_11,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_12,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_13,dados1$P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P14_14,dados1$P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_15,dados1$P22b,'2','fisher'))

Tabela6.1 = rbind(QuiQuadrado_Fisher(dados1$P3,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P4,dados1$P22c,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P5,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$faixa_etaria,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P7,dados1$P22c,'2','chisq.simulate'),
                  QuiQuadrado_Fisher(dados1$P10,dados1$P22c,'2','chisq.simulate'),
                  QuiQuadrado_Fisher(dados1$P11,dados1$P22c,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P15,dados1$P22c,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P16,dados1$P22c,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P17,dados1$P22c,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P18,dados1$P22c,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P14_1,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_2,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_3,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_4,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_5,dados1$P22c,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P14_6,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_7,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_8,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_9,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_10,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_11,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_12,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_13,dados1$P22c,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P14_14,dados1$P22c,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_15,dados1$P22c,'2','fisher'))
# write.xlsx(Tabela4.1 %>% as.data.frame(), 'Tabela 4.1.xlsx', rowNames = T)
# write.xlsx(Tabela5.1 %>% as.data.frame(), 'Tabela 5.1.xlsx', rowNames = T)
# write.xlsx(Tabela6.1 %>% as.data.frame(), 'Tabela 6.1.xlsx', rowNames = T)

Tabela7 = rbind(QuiQuadrado_Fisher(dados1$P3,dados1$P22a_P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P4,dados1$P22a_P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P5,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$faixa_etaria,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P7,dados1$P22a_P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P10,dados1$P22a_P22b,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados1$P11,dados1$P22a_P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P15,dados1$P22a_P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P16,dados1$P22a_P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P17,dados1$P22a_P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P18,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_1,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_2,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_3,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_4,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_5,dados1$P22a_P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P14_6,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_7,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_8,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_9,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_10,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_11,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_12,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_13,dados1$P22a_P22b,'1','chisq'),
                QuiQuadrado_Fisher(dados1$P14_14,dados1$P22a_P22b,'1','fisher'),
                QuiQuadrado_Fisher(dados1$P14_15,dados1$P22a_P22b,'1','fisher'))
Tabela7.1 = rbind(QuiQuadrado_Fisher(dados1$P3,dados1$P22a_P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P4,dados1$P22a_P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P5,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$faixa_etaria,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P7,dados1$P22a_P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P10,dados1$P22a_P22b,'2','chisq.simulate'),
                  QuiQuadrado_Fisher(dados1$P11,dados1$P22a_P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P15,dados1$P22a_P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P16,dados1$P22a_P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P17,dados1$P22a_P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P18,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_1,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_2,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_3,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_4,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_5,dados1$P22a_P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P14_6,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_7,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_8,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_9,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_10,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_11,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_12,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_13,dados1$P22a_P22b,'2','chisq'),
                  QuiQuadrado_Fisher(dados1$P14_14,dados1$P22a_P22b,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$P14_15,dados1$P22a_P22b,'2','fisher'))
# write.xlsx(Tabela7 %>% as.data.frame(), 'Tabela 7.xlsx', rowNames = T)
# write.xlsx(Tabela7.1 %>% as.data.frame(), 'Tabela 7.1.xlsx', rowNames = T)

####============================================
#### Remoção de categorias com baixa frequência
####============================================
dados1$faixa_etaria2 = case_when(dados1$P6 < 30 ~ '18 a 29 anos',
                                 dados1$P6 >= 30 & dados1$P6 < 40 ~ '30 a 39 anos',
                                 dados1$P6 >= 40 & dados1$P6 < 50 ~ '40 a 49 anos',
                                 dados1$P6 >= 50 ~ '50 anos ou mais')
dados1$area = case_when(dados1$P3 == 'Rural' ~ 'Rural',
                        dados1$P3 == 'Urbana' ~ 'Urbana')
dados1$raca = case_when(dados1$P5 == 'Branca' ~ 'Branca',
                        dados1$P5 == 'Parda' | dados1$P5 == 'Preta' ~ 'Parda/Preta')
dados1$escolaridade = case_when(dados1$P7 == 'Sem instrução formal' ~ 'Sem instrução formal/Ensino fundamental',
                                dados1$P7 == 'Ensino fundamental incompleto' | dados1$P7 == 'Ensino fundamental completo' ~ 'Sem instrução formal/Ensino fundamental',
                                dados1$P7 == 'Ensino médio incompleto' | dados1$P7 == 'Ensino médio completo' ~ 'Ensino médio',
                                dados1$P7 == 'Superior incompleto' | dados1$P7 == 'Superior completo' | dados1$P7 == 'Pós-graduação' ~ 'Superior')
dados1$escolaridade = factor(dados1$escolaridade, c('Sem instrução formal/Ensino fundamental','Ensino médio','Superior'))
dados1$renda = case_when(dados1$P10 == 'Não tem renda' ~ 'Não tem renda',
                         dados1$P10 == 'Até 1 salário-mínimo (Até R$1.320,00)' | dados1$P10 == 'Mais de 1 até 2 SM (R$1.320,01 a R$2.640,00)' |
                           dados1$P10 == 'Mais de 2 a 3 SM (R$2.640,01 a R$ 3.960,00)' ~ 'Tem renda até 3 SM',
                         dados1$P10 == 'Mais de 3 a 5 SM (R$3.960,01 a R$6.600,00)' | dados1$P10 == 'Mais de 5 a 10 SM (R$6.600,01 a R$13.200,00)' ~ 'Entre 3 e 10 SM',
                         dados1$P10 == 'Mais de 10 SM (R$13.200,01 ou mais)' ~ 'Mais de 10 SM')
dados1$renda = factor(dados1$renda, c('Não tem renda','Tem renda até 3 SM','Entre 3 e 10 SM','Mais de 10 SM'))

Tabela8 = do.call(rbind,dados1 %>% select(area,raca,faixa_etaria2,escolaridade,renda) %>% map(DescritivaCat))
#write.xlsx(Tabela8 %>% as.data.frame(), 'Tabela 8.xlsx', rowNames = T)

# Desfecho: P22a, P22b e P22c
Tabela9 = rbind(QuiQuadrado_Fisher(dados1$area,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$raca,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$faixa_etaria2,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$escolaridade,dados1$P22a,'1','fisher'),
                QuiQuadrado_Fisher(dados1$renda,dados1$P22a,'1','fisher'))

Tabela10 = rbind(QuiQuadrado_Fisher(dados1$area,dados1$P22b,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$raca,dados1$P22b,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$faixa_etaria2,dados1$P22b,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$escolaridade,dados1$P22b,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$renda,dados1$P22b,'1','fisher'))

Tabela11 = rbind(QuiQuadrado_Fisher(dados1$area,dados1$P22c,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$raca,dados1$P22c,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$faixa_etaria2,dados1$P22c,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$escolaridade,dados1$P22c,'1','fisher'),
                 QuiQuadrado_Fisher(dados1$renda,dados1$P22c,'1','fisher'))

Tabela12 = rbind(QuiQuadrado_Fisher(dados1$area,dados1$P22a_P22b,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$raca,dados1$P22a_P22b,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$faixa_etaria2,dados1$P22a_P22b,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$escolaridade,dados1$P22a_P22b,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$renda,dados1$P22a_P22b,'1','fisher'))

Tabela9.1 = rbind(QuiQuadrado_Fisher(dados1$area,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$raca,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$faixa_etaria2,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$escolaridade,dados1$P22a,'2','fisher'),
                  QuiQuadrado_Fisher(dados1$renda,dados1$P22a,'2','fisher'))

Tabela10.1 = rbind(QuiQuadrado_Fisher(dados1$area,dados1$P22b,'2','chisq'),
                   QuiQuadrado_Fisher(dados1$raca,dados1$P22b,'2','chisq'),
                   QuiQuadrado_Fisher(dados1$faixa_etaria2,dados1$P22b,'2','chisq'),
                   QuiQuadrado_Fisher(dados1$escolaridade,dados1$P22b,'2','chisq'),
                   QuiQuadrado_Fisher(dados1$renda,dados1$P22b,'2','fisher'))

Tabela11.1 = rbind(QuiQuadrado_Fisher(dados1$area,dados1$P22c,'2','chisq'),
                   QuiQuadrado_Fisher(dados1$raca,dados1$P22c,'2','chisq'),
                   QuiQuadrado_Fisher(dados1$faixa_etaria2,dados1$P22c,'2','chisq'),
                   QuiQuadrado_Fisher(dados1$escolaridade,dados1$P22c,'2','fisher'),
                   QuiQuadrado_Fisher(dados1$renda,dados1$P22c,'2','fisher'))

Tabela12.1 = rbind(QuiQuadrado_Fisher(dados1$area,dados1$P22a_P22b,'2','chisq'),
                   QuiQuadrado_Fisher(dados1$raca,dados1$P22a_P22b,'2','chisq'),
                   QuiQuadrado_Fisher(dados1$faixa_etaria2,dados1$P22a_P22b,'2','chisq'),
                   QuiQuadrado_Fisher(dados1$escolaridade,dados1$P22a_P22b,'2','chisq'),
                   QuiQuadrado_Fisher(dados1$renda,dados1$P22a_P22b,'2','fisher'))

# write.xlsx(Tabela9 %>% as.data.frame(), 'Tabela 9.xlsx', rowNames = T)
# write.xlsx(Tabela10 %>% as.data.frame(), 'Tabela 10.xlsx', rowNames = T)
# write.xlsx(Tabela11 %>% as.data.frame(), 'Tabela 11.xlsx', rowNames = T)
# write.xlsx(Tabela12 %>% as.data.frame(), 'Tabela 12.xlsx', rowNames = T)
# write.xlsx(Tabela9.1 %>% as.data.frame(), 'Tabela 9.1.xlsx', rowNames = T)
# write.xlsx(Tabela10.1 %>% as.data.frame(), 'Tabela 10.1.xlsx', rowNames = T)
# write.xlsx(Tabela11.1 %>% as.data.frame(), 'Tabela 11.1.xlsx', rowNames = T)
# write.xlsx(Tabela12.1 %>% as.data.frame(), 'Tabela 12.1.xlsx', rowNames = T)

####===========
#### Pontuação
####===========
categoriza_pontos = function(x){
  return(case_when(x == 'Discordo totalmente' ~ 1,
                   x == 'Discordo' ~ 2,
                   x == 'Nem concordo e nem discordo' ~ 3,
                   x == 'Concordo' ~ 4,
                   x == 'Concordo totalmente' ~ 5))
}
categoriza_pontos_inv = function(x){
  return(case_when(x == 'Discordo totalmente' ~ 5,
                   x == 'Discordo' ~ 4,
                   x == 'Nem concordo e nem discordo' ~ 3,
                   x == 'Concordo' ~ 2,
                   x == 'Concordo totalmente' ~ 1))
}
dados1$P14_1_num = categoriza_pontos(dados1$P14_1)
dados1$P14_2_num = categoriza_pontos(dados1$P14_2)
dados1$P14_3_num = categoriza_pontos(dados1$P14_3)
dados1$P14_4_num = categoriza_pontos(dados1$P14_4)
dados1$P14_5_num = categoriza_pontos_inv(dados1$P14_5)
dados1$P14_6_num = categoriza_pontos(dados1$P14_6)
dados1$P14_7_num = categoriza_pontos(dados1$P14_7)
dados1$P14_8_num = categoriza_pontos(dados1$P14_8)
dados1$P14_9_num = categoriza_pontos_inv(dados1$P14_9)
dados1$P14_10_num = categoriza_pontos_inv(dados1$P14_10)
dados1$P14_11_num = categoriza_pontos(dados1$P14_11)
dados1$P14_12_num = categoriza_pontos(dados1$P14_12)
dados1$P14_13_num = categoriza_pontos_inv(dados1$P14_13)
dados1$P14_14_num = categoriza_pontos_inv(dados1$P14_14)
dados1$P14_15_num = categoriza_pontos_inv(dados1$P14_15)

dados1$P14_soma = rowSums(dados1[paste0("P14_", 1:15, "_num")], na.rm = TRUE)
dados1$P14_soma_cat = case_when(dados1$P14_soma < 60 ~ 'Com hesitação',
                                dados1$P14_soma >= 60 ~ 'Sem hesitação')

Tabela13 = rbind(MannWhitney(dados1$P14_soma, dados1$P22a),
                 do.call(rbind, lapply(1:15, function(i) {MannWhitney(dados1[[paste0("P14_", i, "_num")]], dados1$P22a)})),
                 MannWhitney(dados1$P14_soma, dados1$P22b),
                 do.call(rbind, lapply(1:15, function(i) {MannWhitney(dados1[[paste0("P14_", i, "_num")]], dados1$P22b)})),
                 MannWhitney(dados1$P14_soma, dados1$P22c),
                 do.call(rbind, lapply(1:15, function(i) {MannWhitney(dados1[[paste0("P14_", i, "_num")]], dados1$P22b)})),
                 MannWhitney(dados1$P14_soma, dados1$P22a_P22b),
                 do.call(rbind, lapply(1:15, function(i) {MannWhitney(dados1[[paste0("P14_", i, "_num")]], dados1$P22a_P22b)})))
# write.xlsx(Tabela13 %>% as.data.frame(), 'Tabela 13.xlsx', rowNames = T)

Tabela14 = rbind(QuiQuadrado_Fisher(dados1$P14_soma_cat,dados1$P22a,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$P14_soma_cat,dados1$P22b,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$P14_soma_cat,dados1$P22c,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$P14_soma_cat,dados1$P22a_P22b,'1','chisq'),
                 QuiQuadrado_Fisher(dados1$P14_soma_cat,dados1$P22a,'2','chisq'),
                 QuiQuadrado_Fisher(dados1$P14_soma_cat,dados1$P22b,'2','chisq'),
                 QuiQuadrado_Fisher(dados1$P14_soma_cat,dados1$P22c,'2','chisq'),
                 QuiQuadrado_Fisher(dados1$P14_soma_cat,dados1$P22a_P22b,'2','chisq'))
#write.xlsx(Tabela14 %>% as.data.frame(), 'Tabela 14.xlsx', rowNames = T)

####====================
#### Modelos bivariados
####====================
dados1$P22a_P22b_num = case_when(dados1$P22a_P22b == 'Não' ~ 0,
                                 dados1$P22a_P22b == 'Sim' ~ 1)

bi_a_b1 = glm(P22a_P22b_num ~ area, data = dados1, family = binomial(link = 'logit'))
bi_a_b2 = glm(P22a_P22b_num ~ P4, data = dados1, family = binomial(link = 'logit'))
bi_a_b3 = glm(P22a_P22b_num ~ raca, data = dados1, family = binomial(link = 'logit'))
bi_a_b4 = glm(P22a_P22b_num ~ P6, data = dados1, family = binomial(link = 'logit'))
bi_a_b5 = glm(P22a_P22b_num ~ faixa_etaria2, data = dados1, family = binomial(link = 'logit'))
bi_a_b6 = glm(P22a_P22b_num ~ escolaridade, data = dados1, family = binomial(link = 'logit'))
bi_a_b7 = glm(P22a_P22b_num ~ renda, data = dados1, family = binomial(link = 'logit'))
bi_a_b8 = glm(P22a_P22b_num ~ P11, data = dados1, family = binomial(link = 'logit'))
bi_a_b9 = glm(P22a_P22b_num ~ P15, data = dados1, family = binomial(link = 'logit'))
bi_a_b10 = glm(P22a_P22b_num ~ P16, data = dados1, family = binomial(link = 'logit'))
bi_a_b11 = glm(P22a_P22b_num ~ P17, data = dados1, family = binomial(link = 'logit'))
bi_a_b12 = glm(P22a_P22b_num ~ P18, data = dados1, family = binomial(link = 'logit'))
bi_a_b13 = glm(P22a_P22b_num ~ P14_soma, data = dados1, family = binomial(link = 'logit'))
bi_a_b14 = glm(P22a_P22b_num ~ P14_soma_cat, data = dados1, family = binomial(link = 'logit'))
Tabela15 = rbind(TabelaGLMLogistica(bi_a_b1),TabelaGLMLogistica(bi_a_b2),
                 TabelaGLMLogistica(bi_a_b3),TabelaGLMLogistica(bi_a_b4),
                 TabelaGLMLogistica(bi_a_b5),TabelaGLMLogistica(bi_a_b6),
                 TabelaGLMLogistica(bi_a_b7),TabelaGLMLogistica(bi_a_b8),
                 TabelaGLMLogistica(bi_a_b9),TabelaGLMLogistica(bi_a_b10),
                 TabelaGLMLogistica(bi_a_b11),TabelaGLMLogistica(bi_a_b12),
                 TabelaGLMLogistica(bi_a_b13),TabelaGLMLogistica(bi_a_b14))

bi_c1 = glm(P22c ~ area, data = dados1, family = binomial(link = 'logit'))
bi_c2 = glm(P22c ~ P4, data = dados1, family = binomial(link = 'logit'))
bi_c3 = glm(P22c ~ raca, data = dados1, family = binomial(link = 'logit'))
bi_c4 = glm(P22c ~ P6, data = dados1, family = binomial(link = 'logit'))
bi_c5 = glm(P22c ~ faixa_etaria2, data = dados1, family = binomial(link = 'logit'))
bi_c6 = glm(P22c ~ escolaridade, data = dados1, family = binomial(link = 'logit'))
bi_c7 = glm(P22c ~ renda, data = dados1, family = binomial(link = 'logit'))
bi_c8 = glm(P22c ~ P11, data = dados1, family = binomial(link = 'logit'))
bi_c9 = glm(P22c ~ P15, data = dados1, family = binomial(link = 'logit'))
bi_c10 = glm(P22c ~ P16, data = dados1, family = binomial(link = 'logit'))
bi_c11 = glm(P22c ~ P17, data = dados1, family = binomial(link = 'logit'))
bi_c12 = glm(P22c ~ P18, data = dados1, family = binomial(link = 'logit'))
bi_c13 = glm(P22c ~ P14_soma, data = dados1, family = binomial(link = 'logit'))
bi_c14 = glm(P22c ~ P14_soma_cat, data = dados1, family = binomial(link = 'logit'))
Tabela16 = rbind(TabelaGLMLogistica(bi_c1),TabelaGLMLogistica(bi_c2),
                 TabelaGLMLogistica(bi_c3),TabelaGLMLogistica(bi_c4),
                 TabelaGLMLogistica(bi_c5),TabelaGLMLogistica(bi_c6),
                 TabelaGLMLogistica(bi_c7),TabelaGLMLogistica(bi_c8),
                 TabelaGLMLogistica(bi_c9),TabelaGLMLogistica(bi_c10),
                 TabelaGLMLogistica(bi_c11),TabelaGLMLogistica(bi_c12),
                 TabelaGLMLogistica(bi_c13),TabelaGLMLogistica(bi_c14))
# write.xlsx(Tabela15 %>% as.data.frame(), 'Tabela 15.xlsx', rowNames = F)
# write.xlsx(Tabela16 %>% as.data.frame(), 'Tabela 16.xlsx', rowNames = F)

####=======================
#### Modelos multivariados
####=======================
dados_modelos = dados1 %>% select(P22a_P22b_num,P22c,area,P4,raca,faixa_etaria2,escolaridade,renda,P11,
                                  P15,P16,P17,P18,P14_soma_cat)

table(dados_modelos$P16,dados_modelos$P15)
table(dados_modelos$P14_soma_cat,dados_modelos$area)
table(dados_modelos$P16,dados_modelos$P15)

multi_a_b = glm(P22a_P22b_num ~ #area + 
                  #P4 + 
                  #raca + 
                  faixa_etaria2 + 
                  escolaridade + 
                  #renda + 
                  #P11 + 
                  P15 + 
                  P14_soma_cat*P17 + 
                  #P18 + 
                  P14_soma_cat
                , 
                data = dados1, family = binomial(link = 'logit'))
summary(multi_a_b)

multi_c = glm(P22c ~ area + 
                #P4 + 
                #raca + 
                faixa_etaria2 + 
                escolaridade + 
                #renda + 
                P11 + 
                P14_soma_cat*P15 + 
                P17 + 
                #P18 + 
                P14_soma_cat, 
              data = dados1, family = binomial(link = 'logit'))
summary(multi_c)

TabelaGLMLogistica(multi_a_b)
TabelaGLMLogistica(multi_c)

#write.xlsx(TabelaGLMLogistica(multi_a_b) %>% as.data.frame(), 'Tabela 15.1.xlsx', rowNames = F)
#write.xlsx(TabelaGLMLogistica(multi_c) %>% as.data.frame(), 'Tabela 16.1.xlsx', rowNames = F)
