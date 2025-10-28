####===========================
#### Trabalho Censo - Análises
####===========================
####=============================
#### Preparando o R para análise
####=============================
rm(list=ls(all=T))#Limpar ambiente/histórico
tryCatch({setwd("C:/Users/cesar_macieira/Desktop/Projetos/Nescon/censo")},
         error = function(e) { setwd("D:/NESCON/Trabalho - Censo/censo") })

####=================================
#### Instalando e carregando pacotes
####=================================
if(!require(openxlsx)){ install.packages("openxlsx"); require(openxlsx)}#Ler e exportar excel
if(!require(purrr)){ install.packages("purrr"); require(purrr)}#Programação funcional
if(!require(tidyverse)){ install.packages("tidyverse"); require(tidyverse)}#Manipulação de dados
if(!require(haven)){ install.packages("haven"); require(haven)}

####=========
#### Funções
####=========
DescritivaCat = function(x){
  tabela = cbind(table(x), prop.table(table(x)))
  colnames(tabela) = c("Freq. Absoluta (N)", "Freq. Relativa (%)")
  return(tabela)
}

DescritivaNum = function(x, more = F, valorp = FALSE) {
  stats = list();
  clean.x = x[!is.na(x)]
  stats$N_validos = round(length(clean.x),3)
  stats$Média = round(mean(clean.x),3)
  stats$Var = round(var(clean.x),3)
  stats$D.P = round(sd(clean.x),3)
  stats$E.P = round(sd(clean.x)/sqrt(length(clean.x)),3)
  stats$Mín. = round(min(clean.x),3)
  stats$Q1 = round(fivenum(clean.x)[2],3)
  stats$Q2 = round(fivenum(clean.x)[3],3)
  stats$Q3 = round(fivenum(clean.x)[4],3)
  stats$Máx. = round(max(clean.x),3)
  if(valorp == TRUE){
    stats$Valorp = '-'
    t1 = unlist(stats)
    names(t1) = c("N","Média","Variância","D.P.","E.P.","Mínimo","1ºQ","2ºQ","3ºQ","Máximo",'Valor-p')    
  }else{
    t1 = unlist(stats)
    names(t1) = c("N","Média","Variância","D.P.","E.P.","Mínimo","1ºQ","2ºQ","3ºQ","Máximo")
  }
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
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo")
  rownames(tab)= levels(factor(z))
  tab
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
  names(t1) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo")
  t1
}

TabelaContingencia = function(x, y, type.sum){
  t0 = table(x, y)
  if(type.sum==2) {
    t1 = prop.table(t0, 2)
  } else {
    t1 = prop.table(t0, 1)
  }
  colnames(t0) = paste0("X", 1:dim(t0)[2])
  colnames(t1) = paste0("X", 1:dim(t1)[2])
  t2_aux = cbind(t0, t1)
  t3 = t2_aux[, order(colnames(t2_aux))]
  colnames(t3) = c(rep(c("N", "%"), dim(t3)[2]/2))
  return(t3)
}

QuiQuadrado_Fisher = function(x, y, type.sum, teste){
  t0 = table(x, y)
  if(type.sum==2) {
    t1 = prop.table(t0, 2)
  } else {
    t1 = prop.table(t0, 1)
  }
  colnames(t0) = paste0("X", 1:dim(t0)[2])
  colnames(t1) = paste0("X", 1:dim(t1)[2])
  t2_aux = cbind(t0, t1)
  t3 = t2_aux[, order(colnames(t2_aux))]
  colnames(t3) = c(rep(c("N", "%"), dim(t3)[2]/2))
  if(teste=="chisq") {
    Valor_p = chisq.test(t0)$p.value
  }
  if(teste=="fisher") {
    Valor_p = fisher.test(t0)$p.value
  } 
  if(teste=="chisq.simulate"){
    Valor_p = chisq.test(t0, simulate.p.value=TRUE, B=10000)$p.value
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
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p")
  rownames(tab)= levels(factor(z))
  if(!require(PMCMRplus)){ install.packages("PMCMRplus"); require(PMCMRplus) }
  #CM = posthoc.kruskal.nemenyi.test(y ~ factor(z), dist="Chisq")$p.value
  CM = kwAllPairsNemenyiTest(y ~ factor(z), dist="Chisquare")$p.value
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

FriedmanTeste = function(y, z, id, more = F){
  dados = data.frame(y = y, grupos = as.factor(z), id = id)
  dados_agg = dados %>% select(y,grupos,id) %>% group_by(grupos,id) %>%
    summarize(y = mean(y, na.rm = TRUE)) %>% na.omit()
  tab = matrix(NA, length(levels(factor(dados_agg$grupos))), 10)
  for(i in 1:length(levels(factor(dados_agg$grupos)))){ 
    desc = tapply(dados_agg$y, factor(dados_agg$grupos),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  dados_completos = dados_agg %>%
    pivot_wider(names_from = grupos, values_from = y, names_prefix = "Grupo_") %>% na.omit() %>% 
    pivot_longer(cols = starts_with("Grupo_"), names_to = "grupos", values_to = "y") %>%
    mutate(grupos = gsub("Grupo_", "", grupos)) %>%
    group_by(id) %>% filter(!any(is.na(y))) %>% ungroup()
  p_valor = rep(friedman.test(y ~ grupos | id, data = dados_completos)$p.value, length(levels(factor(dados_agg$grupos))))
  tab = cbind(tab, p_valor)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p")
  rownames(tab)= levels(factor(dados_agg$grupos))
  if(!require(PMCMRplus)){ install.packages("PMCMRplus"); require(PMCMRplus) }
  #CM = pairwise.wilcox.test(dados_completos$media, factor(dados_completos$grupos), p.adjust.method = "bonferroni")$p.value
  CM = frdAllPairsConoverTest(y = dados_completos$y, groups = dados_completos$grupos, 
                              blocks = dados_completos$id, p.adjust.method = 'none')$p.value
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

MannWhitney = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = wilcox.test(y ~ x, exact=FALSE)$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

WilcoxonDependente = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = wilcox.test(y ~ x, exact=FALSE, paired = TRUE, alternative = "two.sided")$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

AnovaIndepTeste = function(y, z, CM_teste = "bonferroni", more = FALSE){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  anova_result = summary(aov(y ~ factor(z)))
  p_valor_anova = anova_result[[1]]$"Pr(>F)"[1]
  #CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  if(CM_teste == "tukey") {
    CM = TukeyHSD(aov(y ~ factor(z)))$`factor(z)`
  } else if(CM_teste == "bonferroni") {
    if(!require(PMCMRplus)){ 
      install.packages("PMCMRplus")
      require(PMCMRplus) 
    }
    CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  }
  tab = cbind(tab, p_valor_anova)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p_ANOVA")
  rownames(tab)= levels(factor(z))
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

AnovaDepTeste = function(y, z, unid_amostral, CM_teste = "tukey", more = FALSE){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  anova_result = aov(y ~ factor(z) + Error(factor(unid_amostral)), data = data.frame(y, z, unid_amostral))
  p_valor_anova = summary(anova_result)[[1]]$"Pr(>F)"[1]
  #CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  if(CM_teste == "tukey") {
    CM = TukeyHSD(aov(y ~ factor(z)))$`factor(z)`
  } else if(CM_teste == "bonferroni") {
    if(!require(PMCMRplus)){install.packages("PMCMRplus"); require(PMCMRplus)}
    CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  }
  tab = cbind(tab, p_valor_anova)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p_ANOVA")
  rownames(tab)= levels(factor(z))
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

TesteTpareado = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = t.test(y ~ x, exact = FALSE, paired = TRUE, alternative = "two.sided")$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p")
  return(tab)
}

TesteTindep = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = t.test(y ~ x, exact = FALSE, paired = F)$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

TesteDeNormalidade = function(x){
  if(!require(dgof)){ install.packages("dgof"); require(dgof)}#Teste de Kolmogorov-Smirnov
  if(!require(nortest)){ install.packages("nortest"); require(nortest)}#Anderson-Darling
  AndersonDarling = round(ad.test(x)$p.value,3)
  KolmogorovSmirnov = round(ks.test(x, "pnorm", mean(x, na.rm = T), sd(x, na.rm = T))$p.value,3)
  Lilliefors = round(lillie.test(x)$p.value,3)
  CramerVonMises = round(cvm.test(x)$p.value,3)
  if(length(x) > 5000){
    ShapiroWilk = "N > 5000"
    ShapiroFrancia = "N > 5000"
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

TesteDeNormalidadeGrupos = function(y, z){
  if(!require(dgof)){ install.packages("dgof"); require(dgof)}#Teste de Kolmogorov-Smirnov
  if(!require(nortest)){ install.packages("nortest"); require(nortest)}#Anderson-Darling
  dados = data.frame(y = y, Grupos = as.factor(z))
  if(dim(dados)[1] < 5000){
    result = dados %>% group_by(Grupos)%>% na.omit() %>%
      summarise(ShapiroWilk = round(shapiro.test(y)$p.value,3),
                ShapiroFrancia = round(sf.test(y)$p.value,3),
                AndersonDarling = round(ad.test(y)$p.value,3),
                KolmogorovSmirnov = round(ks.test(y, "pnorm", 
                                                  mean(y, na.rm = T), 
                                                  sd(y, na.rm = T))$p.value,3),
                Lilliefors = round(lillie.test(y)$p.value,3),
                CramerVonMises = round(cvm.test(y)$p.value,3)) %>% na.omit()
  }else{
    result = dados %>% group_by(Grupos) %>% na.omit() %>%
      summarise(ShapiroWilk = "N > 5000",
                ShapiroFrancia = "N > 5000",
                AndersonDarling = round(ad.test(y)$p.value,3),
                KolmogorovSmirnov = round(ks.test(y, "pnorm", 
                                                  mean(y, na.rm = T), 
                                                  sd(y, na.rm = T))$p.value,3),
                Lilliefors = round(lillie.test(y)$p.value,3),
                CramerVonMises = round(cvm.test(y)$p.value,3))
  }
  return(result)
}

HomogeneidadeVariancias = function(y, z){
  if(!require(car)){ install.packages("car"); require(car)}
  valor_p_Levene = leveneTest(y ~ as.factor(z))$`Pr(>F)`[1]
  return(valor_p_Levene)
}

TabelaGEEGama = function(modelo,casasdecimaisExpB=F){
  options(OutDec=",")
  if(casasdecimaisExpB == F){
    Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                        "β" = summary(modelo)$coefficients[,1],
                        "Exp β" = exp(summary(modelo)$coefficients[,1]),
                        "Alteração" = (exp(summary(modelo)$coefficients[,1]) - 1),
                        "I.C." = paste0("[",round(exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2]),3),"; ",
                                        round(exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2]),3),"]"),
                        "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2])-1)*100,2),"%; ",
                                                    round((exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2])-1)*100,2),"%]"),
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

TabelaGEENormal = function(modelo){
  options(OutDec=",")
  Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                      "β" = summary(modelo)$coefficients[,1],
                      "I.C. (95%)" = paste0("[",round(summary(modelo)$coefficients[,1]-(1.96*summary(modelo)$coefficients[,2]),3),"; ",
                                            round(summary(modelo)$coefficients[,1]+(1.96*summary(modelo)$coefficients[,2]),3),"]"),
                      "Valor-p" = round(summary(modelo)$coefficients[,4],4))
  return(Tabela)
}

TabelaGLMMBeta = function(modelo){
  options(OutDec=",")
  Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients$cond),
                      "β" = summary(modelo)$coefficients$cond[,1],
                      "Exp β" = exp(summary(modelo)$coefficients$cond[,1]),
                      "Alteração" = (exp(summary(modelo)$coefficients$cond[,1]) - 1),
                      "I.C." = paste0("[",round(exp(summary(modelo)$coefficients$cond[,1]-1.96*summary(modelo)$coefficients$cond[,2]),3),"; ",
                                      round(exp(summary(modelo)$coefficients$cond[,1]+1.96*summary(modelo)$coefficients$cond[,2]),3),"]"),
                      "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients$cond[,1]-1.96*summary(modelo)$coefficients$cond[,2])-1)*100,2),"%; ",
                                                  round((exp(summary(modelo)$coefficients$cond[,1]+1.96*summary(modelo)$coefficients$cond[,2])-1)*100,2),"%]"),
                      "Valor-p" = round(summary(modelo)$coefficients$cond[,4],4))
  return(Tabela)
}

####=============================
#### Carregando o banco de dados 
####=============================
dados_originais = tryCatch({read_dta("C:/Users/cesar_macieira/Desktop/Projetos/Nescon/censo/censo2024 22_01_25.dta")},
                           error = function(e) {read_dta("D:/NESCON/Trabalho - Censo/censo/censo2024 22_01_25.dta")})
#write.xlsx(dados_originais %>% as.data.frame(), 'Dados Censo 28-01-2025.xlsx')

####=====================
#### Tratamento de dados
####=====================
df = as.data.frame(lapply(dados_originais, function(col) { 
  attributes(col) <- NULL 
  return(col)}), stringsAsFactors = FALSE)
df1 = df %>% mutate(across(everything(), ~ na_if(as.character(.), 'NA'))) 
dados = df1 %>% 
  mutate(V321 = as.numeric(V321),
         V322 = as.numeric(V322),
         V323 = as.numeric(V323),
         V324 = as.numeric(V324),
         V325 = as.numeric(V325),
         V326 = as.numeric(V326))

dados$Regiao = 
  factor(case_when(dados$V17 == "MG" ~ "Sudeste", dados$V17 == "SP" ~ "Sudeste", 
                   dados$V17 == "RJ" ~ "Sudeste", dados$V17 == "ES" ~ "Sudeste", 
                   dados$V17 == "AM" ~ "Norte", dados$V17 == "AC" ~ "Norte", 
                   dados$V17 == "RO" ~ "Norte", dados$V17 == "RR" ~ "Norte", 
                   dados$V17 == "AP" ~ "Norte", dados$V17 == "PA" ~ "Norte", 
                   dados$V17 == "TO" ~ "Norte", dados$V17 == "MA" ~ "Nordeste", 
                   dados$V17 == "PI" ~ "Nordeste", dados$V17 == "RN" ~ "Nordeste", 
                   dados$V17 == "CE" ~ "Nordeste", dados$V17 == "BA" ~ "Nordeste", 
                   dados$V17 == "PE" ~ "Nordeste", dados$V17 == "AL" ~ "Nordeste", 
                   dados$V17 == "SE" ~ "Nordeste", dados$V17 == "PB" ~ "Nordeste", 
                   dados$V17 == "GO" ~ "Centro-Oeste", dados$V17 == "MT" ~ "Centro-Oeste", 
                   dados$V17 == "MS" ~ "Centro-Oeste", dados$V17 == "DF" ~ "Centro-Oeste", 
                   dados$V17 == "SC" ~ "Sul", dados$V17 == "PR" ~ "Sul", 
                   dados$V17 == "RS" ~ "Sul"), c('Norte','Nordeste','Sudeste','Sul','Centro-Oeste'))

dados$V7esf = as.numeric(dados$V7esf)
dados$V7eap = as.numeric(dados$V7eap)
dados = dados %>% mutate(V7esf_eap = rowSums(across(c(V7esf, V7eap)), na.rm = TRUE))
dados$V7esf_eap_cat = ifelse(dados$V7esf_eap >= 7,'7 ou mais',dados$V7esf_eap)

dados$V321cat = factor(case_when( (dados$V7esf_eap <= 1) & (dados$V321 >= 0  & dados$V321 <= 1) ~ 'Péssimo',
                                  (dados$V7esf_eap <= 1) & (dados$V321 >= 2  & dados$V321 <= 4) ~ 'Ruim',
                                  (dados$V7esf_eap <= 1) & (dados$V321 >= 5  & dados$V321 <= 7) ~ 'Regular',
                                  (dados$V7esf_eap <= 1) & (dados$V321 >= 8  & dados$V321 <= 9) ~ 'Bom',
                                  (dados$V7esf_eap <= 1) & (dados$V321 >= 10) ~ 'Ótimo',
                                  
                                  (dados$V7esf_eap == 2) & (dados$V321 >= 0  & dados$V321 <= 2) ~ 'Péssimo',
                                  (dados$V7esf_eap == 2) & (dados$V321 >= 3  & dados$V321 <= 7) ~ 'Ruim',
                                  (dados$V7esf_eap == 2) & (dados$V321 >= 8  & dados$V321 <= 10) ~ 'Regular',
                                  (dados$V7esf_eap == 2) & (dados$V321 >= 11  & dados$V321 <= 14) ~ 'Bom',
                                  (dados$V7esf_eap == 2) & (dados$V321 >= 15) ~ 'Ótimo',
                                  
                                  (dados$V7esf_eap == 3) & (dados$V321 >= 0  & dados$V321 <= 3) ~ 'Péssimo',
                                  (dados$V7esf_eap == 3) & (dados$V321 >= 4  & dados$V321 <= 10) ~ 'Ruim',
                                  (dados$V7esf_eap == 3) & (dados$V321 == 11) ~ 'Regular',
                                  (dados$V7esf_eap == 3) & (dados$V321 >= 12  & dados$V321 <= 20) ~ 'Bom',
                                  (dados$V7esf_eap == 3) & (dados$V321 >= 21) ~ 'Ótimo',
                                  
                                  
                                  (dados$V7esf_eap == 4) & (dados$V321 >= 0  & dados$V321 <= 4) ~ 'Péssimo',
                                  (dados$V7esf_eap == 4) & (dados$V321 >= 5  & dados$V321 <= 13) ~ 'Ruim',
                                  (dados$V7esf_eap == 4) & (dados$V321 == 14) ~ 'Regular',
                                  (dados$V7esf_eap == 4) & (dados$V321 >= 15  & dados$V321 <= 26) ~ 'Bom',
                                  (dados$V7esf_eap == 4) & (dados$V321 >= 27) ~ 'Ótimo',
                                  
                                  
                                  (dados$V7esf_eap == 5) & (dados$V321 >= 0  & dados$V321 <= 5) ~ 'Péssimo',
                                  (dados$V7esf_eap == 5) & (dados$V321 >= 6  & dados$V321 <= 16) ~ 'Ruim',
                                  (dados$V7esf_eap == 5) & (dados$V321 == 17) ~ 'Regular',
                                  (dados$V7esf_eap == 5) & (dados$V321 >= 18  & dados$V321 <= 32) ~ 'Bom',
                                  (dados$V7esf_eap == 5) & (dados$V321 >= 33) ~ 'Ótimo',
                                  
                                  (dados$V7esf_eap == 6) & (dados$V321 >= 0  & dados$V321 <= 6) ~ 'Péssimo',
                                  (dados$V7esf_eap == 6) & (dados$V321 >= 7  & dados$V321 <= 19) ~ 'Ruim',
                                  (dados$V7esf_eap == 6) & (dados$V321 == 20) ~ 'Regular',
                                  (dados$V7esf_eap == 6) & (dados$V321 >= 21  & dados$V321 <= 38) ~ 'Bom',
                                  (dados$V7esf_eap == 6) & (dados$V321 >= 39) ~ 'Ótimo',
                                  
                                  (dados$V7esf_eap >= 7) & (dados$V321 >= 0  & dados$V321 <= 7) ~ 'Péssimo',
                                  (dados$V7esf_eap >= 7) & (dados$V321 >= 8  & dados$V321 <= 22) ~ 'Ruim',
                                  (dados$V7esf_eap >= 7) & (dados$V321 == 23) ~ 'Regular',
                                  (dados$V7esf_eap >= 7) & (dados$V321 >= 24  & dados$V321 <= 45) ~ 'Bom',
                                  (dados$V7esf_eap >= 7) & (dados$V321 >= 45) ~ 'Ótimo'), c('Péssimo','Ruim','Regular','Bom','Ótimo'))

dados$V325cat = case_when(dados$V325 == 0 ~ 'Ausente',
                          dados$V325 >= 1 ~ 'Presente')

dados$V324cat = case_when(dados$V324 == 0 ~ 'Ausente',
                          dados$V324 >= 1 ~ 'Presente')

dados$V86 = as.numeric(dados$V86)
dados$V323 = as.numeric(dados$V323)
dados$V86cat = factor(case_when(dados$V86 >= 0 & dados$V86 <= 3 ~ "0 a 3",
                                dados$V86 >= 4 & dados$V86 <= 7 ~ "4 a 7",
                                dados$V86 >= 8 & dados$V86 <= 11 ~ "8 a 11",
                                dados$V86 >= 12 & dados$V86 <= 15 ~ "12 a 15",
                                dados$V86 >= 16 & dados$V86 <= 19 ~ "16 a 19",
                                dados$V86 >= 20 & dados$V86 <= 23 ~ "20 a 23",
                                dados$V86 >= 24 & dados$V86 <= 27 ~ "24 a 27",
                                dados$V86 >= 28 ~ "28 ou mais"), c("0 a 3","4 a 7","8 a 11","12 a 15","16 a 19","20 a 23","24 a 27","28 ou mais"))

dados$V323cat = factor(case_when( (dados$V86cat == "0 a 3") & (dados$V323 == 0) ~ 'Péssimo',
                                  (dados$V86cat == "0 a 3") & (dados$V323 == 1) ~ 'Ruim',
                                  (dados$V86cat == "0 a 3") & (dados$V323 == 2) ~ 'Regular',
                                  (dados$V86cat == "0 a 3") & (dados$V323 == 3) ~ 'Bom',
                                  (dados$V86cat == "0 a 3") & (dados$V323 >= 4) ~ 'Ótimo',
                                  
                                  (dados$V86cat == "4 a 7") & (dados$V323 == 0) ~ 'Péssimo',
                                  (dados$V86cat == "4 a 7") & (dados$V323 == 1) ~ 'Ruim',
                                  (dados$V86cat == "4 a 7") & (dados$V323 == 2) ~ 'Regular',
                                  (dados$V86cat == "4 a 7") & (dados$V323 == 3) ~ 'Bom',
                                  (dados$V86cat == "4 a 7") & (dados$V323 >= 4) ~ 'Ótimo',
                                  
                                  (dados$V86cat == "8 a 11") & (dados$V323 == 0) ~ 'Péssimo',
                                  (dados$V86cat == "8 a 11") & (dados$V323 >= 1  & dados$V323 <= 3) ~ 'Ruim',
                                  (dados$V86cat == "8 a 11") & (dados$V323 == 4) ~ 'Regular',
                                  (dados$V86cat == "8 a 11") & (dados$V323 >= 5  & dados$V323 <= 7) ~ 'Bom',
                                  (dados$V86cat == "8 a 11") & (dados$V323 >= 8) ~ 'Ótimo',
                                  
                                  (dados$V86cat == "12 a 15") & (dados$V323 >= 0  & dados$V323 <= 2) ~ 'Péssimo',
                                  (dados$V86cat == "12 a 15") & (dados$V323 >= 3  & dados$V323 <= 5) ~ 'Ruim',
                                  (dados$V86cat == "12 a 15") & (dados$V323 >= 6  & dados$V323 <= 7) ~ 'Regular',
                                  (dados$V86cat == "12 a 15") & (dados$V323 >= 8  & dados$V323 <= 10) ~ 'Bom',
                                  
                                  (dados$V86cat == "16 a 19") & (dados$V323 >= 0  & dados$V323 <= 3) ~ 'Péssimo',
                                  (dados$V86cat == "16 a 19") & (dados$V323 >= 4  & dados$V323 <= 7) ~ 'Ruim',
                                  (dados$V86cat == "16 a 19") & (dados$V323 >= 8  & dados$V323 <= 10) ~ 'Regular',
                                  (dados$V86cat == "16 a 19") & (dados$V323 >= 11  & dados$V323 <= 14) ~ 'Bom',
                                  (dados$V86cat == "16 a 19") & (dados$V323 >= 15) ~ 'Ótimo',
                                  
                                  (dados$V86cat == "20 a 23") & (dados$V323 >= 0  & dados$V323 <= 4) ~ 'Péssimo',
                                  (dados$V86cat == "20 a 23") & (dados$V323 >= 5  & dados$V323 <= 9) ~ 'Ruim',
                                  (dados$V86cat == "20 a 23") & (dados$V323 >= 10  & dados$V323 <= 14) ~ 'Regular',
                                  (dados$V86cat == "20 a 23") & (dados$V323 >= 15  & dados$V323 <= 18) ~ 'Bom',
                                  (dados$V86cat == "20 a 23") & (dados$V323 >= 19) ~ 'Ótimo',
                                  
                                  (dados$V86cat == "24 a 27") & (dados$V323 >= 0  & dados$V323 <= 5) ~ 'Péssimo',
                                  (dados$V86cat == "24 a 27") & (dados$V323 >= 6  & dados$V323 <= 11) ~ 'Ruim',
                                  (dados$V86cat == "24 a 27") & (dados$V323 >= 12  & dados$V323 <= 16) ~ 'Regular',
                                  (dados$V86cat == "24 a 27") & (dados$V323 >= 17  & dados$V323 <= 21) ~ 'Bom',
                                  (dados$V86cat == "24 a 27") & (dados$V323 >= 22) ~ 'Ótimo',
                                  
                                  (dados$V86cat == "28 ou mais") & (dados$V323 >= 0  & dados$V323 <= 6) ~ 'Péssimo',
                                  (dados$V86cat == "28 ou mais") & (dados$V323 >= 7  & dados$V323 <= 13) ~ 'Ruim',
                                  (dados$V86cat == "28 ou mais") & (dados$V323 >= 14  & dados$V323 <= 16) ~ 'Regular',
                                  (dados$V86cat == "28 ou mais") & (dados$V323 >= 17  & dados$V323 <= 24) ~ 'Bom',
                                  (dados$V86cat == "28 ou mais") & (dados$V323 >= 25) ~ 'Ótimo'), c('Péssimo','Ruim','Regular','Bom','Ótimo'))

dados = dados %>% mutate(V2537_num = case_when(V2537 == 'Não' ~ 0,V2537 == 'Sim' ~ 1),
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

dados = dados %>% mutate(
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

dados = dados %>% mutate(V361_num = case_when(V361 == 'Não' ~ 0,V361 == 'Sim' ~ 1),
                         V364_num = case_when(V364 == 'Não' ~ 0,V364 == 'Sim' ~ 1),
                         V361_V364 = rowSums(across(c(V361_num, V364_num)), na.rm = TRUE))

dados = dados %>% mutate(V3711_num = case_when(V3711 == 'Não' ~ 0,V3711 == 'Sim' ~ 2),
                         V3712_num = case_when(V3712 == 'Não' ~ 0,V3712 == 'Sim' ~ 1),
                         V3713_num = case_when(V3713 == 'Não' ~ 0,V3713 == 'Sim' ~ 1),
                         V3714_num = case_when(V3714 == 'Não' ~ 0,V3714 == 'Sim' ~ 1),
                         V3715_num = case_when(V3715 == 'Não' ~ 0,V3715 == 'Sim' ~ 1),
                         V3716_num = case_when(V3716 == 'Não' ~ 0,V3716 == 'Sim' ~ 1),
                         V3711_V3712_V3713_V3714_V3715_V3716 = rowSums(across(c(V3711_num,V3712_num,V3713_num,V3714_num,V3715_num,V3716_num)), na.rm = TRUE))
dados$V3711_V3712_V3713_V3714_V3715_V3716cat = ifelse((dados$V37 == 'Não' & dados$V3711_V3712_V3713_V3714_V3715_V3716 != 0), 0, dados$V3711_V3712_V3713_V3714_V3715_V3716)
dados$V3711_V3712_V3713_V3714_V3715_V3716cat5 = 
  factor(case_when(dados$V3711_V3712_V3713_V3714_V3715_V3716 == 0 ~ 'Péssimo',
                   dados$V3711_V3712_V3713_V3714_V3715_V3716 == 1 | dados$V3711_V3712_V3713_V3714_V3715_V3716 == 2 ~ 'Ruim',
                   dados$V3711_V3712_V3713_V3714_V3715_V3716 == 3 | dados$V3711_V3712_V3713_V3714_V3715_V3716 == 4 ~ 'Regular',
                   dados$V3711_V3712_V3713_V3714_V3715_V3716 == 5 | dados$V3711_V3712_V3713_V3714_V3715_V3716 == 6 ~ 'Bom',
                   dados$V3711_V3712_V3713_V3714_V3715_V3716 == 7 ~ 'Ótimo'), c('Péssimo','Ruim','Regular','Bom','Ótimo'))

dados = dados %>% mutate(V3741_num = case_when(V3741 == 'Não' ~ 0,V3741 == 'Sim' ~ 1),
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
dados$V374_cat = ifelse((dados$V37 == 'Não' & dados$V374_num != 0), 0, dados$V374_num)
dados$V374_cat = as.numeric(dados$V374_cat)
dados$V374_cat5 = 
  factor(case_when(dados$V374_cat == 0 ~ 'Péssimo/Ruim',
                   dados$V374_cat >= 1 & dados$V374_cat <= 2 ~ 'Regular',
                   dados$V374_cat >= 3 & dados$V374_cat <= 7 ~ 'Bom',
                   dados$V374_cat >= 8 ~ 'Ótimo'), c('Péssimo/Ruim','Regular','Bom','Ótimo'))

dados = dados %>% mutate(V911_num = case_when(V911 == 'Não' ~ 0,V911 == 'Sim' ~ 1),
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

dados = dados %>% mutate(V1031_num = case_when(V1031 == 'Não' ~ 0,V1031 == 'Sim' ~ 1),
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

dados = dados %>% mutate(v1063_num = case_when(v1063 == 'Não' ~ 0,v1063 == 'Sim' ~ 1),
                         v1064_num = case_when(v1064 == 'Não' ~ 0,v1064 == 'Sim' ~ 1),
                         v1065_num = case_when(v1065 == 'Não' ~ 0,v1065 == 'Sim' ~ 1),
                         v1066_num = case_when(v1066 == 'Não' ~ 0,v1066 == 'Sim' ~ 1),
                         v1065_v1064_v1065_v1066 = rowSums(across(c(v1063_num,v1064_num,v1065_num,v1066_num)), na.rm = TRUE))
dados$v1065_v1064_v1065_v1066cat = 
  factor(case_when(dados$v1065_v1064_v1065_v1066 == 0 ~ 'Péssimo/Ruim',
                   dados$v1065_v1064_v1065_v1066 == 1 ~ 'Regular',
                   dados$v1065_v1064_v1065_v1066 == 2 | dados$v1065_v1064_v1065_v1066 == 3 ~ 'Bom',
                   dados$v1065_v1064_v1065_v1066 == 4 ~ 'Ótimo'), c('Péssimo/Ruim','Regular','Bom','Ótimo'))

dados$v12026_cat = ifelse(dados$v120 == 'Não', 'Não', dados$v12026)
dados$v12026_cat_imp = ifelse(is.na(dados$v12026_cat) == TRUE, 'Não', dados$v12026_cat)
dados = dados %>% mutate(v1263_num = case_when(v1263 == 'Não' ~ 0,v1263 == 'Sim' ~ 1),
                         v1264_num = case_when(v1264 == 'Não' ~ 0,v1264 == 'Sim' ~ 1),
                         v1265_num = case_when(v1263 == 'Não' ~ 0,v1265 == 'Sim' ~ 1),
                         v1266_num = case_when(v1263 == 'Não' ~ 0,v1266 == 'Sim' ~ 1),
                         v1267_num = case_when(v1264 == 'Não' ~ 0,v1267 == 'Sim' ~ 1),
                         v1268_num = case_when(v1263 == 'Não' ~ 0,v1268 == 'Sim' ~ 1),
                         v1269_num = case_when(v1263 == 'Não' ~ 0,v1269 == 'Sim' ~ 1),
                         v126_num = rowSums(across(c(v1263_num,v1264_num,v1265_num,v1266_num,v1267_num,v1268_num,v1269_num)), na.rm = TRUE))
dados$v126_cat = 
  factor(case_when(dados$v126_num == 0 | dados$v12616 == 'Sim' ~ 'Péssimo',
                   dados$v126_num == 1 | dados$v126_num == 2 ~ 'Ruim',
                   dados$v126_num == 3 | dados$v126_num == 4 ~ 'Regular',
                   dados$v126_num == 5 | dados$v126_num == 6 ~ 'Bom',
                   dados$v126_num >= 7 ~ 'Ótimo'), c('Péssimo','Ruim','Regular','Bom','Ótimo'))
dados$V33 = factor(dados$V33, c('Não tem acesso à internet',
                                'Possui acesso à Internet, mas funciona de maneira inadequada (quedas e instabilidades frequentes)',
                                'Possui acesso à Internet adequado para a execução das atividades'))

numerico = function(x){
  return(case_when(x == 'Péssimo' ~ 1,
                   x == 'Ruim' ~ 2,
                   x == 'Regular' ~ 3,
                   x == 'Bom' ~ 4,
                   x == 'Ótimo' ~ 5))
}

dados = dados %>% mutate(V321cat_num = numerico(V321cat),
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
                         Indicador = rowSums(across(c(V321cat_num,V325cat_num,V324cat_num,V323cat_num,V2537_V2567_V2564_V2565_cat_num,
                                                      V34_cat_num,V358_num,V361_V364_num,V37_num,V3711_V3712_V3713_V3714_V3715_V3716cat5_num,V374_cat5_num,
                                                      V91cat_num,V103cat_num,v1065_v1064_v1065_v1066cat_num,v12026_num,v126_cat_num,V33_num)), na.rm = TRUE),
                         Indicador_cat = factor(case_when(Indicador <= 25 ~ 'Péssimo',
                                                          Indicador > 25 & Indicador <= 35 ~ 'Ruim',
                                                          Indicador > 35 & Indicador <= 45 ~ 'Regular',
                                                          Indicador > 45 & Indicador <= 60 ~ 'Bom',
                                                          Indicador > 60 ~ 'Ótimo'), c('Péssimo','Ruim','Regular','Bom','Ótimo')
                         )
)

dados$V323_bin = ifelse(dados$V323 > 0, 1, dados$V323)
dados$V321_bin = ifelse(dados$V321 > 0, 1, dados$V321)

####==========
#### Análises
####==========
#Acesso à internet
DescritivaCat(dados$V33)
Tabela1 = QuiQuadrado_Fisher(dados$V17,dados$V33,'2','chisq')
#write.xlsx(Tabela1 %>% as.data.frame(), 'Tabela 1.xlsx', rowNames = T)

#Conexão à internet
DescritivaCat(dados$V34_num_cat)
Tabela2 = QuiQuadrado_Fisher(dados$V17,dados$V34_num_cat,'2','chisq')
#write.xlsx(Tabela2 %>% as.data.frame(), 'Tabela 2.xlsx', rowNames = T)

#Nenhum computador
#Sala de ACS
#Sala de vacina
#Recepção
#Farmácia
#Alguns consultórios
#Todos os consultórios
Tabela3 = cbind(QuiQuadrado_Fisher(dados$V17,dados$V347,'1','chisq'),
                QuiQuadrado_Fisher(dados$V17,dados$V346,'1','chisq'),
                QuiQuadrado_Fisher(dados$V17,dados$V345,'1','chisq'),
                QuiQuadrado_Fisher(dados$V17,dados$V344,'1','chisq'),
                QuiQuadrado_Fisher(dados$V17,dados$V343,'1','chisq'),
                QuiQuadrado_Fisher(dados$V17,dados$V342,'1','chisq'),
                QuiQuadrado_Fisher(dados$V17,dados$V341,'1','chisq'))
#write.xlsx(Tabela3 %>% as.data.frame(), 'Tabela 3.xlsx', rowNames = T)

#Tablets
#Nº computadores
# dadosACS_0a3 = dados %>% filter(V86cat == '0 a 3')
# dadosACS_4a7 = dados %>% filter(V86cat == '4 a 7')
# dadosACS_8a11 = dados %>% filter(V86cat == '8 a 11')
# dadosACS_12a15 = dados %>% filter(V86cat == '12 a 15')
# dadosACS_16a19 = dados %>% filter(V86cat == '16 a 19')
# dadosACS_20a23 = dados %>% filter(V86cat == '20 a 23')
# dadosACS_24a27 = dados %>% filter(V86cat == '24 a 27')
# dadosACS_28oumais = dados %>% filter(V86cat == '28 ou mais')
# DescritivaCat(dadosACS_0a3$V323)
# DescritivaCat(dadosACS_4a7$V323)
# DescritivaCat(dadosACS_7a11$V323)
# DescritivaCat(dadosACS_12a15$V323)
# DescritivaCat(dadosACS_16a19$V323)
# DescritivaCat(dadosACS_20a23$V323)
# DescritivaCat(dadosACS_24a27$V323)
# DescritivaCat(dadosACS_28oumaisV323)

# AC_1 = dadosACS_0a3 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
# AC_2 = dadosACS_4a7 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
# AC_3 = dadosACS_8a11 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
# AC_4 = dadosACS_12a15 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
# AC_5 = dadosACS_16a19 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
# AC_6 = dadosACS_20a23 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
# AC_7 = dadosACS_24a27 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
# AC_8 = dadosACS_28oumais %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
# (4-as.numeric(rownames(AC_1)))*AC_1$V323.Freq..Absoluta..N.
# (4-as.numeric(rownames(AC_2)))*AC_2$V323.Freq..Absoluta..N.
# (8-as.numeric(rownames(AC_3)))*AC_3$V323.Freq..Absoluta..N.
# (11-as.numeric(rownames(AC_4)))*AC_4$V323.Freq..Absoluta..N.
# (15-as.numeric(rownames(AC_5)))*AC_5$V323.Freq..Absoluta..N.
# (19-as.numeric(rownames(AC_6)))*AC_6$V323.Freq..Absoluta..N.
# (22-as.numeric(rownames(AC_7)))*AC_7$V323.Freq..Absoluta..N.
# (25-as.numeric(rownames(AC_8)))*AC_8$V323.Freq..Absoluta..N.

Tabela4.1 = dados %>% mutate(meta_tablets = case_when(V86cat == '0 a 3' ~ 4,
                                                      V86cat == '4 a 7' ~ 4,
                                                      V86cat == '8 a 11' ~ 8,
                                                      V86cat == '12 a 15' ~ 11,
                                                      V86cat == '16 a 19' ~ 15,
                                                      V86cat == '20 a 23' ~ 19,
                                                      V86cat == '24 a 27' ~ 22,
                                                      V86cat == '28 ou mais' ~ 25,
                                                      TRUE ~ NA_real_),
  faltam_tablets = pmax(meta_tablets - V323, 0)) %>% 
  group_by(V17) %>%
  summarise(Total_faltam_tablets = sum(faltam_tablets, na.rm = TRUE))
#write.xlsx(Tabela4.1 %>% as.data.frame(), 'Tabela 4.1.xlsx', rowNames = T)

dadosesf_eap_1 = dados %>% filter(V7esf_eap <= 1)
dadosesf_eap_2 = dados %>% filter(V7esf_eap == 2)
dadosesf_eap_3 = dados %>% filter(V7esf_eap == 3)
dadosesf_eap_4 = dados %>% filter(V7esf_eap == 4)
dadosesf_eap_5 = dados %>% filter(V7esf_eap == 5)
dadosesf_eap_6 = dados %>% filter(V7esf_eap == 6)
dadosesf_eap_7 = dados %>% filter(V7esf_eap >= 7)

esf_eap_AC_1 = dadosesf_eap_1 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
esf_eap_AC_2 = dadosesf_eap_2 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
esf_eap_AC_3 = dadosesf_eap_3 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
esf_eap_AC_4 = dadosesf_eap_4 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
esf_eap_AC_5 = dadosesf_eap_5 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
esf_eap_AC_6 = dadosesf_eap_6 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()
esf_eap_AC_7 = dadosesf_eap_7 %>% select(V17, V323) %>% filter(V17 == 'AC') %>% select(V323) %>% map(DescritivaCat) %>% as.data.frame()

(10-as.numeric(rownames(esf_eap_AC_1)))*esf_eap_AC_1$V323.Freq..Absoluta..N.
(15-as.numeric(rownames(esf_eap_AC_2)))*esf_eap_AC_2$V323.Freq..Absoluta..N.
(21-as.numeric(rownames(esf_eap_AC_3)))*esf_eap_AC_3$V323.Freq..Absoluta..N.
(27-as.numeric(rownames(esf_eap_AC_4)))*esf_eap_AC_4$V323.Freq..Absoluta..N.
(33-as.numeric(rownames(esf_eap_AC_5)))*esf_eap_AC_5$V323.Freq..Absoluta..N.
(39-as.numeric(rownames(esf_eap_AC_6)))*esf_eap_AC_6$V323.Freq..Absoluta..N.
(45-as.numeric(rownames(esf_eap_AC_7)))*esf_eap_AC_7$V323.Freq..Absoluta..N.

Tabela4.2 = dados %>% mutate(meta_comp = case_when(V7esf_eap <= 1 ~ 10,
                                                   V7esf_eap == 2 ~ 15,
                                                   V7esf_eap == 3 ~ 21,
                                                   V7esf_eap == 4 ~ 27,
                                                   V7esf_eap == 5 ~ 33,
                                                   V7esf_eap == 6 ~ 39,
                                                   V7esf_eap >= 7 ~ 45,
                                                   TRUE ~ NA_real_),
                             faltam_comp = pmax(meta_comp - V321, 0)) %>% 
  group_by(V17) %>%
  summarise(Total_faltam_comp = sum(faltam_comp, na.rm = TRUE))
#write.xlsx(Tabela4.2 %>% as.data.frame(), 'Tabela 4.2.xlsx', rowNames = T)

Tabela4.3 = dados %>% select(V17, V323, V321) %>% group_by(V17) %>%
  summarise(Soma_Tablets = sum(V323, na.rm = TRUE),
            Soma_Comp = sum(V321, na.rm = TRUE))
#write.xlsx(Tabela4.3 %>% as.data.frame(), 'Tabela 4.3.xlsx', rowNames = T)

Tabela4.4 = cbind(QuiQuadrado_Fisher(dados$V17,dados$V323_bin,'1','chisq.simulate'),
                  QuiQuadrado_Fisher(dados$V17,dados$V321_bin,'1','chisq.simulate'))
#write.xlsx(Tabela4.4 %>% as.data.frame(), 'Tabela 4.4.xlsx', rowNames = T)

dados$V321_4cat = case_when(dados$V321 == 0 ~ "0",
                            dados$V321 == 1 ~ "1",
                            dados$V321 >= 2 & dados$V321 <= 4 ~ "2 a 4",
                            dados$V321 > 4 ~ "Mais que 4")
DescritivaCat(dados$V321_4cat)
#write.xlsx(DescritivaCat(dados$V321_4cat) %>% as.data.frame(), 'Tabela 4 comparação PMAQ.xlsx', rowNames = T)

#Espirômetro digital
#Retinógrafo digital
#ECG digital ou Eletrocardiógrafo digital
Tabela5 = cbind(QuiQuadrado_Fisher(dados$V17,dados$V2565,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados$V17,dados$V2564,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados$V17,dados$V2537_V2567_num,'1','chisq.simulate'))
#write.xlsx(Tabela5 %>% as.data.frame(), 'Tabela 5.xlsx', rowNames = T)

#UBS que utilizam telessaúde
#UBS que utilizam prontuário eletrônico
DescritivaCat(dados$V37)
Tabela6 = cbind(QuiQuadrado_Fisher(dados$V17,dados$V3612,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados$V17,dados$V37,'1','chisq.simulate'))
#write.xlsx(Tabela6 %>% as.data.frame(), 'Tabela 6.xlsx', rowNames = T)

#Agendamento de consulta
Tabela7 = cbind(QuiQuadrado_Fisher(dados$V17,dados$V1031,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados$V17,dados$V1032,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados$V17,dados$V1033,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados$V17,dados$V1034,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados$V17,dados$V1035,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados$V17,dados$V1036,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados$V17,dados$V1037,'1','chisq.simulate'),
                QuiQuadrado_Fisher(dados$V17,dados$v1038,'1','chisq.simulate'))
#write.xlsx(Tabela7 %>% as.data.frame(), 'Tabela 7.xlsx', rowNames = T)

#Indicador
Tabela8 = QuiQuadrado_Fisher(dados$V17,dados$Indicador_cat,'2','chisq.simulate')
#write.xlsx(Tabela8 %>% as.data.frame(), 'Tabela 8.xlsx', rowNames = T)

#Distribuição de tablets por faixa de ACS
DescritivaCat(dados$V323cat)
dadosACS_0a3 = dados %>% filter(V86cat == '0 a 3')
dadosACS_4a7 = dados %>% filter(V86cat == '4 a 7')
dadosACS_8a11 = dados %>% filter(V86cat == '8 a 11')
dadosACS_12a15 = dados %>% filter(V86cat == '12 a 15')
dadosACS_16a19 = dados %>% filter(V86cat == '16 a 19')
dadosACS_20a23 = dados %>% filter(V86cat == '20 a 23')
dadosACS_24a27 = dados %>% filter(V86cat == '24 a 27')
dadosACS_28oumais = dados %>% filter(V86cat == '28 ou mais')

Tabela10.1 = QuiQuadrado_Fisher(dadosACS_0a3$V17,dadosACS_0a3$V323cat,'2','chisq.simulate')
Tabela10.2 = QuiQuadrado_Fisher(dadosACS_4a7$V17,dadosACS_4a7$V323cat,'2','chisq.simulate')
Tabela10.3 = QuiQuadrado_Fisher(dadosACS_8a11$V17,dadosACS_8a11$V323cat,'2','chisq.simulate')
Tabela10.4 = QuiQuadrado_Fisher(dadosACS_12a15$V17,dadosACS_12a15$V323cat,'2','chisq.simulate')
Tabela10.5 = QuiQuadrado_Fisher(dadosACS_16a19$V17,dadosACS_16a19$V323cat,'2','chisq.simulate')
Tabela10.6 = QuiQuadrado_Fisher(dadosACS_20a23$V17,dadosACS_20a23$V323cat,'2','chisq.simulate')
Tabela10.7 = QuiQuadrado_Fisher(dadosACS_24a27$V17,dadosACS_24a27$V323cat,'2','chisq.simulate')
Tabela10.8 = QuiQuadrado_Fisher(dadosACS_28oumais$V17,dadosACS_28oumais$V323cat,'2','chisq.simulate')

# write.xlsx(Tabela10.1 %>% as.data.frame(), 'Tabela 10.1.xlsx', rowNames = T)
# write.xlsx(Tabela10.2 %>% as.data.frame(), 'Tabela 10.2.xlsx', rowNames = T)
# write.xlsx(Tabela10.3 %>% as.data.frame(), 'Tabela 10.3.xlsx', rowNames = T)
# write.xlsx(Tabela10.4 %>% as.data.frame(), 'Tabela 10.4.xlsx', rowNames = T)
# write.xlsx(Tabela10.5 %>% as.data.frame(), 'Tabela 10.5.xlsx', rowNames = T)
# write.xlsx(Tabela10.6 %>% as.data.frame(), 'Tabela 10.6.xlsx', rowNames = T)
# write.xlsx(Tabela10.7 %>% as.data.frame(), 'Tabela 10.7.xlsx', rowNames = T)
# write.xlsx(Tabela10.8 %>% as.data.frame(), 'Tabela 10.8.xlsx', rowNames = T)

#UBS que utilizam telessaúde
DescritivaCat(dados$V2537_V2567_V2564_V2565_cat)
DescritivaCat(dados$V2537_V2567_V2564_V2565_cat)
Tabela14 = QuiQuadrado_Fisher(dados$V17,dados$V2537_V2567_V2564_V2565_cat,'2','chisq.simulate')
#write.xlsx(Tabela14 %>% as.data.frame(), 'Tabela 14.xlsx', rowNames = T)

#UBS que utilizam prontuário eletrônico
Tabela15 = QuiQuadrado_Fisher(dados$V17,dados$V37,'2','chisq.simulate')
#write.xlsx(Tabela15 %>% as.data.frame(), 'Tabela 15.xlsx', rowNames = T)

####=====================
#### Análises 21-08-2025
####=====================
Tabela1 = cbind(TabelaContingencia(dados$V17,dados$V351,'1'),
                TabelaContingencia(dados$V17,dados$V352,'1'),
                TabelaContingencia(dados$V17,dados$V353,'1'),
                TabelaContingencia(dados$V17,dados$V354,'1'),
                TabelaContingencia(dados$V17,dados$V355,'1'),
                TabelaContingencia(dados$V17,dados$V356,'1'),
                TabelaContingencia(dados$V17,dados$V357,'1'),
                TabelaContingencia(dados$V17,dados$V358,'1'))
#write.xlsx(Tabela1 %>% as.data.frame(), 'Tabela 1.xlsx', rowNames = T)

Tabela2 = cbind(TabelaContingencia(dados$V17,dados$V361,'1'),
                TabelaContingencia(dados$V17,dados$V362,'1'),
                TabelaContingencia(dados$V17,dados$V363,'1'),
                TabelaContingencia(dados$V17,dados$V364,'1'),
                TabelaContingencia(dados$V17,dados$V365,'1'),
                TabelaContingencia(dados$V17,dados$V366,'1'),
                TabelaContingencia(dados$V17,dados$V367,'1'),
                TabelaContingencia(dados$V17,dados$V368,'1'),
                TabelaContingencia(dados$V17,dados$V369,'1'),
                TabelaContingencia(dados$V17,dados$V3610,'1'),
                TabelaContingencia(dados$V17,dados$V3611,'1'),
                TabelaContingencia(dados$V17,dados$V3612,'1'))
#write.xlsx(Tabela2 %>% as.data.frame(), 'Tabela 2.xlsx', rowNames = T)

Tabela3 = cbind(TabelaContingencia(dados$V17,dados$v304,'1'),
                TabelaContingencia(dados$V17,dados$v305,'1'),
                TabelaContingencia(dados$V17,dados$V3613,'1'),
                TabelaContingencia(dados$V17,dados$V3614,'1'),
                TabelaContingencia(dados$V17,dados$V3615,'1'),
                TabelaContingencia(dados$V17,dados$V3616,'1'),
                TabelaContingencia(dados$V17,dados$V3617,'1'),
                TabelaContingencia(dados$V17,dados$V3618,'1'))
#write.xlsx(Tabela3 %>% as.data.frame(), 'Tabela 3.xlsx', rowNames = T)

Tabela4 = cbind(TabelaContingencia(dados$V17,dados$V3621,'1'),
                TabelaContingencia(dados$V17,dados$V3622,'1'),
                TabelaContingencia(dados$V17,dados$V3623,'1'),
                TabelaContingencia(dados$V17,dados$V3624,'1'),
                TabelaContingencia(dados$V17,dados$V3625,'1'),
                TabelaContingencia(dados$V17,dados$V3626,'1'),
                TabelaContingencia(dados$V17,dados$V3627,'1'))
#write.xlsx(Tabela4 %>% as.data.frame(), 'Tabela 4.xlsx', rowNames = T)

Tabela5 = cbind(TabelaContingencia(dados$V17,dados$V37,'1'),
                TabelaContingencia(dados$V17,dados$V3711,'1'),
                TabelaContingencia(dados$V17,dados$V3712,'1'),
                TabelaContingencia(dados$V17,dados$V3713,'1'),
                TabelaContingencia(dados$V17,dados$V3714,'1'),
                TabelaContingencia(dados$V17,dados$V3715,'1'),
                TabelaContingencia(dados$V17,dados$V3716,'1'))
#write.xlsx(Tabela5 %>% as.data.frame(), 'Tabela 5.xlsx', rowNames = T)

Tabela6 = cbind(TabelaContingencia(dados$V17,dados$V372,'1'),
                TabelaContingencia(dados$V17,dados$V373,'1'),
                TabelaContingencia(dados$V17,dados$V3741,'1'),
                TabelaContingencia(dados$V17,dados$V3742,'1'),
                TabelaContingencia(dados$V17,dados$V3743,'1'),
                TabelaContingencia(dados$V17,dados$V3744,'1'),
                TabelaContingencia(dados$V17,dados$V3745,'1'),
                TabelaContingencia(dados$V17,dados$V3746,'1'),
                TabelaContingencia(dados$V17,dados$V3747,'1'),
                TabelaContingencia(dados$V17,dados$V3748,'1'),
                TabelaContingencia(dados$V17,dados$V3749,'1'),
                TabelaContingencia(dados$V17,dados$V37410,'1'),
                TabelaContingencia(dados$V17,dados$V37411,'1'))
#write.xlsx(Tabela6 %>% as.data.frame(), 'Tabela 6.xlsx', rowNames = T)

#write.xlsx(TabelaContingencia(dados$V325,dados$V17,'1') %>% as.data.frame(), 'Tabela 7.xlsx', rowNames = T)

##########################
###########################
DescritivaCat(dadosACS_0a3$V323)

Tabela2 = QuiQuadrado_Fisher(dados$V325cat,dados$V7esf_eap_cat,'2','chisq')
#write.xlsx(DescritivaCat(dados$V325cat) %>% as.data.frame(), 'Tabela 2.xlsx', rowNames = T)

Tabela3 = QuiQuadrado_Fisher(dados$V324cat,dados$V7esf_eap_cat,'2','chisq')
#write.xlsx(DescritivaCat(dados$V324cat) %>% as.data.frame(), 'Tabela 3.xlsx', rowNames = T)

Tabela4 = QuiQuadrado_Fisher(dados$V323cat,dados$V86cat,'2','chisq.simulate')
#write.xlsx(Tabela4 %>% as.data.frame(), 'Tabela 4.xlsx', rowNames = T)

Tabela5 = QuiQuadrado_Fisher(dados$V2537_V2567_V2564_V2565,dados$V86cat,'2','chisq.simulate')
#write.xlsx(DescritivaCat(dados$V2537_V2567_V2564_V2565_cat) %>% as.data.frame(), 'Tabela 5.xlsx', rowNames = T)

#write.xlsx(DescritivaCat(dados$V34_cat) %>% as.data.frame(), 'Tabela 6.xlsx', rowNames = T)
#write.xlsx(DescritivaCat(dados$V34_num_cat) %>% as.data.frame(), 'Tabela 6.1.xlsx', rowNames = T)

#write.xlsx(DescritivaCat(dados$V358) %>% as.data.frame(), 'Tabela 7.xlsx', rowNames = T)

#write.xlsx(DescritivaCat(dados$V361_V364) %>% as.data.frame(), 'Tabela 8.xlsx', rowNames = T)

#write.xlsx(DescritivaCat(dados$V37) %>% as.data.frame(), 'Tabela 9.xlsx', rowNames = T)

Tabela10 = rbind(DescritivaCat(dados$V3711_V3712_V3713_V3714_V3715_V3716cat),
                 DescritivaCat(dados$V3711_V3712_V3713_V3714_V3715_V3716cat5))
#write.xlsx(Tabela10 %>% as.data.frame(), 'Tabela 10.xlsx', rowNames = T)

Tabela11 = rbind(DescritivaCat(dados$V374_cat),
                 DescritivaCat(dados$V374_cat5))
#write.xlsx(Tabela11 %>% as.data.frame(), 'Tabela 11.xlsx', rowNames = T)
#write.xlsx(DescritivaCat(dados$V91cat) %>% as.data.frame(), 'Tabela 12.xlsx', rowNames = T)
#write.xlsx(DescritivaCat(dados$V91_num_cat) %>% as.data.frame(), 'Tabela 12.1.xlsx', rowNames = T)
#write.xlsx(DescritivaCat(dados$V103_num_cat) %>% as.data.frame(), 'Tabela 13.xlsx', rowNames = T)
#write.xlsx(DescritivaCat(dados$v1065_v1064_v1065_v1066cat) %>% as.data.frame(), 'Tabela 14.xlsx', rowNames = T)
#write.xlsx(DescritivaCat(dados$v12026) %>% as.data.frame(), 'Tabela 15.xlsx', rowNames = T)
#write.xlsx(DescritivaCat(dados$v12026_cat_imp) %>% as.data.frame(), 'Tabela 15.1.xlsx', rowNames = T)
#write.xlsx(DescritivaCat(dados$v126_cat) %>% as.data.frame(), 'Tabela 16.xlsx', rowNames = T)
#write.xlsx(DescritivaCat(dados$V33) %>% as.data.frame(), 'Tabela 17.xlsx', rowNames = T)

write.xlsx(DescritivaCat(dados$Indicador) %>% as.data.frame(), 'Tabela 18.xlsx', rowNames = T)
write.xlsx(DescritivaCat(dados$Indicador_cat) %>% as.data.frame(), 'Tabela 19.xlsx', rowNames = T)

Tabela20 = rbind(QuiQuadrado_Fisher(dados$Regiao,dados$Indicador_cat,'2','chisq'),
                 QuiQuadrado_Fisher(dados$V17,dados$Indicador_cat,'2','chisq.simulate'))
write.xlsx(Tabela20 %>% as.data.frame(), 'Tabela 20.xlsx', rowNames = T)

Tabela21 = rbind(QuiQuadrado_Fisher(dados$Regiao,dados$Indicador_cat,'1','chisq'),
                 QuiQuadrado_Fisher(dados$V17,dados$Indicador_cat,'1','chisq.simulate'))
write.xlsx(Tabela21 %>% as.data.frame(), 'Tabela 21.xlsx', rowNames = T)



QuiQuadrado_Fisher(dados$V7esf,dados$V321cat,'2','chisq.simulate')
QuiQuadrado_Fisher(dados$V7eap,dados$V321cat,'2','chisq.simulate')
DescritivaCat(dados$V321cat)

QuiQuadrado_Fisher(dados$V325,dados$Regiao,'2','chisq')
QuiQuadrado_Fisher(dados$V324,dados$Regiao,'2','chisq')
QuiQuadrado_Fisher(dados$V323,dados$Regiao,'2','chisq')


Tabela1 = rbind(QuiQuadrado_Fisher(dados$V251,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V252,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V253,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V254,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V255,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V256,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V257,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V258,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V259,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V2510,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V2511,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V2512,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V2513,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V2514,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V2515,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V2516,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V2517,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V2518,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V2519,dados$Regiao,'2','chisq'),
                
                QuiQuadrado_Fisher(dados$v2520,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2521,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2522,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2523,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2524,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2525,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2526,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2527,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2528,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2529,dados$Regiao,'2','chisq'),
                
                QuiQuadrado_Fisher(dados$v2530,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2531,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2532,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2533,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2534,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2535,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2536,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2537,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2538,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2539,dados$Regiao,'2','chisq'),
                
                QuiQuadrado_Fisher(dados$v2540,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2541,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2542,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2543,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2544,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2545,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2546,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2547,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2548,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2549,dados$Regiao,'2','chisq'),
                
                QuiQuadrado_Fisher(dados$v2550,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2551,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2552,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2553,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2554,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2555,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2556,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2557,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2558,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2559,dados$Regiao,'2','chisq'),
                
                QuiQuadrado_Fisher(dados$v2560,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2561,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2562,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2563,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2564,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2565,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2566,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2567,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2568,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2569,dados$Regiao,'2','chisq'),
                
                QuiQuadrado_Fisher(dados$v2570,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v2571,dados$Regiao,'2','chisq'))
write.xlsx(Tabela1 %>% as.data.frame(), 'Tabela 1.xlsx', rowNames = T)

# Questão 33: Nesta UBS, como pode ser considerado o acesso à Internet?
Tabela2 = QuiQuadrado_Fisher(dados$v33,dados$Regiao,'2','chisq')
write.xlsx(Tabela2 %>% as.data.frame(), 'Tabela 2.xlsx', rowNames = T)

# Questão 36: Nesta UBS, quais os tipos de atividades de telessaúde são realizadas?
Tabela3 = rbind(QuiQuadrado_Fisher(dados$v361,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v362,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v363,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v364,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v365,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v366,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v367,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v368,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v369,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3610,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3611,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3612,dados$Regiao,'2','chisq'))
write.xlsx(Tabela3 %>% as.data.frame(), 'Tabela 3.xlsx', rowNames = T)

# Questão 37: Nesta UBS, utiliza-se prontuário eletrônico?
# Questão 37.2: Se sim, qual software de prontuário eletrônico é utilizado nesta UBS?
# Questão 37.4: Se sim, com quais pontos de atenção o prontuário eletrônico é compartilhado?
Tabela4 = rbind(QuiQuadrado_Fisher(dados$v37,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v372,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3741,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3742,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3743,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3744,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3745,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3746,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3747,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3748,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v3749,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v37410,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v37411,dados$Regiao,'2','chisq'))
write.xlsx(Tabela4 %>% as.data.frame(), 'Tabela 4.xlsx', rowNames = T)