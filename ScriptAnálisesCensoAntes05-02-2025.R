####===========================
#### Trabalho Censo - Análises
####===========================
####=============================
#### Preparando o R para análise
####=============================
rm(list=ls(all=T))#Limpar ambiente/histórico
tryCatch({setwd("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/censo")},
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
dados_originais = tryCatch({read_dta("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/censo/censo2024 22_01_25.dta")},
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

####==========
#### Análises
####==========
Tabela1 = rbind(KruskalTeste(dados$V321,dados$Regiao)$tabela,DescritivaNum(dados$V321),
                KruskalTeste(dados$V322,dados$Regiao)$tabela,DescritivaNum(dados$V322),
                KruskalTeste(dados$V323,dados$Regiao)$tabela,DescritivaNum(dados$V323),
                KruskalTeste(dados$V324,dados$Regiao)$tabela,DescritivaNum(dados$V324),
                KruskalTeste(dados$V325,dados$Regiao)$tabela,DescritivaNum(dados$V325),
                KruskalTeste(dados$V326,dados$Regiao)$tabela,DescritivaNum(dados$V326))
#write.xlsx(Tabela1 %>% as.data.frame(), 'Tabela 1.xlsx', rowNames = T)

Tabela2 = QuiQuadrado_Fisher(dados$V33,dados$Regiao,'2','chisq')
#write.xlsx(Tabela2 %>% as.data.frame(), 'Tabela 2.xlsx', rowNames = T)

Tabela3 = rbind(QuiQuadrado_Fisher(dados$V341,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V342,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V343,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V344,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V345,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V346,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V347,dados$Regiao,'2','chisq'))
#write.xlsx(Tabela3 %>% as.data.frame(), 'Tabela 3.xlsx', rowNames = T)

Tabela4 = rbind(QuiQuadrado_Fisher(dados$V351,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V352,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V353,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V354,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V355,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V356,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V357,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V358,dados$Regiao,'2','chisq'))
#write.xlsx(Tabela4 %>% as.data.frame(), 'Tabela 4.xlsx', rowNames = T)

Tabela5 = rbind(QuiQuadrado_Fisher(dados$V361,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V362,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V363,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V364,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V365,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V366,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V367,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V368,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V369,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3610,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3611,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3612,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3613,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3614,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3615,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3616,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3617,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3618,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3621,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3622,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3623,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3624,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3625,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3626,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3627,dados$Regiao,'2','chisq'))
#write.xlsx(Tabela5 %>% as.data.frame(), 'Tabela 5.xlsx', rowNames = T)

Tabela6 = rbind(QuiQuadrado_Fisher(dados$V37,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3711,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3712,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3713,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3714,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3715,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3716,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V372,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V373,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3741,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3742,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3743,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3744,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3745,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3746,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3747,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3748,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V3749,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V37410,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V37411,dados$Regiao,'2','chisq'))
#write.xlsx(Tabela6 %>% as.data.frame(), 'Tabela 6.xlsx', rowNames = T)

Tabela7 = rbind(QuiQuadrado_Fisher(dados$V911,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V912,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V913,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V914,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V915,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V916,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V917,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V918,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V919,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V9110,dados$Regiao,'2','chisq'))
#write.xlsx(Tabela7 %>% as.data.frame(), 'Tabela 7.xlsx', rowNames = T)

Tabela8 = rbind(QuiQuadrado_Fisher(dados$V1031,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V1032,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V1033,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$V1034,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1035,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1036,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1037,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1038,dados$Regiao,'2','chisq'))
#write.xlsx(Tabela8 %>% as.data.frame(), 'Tabela 8.xlsx', rowNames = T)

Tabela9 = rbind(QuiQuadrado_Fisher(dados$v1061,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1062,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1063,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1064,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1065,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1066,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1067,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1068,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v1069,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v10610,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v10611,dados$Regiao,'2','chisq'),
                QuiQuadrado_Fisher(dados$v10612,dados$Regiao,'2','chisq'))
#write.xlsx(Tabela9 %>% as.data.frame(), 'Tabela 9.xlsx', rowNames = T)

Tabela10 = rbind(QuiQuadrado_Fisher(dados$v120,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v1201,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12021,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12022,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12023,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12024,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12025,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12026,dados$Regiao,'2','chisq'))
#write.xlsx(Tabela10 %>% as.data.frame(), 'Tabela 10.xlsx', rowNames = T)

Tabela11 = rbind(QuiQuadrado_Fisher(dados$V126,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v1261,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v1262,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v1263,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v1264,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v1265,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v1266,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v1267,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v1268,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v1269,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12610,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12611,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12612,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12613,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12614,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12615,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$v12616,dados$Regiao,'2','chisq'))
#write.xlsx(Tabela11 %>% as.data.frame(), 'Tabela 11.xlsx', rowNames = T)

# Questão 25: Nesta UBS, quais equipamentos estão disponíveis e em condições de uso?
dados$V2537num = case_when(dados$V2537 == 'Não' ~ 0, dados$V2537 == 'Sim' ~ 1)
dados$V2567num = case_when(dados$V2567 == 'Não' ~ 0, dados$V2567 == 'Sim' ~ 1)

dados = dados %>% mutate(V2537_V2567num = rowSums(across(c(V2537num, V2567num)), na.rm = TRUE),
                         V2537_V2567cat = ifelse(V2537_V2567num > 0, 1, 0))

Tabela12 = rbind(QuiQuadrado_Fisher(dados$V2537_V2567num,dados$Regiao,'2','chisq'),
                 QuiQuadrado_Fisher(dados$V2537_V2567cat,dados$Regiao,'2','chisq'))
#write.xlsx(Tabela12 %>% as.data.frame(), 'Tabela 12.xlsx', rowNames = T)

#### Extras
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