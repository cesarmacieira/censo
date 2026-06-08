####===================================
#### Trabalho Censo - Dados e análises
####===================================
####=============================
#### Preparando o R para análise
####=============================
rm(list=ls(all=T))#Limpar ambiente/histórico
tryCatch({setwd("C:/Users/cesar.macieira/Desktop/Trabalho/censo")},
         error = function(e) { setwd("D:/NESCON/Trabalho - Censo/censo") })

####=================================
#### Instalando e carregando pacotes
####=================================
if(!require(openxlsx)){ install.packages('openxlsx'); require(openxlsx)}#Ler e exportar excel
if(!require(purrr)){ install.packages('purrr'); require(purrr)}#Programação funcional
if(!require(tidyverse)){ install.packages('tidyverse'); require(tidyverse)}#Manipulação de dados
if(!require(stringi)){ install.packages('stringi'); require(stringi)}
if(!require(haven)){ install.packages("haven"); require(haven)}
if(!require(NbClust)){ install.packages("NbClust"); require(NbClust)}

####=========
#### Funções
####=========
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
dados_originais = tryCatch({arrow::read_parquet("C:/Users/cesar.macieira/Desktop/Trabalho/censo/44938_censo2024 06_08_25.parquet")},
                           error = function(e) {arrow::read_parquet("D:/NESCON/Trabalho - Censo/censo/44938_censo2024 06_08_25.parquet")})
#write.xlsx(dados %>% as.data.frame(), 'dados Censo 07-08-2025.xlsx')
dados_originais = dados_originais %>% filter(V362 != '.' & V16 == "CONTAGEM")

####=====================
#### Tratamento de dados
####=====================
df = as.data.frame(lapply(dados_originais, function(col) { 
  attributes(col) <- NULL 
  return(col)}), stringsAsFactors = FALSE)
df1 = df %>% mutate(across(everything(), ~ na_if(as.character(.), 'NA'))) 
#write.xlsx(df1 %>% as.data.frame(),'dados Contagem.xlsx')
# 25. Assinale os equipamento e insumos disponíveis e em condições de uso nesta UBS:
# Não e Sim - ( ) Eletrocardiógrafo digital 
# Não e Sim - ( ) Retinógrafo portátil digital 
# Não e Sim - ( ) Espirômetro digital 
# Não e Sim - ( ) Eletrocardiograma (ECG) digital 
DescritivaCat(df1$V2537)
DescritivaCat(df1$V2564)
DescritivaCat(df1$V2565)
DescritivaCat(df1$V2567)

# 32. Nesta UBS, quantos equipamentos de informática estão em condições de uso?
# ___ Computador desktop (CPU, monitor, teclado e mouse) 
# ___ Notebook 
# ___ Tablets
# ___ Smartphone – celular institucional 
# ___ Impressora 
# ___ TV 
df1$V321
df1$V322
df1$V323
df1$V324
df1$V325
df1$V326

# 33. Nesta UBS, como pode ser considerado o acesso à Internet?
df1$V33

# 34. Nesta UBS, em quais ambientes existem computadores conectados à Internet?
# Não e Sim - ( ) Em todos os consultórios 
# Não e Sim - ( ) Em alguns consultórios 
# Não e Sim - ( ) Farmácia 
# Não e Sim - ( ) Recepção 
# Não e Sim - ( ) Sala de vacina 
# Não e Sim - ( ) Sala de ACS 
# Não e Sim - ( ) Nenhum computador conectado à Internet 
df1$V341
df1$V342
df1$V343
df1$V344
df1$V345
df1$V346
df1$V347

# 35. Nesta UBS, quais equipamentos estão disponíveis para realização de webconferência?
# Não e Sim - ( ) Computadores com câmera de vídeo e microfone 
# Não e Sim - ( ) Datashow 
# Não e Sim - ( ) Televisão 
# Não e Sim - ( ) Câmera com cabo USB ou cabo de vídeo compatível com computador/televisão 
# Não e Sim - ( ) Microfone com cabo USB ou cabo de áudio compatível com computador/televisão
# Não e Sim - ( ) Fone de ouvido com microfone acoplado com cabo USB ou cabo de áudio compatível com computador
# Não e Sim - ( ) Caixa de som 
# Não e Sim - ( ) Não possui equipamentos disponíveis para realização de webconferência 
df1$V351
df1$V352
df1$V353
df1$V354
df1$V355
df1$V356
df1$V357
df1$V358

# 36. Nesta UBS, quais os tipos de atividades de telessaúde são realizadas:
# Não e Sim - ( ) Teleconsultorias (Serviço de consultoria à distância entre profissionais de saúde para apoio no diagnóstico e manejo clínico)
# Não e Sim - ( ) Teletriagem (Processo remoto de avaliação e direcionamento de pacientes para o atendimento apropriado, baseado na urgência dos sintomas)
# Não e Sim - ( ) Telemonitoramento remoto de pacientes (Acompanhamento à distância de sinais vitais e dados de saúde, como pressão arterial e glicemia), por profissionais de saúde)
# Não e Sim - ( ) Teleconsulta (Consultas realizadas por meio digital, permitindo interação direta entre profissionais de saúde e pacientes)
# Não e Sim - ( ) Laudo a distância de eletrocardiograma (ECG) (Análise remota de eletrocardiograma por especialista, proporcionando diagnóstico e orientações sem a necessidade de presença física)
# Não e Sim - ( ) Laudo a distância de retinografia (Emissão de laudos de exames de retina realizados à distância por profissionais qualificados)
# Não e Sim - ( ) Laudo a distância de espirometria (Interpretação de testes de função pulmonar feitos à distância, com laudos emitidos por especialistas remotamente)
# Não e Sim - ( ) Laudo a distância de dermatologia (emissão de laudos de exames de lesões de pele realizados à distância por profissionais qualificados)
# Não e Sim - ( ) Participação em webconferências formativas (Participação em conferências online destinadas à educação e atualização de profissionais de saúde)
# Não e Sim - ( ) Participação em curso a distância ofertado pelos núcleos de telessaúde (Participação em cursos oferecidos online por núcleos de telessaúde, visando a capacitação contínua de profissionais da saúde)
# Não e Sim - ( ) Outro 
# Não e Sim - ( ) Não possui atividades de telessaúde 
df1$V361
df1$V362
df1$V363
df1$V364
df1$V365
df1$V366
df1$V367
df1$V368
df1$V369
df1$V3610
df1$V3611
df1$V3612

# 36.1. Se possui atividades de telessaúde, quais os equipamentos disponíveis? (pergunta condicionada à resposta da questão 36)
# Não e Sim - ( ) Computadores com câmera de vídeo e microfone 
# Não e Sim - ( ) Datashow 
# Não e Sim - ( ) Televisão 
# Não e Sim - ( ) Câmera com cabo USB ou cabo de vídeo compatível com computador/televisão 
# Não e Sim - ( ) Microfone com cabo USB ou cabo de áudio compatível com computador/televisão 
# Não e Sim - ( ) Fone de ouvido com microfone acoplado com cabo USB ou cabo de áudio compatível com computador
# Não e Sim - ( ) Caixa de som 
# Não e Sim - ( ) Outro
df1$v304
df1$v305
df1$V3613
df1$V3614
df1$V3615
df1$V3616
df1$V3617
df1$V3618

# 36.2. Se possui atividades de telessaúde, as ações são realizadas por: (pergunta condicionada à resposta da questão 36)	
# Não e Sim - ( ) Núcleos de telessaúde universitários públicos 
# Não e Sim - ( ) Iniciativa municipal 
# Não e Sim - ( ) Iniciativa estadual 
# Não e Sim - ( ) Outras instituições públicas 
# Não e Sim - ( ) Hospitais PROADI (Beneficência Portuguesa, Oswaldo Cruz, Albert Einstein, Moinhos de Vento, Sírio Libanês, HCor)
# Não e Sim - ( ) Outras ofertas privadas 
# Não e Sim - ( ) Outro
df1$V3621
df1$V3622
df1$V3623
df1$V3624
df1$V3625
df1$V3626
df1$V3627

# 37. Nesta UBS, utiliza-se prontuário eletrônico?
df1$V37

# 37.1. Se sim, como o prontuário eletrônico é utilizado? (pergunta condicionada à resposta da questão 37)
# Não e Sim - ( ) Para os atendimentos de médicas(os) 
# Não e Sim - ( ) Para os atendimentos de enfermeiras(os) 
# Não e Sim - ( ) Para os atendimentos da equipe multiprofissional (eMulti) 
# Não e Sim - ( ) Para os atendimentos de cirurgiãs(ões)-dentista(s)
# Não e Sim - ( ) Para os atendimentos dos demais profissionais de nível superior 
# Não e Sim - ( ) Para os atendimentos dos profissionais de nível médio e técnico 
df1$V3711
df1$V3712
df1$V3713
df1$V3714
df1$V3715
df1$V3716

# 37.2. Se sim, qual software de prontuário eletrônico é utilizado nesta UBS? (pergunta condicionada à resposta da questão 37)
df1$V372

# 37.3. Se sim, nesta UBS houve treinamento para utilização do prontuário eletrônico? (pergunta condicionada à resposta da questão 37)
df1$V373

# 37.4. Se sim, com quais pontos de atenção o prontuário eletrônico é compartilhado? (pergunta condicionada à resposta da questão 37)
# Não e Sim - ( ) Outras UBS 
# Não e Sim - ( ) Serviços exames/laboratoriais 
# Não e Sim - ( ) Unidade de Pronto Atendimento (UPA) 
# Não e Sim - ( ) Prontos-Socorros e outros serviços de urgência 
# Não e Sim - ( ) Serviços especializados públicos 
# Não e Sim - ( ) Serviços especializados privados 
# Não e Sim - ( ) Hospitais da rede pública 
# Não e Sim - ( ) Hospitais da rede contratada/conveniada 
# Não e Sim - ( ) Centro de Atenção Psicossocial (CAPS) 
# Não e Sim - ( ) Centro de Especialidades Odontológicas (CEO) 
# Não e Sim - ( ) O prontuário eletrônico não é compartilhado com outros pontos da rede de atenção
df1$V3741
df1$V3742
df1$V3743
df1$V3744
df1$V3745
df1$V3746
df1$V3747
df1$V3748
df1$V3749
df1$V37410
df1$V37411

df1 = df1 %>%
  mutate(
    across(c(v304, v305, V3613, V3614, V3615, V3616, V3617, V3618), ~ ifelse(V3612 == "Sim", "Não", .x), .names = "{.col}_nova"),
    across(c(V3621, V3622, V3623, V3624, V3625, V3626, V3627), ~ ifelse(V3612 == "Sim", "Não", .x), .names = "{.col}_nova"),
    across(c(V3711, V3712, V3713, V3714, V3715, V3716), ~ ifelse(V37 == "Não", "Não", .x), .names = "{.col}_nova"),
    across(c(V3741, V3742, V3743, V3744, V3745, V3746, V3747, V3748, V3749, V37410, V37411), ~ ifelse(V37 == "Não", "Não", .x), .names = "{.col}_nova"),
    V372_nova = ifelse(V37 == "Não", "Não utiliza prontuário eletrônico", V372),
    V373_nova = ifelse(V37 == "Não", "Não", V373))
vars_descritiva = c(
  "V2537","V2564","V2565","V2567",
  "V321","V322","V323","V324","V325","V326",
  "V33",
  "V341","V342","V343","V344","V345","V346","V347",
  "V351","V352","V353","V354","V355","V356","V357","V358",
  "V361","V362","V363","V364","V365","V366","V367","V368","V369","V3610","V3611","V3612",
  "v304_nova","v305_nova","V3613_nova","V3614_nova","V3615_nova","V3616_nova","V3617_nova","V3618_nova",
  "V3621_nova","V3622_nova","V3623_nova","V3624_nova","V3625_nova","V3626_nova","V3627_nova",
  "V37",
  "V3711_nova","V3712_nova","V3713_nova","V3714_nova","V3715_nova","V3716_nova",
  "V372_nova","V373_nova",
  "V3741_nova","V3742_nova","V3743_nova","V3744_nova","V3745_nova","V3746_nova","V3747_nova","V3748_nova","V3749_nova","V37410_nova","V37411_nova"
)

resultado_descritivo = lapply(vars_descritiva, function(v){DescritivaCat(df1[[v]])})

names(resultado_descritivo) = vars_descritiva
enunciados = c(
  V2537 = "Eletrocardiógrafo digital disponível e em condições de uso",
  V2564 = "Retinógrafo portátil digital disponível e em condições de uso",
  V2565 = "Espirômetro digital disponível e em condições de uso",
  V2567 = "Eletrocardiograma (ECG) digital disponível e em condições de uso",
  
  V321 = "Quantidade de computadores desktop em condições de uso",
  V322 = "Quantidade de notebooks em condições de uso",
  V323 = "Quantidade de tablets em condições de uso",
  V324 = "Quantidade de smartphones institucionais em condições de uso",
  V325 = "Quantidade de impressoras em condições de uso",
  V326 = "Quantidade de TVs em condições de uso",
  
  V33 = "Como pode ser considerado o acesso à Internet",
  
  V341 = "Computadores conectados à Internet em todos os consultórios",
  V342 = "Computadores conectados à Internet em alguns consultórios",
  V343 = "Computadores conectados à Internet na farmácia",
  V344 = "Computadores conectados à Internet na recepção",
  V345 = "Computadores conectados à Internet na sala de vacina",
  V346 = "Computadores conectados à Internet na sala de ACS",
  V347 = "Nenhum computador conectado à Internet",
  
  V351 = "Computadores com câmera de vídeo e microfone para webconferência",
  V352 = "Datashow disponível para webconferência",
  V353 = "Televisão disponível para webconferência",
  V354 = "Câmera com cabo USB ou vídeo para webconferência",
  V355 = "Microfone com cabo USB ou áudio para webconferência",
  V356 = "Fone de ouvido com microfone para webconferência",
  V357 = "Caixa de som disponível para webconferência",
  V358 = "Não possui equipamentos disponíveis para webconferência",
  
  V361 = "Realiza teleconsultorias",
  V362 = "Realiza teletriagem",
  V363 = "Realiza telemonitoramento remoto de pacientes",
  V364 = "Realiza teleconsulta",
  V365 = "Realiza laudo a distância de ECG",
  V366 = "Realiza laudo a distância de retinografia",
  V367 = "Realiza laudo a distância de espirometria",
  V368 = "Realiza laudo a distância de dermatologia",
  V369 = "Participa de webconferências formativas",
  V3610 = "Participa de curso a distância ofertado pelos núcleos de telessaúde",
  V3611 = "Realiza outra atividade de telessaúde",
  V3612 = "Não possui atividades de telessaúde",
  
  v304_nova = "Computadores com câmera e microfone disponíveis para telessaúde",
  v305_nova = "Datashow disponível para telessaúde",
  V3613_nova = "Televisão disponível para telessaúde",
  V3614_nova = "Câmera com cabo USB ou vídeo disponível para telessaúde",
  V3615_nova = "Microfone com cabo USB ou áudio disponível para telessaúde",
  V3616_nova = "Fone de ouvido com microfone disponível para telessaúde",
  V3617_nova = "Caixa de som disponível para telessaúde",
  V3618_nova = "Outro equipamento disponível para telessaúde",
  
  V3621_nova = "Ações de telessaúde realizadas por núcleos universitários públicos",
  V3622_nova = "Ações de telessaúde realizadas por iniciativa municipal",
  V3623_nova = "Ações de telessaúde realizadas por iniciativa estadual",
  V3624_nova = "Ações de telessaúde realizadas por outras instituições públicas",
  V3625_nova = "Ações de telessaúde realizadas por hospitais PROADI",
  V3626_nova = "Ações de telessaúde realizadas por outras ofertas privadas",
  V3627_nova = "Ações de telessaúde realizadas por outro responsável",
  
  V37 = "Utiliza prontuário eletrônico",
  
  V3711_nova = "Prontuário eletrônico utilizado para atendimentos médicos",
  V3712_nova = "Prontuário eletrônico utilizado para atendimentos de enfermagem",
  V3713_nova = "Prontuário eletrônico utilizado para atendimentos da equipe multiprofissional",
  V3714_nova = "Prontuário eletrônico utilizado para atendimentos odontológicos",
  V3715_nova = "Prontuário eletrônico utilizado para demais profissionais de nível superior",
  V3716_nova = "Prontuário eletrônico utilizado para profissionais de nível médio e técnico",
  
  V372_nova = "Software de prontuário eletrônico utilizado",
  V373_nova = "Houve treinamento para utilização do prontuário eletrônico",
  
  V3741_nova = "Prontuário eletrônico compartilhado com outras UBS",
  V3742_nova = "Prontuário eletrônico compartilhado com serviços de exames/laboratoriais",
  V3743_nova = "Prontuário eletrônico compartilhado com UPA",
  V3744_nova = "Prontuário eletrônico compartilhado com prontos-socorros e urgência",
  V3745_nova = "Prontuário eletrônico compartilhado com serviços especializados públicos",
  V3746_nova = "Prontuário eletrônico compartilhado com serviços especializados privados",
  V3747_nova = "Prontuário eletrônico compartilhado com hospitais da rede pública",
  V3748_nova = "Prontuário eletrônico compartilhado com hospitais da rede contratada/conveniada",
  V3749_nova = "Prontuário eletrônico compartilhado com CAPS",
  V37410_nova = "Prontuário eletrônico compartilhado com CEO",
  V37411_nova = "Prontuário eletrônico não compartilhado com outros pontos da rede"
)
resultado_final = do.call(rbind, lapply(vars_descritiva, function(v){
  
  tab = as.data.frame(DescritivaCat(df1[[v]]))
  
  tab$Categoria = rownames(tab)
  tab$Variavel = v
  tab$Enunciado = enunciados[v]
  
  rownames(tab) = NULL
  
  tab = tab[, c("Variavel", "Enunciado", "Categoria", "Freq. Absoluta (N)", "Freq. Relativa (%)")]
  
  return(tab)
}))
write.xlsx(resultado_final %>% as.data.frame(),"Análise descritiva Contagem 08-06-2026.xlsx")
