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

####===============
#### Saúde digital
####===============
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
vars_descritiva_saude = c(
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

resultado_descritivo_saude = lapply(vars_descritiva_saude, function(v){DescritivaCat(df1[[v]])})

names(resultado_descritivo_saude) = vars_descritiva_saude
enunciados_saude = c(
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
resultado_final = do.call(rbind, lapply(vars_descritiva_saude, function(v){
  
  tab = as.data.frame(DescritivaCat(df1[[v]]))
  
  tab$Categoria = rownames(tab)
  tab$Variavel = v
  tab$Enunciado = enunciados_saude[v]
  
  rownames(tab) = NULL
  
  tab = tab[, c("Variavel", "Enunciado", "Categoria", "Freq. Absoluta (N)", "Freq. Relativa (%)")]
  
  return(tab)
}))
#write.xlsx(resultado_final %>% as.data.frame(),"Análise descritiva Contagem 08-06-2026.xlsx")

####========================
#### Coordenação do cuidado
####========================
# 102. Nesta UBS, as consultas são agendadas com hora marcada?
DescritivaCat(df1$V102)

# V.103. Selecione a(s) forma(s) de agendamento de consulta na UBS pelo usuário:
# V.103.1   Não e Sim - ( ) Presencialmente na UBS 
# V.103.2		Não e Sim - ( ) Por telefone 
# V.103.3		Não e Sim - ( ) Pelo WhatsApp
# V.103.4		Não e Sim - ( ) Por site específico para agendamento de consulta na UBS 
# V.103.5		Não e Sim - ( ) Meu SUS digital 
# V.103.6		Não e Sim - ( ) Por aplicativo desenvolvido para este fim 
# V.103.7		Não e Sim - ( ) Consulta agendada pelo ACS 
# V.103.8		Não e Sim - ( ) Outra
df1 %>% select(V1031,V1032,V1033,V1034,v1035,v1036,v1037,v1038) %>% map(DescritivaCat)

# 104. Nesta UBS, como é realizada a marcação de consulta da demanda programada para os grupos prioritários?
# V.104.1		Não e Sim - ( ) O próximo atendimento é agendado no final de cada consulta 
# V.104.2		Não e Sim - ( ) A consulta é marcada pela equipe e depois comunicada ao usuário 
# V.104.3		Não e Sim - ( ) A próxima consulta é marcada pelo usuário na UBS 
# V.104.4		Não e Sim - ( ) O usuário é orientado a vir à UBS em dia específico, sem agendamento prévio 
# V.104.5		Não e Sim - ( ) O usuário é orientado a vir à UBS quando sentir necessidade e sem agendamento prévio 
# V.104.6		Não e Sim - ( ) Outro 
df1 %>% select(v1041,v1042,v1043,v1044,v1045,v1046) %>% map(DescritivaCat)

# V.105	Nesta UBS, há disponibilidade de consultas para atendimento à demanda espontânea todos os dias?	
# ( ) Sim, para todos os turnos de funcionamento da UBS 
# ( ) Sim, apenas em um turno 
# ( ) Não são reservadas vagas de consultas para atendimento à demanda espontânea.
DescritivaCat(df1$v105)

# 106. Quais são as estratégias de comunicação entre os profissionais da(s) equipe(s) e profissionais de outros pontos da rede?
# V.106.1		Não e Sim - ( ) Reuniões técnicas ou sessões clínicas conjuntas 
# V.106.2		Não e Sim - ( ) Consulta compartilhada 
# V.106.3		Não e Sim - ( ) Telessaúde 
# V.106.4		Não e Sim - ( ) Prontuário Eletrônico compartilhado 
# V.106.5		Não e Sim - ( ) Comunicação via e-mail
# V.106.6		Não e Sim - ( ) Comunicação via Whatsapp
# V.106.7		Não e Sim - ( ) Ficha de referência/contrarreferência 
# V.106.8		Não e Sim - ( ) Contato telefônico 
# V.106.9		Não e Sim - ( ) Formulário de Compartilhamento do Cuidado 
# V.106.10		Não e Sim - ( ) Plano de Cuidado Compartilhado ou Projeto Terapêutico Singular (PTS) 
# V.106.11		Não e Sim - ( ) Outro
# V.106.12		Não e Sim - ( ) Não há estratégia de comunicação entre os profissionais da(s) equipe(s) e profissionais de outros pontos da rede. 
df1 %>% select(v1061,v1062,v1063,v1064,v1065,v1066,v1067,v1068,v1069,v10610,v10611,v10612) %>% map(DescritivaCat)

# 107. Com que frequência a(s) equipe(s) desta UBS troca(m) informações com especialistas de outros pontos da rede sobre os usuários encaminhados?	
DescritivaCat(df1$v107)
df1$v107 = factor(df1$v107, c("Nunca","Raramente","Algumas vezes","Quase sempre","Sempre"))

# 108. Com que frequência a(s) equipe(s) desta UBS recebe(m) o resumo de alta hospitalar dos usuários do território?
DescritivaCat(df1$v108_01)
df1$v108_01 = factor(df1$v108_01, levels = c("Nunca", "Raramente", "Algumas vezes", "Quase sempre", "Sempre"), ordered = TRUE)

# 109. Quando um usuário é atendido nesta UBS e necessita ser encaminhado para uma consulta com outros especialistas, quais são as formas possíveis?
# V.109.1		Não e Sim - ( ) A consulta é marcada pela UBS e informada na hora para o usuário 
# V.109.2		Não e Sim - ( ) A consulta é marcada pela UBS, pelo sistema de regulação e a data posteriormente informada ao usuário 
# V.109.3		Não e Sim - ( ) A consulta é marcada pelo próprio usuário junto à central de marcação de consultas especializadas 
# V.109.4		Não e Sim - ( ) O usuário recebe uma ficha de encaminhamento/referência e deve se dirigir a um serviço indicado pela equipe 
# V.109.5		Não e Sim - ( ) O usuário busca a consulta especializada por conta própria
df1 %>% select(v1091, v1092, v1093, v1094, v1095) %>% map(DescritivaCat)

# 110. Indique para quais condições abaixo existem serviços de referência definidos:
# V.110.1		Não e Sim - ( ) Suspeita de câncer de mama 
# V.110.2		Não e Sim - ( ) Suspeita de câncer do colo do útero 
# V.110.3		Não e Sim - ( ) Gestação de alto risco 
# V.110.4		Não e Sim - ( ) Parto (maternidade) 
# V.110.5		Não e Sim - ( ) Síndrome coronariana aguda 
# V.110.6		Não e Sim - ( ) Acidente Vascular Encefálico (AVE) 
# V.110.7		Não e Sim - ( ) Atendimento odontológico especializado 
# V.110.8		Não e Sim - ( ) Suspeita de câncer de boca 
# V.110.9		Não e Sim - ( ) Violência(s) 
# V.110.10		Não e Sim - ( ) Saúde Mental 
# V.110.11		Não e Sim - ( ) Outro 
# V.110.12		Não e Sim - ( ) Não existem serviços de referência
df1 %>% select(v1101, v1102, v1103, v1104, v1105, v1106, v1107, v1108, v1109, v11010, v11011, v11012) %>% map(DescritivaCat)

# 111. Nesta UBS são utilizados protocolos técnicos para acesso aos serviços de atenção especializada?
DescritivaCat(df1$v111)
  
# 112. Nesta UBS, há profissional responsável pela regulação assistencial que analisa os encaminhamentos à atenção especializada antes da inserção no sistema de regulação?
DescritivaCat(df1$v112)
  
# 113. Nesta UBS, os encaminhamentos solicitados por enfermeiras(os) à atenção especializada são aceitos para inserção no sistema de regulação?
DescritivaCat(df1$v113)
  
# 114. Nesta UBS, há registro/lista dos usuários encaminhados para outros pontos de atenção da rede?
DescritivaCat(df1$v114)

# 115. Nesta UBS, é realizado o monitoramento dos tempos de espera dos usuários encaminhados a outros pontos de atenção da rede?
DescritivaCat(df1$v115_01) 

# 116. Esta UBS, dispõe de acesso a serviço de transporte para usuárias (os) encaminhados para serviços de saúde especializados?
DescritivaCat(df1$v116_01)

# 117. Com quais serviços as(os) profissionais da UBS compartilham o cuidado?
# V.117.1		Não e Sim - ( ) Equipe multiprofissional (eMulti) 
# V.117.2		Não e Sim - ( ) Centro de Atenção Psicossocial (CAPS) 
# V.117.3		Não e Sim - ( ) Centro de Testagem e Aconselhamento (CTA) 
# V.117.4		Não e Sim - ( ) Serviços de Atenção Especializada em Infecções Sexualmente Transmissíveis (SAE) 
# V.117.5		Não e Sim - ( ) Hospitais 
# V.117.6		Não e Sim - ( ) Policlínicas regionais 
# V.117.7		Não e Sim - ( ) Centro Ambulatorial de Especialidades 
# V.117.8		Não e Sim - ( ) Maternidades 
# V.117.9		Não e Sim - ( ) Polo da Academia da Saúde 
# V.117.10		Não e Sim - ( ) Centros especializados: idoso 
# V.117.11		Não e Sim - ( ) Centros especializados: doenças crônicas 
# V.117.12		Não e Sim - ( ) Centros especializados: obesidade 
# V.117.13		Não e Sim - ( ) Centros especializados: oncologia 
# V.117.14		Não e Sim - ( ) Centros especializados: reabilitação 
# V.117.15		Não e Sim - ( ) Centros de referência de saúde do trabalhador (CEREST) 
# V.117.16		Não e Sim - ( ) Centros especializados: materno-infantil 
# V.117.17		Não e Sim - ( ) Centro de Especialidades Odontológicas (CEO) 
# V.117.18		Não e Sim - ( ) Serviço de Especialidade em Saúde Bucal (Sesb) 
# V.117.19		Não e Sim - ( ) Serviço de Atenção Domiciliar 
# V.117.20		Não e Sim - ( ) Não compartilham o cuidado
df1 %>% select(v1171, v1172, v1173, v1174, v1175, v1176, v1177, v1178, v1179, v11710, v11711, v11712, 
               v11713, v11714, v11715, v11716, v11717, v11718, v11719, v11720) %>% map(DescritivaCat)

# 118. Nesta UBS, quais serviços de vigilância apoiam a(s) equipe(s)?
# V.118.1		Não e Sim - ( ) Vigilância epidemiológica 
# V.118.2		Não e Sim - ( ) Vigilância sanitária 
# V.118.3		Não e Sim - ( ) Vigilância em saúde do trabalhador 
# V.118.4		Não e Sim - ( ) Vigilância ambiental 
# V.118.5		Não e Sim - ( ) Não recebe apoio de nenhum serviço de vigilância
df1 %>% select(v1181,v1182,v1183,v1184,v1185) %>% map(DescritivaCat)

# 119. Quais profissionais integram a(s) equipe(s) multiprofissional(is) (eMulti) vinculada a esta UBS?
# V.119.1		Não e Sim - ( ) Arte educador 
# V.119.2		Não e Sim - ( ) Assistente social 
# V.119.3		Não e Sim - ( ) Profissional de Educação Física 
# V.119.4		Não e Sim - ( ) Farmacêutica(o) clínica(o) 
# V.119.5		Não e Sim - ( ) Fisioterapeuta 
# V.119.6		Não e Sim - ( ) Fonoaudióloga(o) 
# V.119.7		Não e Sim - ( ) Médica(o) acupunturista 
# V.119.8		Não e Sim - ( ) Médica(o) cardiologista 
# V.119.9		Não e Sim - ( ) Médica(o) dermatologista 
# V.119.10		Não e Sim - ( ) Médica(o) endocrinologista 
# V.119.11		Não e Sim - ( ) Médica(o) geriatra 
# V.119.12		Não e Sim - ( ) Médica(o) ginecologista/Obstetra 
# V.119.13		Não e Sim - ( ) Médica(o) hansenologista 
# V.119.14		Não e Sim - ( ) Médica(o) homeopata 
# V.119.15		Não e Sim - ( ) Médica(o) infectologista 
# V.119.16		Não e Sim - ( ) Médica(o) pediatra 
# V.119.17		Não e Sim - ( ) Médica(o) psiquiatra 
# V.119.18		Não e Sim - ( ) Médica(o) veterinária(o) 
# V.119.19		Não e Sim - ( ) Nutricionista 
# V.119.20		Não e Sim - ( ) Psicóloga(o) 
# V.119.21		Não e Sim - ( ) Sanitarista 
# V.119.22		Não e Sim - ( ) Terapeuta ocupacional
df1 %>% select(v1191, v1192, v1193, v1194, v1195, v1196, v1197, v1198, v1199, v11910, 
               v11911, v11912, v11913, v11914, v11915, v11916, v11917, v11918, v11919, v11920, v11921, v1192) %>% map(DescritivaCat)

# 120. Nesta UBS, são realizadas reuniões das demais equipes da UBS com os profissionais da equipe multiprofissional (eMulti)?
DescritivaCat(df1$v120)
df1$V7emulti_cat = case_when(as.numeric(df1$V7emulti) == 0 ~ "Não",
                             as.numeric(df1$V7emulti) > 0 ~ "Sim")
df1$v120_nova = ifelse(df1$V7emulti_cat == "Não", "Não", df1$v120)

# 120.1. Se sim, qual a periodicidade das reuniões das demais equipes da UBS com os profissionais da equipe multiprofissional (eMulti)?
DescritivaCat(df1$v1201)

# 120. Nesta UBS, são realizadas reuniões das demais equipes da UBS com os profissionais da equipe multiprofissional (eMulti)?
# V.120.2.1		Não e Sim - ( ) Ações de caráter assistencial 
# V.120.2.2		Não e Sim - ( ) Consulta presencial compartilhada 
# V.120.2.3		Não e Sim - ( ) Plano de cuidado da APS/Projeto Terapêutico Singular 
# V.120.2.4		Não e Sim - ( ) Grupos terapêuticos 
# V.120.2.5		Não e Sim - ( ) Atendimento domiciliar 
# V.120.2.6		Não e Sim - ( ) Teleconsulta 
df1 %>% select(v12021, v12022, v12023, v12024, v12025, v12026) %>% map(DescritivaCat)

#121. Nesta UBS, o atendimento à(s) pessoa(s) com deficiência é realizado com o apoio da equipe multiprofissional (eMulti)?
DescritivaCat(df1$v121)

df1 = df1 %>%
  mutate(
    # 119. Quais profissionais integram a(s) equipe(s) multiprofissional(is) (eMulti) vinculada a esta UBS?
    across(c(v1191, v1192, v1193, v1194, v1195, v1196, v1197, v1198, v1199, v11910, 
             v11911, v11912, v11913, v11914, v11915, v11916, v11917, v11918, v11919, 
             v11920, v11921, v11922), ~ ifelse(V7emulti_cat == "Não", "Não", .x), .names = "{.col}_nova"),
    
    # Q120.1 - se não realiza reunião com eMulti, periodicidade recebe categoria própria
    v1201_nova = case_when(v120_nova == "Não" ~ "Não realiza reuniões com eMulti", 
                           TRUE ~ v1201),
    # Q120.2 - se não realiza reunião com eMulti, as ações vinculadas viram "Não"
    across(c(v12021, v12022, v12023, v12024, v12025, v12026), ~ ifelse(v120_nova == "Não", "Não", .x), .names = "{.col}_nova"),
    
    #121. Nesta UBS, o atendimento à(s) pessoa(s) com deficiência é realizado com o apoio da equipe multiprofissional (eMulti)?
    v121_nova = case_when(v120_nova == "Não" ~ "Não realiza reuniões com eMulti", TRUE ~ v121))

vars_descritiva_coord = c(
  "V102",
  
  "V1031", "V1032", "V1033", "V1034", "v1035", "v1036", "v1037", "v1038",
  
  "v1041", "v1042", "v1043", "v1044", "v1045", "v1046",
  
  "v105",
  
  "v1061", "v1062", "v1063", "v1064", "v1065", "v1066",
  "v1067", "v1068", "v1069", "v10610", "v10611", "v10612",
  
  "v107",
  "v108_01",
  
  "v1091", "v1092", "v1093", "v1094", "v1095",
  
  "v1101", "v1102", "v1103", "v1104", "v1105", "v1106",
  "v1107", "v1108", "v1109", "v11010", "v11011", "v11012",
  
  "v111", "v112", "v113", "v114", "v115_01", "v116_01",
  
  "v1171", "v1172", "v1173", "v1174", "v1175",
  "v1176", "v1177", "v1178", "v1179", "v11710",
  "v11711", "v11712", "v11713", "v11714", "v11715",
  "v11716", "v11717", "v11718", "v11719", "v11720",
  
  "v1181", "v1182", "v1183", "v1184", "v1185",
  
  "v1191_nova", "v1192_nova", "v1193_nova", "v1194_nova", "v1195_nova", "v1196_nova", "v1197_nova", "v1198_nova", "v1199_nova",
  "v11910_nova", "v11911_nova", "v11912_nova", "v11913_nova", "v11914_nova", "v11915_nova", "v11916_nova", "v11917_nova",
  "v11918_nova", "v11919_nova", "v11920_nova", "v11921_nova", "v11922_nova",
  
  "v120_nova",
  "v1201_nova",
  "v12021_nova", "v12022_nova", "v12023_nova", "v12024_nova", "v12025_nova", "v12026_nova",
  
  "v121_nova")

df1$v1201_nova = factor(df1$v1201_nova, levels = c("Não realiza reuniões com eMulti", "Sem periodicidade definida", "Mensal", "Quinzenal", "Semanal"), ordered = TRUE)

enunciados_coord = c(
  V102 = "Consultas são agendadas com hora marcada",
  
  V1031 = "Forma de agendamento: presencialmente na UBS",
  V1032 = "Forma de agendamento: por telefone",
  V1033 = "Forma de agendamento: pelo WhatsApp",
  V1034 = "Forma de agendamento: por site específico da UBS",
  v1035 = "Forma de agendamento: Meu SUS Digital",
  v1036 = "Forma de agendamento: aplicativo desenvolvido para este fim",
  v1037 = "Forma de agendamento: consulta agendada pelo ACS",
  v1038 = "Forma de agendamento: outra",
  
  v1041 = "Demanda programada: próximo atendimento agendado ao final da consulta",
  v1042 = "Demanda programada: consulta marcada pela equipe e comunicada ao usuário",
  v1043 = "Demanda programada: próxima consulta marcada pelo usuário na UBS",
  v1044 = "Demanda programada: usuário orientado a vir em dia específico sem agendamento prévio",
  v1045 = "Demanda programada: usuário orientado a vir quando sentir necessidade, sem agendamento prévio",
  v1046 = "Demanda programada: outro formato de marcação",
  
  v105 = "Disponibilidade de consultas para demanda espontânea todos os dias",
  
  v1061 = "Estratégia de comunicação: reuniões técnicas ou sessões clínicas conjuntas",
  v1062 = "Estratégia de comunicação: consulta compartilhada",
  v1063 = "Estratégia de comunicação: telessaúde",
  v1064 = "Estratégia de comunicação: prontuário eletrônico compartilhado",
  v1065 = "Estratégia de comunicação: e-mail",
  v1066 = "Estratégia de comunicação: WhatsApp",
  v1067 = "Estratégia de comunicação: ficha de referência/contrarreferência",
  v1068 = "Estratégia de comunicação: contato telefônico",
  v1069 = "Estratégia de comunicação: formulário de compartilhamento do cuidado",
  v10610 = "Estratégia de comunicação: plano de cuidado compartilhado ou PTS",
  v10611 = "Estratégia de comunicação: outro",
  v10612 = "Não há estratégia de comunicação com outros pontos da rede",
  
  v107 = "Frequência de troca de informações com especialistas sobre usuários encaminhados",
  v108_01 = "Frequência de recebimento do resumo de alta hospitalar dos usuários do território",
  
  v1091 = "Encaminhamento especializado: consulta marcada pela UBS e informada na hora ao usuário",
  v1092 = "Encaminhamento especializado: consulta marcada pela UBS/sistema de regulação e informada posteriormente",
  v1093 = "Encaminhamento especializado: consulta marcada pelo próprio usuário na central",
  v1094 = "Encaminhamento especializado: usuário recebe ficha de encaminhamento/referência",
  v1095 = "Encaminhamento especializado: usuário busca consulta especializada por conta própria",
  
  v1101 = "Serviço de referência definido: suspeita de câncer de mama",
  v1102 = "Serviço de referência definido: suspeita de câncer do colo do útero",
  v1103 = "Serviço de referência definido: gestação de alto risco",
  v1104 = "Serviço de referência definido: parto/maternidade",
  v1105 = "Serviço de referência definido: síndrome coronariana aguda",
  v1106 = "Serviço de referência definido: acidente vascular encefálico",
  v1107 = "Serviço de referência definido: atendimento odontológico especializado",
  v1108 = "Serviço de referência definido: suspeita de câncer de boca",
  v1109 = "Serviço de referência definido: violência(s)",
  v11010 = "Serviço de referência definido: saúde mental",
  v11011 = "Serviço de referência definido: outro",
  v11012 = "Não existem serviços de referência definidos",
  
  v111 = "Utiliza protocolos técnicos para acesso aos serviços de atenção especializada",
  v112 = "Há profissional responsável pela regulação assistencial antes da inserção no sistema",
  v113 = "Encaminhamentos solicitados por enfermeiras(os) são aceitos no sistema de regulação",
  v114 = "Há registro/lista dos usuários encaminhados para outros pontos da rede",
  v115_01 = "Realiza monitoramento dos tempos de espera dos usuários encaminhados",
  v116_01 = "Dispõe de acesso a transporte para usuários encaminhados a serviços especializados",
  
  v1171 = "Compartilha cuidado com equipe multiprofissional/eMulti",
  v1172 = "Compartilha cuidado com CAPS",
  v1173 = "Compartilha cuidado com CTA",
  v1174 = "Compartilha cuidado com SAE/IST",
  v1175 = "Compartilha cuidado com hospitais",
  v1176 = "Compartilha cuidado com policlínicas regionais",
  v1177 = "Compartilha cuidado com centro ambulatorial de especialidades",
  v1178 = "Compartilha cuidado com maternidades",
  v1179 = "Compartilha cuidado com polo da Academia da Saúde",
  v11710 = "Compartilha cuidado com centro especializado: idoso",
  v11711 = "Compartilha cuidado com centro especializado: doenças crônicas",
  v11712 = "Compartilha cuidado com centro especializado: obesidade",
  v11713 = "Compartilha cuidado com centro especializado: oncologia",
  v11714 = "Compartilha cuidado com centro especializado: reabilitação",
  v11715 = "Compartilha cuidado com CEREST",
  v11716 = "Compartilha cuidado com centro especializado materno-infantil",
  v11717 = "Compartilha cuidado com CEO",
  v11718 = "Compartilha cuidado com Serviço de Especialidade em Saúde Bucal",
  v11719 = "Compartilha cuidado com Serviço de Atenção Domiciliar",
  v11720 = "Não compartilha o cuidado",
  
  v1181 = "Apoio da vigilância epidemiológica",
  v1182 = "Apoio da vigilância sanitária",
  v1183 = "Apoio da vigilância em saúde do trabalhador",
  v1184 = "Apoio da vigilância ambiental",
  v1185 = "Não recebe apoio de nenhum serviço de vigilância",
  
  v1191_nova = "eMulti: arte educador",
  v1192_nova = "eMulti: assistente social",
  v1193_nova = "eMulti: profissional de educação física",
  v1194_nova = "eMulti: farmacêutica(o) clínica(o)",
  v1195_nova = "eMulti: fisioterapeuta",
  v1196_nova = "eMulti: fonoaudióloga(o)",
  v1197_nova = "eMulti: médica(o) acupunturista",
  v1198_nova = "eMulti: médica(o) cardiologista",
  v1199_nova = "eMulti: médica(o) dermatologista",
  v11910_nova = "eMulti: médica(o) endocrinologista",
  v11911_nova = "eMulti: médica(o) geriatra",
  v11912_nova = "eMulti: médica(o) ginecologista/obstetra",
  v11913_nova = "eMulti: médica(o) hansenologista",
  v11914_nova = "eMulti: médica(o) homeopata",
  v11915_nova = "eMulti: médica(o) infectologista",
  v11916_nova = "eMulti: médica(o) pediatra",
  v11917_nova = "eMulti: médica(o) psiquiatra",
  v11918_nova = "eMulti: médica(o) veterinária(o)",
  v11919_nova = "eMulti: nutricionista",
  v11920_nova = "eMulti: psicóloga(o)",
  v11921_nova = "eMulti: sanitarista",
  v11922_nova = "eMulti: terapeuta ocupacional",
  
  v120_nova = "Realiza reuniões das equipes da UBS com profissionais da eMulti",
  v1201_nova = "Periodicidade das reuniões com profissionais da eMulti",
  v12021_nova = "Atividade com eMulti: ações de caráter assistencial",
  v12022_nova = "Atividade com eMulti: consulta presencial compartilhada",
  v12023_nova = "Atividade com eMulti: plano de cuidado da APS/PTS",
  v12024_nova = "Atividade com eMulti: grupos terapêuticos",
  v12025_nova = "Atividade com eMulti: atendimento domiciliar",
  v12026_nova = "Atividade com eMulti: teleconsulta",
  
  v121_nova = "Atendimento à pessoa com deficiência realizado com apoio da eMulti"
)
resultado_descritivo_coord = lapply(vars_descritiva_coord, function(v){
  DescritivaCat(df1[[v]])
})

names(resultado_descritivo_coord) = vars_descritiva_coord

resultado_final_coord = do.call(rbind, lapply(vars_descritiva_coord, function(v){
  
  tab = as.data.frame(DescritivaCat(df1[[v]]))
  
  tab$Categoria = rownames(tab)
  tab$Variavel = v
  tab$Enunciado = enunciados_coord[v]
  
  rownames(tab) = NULL
  
  tab = tab[, c("Variavel", "Enunciado", "Categoria", "Freq. Absoluta (N)", "Freq. Relativa (%)")]
  
  return(tab)
}))

# write.xlsx(
#   resultado_final_coord %>% as.data.frame(),
#   "Análise descritiva Coordenação do cuidado Contagem.xlsx"
# )
