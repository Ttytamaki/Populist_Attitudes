# ---
# 00 - Tratando e Criando as Variáveis
# Tratamento do Banco de Dados e Criação da Variável: Atitudes Populistas
# Atitudes Populistas e Voto em Bolsonaro em 2018 (2020)
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 11/09/2020
# ---


## PREAMBULO -------------------------------------------------------------------

###
# Carregando Pacotes Necessários:
library(tidyverse)
library(haven)
library(here)
library(sjlabelled)

###
# Carregando o banco de dados do Github:
url <- "https://raw.github.com/Ttytamaki/Pop_att/master/ESEB2018.sav"
e19 <- read_sav(url)

###
# Removendo acentos, mudando o padrão das variáveis para letras minusculas, e removendo 
  # caracteres especiais:
source(here::here("functions", "remove_accent.R"), encoding = "UTF-8")
names(e19) <- remove_accent(names(e19))


###
# Quais variáveis serão utilizadas:
#
# Dos dados do ESEB 2018, nos interessa as seguintes variáveis:
#   
# **M1** (ESEB18 - Q401):
# O que as pessoas chamam de compromisso em política é, na verdade, 
# apenas uma forma de negociar os princípios;
# 
# **AE1** (ESEB18 - Q402: 
# A maior parte dos políticos não se importa com as pessoas; 
#
# **AE2** (ESEB18 - Q403):
# A maior parte dos políticos é confiável; 
#            
# **AE3** (ESEB18 - Q404):
# Os políticos são o principal problema do Brasil;
#            
# **AE4** (ESEB18 - Q407):
# A maior parte dos políticos se preocupa apenas com os 
# interesses dos ricos e poderosos; 
#            
# **PC1** (ESEB18 - Q406):
# O povo, e não os políticos, deveria tomar as decisões políticas mais importantes;
#            
# **PC2** (ESEB18 - Q502):
# A vontade da maioria deveria sempre prevalecer,
# mesmo que prejudique os direitos das minorias;  
#            
# **Idade** (ESEB18 - D1A-ID)
#
# **Sexo** (ESEB18 - D2_SEXO) (categórica tendo como referência ser "Mulher")
#
# **Educação** (ESEB18 - D3_ESCOLA)
#
# **Ideo2.2** (ESEB18 - Q18) (categórica tendo como referência "centro")
#
# **Corrup1** (ESEB18 - P12):
# Você diria que a corrupção no Brasil é um problema muito sério, sério, pouco sério ou 
# não é um problema sério:
#     1 - Muito sério; 4 - Não é um problema sério; 8 e 9 - NA's
################################



# Atitudes Populistas:
  ## [[Para o Indice e para a EFA]]

## 1a Dimensão: Maniqueísmo [Divisão do mundo e da política]:
### Q401: [M1]
# O que as pessoas chamam de compromisso em política é, na verdade, 
# apenas uma forma de negociar os princípios;
# Var. Original:
## 1 - Concorda Muito;
## 5 - Discorda Muito;
# Var. AE1:
## 1 - Discorda Muito;
## 5 - Concorda Muito;

lbs <- c("Discorda Muito", "Discorda um Pouco", "Nem concorda nem discorda", 
         "Concorda um pouco", "Concorda Muito")

e19 <- e19 %>%
  mutate(M1 = case_when(
    q401 == 7 ~ as.numeric(NA),
    q401 == 8 ~ as.numeric(NA),
    TRUE ~ as.numeric(6 - q401)
  )) %>% 
  set_labels(M1, labels = lbs)



## 2a Dimensão: Antielitismo [Atitudes referentes a ELITE]:
### Q402: [AE1]
# A maior parte dos políticos não se importa com as pessoas;
# Var. Original:
## 1 - Concorda Muito;
## 5 - Discorda Muito;
# Var. AE1:
## 1 - Discorda Muito;
## 5 - Concorda Muito;


e19 <- e19 %>%
  mutate(AE1 = case_when(
    q402 == 7 ~ as.numeric(NA),
    q402 == 8 ~ as.numeric(NA),
    TRUE ~ as.numeric(6 - q402)
  )) %>%
  set_labels(AE1, labels = lbs)


### Q403: [AE2]
# A maior parte dos políticos é confiável;
# Var. Original:
## 1 - Concorda Muito;
## 5 - Discorda Muito;
# Var. AE2:
## 1 - Concorda Muito;
## 5 - Discorda Muito;

lbs <- c("Concorda Muito", "Concorda um Pouco",
         "Nem Concorda Nem Discorda",
         "Discorda um Pouco", "Discorda Muito")

e19 <- e19 %>%
  mutate(AE2 = case_when(
    q403 == 7 ~ as.numeric(NA),
    q403 == 8 ~ as.numeric(NA),
    TRUE ~ as.numeric(q403)
  )) %>%
  set_labels(AE2, labels = lbs)


### Q404: [AE3]
# Os políticos são o principal problema do Brasil;
# Var. Original:
## 1 - Concorda Muito
## 5 - Discorda Muito
# Var. AE3:
## 1 - Discorda Muito
## 5 - Concorda Muito

lbs <- c("Discorda Muito", "Discorda um Pouco",
         "Nem Concorda Nem Discorda",
         "Concorda um Pouco", "Concorda Muito")

e19 <- e19 %>%
  mutate(AE3 = case_when(
    q404 == 8 ~ as.numeric(NA),
    q404 == 7 ~ as.numeric(NA),
    TRUE ~ as.numeric(6 - q404)
  )) %>%
  set_labels(AE3, labels = lbs)



### Q407: [AE4]
# A Maior parte dos políticos se preocupa apenas com os interesses dos ricos e poderosos
# Var. Original:
## 1 - Concorda Muito 
## 5 - Discorda Muito
# Var. AE4:
## 1 - Discorda Muito
## 5 - Concorda Muito

lbs <- c("Discorda Muito", "Discorda um Pouco",
         "Nem Concorda Nem Discorda",
         "Concorda um Pouco", "Concorda Muito")

e19 <- e19 %>%
  mutate(AE4 = case_when(
    q407 == 7 ~ as.numeric(NA),
    q407 == 8 ~ as.numeric(NA),
    TRUE ~ as.numeric(6 - q407)
  )) %>%
  set_labels(AE4, labels = lbs)



## 3a Dimensão: Soberania Popular [Atitudes referentes ao POVO]:
### Q406: [PC1]
# O povo, e não os políticos, deveria tomar as decisões políticas mais importantes;
# Var. Original:
## 1 - Concorda Muito;
## 5 - Discorda Muito;
# Var. AE1:
## 1 - Discorda Muito;
## 5 - Concorda Muito;


lbs <- c("Discorda Muito", "Discorda um Pouco",
         "Nem Concorda nem Discorda",
         "Concorda um Pouco", "Concorda Muito")

e19 <- e19 %>% 
  mutate(PC1 = case_when(
    q406 == 7 ~ as.numeric(NA),
    q406 == 8 ~ as.numeric(NA),
    TRUE ~ as.numeric(6 - q406)
  )) %>%
  set_labels(PC1, labels = lbs)


### Q502: [PC2]
# A vontade da maioria deveria sempre prevalecer, mesmo que prejudique os 
# direitos das minorias;
# Var. Original:
## 1 - Concorda Muito;
## 5 - Discorda Muito;
# Var. AE1:
## 1 - Discorda Muito;
## 5 - Concorda Muito;


lbs <- c("Discorda Muito", "Discorda um Pouco",
         "Nem Concorda nem Discorda",
         "Concorda um Pouco", "Concorda Muito")

e19 <- e19 %>%
  mutate(PC2 = case_when(
    q502 == 7 ~ as.numeric(NA),
    q502 == 8 ~ as.numeric(NA),
    TRUE ~ as.numeric(6 - q502)
  )) %>%
  set_labels(PC2, labels = lbs)



################################

# Variáveis Controle:

## SEXO:
### D2_SEXO: [sexo]
# 1 - Homem -> 1
# 2 - Mulher -> 0

## EDUCAÇÃO:
### D3_ESCOLA: [ed]

## IDADE:
### D1A_ID: [id]

e19$id <- e19$d1a_id

e19 <- e19 %>%
  mutate(sexo = case_when(
    d2_sexo == 1 ~ 1,
    d2_sexo == 2 ~ 0,
    TRUE ~ as.numeric(d2_sexo)
  )) %>%
  mutate(ed = d3_escola,
         id = d1a_id) 



## Religião 
### d10: [relig]
# Evangélicos 
# Resto

e19 <- e19 %>%
  mutate(relig = case_when(
    d10 == 5 ~ as.character("Evangélico"),
    d10 >= 96 ~ as.character(NA),
    TRUE ~ as.character("Outros")
  ))

e19$relig <- factor(e19$relig, ordered = F)
e19$relig <- relevel(e19$relig, ref = "Outros")


## AntiPTismo:
### q1501: [antipt]

e19 <- e19 %>%
  mutate(antipt = case_when(
    q1501 == 0 ~ as.character("AntiPT"),
    q1501 == 1 ~ as.character("AntiPT"),
    q1501 >= 96 ~ as.character(NA),
    q1501 > 1 & q1501 < 96 ~ as.character("Resto"),
    TRUE ~ as.character(NA)
  ))

e19$antipt <- factor(e19$antipt, ordered = F)
e19$antipt <- relevel(e19$antipt, ref = "Resto")



## AUTOLOCALIZAÇÃO IDEOLÓGICA:
### Q18: [ideo]
# 00 - Esquerda
# 10 - Direita

# 0 ---------------- 10 

###[ideo2]
# [0, 1] -> Esquerda
# [2, 3, 4, 5, 6, 7, 8] -> Centro
# [9, 10] -> Direita

e19 <- e19 %>%
  mutate(ideo2 = case_when(
    q18 <= 1 ~ as.character("Esquerda"),
    q18 > 1 & q18 < 9 ~ as.character("Centro"),
    q18 == 9 ~ as.character("Direita"),
    q18 == 10 ~ as.character("Direita"),
    TRUE ~ as.character(NA)
  ))

e19$ideo2 <- factor(e19$ideo2, ordered = F)
e19$ideo2.2 <- relevel(e19$ideo2, ref = "Centro") # utilizaremos essa (com "Centro" como referência)


## COrrupção:
### P12. Você diria que a corrupção no Brasil é um problema muito sério, sério, pouco sério ou 
# não é um problema sério:
#     1 - Muito sério; 
#     4 - Não é um problema sério; 
#     8 e 9 - NA's
## Nova codificação:
#     1 - Não é um problema sério; 
#     4 - Muito Sério;

lbs <- c("Não é um Problema Sério", "Pouco Sério",
         "Sério",
         "Muito Sério")

e19 <- e19 %>%
  mutate(corrup1 = case_when(
    p12 == 8 ~ as.numeric(NA),
    p12 == 9 ~ as.numeric(NA),
    TRUE ~ as.numeric(5 - p12)
  )) %>%
  set_labels(corrup1, labels = lbs)


## Renda:
### D9b_faixa_rendaf: [fx_renda]

labels <- c("Até R$ 954,00 (até 1 salário mínimo)",
            "Mais de R$ 954,00 até R$ 1.908,00 (mais de 1 até 2 salários mínimos)", 
            "Mais de R$ 1.908,00 até R$ 4.770,00 (mais de 2 até 5 salários mínimos)",
            "Mais de R$ 4.770,00 até R$ 9.540,00 (mais de 5 até 10 salários mínimos)",
            "Mais de R$ 9.540,00 até R$ 14.310,00 (mais de 10 até 15 salários mínimos)",
            "Mais de R$ 14.310,00 até R$ 19.080,00 (mais de 15 até 20 salários mínimos)",
            "Mais de R$ 19.080,00 (mais de 20 salários mínimos)")

e19$d9b_faixa_rendaf

e19 <- e19 %>%
  mutate(fx_renda = case_when(
    d9b_faixa_rendaf == 98 ~ as.numeric(NA),
    d9b_faixa_rendaf == 99 ~ as.numeric(NA),
    TRUE ~ as.numeric(d9b_faixa_rendaf)
  )) %>%
  set_labels(fx_renda, labels = labels)



################################

# Variável Independente
## VOTO EM BOLSONARO (1o TURNO)
### Q12P1_B : [voto_X]
#### [voto_b] -> Bolsonaro (Q12P1_B == 9)
  ## Outros: Outros Candidatos + Nulo e Branco;

e19 <- e19 %>% 
  mutate(voto_b = case_when(
    q12p1_b == 9 ~ as.character("Bolsonaro"),
    q12p1_b >= 97 ~ as.character(NA),
    TRUE ~ as.character("Outros")
  ))

e19$voto_b <- factor(e19$voto_b, ordered = F)
e19$voto_b <- relevel(e19$voto_b, ref = "Outros")



################################

### Atitudes Populistas

# NORMALIZANDO ENTRE 0 E 1 E MULTIPLICANDO:
## Criando a função para normalizar (0 a 1):
range01 <- function(x){
  (x-min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T))
}


# Criando a variável
## Normalizando (entre 0 e 1) e depois multiplicando:
e19 <- e19 %>%
  mutate(M1_n = range01(M1),
         AE1_n = range01(AE1),
         PC1_n = range01(PC1),
         PC2_n = range01(PC2)) %>%
  mutate(pop = M1_n * AE1_n * PC1_n)

table(e19$pop)


###
# Transformando a variável em duas categorias:
  # Populistas: Minimo de 0.75 em TODAS as dimensões;
  # Não Populistas: O resto;

e19 <- e19 %>%
  mutate(pop_2c = case_when(
    pop == 0.421875 ~ as.numeric(1),
    pop == 0.5625 ~ as.numeric(1),
    pop == 0.75 ~ as.numeric(1),
    pop == 1 ~ as.numeric(1),
    pop >= 0 & pop <= 0.375 ~ as.numeric(0),
    pop == 0.5 ~ as.numeric(0),
    TRUE ~ as.numeric(NA)
  )) %>%
  mutate(pop_2c = as.factor(pop_2c))

###
# Criando a Tipologia:
  # Populistas de Esquerda;
  # Populistas de Direita;
  # Populistas de Centro;
  # Não Populistas;

# Matendo o ponto de corte como sendo 0.75 (minimo em todas as dimensões)
## As pessoas com minimo de 0.75 em todas as dimensões:
## 0.421875
## 0.5625
## 0.75

e19 <- e19 %>% 
  mutate(pop_ed_sart = case_when(
    ideo2.2 == "Direita" & pop == 0.421875 ~ as.character("Populista de Direita"),
    ideo2.2 == "Direita" & pop == 0.5625 ~ as.character("Populista de Direita"),
    ideo2.2 == "Direita" & pop == 0.75 ~ as.character("Populista de Direita"),
    ideo2.2 == "Direita" & pop == 1 ~ as.character("Populista de Direita"),
    ideo2.2 == "Esquerda" & pop == 0.421875 ~ as.character("Populista de Esquerda"),
    ideo2.2 == "Esquerda" & pop == 0.5625 ~ as.character("Populista de Esquerda"),
    ideo2.2 == "Esquerda" & pop == 0.75 ~ as.character("Populista de Esquerda"),
    ideo2.2 == "Esquerda" & pop == 1 ~ as.character("Populista de Esquerda"),
    ideo2.2 == "Centro" & pop == 0.421875 ~ as.character("Populista de Centro"),
    ideo2.2 == "Centro" & pop == 0.5625 ~ as.character("Populista de Centro"),
    ideo2.2 == "Centro" & pop == 0.75 ~ as.character("Populista de Centro"),
    ideo2.2 == "Centro" & pop == 1 ~ as.character("Populista de Centro"),
    ideo2.2 == "Direita" & pop < 1 ~ as.character("Não Populista"),
    ideo2.2 == "Esquerda" & pop < 1 ~ as.character("Não Populista"),
    ideo2.2 == "Centro" & pop < 1 ~ as.character("Não Populista"),
    TRUE ~ as.character(NA)
  )) %>%
  mutate(pop_ed_sart = as.factor(pop_ed_sart))


###
# Abordagem Goertziana (Minimo)
e19 <- e19 %>%
  rowwise() %>%
  mutate(pop_gz = min(c(M1, AE1, PC1))) %>%
  ungroup()


###
# Additive Approach
  # Using Normalized variables and Normalizing the Composite Variable
e19 <- e19 %>%
  mutate(pop_ad = M1_n + AE1_n + PC1_n) %>%
  mutate(pop_ad_n = round(range01(pop_ad),2)) %>%
  mutate(pop_ad_n = as.numeric(pop_ad_n))


###
# Filtrando apenas os eleitores que se autolocalização na direita:
  # Escala de autolocalização: 1-5-10
    # Direita: A partir do meio - 6 > 10;

e19_dir <- e19 %>%
  filter(q18 > 5) %>%
  mutate(ideo = case_when(
    q18 == 95 ~ as.numeric(NA),
    q18 == 97 ~ as.numeric(NA),
    q18 == 98 ~ as.numeric(NA),
    TRUE ~ as.numeric(q18)
  ))



