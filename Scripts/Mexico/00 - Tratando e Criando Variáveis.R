# ---
# 00 - Tratando e Criando as Variáveis
# Tratamento do Banco de Dados e Criação da Variável: Atitudes Populistas
# ECPR: Brazil, Mexico, and Chile
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 08/04/2021
# ---


## PREAMBULO -------------------------------------------------------------------

# Limpando o environment:
rm(list = ls())

# Carregando os pacotes:
pacman::p_load(tidyverse, haven, sjlabelled, polycor, psych, tikzDevice, stargazer)
options(tikzDocumentDeclaration = "\\documentclass[12pt]{article}")
# Tikz, mesmo não trabalhando com LaTeX


##########
# MEXICO #
##########


# Abrindo o banco:
mx18 <- read_sav("D:\\Dudu\\Pesquisas\\Banco de Dados\\CSES\\5a Onda\\Mexico\\BD CSES_2018.sav")


## Antes de começarmos:
# Verificar como estão os nomes das variáveis:
names(mx18)
# Mudar para letras minusculas para facilitar:
names(mx18) <- tolower(names(mx18))
names(mx18)

dim(mx18)
# 2600 x 185


# Por segurança:
mx <- mx18


###---------------------

##
# Quais Variáveis serão usadas:
#
# Variáveis com prefixo "apop_"
#
# Dimensões:
#
## Maniqueísmo:
#
# apop_1_o3 (M1):
# Lo que la gente llama compromiso en política es en realidad vender los principios
# 
###
## Anti-Elitismo:
#
# apop_2_o3 (AE1):
# A la mayoría de los políticos no les importa la gente
#
# apop_4_o3 (AE2):
# La mayoría de los políticos son confiables
#
# apop_5_o3 (AE3):
# Los políticos son el principal problema en México 
#
# apop_11_o3 (AE4):
# A la mayoría de los políticos sólo le importa los intereses de los ricos y los poderosos 
#
###
## People-Centrism:
#
# apop_7_o3 (PC1):
#  La gente (el pueblo), y no los políticos, es quien debe tomar las decisiones más importantes sobre lo que se debe hacer
#
# apop_13_o3 (PC2):
# La voluntad de la mayoría debe prevalecer siempre, incluso sobre los derechos de las minorías como los indígenas 
##


###---------------------


# Atitudes Populistas:

ap <- list(mx$apop_1_o3, mx$apop_2_o3, mx$apop_4_o3, mx$apop_5_o3, mx$apop_11_o3, 
        mx$apop_7_o3, mx$apop_13_o3)

lapply(X = ap, class)


## 1a Dimensão - Maniqueísmo:

## M1:
# apop_1_o3
# Lo que la gente llama compromiso en política es en realidad vender los principios
### Var. Original:
## 1 - Totalmente de Acuerdo
## 5 - Muy em Desacuerdo

# summary(mx$apop_1_o3)
# table(mx$apop_1_o3)
# var_labels(mx$apop_1_o3)


# Definindo Labels:
lbs <- c("Discorda Muito",
         "Discorda",
         "Nem concorda nem discorda",
         "Concorda",
         "Concorda Muito")

mx <- mx %>%
  mutate(m1 = case_when(
    apop_1_o3 %in% c(8, 9) ~ NA_real_,
    T ~ as.numeric(6 - apop_1_o3)
  )) %>%
  set_labels(m1, labels = lbs)



## 2a Dimensão - Anti-Elitismo:

## AE1:
# apop_2_o3
# A la mayoría de los políticos no les importa la gente
### Var. Original:
## 1 - Totalmente de Acuerdo
## 5 - Muy em Desacuerdo

# summary(mx$apop_2_o3)
# table(mx$apop_2_o3)
# var_labels(mx$apop_2_o3)


# Definindo Labels:
lbs <- c("Discorda Muito",
         "Discorda",
         "Nem concorda nem discorda",
         "Concorda",
         "Concorda Muito")

mx <- mx %>%
  mutate(ae1 = case_when(
    apop_2_o3 %in% c(8, 9) ~ NA_real_,
    T ~ as.numeric(6 - apop_2_o3)
  )) %>%
  set_labels(ae1, labels = lbs)


## AE2:
# apop_4_o3
# La mayoría de los políticos son confiables
### Var. Original:
## 1 - Totalmente de Acuerdo
## 5 - Muy en Desacuerdo

# summary(mx$apop_4_o3)
# table(mx$apop_4_o3)
# var_labels(mx$apop_4_o3)


lbs <- c("Concorda Muito",
         "Concorda",
         "Nem concorda nem discorda",
         "Discorda",
         "Discorda Muito")

mx <- mx %>%
  mutate(ae2 = case_when(
    apop_4_o3 %in% c(8, 9) ~ NA_real_,
    T ~ as.numeric(apop_4_o3)
  )) %>%
  set_labels(ae2, labels = lbs)


## AE3:
# apop_5_o3
# Los políticos son el principal problema en México
### Var. Original:
## 1 - Totalmente de Acuerdo
## 5 - Muy en Desacuerdo


# summary(mx$apop_5_o3)
# table(mx$apop_5_o3)
# var_labels(mx$apop_5_o3)


lbs <- c("Discorda Muito",
         "Discorda",
         "Nem concorda nem discorda",
         "Concorda",
         "Concorda Muito")

mx <- mx %>%
  mutate(ae3 = case_when(
    apop_5_o3 %in% c(8, 9) ~ NA_real_,
    T ~ as.numeric(6 - apop_5_o3)
  )) %>% 
  set_labels(ae3, labels = lbs)


## AE4:
# apop_11_o3:
# A la mayoría de los políticos sólo le importa los intereses de los ricos y los poderosos 
### Var. Original:
## 1 - Totalmente de Acuerdo
## 5 - Muy en Desacuerdo

# summary(mx$apop_11_o3)
# table(mx$apop_11_o3)
# var_labels(mx$apop_11_o3)

lbs <- c("Discorda Muito",
         "Discorda",
         "Nem concorda nem discorda",
         "Concorda",
         "Concorda Muito")

mx <- mx %>%
  mutate(ae4 = case_when(
    apop_11_o3 %in% c(8, 9) ~ NA_real_,
    T ~ as.numeric(6 - apop_11_o3)
  )) %>%
  set_labels(ae4, labels = lbs)



## 3a Dimensão - People-Centrism:

## PC1:
# apop_7_o3
#  La gente (el pueblo), y no los políticos, es quien debe tomar las decisiones más importantes sobre lo que se debe hacer
### Var. Original:
## 1 - Totalmente de Acuerdo
## 5 - Muy en Desacuerdo

# summary(mx$apop_7_o3)
# table(mx$apop_7_o3)
# var_labels(mx$apop_7_o3)

lbs <- c("Discorda Muito",
         "Discorda",
         "Nem concorda nem discorda",
         "Concorda",
         "Concorda Muito")


mx <- mx %>%
  mutate(pc1 = case_when(
    apop_7_o3 %in% c(8, 9) ~ NA_real_,
    T ~ as.numeric(6 - apop_7_o3)
  )) %>%
  set_labels(pc1, labels = lbs)


## PC2:
# apop_13_o3
# La voluntad de la mayoría debe prevalecer siempre, incluso sobre los derechos de las minorías 
# como los indígenas 
### Var. Original:
## 1 - Totalmente de Acuerdo
## 5 - Muy en Desacuerdo

# summary(mx$apop_13_o3)
# table(mx$apop_13_o3)
# var_labels(mx$apop_13_o3)

lbs <- c("Discorda Muito",
         "Discorda",
         "Nem concorda nem discorda",
         "Concorda",
         "Concorda Muito")


mx <- mx %>%
  mutate(pc2 = case_when(
    apop_13_o3 %in% c(8, 9) ~ NA_real_,
    T ~ as.numeric(6 - apop_13_o3)
  )) %>%
  set_labels(pc2, labels = lbs)




# Salvando os dados:
# save(mx, file = "mx.RData")



###---------------------

# Análise Fatorial Exploratória

## PC2 não é parte da bateria de CSES de populismo (mas de autoritarismo), então devemos manter 
## e não considera-la

# Selecionando apenas as variáveis M1, AE1:AE4, PC1
pop_var <- mx %>%
  select(m1, ae1, ae2, ae3, ae4, pc1)

## Usando o Pacote Psych, vamos fazer uma Análise Fatorial Exploratória:
# 1) Teste de Scree
scree(pop_var, main = "Scree Plot for Populist Attitudes Items - Mexico")


# Mesmo não usando LaTeX nessa versão, bom ter guardado:
tikz(file = "Fig_Tables/scree_plot.tex", width = 6, height = 5)
scree(pop_var, main = "Scree Plot for Populist Attitudes Items - Mexico")
dev.off()


# 2) Parallel Analysis Criterion:
parallel <- fa.parallel(pop_var, fm = "minres", fa = "fa")
print(parallel)


# EFA
poly_values <- polychoric(pop_var) # Matriz Correlação Polícórica

fa <- fa(r = poly_values$rho, nfactors = 1, rotate = "oblimin")
fa

## Dos três itens de anti-elitismo, 2 tiveram o maior loading (AE1 e AE4), para questões de 
## comparação, iremos manter o AE1 (igual no caso Brasileiro);

M1 <- c("What people call compromise in politics is really just selling out on one's principles.")
AE1 <- c(" Most politicians do not care about the people.")
AE2 <- c("Most politicians are trustworthy.")
AE3 <- c("Politicians are the main problem in Brazil.")
AE4 <- c("Most politicians care only about the interests of the rich and powerful.")
PC1 <- c("The people, and not politicians, should make our most important policy decisions.")

PC2 <- c("The will of the majority should always prevail, even over the 
         rights of minorities.")



descs <- mx %>%
  select(., m1, ae1, ae2, ae3, ae4, pc1) %>%
  na.omit() %>%
  summarise(across(everything(), 
                   list(mean = mean))) %>%
  t()

df_desc <- data.frame(
  Item = rbind(M1, AE1, AE2, AE3, AE4, PC1),
  Mean = descs,
  Loading = as.vector(fa$loadings)
)

stargazer(df_desc, summary = F, digits = 2,
          title = 'Populist Attitudes Items, Means, and Factor Loadings', label = "tab:efa",
          header = F, type = "html", out = "texte.html")

###---------------------





######
######
# Criando a Variável de Atitudes Populistas:
## Multiplicativa:
range01 <- function(x){
  (x-min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T))
}


mx <- mx %>%
  mutate(m1_n = range01(m1),
         ae1_n = range01(ae1),
         pc1_n = range01(pc1)) %>%
  mutate(pop = m1_n * ae1_n * pc1_n)

mx <- mx %>%
  mutate(pop_2c = case_when(
    pop == 0.421875 ~ as.numeric(1),
    pop == 0.5625 ~ as.numeric(1),
    pop == 0.75 ~ as.numeric(1),
    pop == 1 ~ as.numeric(1),
    pop >= 0 & pop <= 0.375 ~ as.numeric(0),
    pop == 0.5 ~ as.numeric(0),
    TRUE ~ as.numeric(NA)
  ))


# Aditiva:
mx <- mx %>%
  mutate(pop_add = rowMeans(select(., m1, ae1, pc1), na.rm=T))





######
######
# Variáveis Controle e Variável Dependente

### Variável Dependente:
## Voto:
# pelepre_o3
mx <- mx %>%
  mutate(voto_amlo = case_when(
    pelepre_o3 == 3 ~ "Obrador",
    pelepre_o3 %in% c(996, 997, 999) ~ NA_character_,
    is.na(pelepre_o3) ~ NA_character_,
    T ~ "Outros"
  )) %>%
  mutate(voto_amlo = relevel(factor(voto_amlo), ref = "Outros"))




### Variáveis Independente:
## Ideologia:
# p77_o3
mx <- mx %>%
  mutate(ideo = case_when(
    as.numeric(p77_o3) > 10 ~ NA_real_,
    T ~ as.numeric(p77_o3))
  )


############################################
## Sexo     |     Educação     |     Idade   
# s2_o3     |       s3_o3      |     s1_o3
############################################

# Sexo - Ref.: Ser Mulher (Medir se Ser Homem aumenta)

mx <- mx %>%
  mutate(sexo = case_when(
    s2_o3 == 1 ~ 1,
    s2_o3 == 2 ~ 0,
    T ~ as.numeric(s2_o3)
  )) %>%
  mutate(ed = case_when(
    s3_o3 > 10 ~ NA_real_,
    T ~ as.numeric(s3_o3)
    )) %>%
  mutate(id = s1_o3)


## Religião
# s14_o3
# ¿Con qué frecuencia asiste usted a la iglesia o al templo sin contar ocasiones especiales 
# como bodas y funerales? (S13)
## Principalmente sobre Domínguez, 2020,
## Esperamos que os católicos tenham votado mais em AMLO do que os Evangélicos e Protestantes


mx <- mx %>% 
  mutate(relig = case_when(
    s14_o3 == 1 ~ "Católico",
    s14_o3 %in% c(97, 99) ~ NA_character_,
    is.na(s14_o3) ~ NA_character_,
    T ~ "Não Católico"
  )) %>%
  mutate(relig = relevel(factor(relig), ref = "Não Católico"))


# Não acho que dê para usarmos essa variável.



### Ver o Artigo de Rodrigo Castro Cornejo ###


## Corrupção
# p71_o3
# Que tan extendida cree usted que está la corrupción en México:
### Var. Original:
## 1 - Muy Estendida
## 2 - Algo Extendida
## 3 - No Muy Extendida
## 4 - Casi no Ocurre
## 8 y 9 - NA

lbs <- c("Quase não Acontece",
         "Não muito Extensa",
         "Um pouco Extensa",
         "Muito Extensa")


mx <- mx %>%
  mutate(corrup = case_when(
    p71_o3 >= 8 ~ NA_real_,
    T ~ as.numeric(5 - p71_o3)
  )) %>%
  set_labels(corrup, labels = lbs)



## Faixa de Renda
# s10_1_o3
mx <- mx %>%
  mutate(fx_renda = case_when(
    s10_1_o3 == 9 ~ NA_real_,
    T ~ as.numeric(s10_1_o3)
  )) 



## Partisanship
# Seguindo Cornejo, Ley e Beltran (2020): Partisanship moderates the relation Populist Attitudes ->
# Voting Behavior
# p10_o3:
# Independientemente del partido por el que usted vota, ¿usted normalmente se considera panista,
# priísta, perredista, de Morena o de otro partido? (P5)
## Ideia:
# Morena e Independente x Resto

mx <- mx %>%
  mutate(party_id = case_when(
    p10_o3 %in% c(1:6, 96) ~ "Outros",
    p10_o3 %in% c(9:10, 97) ~ "Morena e Independente",
    is.na(p10_o3) ~ NA_character_,
    T ~ NA_character_
  )) %>%
  mutate(party_id = relevel(factor(party_id), ref = "Outros"))




# Salvando os dados:
save(mx, file = "mx.RData")

#####################



#########
# Chile #
#########




