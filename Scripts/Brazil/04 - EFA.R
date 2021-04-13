# ---
# 04 - Análise Fatorial Exploratória 
# Tratamento do Banco de Dados e Criação da Variável: Atitudes Populistas
# Atitudes Populistas e Voto em Bolsonaro em 2018 (2020)
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 20/09/2020
# ---


## PREAMBULO -------------------------------------------------------------------

###
# Carregando Pacotes Necessários:
library(tidyverse)
library(haven)
library(here)
library(polycor)
library(psych)


###
# Carregando o script de Tratamento e Criação das Variáveis:
source(here::here("00 - Tratando e Criando as Variaveis.R"), encoding = "UTF-8")


###
# Análise Fatorial:
## Correlação Policórica (Itens na escala Likert)

## Selecionando as Variáveis que usaremos na EFA:
pop_var <- e19 %>%
  select(M1, AE1, AE2, AE3, AE4, PC1, PC2)

## As transformando em fator, para poder usar a função hetcor:
pop_var_fac <- pop_var %>% 
  mutate_each(funs(factor))

polyc <- hetcor(pop_var_fac, ML = TRUE) # Policórica, necessário que as variáveis sejam fact


### Psych
poly_values <- polychoric(pop_var)


scree(pop_var)
parallel <- fa.parallel(pop_var, fm = 'minres', fa = 'fa')
print(parallel)


# Realizando a Análise Fatorial Exploratória com correlação policórica:

## 1) Valores do pacote "Polycor":
fa <- fa(r = polyc$correlations, nfactors = 2, n.obs = 2248, rotate = "oblimin")
print(fa$loadings, cutoff = 0.35)

## 2) Valores do pacote "Psych":
fa2 <- fa(r = poly_values$rho, nfactors = 2, rotate = "oblimin")
print(fa2$loadings, cutoff = 0.35)

## Rotação Oblimin: Utilizamos uma rotação Oblíqua, pois permitimos (e aceitamos)
# que os fatores estejam correlacionados


## KMO:
KMO(polyc$correlations) #Polyc
KMO(poly_values$rho) #Psych





###############

###
# EFA feita apenas com as quatro variáveis:

pop_var2 <- e19 %>%
  select(M1, AE1, PC1, PC2)


## As transformando em fator, para poder usar a função hetcor:
pop_var_fac2 <- pop_var2 %>% 
  mutate_each(funs(factor))

polyc2 <- hetcor(pop_var_fac2, ML = TRUE) # Policórica, necessário que as variáveis sejam fact
polyc2$correlations

### Psych
poly_values2 <- polychoric(pop_var2)
poly_values2$rho


# Realizando a Análise Fatorial Exploratória com correlação policórica:

scree(pop_var2)
parallel <- fa.parallel(pop_var2, fm = 'minres', fa = 'fa')
print(parallel)

## 1) Valores do pacote "Polycor":
fa_2 <- fa(r = polyc2$correlations, nfactors = 1, n.obs = 2248, rotate = "oblimin")
print(fa_2$loadings)

fa_2.2 <- fa(r = polyc2$correlations, nfactors = 2, n.obs = 2248, rotate = "oblimin")
print(fa_2.2$loadings)


## 2) Valores do pacote "Psych":
fa2_2 <- fa(r = poly_values2$rho, nfactors = 2, rotate = "oblimin")
print(fa2_2$loadings, cutoff = 0.3)

## Rotação Oblimin: Utilizamos uma rotação Oblíqua, pois permitimos (e aceitamos)
# que os fatores estejam correlacionados
