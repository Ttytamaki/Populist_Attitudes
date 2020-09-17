# ---
# 04 - To do: 1a Versão - Bruno (17/09)
# Tratamento do Banco de Dados e Criação da Variável: Atitudes Populistas
# Atitudes Populistas e Voto em Bolsonaro em 2018 (2020)
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 16/09/2020
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
# 1) Table with the four items tested + EFA + Model Fit

## Items Tested:
  # M1
  # AE1
  # PC1
  # PC2

i4 <- e19 %>%
  select(M1, AE1, PC1, PC2)


for (i in 1:length(i4)){
  nomes <- paste("Var", i, sep = "")
  assign(nomes, group_by(i4, i4[i]) %>%
           summarize(Contagem = n()))
}

tabl_4var <- left_join(Var1, Var2, by = c("M1" = "AE1")) %>%
  left_join(Var3, by = c("M1" = "PC1")) %>%
  left_join(Var4, by = c("M1" = "PC2")) 

names(tabl_4var) <- c("Valor", "M1", "AE1", "PC1", "PC2")


###
# 2) Análise Fatorial:
  ## Correlação Policórica (Itens na escala Likert)

## Selecionando as Variáveis que usaremos na EFA:
pop_var <- e19 %>%
  select(M1, AE1, AE2, AE3, AE4, PC1, PC2) %>%

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
fa <- fa(r = polyc$correlations, nfactors = 2, rotate = "oblimin")
print(fa$loadings, cutoff = 0.35)

## 2) Valores do pacote "Psych":
fa2 <- fa(r = poly_values$rho, nfactors = 2, rotate = "oblimin")
print(fa2$loadings, cutoff = 0.35)

## Rotação Oblimin: Utilizamos uma rotação Oblíqua, pois permitimos (e aceitamos)
# que os fatores estejam correlacionados


## KMO:
KMO(polyc$correlations) #Polyc
KMO(poly_values$rho) #Psych

