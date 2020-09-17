<<<<<<< HEAD
# ---
# Tratamento do Banco de Dados e Criação da Variável: Atitudes Populistas
# Atitudes Populistas e Voto em Bolsonaro em 2018 (2020)
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 14/08/2020
# ---


## PREAMBULO -------------------------------------------------------------------


# Função para trazer quais colunas possuem algum valor NA e a quantidade deles:


## Carregando o pacote "purrr"
library(purrr)


check_NA = function(x) {
  for (i in 1:length(x)){
    if(map(x[i], ~sum(is.na(.))) > 0) {
      return (map(x[i], ~sum(is.na(.))))
      
    }
    
  }
  
}
=======
# ---
# Tratamento do Banco de Dados e Criação da Variável: Atitudes Populistas
# Atitudes Populistas e Voto em Bolsonaro em 2018 (2020)
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 14/08/2020
# ---


## PREAMBULO -------------------------------------------------------------------


# Função para trazer quais colunas possuem algum valor NA e a quantidade deles:


## Carregando o pacote "purrr"
library(purrr)


check_NA = function(x) {
  for (i in 1:length(x)){
    if(map(x[i], ~sum(is.na(.))) > 0) {
      return (map(x[i], ~sum(is.na(.))))
      
    }
    
  }
  
}
>>>>>>> 48d1554cb387271c0416869ba405be0c6ad8a4f1
