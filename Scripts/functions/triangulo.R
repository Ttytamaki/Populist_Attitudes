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


# Função para pegar o Triângulo inferior e superior da Matriz de Correlação
## Para depois plotar no heatmap:


# Função para pegar o Triangulo inferior da Matriz de correlação:
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Função para pegar o Triangulo superior da matriz de correlação:
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
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


# Função para pegar o Triângulo inferior e superior da Matriz de Correlação
## Para depois plotar no heatmap:


# Função para pegar o Triangulo inferior da Matriz de correlação:
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Função para pegar o Triangulo superior da matriz de correlação:
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
>>>>>>> 48d1554cb387271c0416869ba405be0c6ad8a4f1
