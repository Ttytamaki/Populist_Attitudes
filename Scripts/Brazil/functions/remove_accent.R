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

# Referência: https://github.com/neylsoncrepalde/big_data_ars

# Remove accent function
# Adapted from https://github.com/harvesthq/chosen/issues/1880


# Função para Remover Acentos, normalizar capitalização:
remove_accent = function(x) {
  if (class(x) != "character") stop("Input is not a character")
  
  r = tolower(x)
  r = gsub("[àáâãäå]", "a", r)
  r = gsub("æ", "ae", r)
  r = gsub("ç", "c", r)
  r = gsub("[èéêë]", "e", r)
  r = gsub("[ìíîï]", "i", r)
  r = gsub("ñ", "n", r)                  
  r = gsub("[òóôõö]", "o", r)
  r = gsub("œ", "oe", r)
  r = gsub("[ùúûü]", "u", r)
  r = gsub("[ýÿ]", "y", r)
  return(r)
}

# Função para Remover as urls
removeURL <- function(x) gsub("(f|ht)tp(s?)://\\S+", "", x)

