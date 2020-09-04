library(tidyverse)
library(haven)
library(here)
library(polycor)
library(psych)
library(reshape2)

source(here::here("Tratamento das Variáveis.R"), encoding = "UTF-8")

pop_var <- e19 %>%
  select(M1, AE1, AE2, AE3, AE4, PC1, PC2)


# Seguiremos com uma Análise Fatorial Exploratória:
# Como as variáveis se encontram na escala likert, utilizaremos uma correlação policórica;

# Há duas maneiras de pegar a matriz de correlação policórica:
## 1) pacote "polycor"
## 2) pacote "psych"

### 1) pacote "polycor": 
# Aqui, antes de seguirmos em frente, precisamos converter as variáveis para fator;
# assim a função irá pegar a correlação policórica;

pop_var_fac <- pop_var %>% 
  mutate_each(funs(factor))

polyc <- hetcor(pop_var_fac, ML = TRUE) # Policórica, necessário que as variáveis sejam fact

valores <- polyc$correlations

values <- round(polyc$correlations, 2)

  # ML = Maximum-Likelihood

### 2) pacote "psych":
poly_values <- polychoric(pop_var)

valores2 <- poly_values$rho

values2 <- round(poly_values$rho, 2)




# Para comparar os valores das correlações retirados de ambos os métodos vamos fazer
# um heatmap das matrizes de correlação:

source(here::here("functions", "triangulo.R"), encoding = "UTF-8")

# Transformar a matriz em só "superior" ou "inferior":
upper_tri <- get_upper_tri(values)
upper_tri2 <- get_upper_tri(values2)



# 1) Pacote "polycor":
# Criar os dados do Heatmap da Matriz de correlação:
melted_cormat <- melt(upper_tri, na.rm = T)


# Plotar o Heatmap:
heatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlação Policórica") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Plotar o Heatmap com os coeficientes nos quadrados:
heatmap <- heatmap +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    plot.margin = unit(c(2, 4, 2, 4), "cm"), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 6, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

print(heatmap)

heatmap + ggsave(here::here("anexos", "Cor. Policórica Polycor.jpeg"),
        height = 6, width = 10, dpi=300)


# 2) Pacote "psych":
# Criar os dados do Heatmap da Matriz de correlação:
melted_cormat2 <- melt(upper_tri2, na.rm = T)


# Plotar o Heatmap:
heatmap2 <- ggplot(data = melted_cormat2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlação Policórica") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Plotar o Heatmap com os coeficientes nos quadrados:
heatmap2 <- heatmap2 +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    plot.margin = unit(c(2, 4, 2, 4), "cm"), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 6, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

print(heatmap2)

heatmap2 + ggsave(here::here("anexos", "Cor. Policórica Psych.jpeg"),
       height = 6, width = 10, dpi=300)


## Há uma minuscula diferença entre as correlações pelos dois métodos, sendo assim não importa
# qual dos dois é o método adotado.


# No entanto, para utilizarmos o scree test e o parallel test, não precisamos
  # da variávl categórica:

scree(pop_var)
parallel <- fa.parallel(pop_var, fm = 'minres', fa = 'fa')
print(parallel)

## Ambos apontam para 2 fatores;


################################

# Realizando a Análise Fatorial Exploratória com correlação policórica:

## 1) Valores do pacote "Polycor":
faPC <- fa(r = polyc$correlations, nfactors = 2, n.obs = 2248, rotate = "oblimin")
print(faPC$loadings, cutoff = 0.35)

## 2) Valores do pacote "Psych":
faPC2 <- fa(r = poly_values$rho, nfactors = 2, n.obs = 2248, rotate = "oblimin")
print(faPC2$loadings, cutoff = 0.35)

## Rotação Oblimin: Utilizamos uma rotação Oblíqua, pois permitimos (e aceitamos)
# que os fatores estejam correlacionados


## Teste de Barlett:
cortest.bartlett(polyc$correlations)
cortest.bartlett(poly_values$rho)

## KMO:
KMO(polyc$correlations)
KMO(poly_values$rho)


# Vendo o diagrama da análise fatorial:
fa.diagram(faPC)

faPC



# SS <- colSums(faPC$Structure^2)
# # Compute percentage of explained variance by factor
# SS/length(faPC$communality)
# # Total explained variability
# mean(faPC$communality)




###
# Com a Análise Fatorial, escolhemos 3 variáveis para serem mantidas em nossa análise:
#   M1
#   AE1
#   PC1
# As outras demais, com exceção da PC2, foram excluídas;
###

