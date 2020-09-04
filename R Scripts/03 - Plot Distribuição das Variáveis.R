# ---
# Gráficos de Distribuição das Variáveis: Atitudes Populistas
# Atitudes Populistas e Voto em Bolsonaro em 2018 (2020)
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 22/08/2020
# ---


## PREAMBULO -------------------------------------------------------------------

library(tidyverse)
library(haven)
library(here)

# Carregando o script com o tratamento das variáveis
source(here::here("00 - Tratamento das Variáveis.R"), encoding = "UTF-8")


# Banco de dados: e19

dim_pop <- e19 %>%
  select(M1_n, AE1_n, PC1_n, pop, pop_3n, pop_2c) %>%
  na.exclude()

### 
# Comparando a distribuição da variável original X com três escalas:

### 1) Original
  # Plotando o gráfico para ver a distribuição da variável de atitudes populistas:
  
  # Antes, uma tabela de porcentagem:
prop.table(table(e19$pop))*100

# 0 - 25.02%
# 0.01 - 1.03%
# 0.03 - 0.21%
# 0.04 - 1.11%
# 0.06 - 2.10%
# 0.09 - 0.68%
# 0.12 - 0.94%
# 0.14 - 1.80%
# 0.18 - 5.20%
# 0.25 - 6.06%
# 0.28 - 1.07%
# 0.37 - 0.84%
# 0.42 - 2.02%
# 0.5 - 3.01%
# 0.56 - 6.79%
# 0.75 - 14.66%
# 1 - 26.40%

  # Plotando com geom_bar():
    # Criando "teste" para que não haja nenhum problema com o 
    # db original caso optemos por fazer mais alterações
# teste <- e19 %>% 
#   dplyr::select(pop, pop_3n)
# 
# teste <- na.exclude(teste) # evitar que NA's sejam incluidos 
# 
#   # Criando uma matriz que representa a tabela (com contagem) de pop
# pop <- apply(teste[c("pop")], 2, table)
# 
#   # Pegando apenas os valores, que serão nossos "nomes":
# t <- unique(teste$pop)
# t <- round((sort(t)), 2)
# 
#   # pop_3 tem como "nome das linhas" os valores (0, 1 e 2). Vamos agora deletar esses nomes:
# rownames(pop) <- NULL
# 
#   # Agora vamos criar um df com os valores de pop_3 e com os números de t_3
# pop <- cbind(pop,t)
# 
#   # O df veio na ordem invertida, então iremos trocar a ordem:
# pop <- pop[, c(2, 1)]
# 
#   # Atribuiremos um "nome" para as linhas, que será a contagem simples:
# rownames(pop) <- seq(1, 17)
# colnames(pop) <- c("Pop", "Cont")
# 
#   # Transformando a variável "valor" (0, 1 e 2) em categórica para que possamos usar um barplot:
# pop$Valor <- as.character(pop$Pop)
# 
#   # Transformando em um DF:
# pop <- as.data.frame(pop)
# 
#   # Criando o plot da variável "atitudes populistas":
# plot_pop1 <- ggplot(data=pop, aes(x=Cont, y=Pop)) +
#   geom_bar(stat="identity", position='dodge') +
#   geom_text(aes(label = Cont),
#             position = position_dodge(width = 0.9), size = 4,
#             hjust = -0.1)+
#   labs(caption = "Escala feita através da normalização (0 a 1) e multiplicação",
#        y = "Contagem",
#        x = "Nivel de Populismo",
#        fill = "Escalas de Populismo") + 
#   theme_minimal()

###
# Outra forma de Plotar o Gráfico:

df_pop_or <- dim_pop %>%
  group_by(pop) %>%
  summarize(counts = n())

df_pop_or$percents <- (df_pop_or$counts / (sum(df_pop_or$counts)))*100


plt_pop_t <- ggplot(data = df_pop_or, aes(x = as.factor(pop), y = counts,
                                       fill = as.factor(pop))) + 
  geom_bar(color = "black", stat = "identity", width = 1) +
  scale_x_discrete(breaks = df_pop_or$pop,
                   labels = df_pop_or$pop) + 
  theme(axis.text.x = element_text(face="bold")) + 
  #ggtitle("Distribuição da Escala de Populismo em 4 Níveis (0, 1, 2 e 3)") +
  xlab("Atitudes Populistas") +
  ylab("Contagem") +  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  coord_flip() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  geom_text(aes(label = counts), position = position_identity(), hjust = -.1, 
            size = 4) +
  theme(legend.position = "none")
plt_pop_t




###
# Variável Atitudes Populistas: Três escalas
# Selecionando a variável e criando uma coluna com a soma das ocorrências:
df_pop_3n <- dim_pop %>%
  group_by(pop_3n) %>%
  summarize(counts = n())

# Criando o plot da variável "atitudes populistas":
plot_pop2 <- ggplot(data=df_pop_3n, aes(x=pop_3n, y=counts)) +
  geom_bar(stat="identity", position='dodge') +
  geom_text(aes(label = pop_3n),
            position = position_dodge(width = 0.9), size = 4,
            hjust = -0.1)+
  labs(caption = "Escala feita através da normalização (0 a 1) e multiplicação",
       y = "Contagem",
       x = "Nivel de Populismo",
       fill = "Escalas de Populismo") + 
  theme_minimal()

plot_pop2



###
# Variável Atitudes Populistas: 4 Níveis:

# Selecionando a variável e criando uma coluna com a soma das ocorrências:
df_pop4n <- e19 %>%
  group_by(pop_4n) %>%
  summarize(counts = n()) %>%
  na.exclude()

# Criando os Labels para o plot:
lbs <- c("Não Populista", "Somewhat Populista", "Populista", "Populista Puro")

# Plotando a Distribuição da Variável:
plt_pop_4n <- ggplot(data = df_pop4n, aes(x = as.factor(pop_4n), y = counts,
                                          fill = as.factor(pop_4n))) + 
  geom_bar(color = "black", stat = "identity", width = .8) +
  scale_fill_brewer(palette = "PuRd") + 
  scale_x_discrete(breaks = df_pop4n$pop_4n,
                   labels = lbs) + 
  theme(axis.text.x = element_text(face="bold")) + 
  ggtitle("Distribuição da Escala de Populismo em 4 Níveis (0, 1, 2 e 3)") +
  xlab("Atitudes Populistas") +
  ylab("Contagem") + 
  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  geom_text(aes(label = counts), vjust = 0, size = 5) +
  labs(fill = "Populismo (4 Níveis)")


# Verificar o gráfico:
plt_pop_4n

# Verficiar a distribuição:
df_pop4n

prop.table(table(e19$pop_4n))
# 0 - 25%
# 1 - 25%
# 2 - 23%
# 3 - 26%



###
# Variável Populismo com 2 categorias: Ser ou Não Populista

# Selecionando a variável e criando uma coluna com a soma das ocorrências:
df_pop_2c <- e19 %>%
  group_by(pop_2c) %>%
  summarize(counts = n()) %>%
  na.exclude()

df_pop_2c$percent <- (df_pop_2c$counts / (sum(df_pop_2c$counts)))*100

# Criando os Labels para o plot:
lbs <- c("Não Populista", "Populista")

# Plotando a Distribuição da Variável:
plt_pop_2c <- ggplot(data = df_pop_2c, aes(x = as.factor(pop_2c), y = counts,
                                           fill = as.factor(pop_2c))) + 
  geom_bar(color = "black", stat = "identity", width = .6) +
  scale_fill_brewer(palette = "PuRd", 
                    labels = c("Não Populista", "Populista")) + 
  scale_x_discrete(breaks = df_pop_2c$pop_2c,
                   labels = lbs) + 
  theme(axis.text.x = element_text(face="bold")) + 
  #ggtitle("Distribuição da Escala de Populismo em 2 Categorias") +
  xlab("Atitudes Populistas") +
  ylab("Contagem") + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  geom_text(aes(label = counts), vjust = 0, size = 5) +
  labs(fill = "Populismo") + 
  theme(legend.position = "none")  

# Verificar o gráfico:
plt_pop_2c

# Verficiar a distribuição:
df_pop_2c

prop.table(table(e19$pop_2c))
# 0 - 50%
# 1 - 49%


