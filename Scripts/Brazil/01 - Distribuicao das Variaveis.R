# ---
# 01 - Distribuição das Variáveis
# Plotando a Distribuição das Variáveis
# Atitudes Populistas e Voto em Bolsonaro em 2018 (2020)
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 11/09/2020
# ---


## PREAMBULO -------------------------------------------------------------------

library(tidyverse)
library(here)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)



source(here::here("00 - Tratando e Criando as Variaveis.R"), encoding = "UTF-8")

###
# Distribuição das Atitudes Populistas:

df_pop_or <- e19 %>%
  group_by(pop) %>%
  summarize(counts = n()) %>%
  na.exclude()

# Criando uma Coluna de Porcentagem:
df_pop_or$percents <- (df_pop_or$counts / (sum(df_pop_or$counts)))*100


# Plotando o Gráfico:
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
# Distribuição das Atitudes Populistas Categóricas em SIM x NÃO:

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

plt_pop_2c


###
# Distribuição da TIPOLOGIA:
pop_ed_sart <- e19 %>%
  group_by(pop_ed_sart) %>%
  summarize(cont = n()) %>%
  na.exclude()

# Textos para acrescentar ao gráfico:
capt2 <- paste(c("N Total: 2506", 
                 "NA's Total: 631", "NA's Ideologia: 533",
                 "NA's Populismo: 180", "Há NA's que Coincidem"), collapse = '\n')


# Plotando o Gráfico:
plt_pop_esq_dir_sart2 <- ggplot(data = pop_ed_sart, aes(x = as.factor(pop_ed_sart), y = cont,
                                                        fill = as.factor(pop_ed_sart))) + 
  geom_bar(color = "black", stat = "identity", width = 1) +
  scale_x_discrete(breaks = pop_ed_sart$pop_ed_sart,
                   labels = pop_ed_sart$pop_ed_sart) +
  scale_fill_brewer(palette = "PuRd") +
  theme(axis.text.x = element_text(face="bold")) + 
  #ggtitle("Distribuição da Escala de Populismo em 4 Níveis (0, 1, 2 e 3)") +
  xlab("Atitudes Populistas") +
  ylab("Contagem") +  theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  geom_text(aes(label = cont), position = position_identity(), vjust = -.5, 
            size = 4) +
  theme(legend.position = "none") + 
  annotate(geom = "label", x = 3, y = 900, 
           label = capt2, hjust = 0, vjust = 1, size = 5,
           fill = "#FFFFFF", family = "serif") 

plt_pop_esq_dir_sart2


###
# Distribuição da Variável GOERTZIANA
pop_goertz <- e19 %>%
  select(pop_gz) %>%
  group_by(pop_gz) %>%
  summarize(counts = n())


# Plotando a Distribuição da Variável:
plt_pop_gz <- ggplot(data = pop_goertz, aes(x = as.factor(pop_gz), y = counts,
                                            fill = as.factor(pop_gz))) + 
  geom_bar(color = "black", stat = "identity", width = .6) +
  scale_fill_brewer(palette = "YlOrBr") + 
  theme(axis.text.x = element_text(face="bold")) + 
  #ggtitle("Distribuição da Escala de Populismo em 2 Categorias") +
  xlab("Atitudes Populistas - Abordagem Goertziana") +
  ylab("Contagem") + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  geom_text(aes(label = counts), vjust = 0, size = 5) +
  labs(fill = "Populismo") + 
  theme(legend.position = "none")  

plt_pop_gz


###
# Distribuição da quatro variáveis:
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

tabl_4var2 <- data.frame(tabl_4var)


## Criando uma Função para Plotar Gráficos:

ggplt_f <- function(z){
  ggplot(data = tabl_4var, aes(x = as.factor(Valor), y = z,
                               fill = as.factor(Valor))) + 
    geom_bar(color = "black", stat = "identity", width = .6) + 
    scale_fill_brewer(palette = "YlOrBr") + 
    theme(axis.text.x = element_text(face="bold")) + 
    ggtitle(paste("Plot", i, sep = " ")) +
    xlab(paste("Atitudes Populistas", z, sep = "__")) +
    ylab("Contagem") +  theme_minimal() +
    geom_text(aes(label = z), vjust = 0, size = 4)
}

loop.vector <- 2:5

## Loop para plotar Gráficos de Todas as 4 variáveis:
for (i in 2:5) { # Loop over loop.vector
  
  # store data in column.i as x
  val <- tabl_4var2[,i]
  nomes_plt <- paste("Plt", names(tabl_4var2[i]), sep = "_")
  
  # Plot histogram of x
  assign(nomes_plt, (ggplt_f(val) + ggtitle(paste("Plot", names(tabl_4var2[i]), sep = " ")) +
                       xlab(paste("Variável", names(tabl_4var2[i]), sep = " ")) +
                       theme(legend.position = "none")))
  
}

# Mostrando os Plots das 4 Variáveis:
ggarrange(Plt_AE1, Plt_M1, Plt_PC1, Plt_PC2,
          labels = c(1, 2, 3, 4),
          ncol = 2, nrow = 2)




ggplt_f(val)
