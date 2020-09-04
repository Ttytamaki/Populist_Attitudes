# ---
# Analisando algumas relações e distribuições: Atitudes Populistas
# Atitudes Populistas e Voto em Bolsonaro em 2018 (2020)
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 24/08/2020
# ---


## PREAMBULO -------------------------------------------------------------------

library(tidyverse)
library(here)
library(RColorBrewer)

###
# Carregar o script que trata as variáveis
source(here::here("00 - Tratamento das Variáveis.R"), encoding = "UTF-8")


###
# Distribuição de Populistas Direita x Esquerda
  # Var. Original:

### Variável Atitudes Populistas Original ###

# Porcentagem de pessoas que se identificam nos extremos: (autolocalização ideológica)
## Var.: ideo2.2 (esquerda = 0, 1; direita = 9, 10;)
prop.table(table(e19$ideo2.2))*100

# Cruzando com a variável de populismo:
## Produzindo um gráfico de barras estacado:

## Filtrando o banco de dados, criando um df (teste), para evitar qualquer problema no
  # original:
teste <- e19 %>%
  select(pop, pop_3n, ideo2.2) %>%
  na.exclude()


# Agrupando as ocorrências de atitudes populistas POR ideologia:
## Forma 1: Dplyr
# teste2 <- e19 %>%
#   select(pop, ideo2.2) %>%
#   na.exclude() %>%
#   group_by(ideo2.2, pop) %>%
#   summarize(n = n()) %>%
# 


## Forma 2:
tab_pop_id <- table(teste$pop, teste$ideo2.2)
dt <- data.frame(tab_pop_id) # Criando o banco por agrupamento;
colnames(dt) <- c("Pop", "Ideologia", "Contagem") # Renomeando as colunas;

# Criando um df com o total de ocorrências por ideologias de acordo 
  ## com os níveis de populismo:
total <- aggregate(dt$Contagem, by = list(Category = dt$Pop), FUN = sum) 

# Criando a coluna de porcentagem:
dt$percent <- round(((dt$Contagem / total$x)*100), 0)

labels <- unique(teste$pop) # Criando Labels para usar no Ggplot
labels <- sort(round(labels, 2))

# Plotando o gráfico de Ideologia x Populismo:
## posição = fill -> Preencher a linha toda (%)
plt_id_pop <- ggplot(data = dt, aes(x = Pop, y = Contagem)) +
  geom_bar(aes(fill = Ideologia), stat = "identity", position = "fill")  +
  ggtitle("Atitudes Populistas por Posição Ideológica") +
  scale_fill_manual(values = c("#F0E68C", "#40E0D0", "#FF6347")) +
  coord_flip() + 
  ylab("Porcentagem em Relação aos Níveis de Atitudes Populistas") + 
  xlab("Atitudes Populistas") +
  scale_x_discrete(breaks = unique(dt$Pop),
                   labels = labels) +  
  geom_text(aes(label = (paste0(percent,"%")), group = Ideologia), 
            position=position_fill(vjust = 0.5), size = 4) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid"))

plt_id_pop


### Variável Atitudes Populistas Categóricas (3 níveis) ###

tab_pop3_id <- table(teste$pop_3n, teste$ideo2.2)
dt2 <- data.frame(tab_pop3_id)
colnames(dt2) <- c("Pop", "Ideologia", "Contagem")

# Criando um df com o total de ocorrências por ideologias de acordo
  ## com os níveis de populismo:
total2 <- aggregate(dt2$Contagem, by = list(Pop = dt2$Pop), FUN = sum)

# Criando a coluna de porcentagem:
dt2$percent <- round(((dt2$Contagem / total2$x)* 100), 0)

labels2 <- unique(teste$pop_3n)
labels2 <- sort(round(labels2, 2))

plot_3n <- ggplot(data = dt2, aes(x = Pop, y = Contagem)) +
  geom_bar(aes(fill = Ideologia), stat = "identity", position = "fill")  +
  #ggtitle("Atitudes Populistas por Posição Ideológica") +
  scale_fill_manual(values = c("#F0E68C", "#40E0D0", "#FF6347")) +
  #coord_flip() + 
  ylab("Porcentagem em Relação aos Níveis de Atitudes Populistas") + 
  xlab("Atitudes Populistas") +
  scale_x_discrete(breaks = unique(dt2$Pop),
                   labels = labels2) +  
  geom_text(aes(label = (paste0(percent,"%")), group = Ideologia), 
            position=position_fill(vjust = 0.5), size = 4) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid"))

plot_3n



### Variável de Atitudes Populistas 2 Categorias ###
df_pop_id <- e19 %>%
  select(pop_2c, ideo2.2) %>%
  na.exclude() %>%
  group_by(pop_2c, ideo2.2) %>%
  summarize(counts = n()) 

df_pop_id$percent <- round(((df_pop_id$counts / sum(df_pop_id$counts))*100),2)

plot_2c_id <- ggplot(data = df_pop_id, aes(x = pop_2c, y = counts)) +
  geom_bar(aes(fill = ideo2.2), stat = "identity", position = "fill")  +
 # ggtitle("Atitudes Populistas por Posição Ideológica") +
  scale_fill_manual(values = c("#F0E68C", "#40E0D0", "#FF6347")) +
  #coord_flip() + 
  ylab("Porcentagem em Relação aos Níveis de Atitudes Populistas") + 
  xlab("Atitudes Populistas") +
  scale_x_discrete(breaks = unique(df_pop_id$pop_2c),
                   labels = c(0,1)) +  
  geom_text(aes(label = (paste0(percent,"%")), group = ideo2.2), 
            position=position_fill(vjust = 0.5), size = 4) +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  labs(fill = "Posição Ideológica")


plot_2c_id




###
# Para comparar a pontuação na escala de atitudes populistas com suas posições ideológicas:
# Podemos utilizar o plot [plot];
# podemos utilizar uma tabela cruzada [tab_pop_id];

plot
plot_3n

tab_pop_id


##################


###
# N de Pop. Esquerda e Direita:

## Atitudes Populistas Original ##

# Filtrando apenas os de ideologia de esquerda;
pop_esquerda <- dt %>%
  select(Pop, Ideologia, Contagem) %>%
  filter(Ideologia == "Esquerda")

pop_esquerda_2c <- df_pop_id %>%
  select(pop_2c, ideo2.2, counts) %>%
  filter(ideo2.2 == "Esquerda")


# Filtrando apenas os de ideologia de direita:
pop_direita <- dt %>%
  select(Pop, Ideologia, Contagem) %>%
  filter(Ideologia == "Direita") 


pop_direita_2c <- df_pop_id %>%
  select(pop_2c, ideo2.2, counts) %>%
  filter(ideo2.2 == "Direita")


pop_direita_2c
  # N. Populistas de Direita: (Aqueles com nota 1):
# 347 Pessoas


pop_esquerda_2c
  # N. Populistas de Esquerda: (Aqueles com nota 1):
# 134 Pessoas


# N. de pop esquerda e direita:
n_pop_id <- df_pop_id %>%
  select(pop_2c, ideo2.2, counts) %>%
  filter(ideo2.2 != "Centro", pop_2c == 1)

n_pop_id <- n_pop_id[-1]



# Gráfico para ver os Ns. de Esquerda, Direita e Centro:
plot2 <- ggplot(data = dt, aes(x = Pop, y = Contagem)) +
  geom_bar(aes(fill = Ideologia), stat = "identity", position = "dodge")  +
  ggtitle("Atitudes Populistas por Posição Ideológica") +
  scale_fill_manual(values = c("#F0E68C", "#40E0D0", "#FF6347")) +
  coord_flip() + 
  ylab("Porcentagem em Relação aos Níveis de Atitudes Populistas") + 
  xlab("Atitudes Populistas") +
  scale_x_discrete(breaks = unique(dt$Pop),
                   labels = labels)  + 
  geom_text(aes(label = Contagem), position = position_dodge(width = 1), 
            size = 5) +
  facet_wrap(~Ideologia)

plot2


## Atitudes Populistas 3 Níveis ##

# Filtrando apenas os de ideologia de esquerda:
pop_esquerda_3 <- dt2 %>%
  select(Pop, Ideologia, Contagem) %>%
  filter(Ideologia == "Esquerda")


# Filtrando apenas os de ideologia de direita:
pop_direita_3 <- dt2 %>%
  select(Pop, Ideologia, Contagem) %>%
  filter(Ideologia == "Direita")

  # N. de populistas de Esquerda:
sum(pop_esquerda_3[2:3,3])
# 189 Pessoas;

  # N. de populistas de Direita:
sum(pop_direita_3[2:3, 3])
# 532 Pessoas;



plot2_3 <- ggplot(data = dt2, aes(x = Pop, y = Contagem)) +
  geom_bar(aes(fill = Ideologia), stat = "identity", position = "dodge") +
  ggtitle("Atitudes Populistas 3 Níveis por Posição Ideológica") + 
  scale_fill_manual(values = c("#F0E68C", "#40E0D0", "#FF6347")) +
  coord_flip() + 
  ylab("Porcentagem em Relação aos Níveis de Atitudes Populistas") + 
  xlab("Atitudes Populistas 3 Níveis") + 
  scale_x_discrete(breaks = unique(dt2$Pop),
                   labels = labels2) + 
  geom_text(aes(label = Contagem), position = position_dodge(width = 1),
            size = 5) + 
  facet_wrap(~Ideologia) 

plot2_3




###
# Os de Esquerda são petistas?

# Petismo : e19$q10b
  # PT == 13
  # NA == 97, 98, 99

table(e19$q10b)

sum(e19$q10b == 13)
sum(e19$q10b >= 97)
# 1932 Não responderam, 274 responderam PT (cerca de )

prop.table(table(e19$q10b))*100
# 77.1% - Não se aplicam/Não responderam/NA's
# 10.93% - PT

## Selecionando os Populistas, de esquerda, Petistas:
# subsetting o banco de dados e19: pegando apenas a variáveis que queremos
t2 <- e19 %>%
  select(pop, pop_2c, pop_3n, ideo2.2, q10b) %>%
  na.exclude()

# Var. Original:
## Filtrando os Populistas de esquerda, petistas:
pt_pop_esquerda <- t2 %>%
  filter(ideo2.2 == "Esquerda", pop_2c == 1, q10b == 13)
  # Ou podemos fazer apenas um "sum":
sum(t2$ideo2.2 == "Esquerda" & t2$pop_2c ==1 & t2$q10b == 13)
# 49 pessoas;


# Var. 3 níveis:
pt_pop_esquerda_3 <- t2 %>%
  filter(ideo2.2 == "Esquerda", pop_3n > 0, q10b == 13)
sum(t2$ideo2.2 == "Esquerda" & t2$pop_3n > 0  & t2$q10b == 13)
# 49 pessoas;


# De um total de 189 pessoas que são populistas e de esquerda, 
# apenas 49 pessoas são petistas; 
(sum(t2$ideo2.2 == "Esquerda" & t2$pop_3n > 0  & t2$q10b == 13)) / 
  (sum(t2$ideo2.2 == "Esquerda" & t2$pop_3n > 0)) * 100
# O que representa um total de 25.92%;


##################


###
# Populismo e Escolaridade;

## Escolaridade:
# 0 - Analfabeto/ Nunca frequentou escola
# 1 - Primário incompleto (até 3ª série ou 4º ano do ensino fundamental)
# 2 - Primário completo (4ª.série  ou 5º ano do ensino fundamental)
# 3 - Ginásio incompleto (até 7ª série ou 8º ano do ensino fundamental)
# 4 - Ginásio completo (8ª série ou 9º ano do ensino fundamental)
# 5 - Colegial incompleto (até 2ª série do ensino médio)
# 6 - Colegial completo (3ª série do ensino médio)
# 7 - Ensino universitário incompleto ou especialização (técnico)
# 8 - Ensino universitário completo
# 9 - Pós-graduação ou mais

esc_pop <- e19 %>%
  select(pop, pop_2c, pop_3n, ed) %>%
  na.exclude()

df_esc_pop <- esc_pop %>%
  group_by(ed, pop_2c) %>%
  summarize(counts = n())

plot_ed_pop <- ggplot(data = df_esc_pop, aes(x = as.factor(ed), y = counts)) +
  geom_bar(aes(fill = pop_2c), stat = "identity", position = "dodge")  +
  #ggtitle("Atitudes Populistas por Faixa de Escolaridade") +
  ylab("Contagem") + 
  xlab("Faixa de Escolaridade") + 
  geom_text(aes(label = counts, group = as.factor(ed)), 
            position = position_dodge2(width = 1), size = 4) +
  labs(fill = "Populismo") + theme_bw() + 
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) +
  scale_fill_discrete(labels = c("Não Populista", "Populista"))

plot_ed_pop

  ## Opcional: dividir os gráficos -> facet_wrap(~Pop)

## Porcentagem

df_esc_pop$percent <- round(((df_esc_pop$counts / sum(df_esc_pop$counts)*100)), 2)


df# Para analisar os populistas de acordo com a faixa de escolaridade:
plot_ed_pop # Gráfico

# Filtrando educação APENAS pelos populistas:
df_esc_pop_t <- df_esc_pop %>%
  filter(pop_2c == 1) 

# Filtrando educação APENAS pelos não populistas:
df_esc_pop_2t <- df_esc_pop %>%
  filter(pop_2c == 0) 

# Renomeando Colunas:
df_e_pop <- df_esc_pop_t[-c(2,3)] 
colnames(df_e_pop) <- c("Ed", "Pop")

# Renomeando Colunas:
df_e_npop <- df_esc_pop_2t[-c(2,3)]
colnames(df_e_npop) <- c("Ed", "N_Pop")

# Merging os dois df's:
df_ed_pop <- left_join(df_e_pop, df_e_npop, by = "Ed")

df_ed_pop$Pop <- paste(df_ed_pop$Pop, "%")
df_ed_pop$N_Pop <- paste(df_ed_pop$N_Pop, "%")


##################



###
# Populismo e Renda

# [1] "Até R$ 954,00 (até 1 salário mínimo)"                                      
# [2] "Mais de R$ 954,00 até R$ 1.908,00 (mais de 1 até 2 salários mínimos)"      
# [3] "Mais de R$ 1.908,00 até R$ 4.770,00 (mais de 2 até 5 salários mínimos)"    
# [4] "Mais de R$ 4.770,00 até R$ 9.540,00 (mais de 5 até 10 salários mínimos)"   
# [5] "Mais de R$ 9.540,00 até R$ 14.310,00 (mais de 10 até 15 salários mínimos)" 
# [6] "Mais de R$ 14.310,00 até R$ 19.080,00 (mais de 15 até 20 salários mínimos)"
# [7] "Mais de R$ 19.080,00 (mais de 20 salários mínimos)"

# Selecionando apenas as variáveis fx_renda e pop:
renda_pop <- e19 %>%
  select(pop_3n, fx_renda) %>%
  na.exclude()

# Criando uma tabela cruzada:
tb_renda_pop <- table(renda_pop$pop_3n, renda_pop$fx_renda)

# Transformando a tabela em um data frame:
df_renda_pop <- data.frame(tb_renda_pop)

# Mudando o nome das colunas do df:
colnames(df_renda_pop) <- c("Pop", "Renda", "Contagem")

# Plotando:
plot_renda_pop <- ggplot(data = df_renda_pop, aes(x = Renda, y = Contagem)) +
  geom_bar(aes(fill = Pop), stat = "identity", position = "dodge")  +
  ggtitle("Atitudes Populistas por Faixa de Renda") +
  ylab("Contagem") + 
  xlab("Faixa de Renda") + 
  geom_text(aes(label = Contagem, group = Renda), 
            position = position_dodge2(width = 1), size = 4) 
## Opcional: dividir os gráficos -> facet_wrap(~Pop)


# Para analisar a distribuição dos populistas de acordo com faixa de renda:
plot_renda_pop # Gráfico
tb_renda_pop # Tabela cruzada



##################


###
# Populismo e Percepção da Corrupção

# Selecionando apenas as variáveis corrup1 e populismo:
corrup_pop <- e19 %>%
  select(pop_3n, corrup1) %>%
  na.exclude()

# Criando uma tabela cruzada com as variáveis:
tb_corrup_pop <- table(corrup_pop$pop_3n, corrup_pop$corrup1)

# Transformando a tabela em um data frame:
df_corrup_pop <- data.frame(tb_corrup_pop)

# Mudando o nome das colunas do df:
colnames(df_corrup_pop) <- c("Pop", "Corrup", "Contagem")

# Criando um vetor com os labels para o eixo X do ggplot:
lbs <- c("Não é um Problema Sério", "Pouco Sério",
         "Sério",
         "Muito Sério")

# Plotando:
plot_corrup_pop <- ggplot(data = df_corrup_pop, aes(x = Corrup, y = Contagem)) +
  geom_bar(aes(fill = Pop), stat = "identity", position = "dodge")  +
  ggtitle("Atitudes Populistas por Percepção da Corrupção") +
  ylab("Contagem") + 
  xlab("Percepção da Corrupção") + 
  scale_x_discrete(breaks = unique(df_corrup_pop$Corrup),
                   labels = lbs) + 
  geom_text(aes(label = Contagem, group = Corrup), 
            position = position_dodge2(width = 1), size = 4) 
## Opcional: dividir os gráficos -> facet_wrap(~Pop)

# Para analisar a distribuição dos populistas de acordo com a percepção da corrupção:
plot_corrup_pop # Gráfico
tb_corrup_pop # Tabela Cruzada 




##################



###
# Distribuição das dimensões separadas:

# Pop: 3 dimensões, não compensatórias;
## Maniqueísmo (M1)
## Antielitismo (AE1)
## People-centrism (PC1)

dim_pop <- e19 %>%
  select(M1_n, AE1_n, PC1_n, pop, pop_3n, pop_2c) %>%
  na.exclude()


####
## 1a dimensão: Maniqueísmo 
table(dim_pop$M1_n)

# Criando o label:
lb <- sort(unique(dim_pop$M1_n)) 
lb <- as.factor(lb)

# Criando um data frame agrupado e com uma coluna de contagem:
df_dim_pop <- dim_pop %>%
  group_by(M1_n) %>%
  summarise(counts = n())

df_dim_pop$percent <- (df_dim_pop$counts / sum(df_dim_pop$counts)) * 100


# Plotando o gráfico:
plot_maniq <- ggplot(data = df_dim_pop, aes(x = as.factor(M1_n), y = counts,
                                            fill = as.factor(M1_n))) +
  geom_bar(color = "black", stat = "identity") +  
  scale_x_discrete(breaks = lb) + 
  scale_fill_brewer(palette = "YlOrRd", direction = 1) +
  #ggtitle("Distribuição da Dimensão de Maniqueísmo") +
  xlab("Maniqueísmo Normalizada entre 0 e 1") + 
  ylab("Contagem")  +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  theme(legend.position="none") + 
  geom_text(aes(label = counts), position = position_stack(vjust = 0.5)) 


plot_maniq


## Comparando Maniqueísmo com Populismo:
# Selecionando apenas as duas variáveis, agrupando por elas e criando uma coluna de
  # contagem:
maniq_pop2 <- dim_pop %>%
  select(M1_n, pop_3n) %>%
  group_by(M1_n, pop_3n)%>%
  summarize(counts = n())

maniq_pop2_c <- dim_pop %>%
  select(M1_n, pop_2c) %>%
  group_by(M1_n, pop_2c)%>%
  summarize(counts = n())


# Plotando:
plot_maniq_pop_2c <- ggplot(data = maniq_pop2_c, aes(x = as.factor(M1_n), y = counts, 
                                                fill = as.factor(pop_2c))) +
  geom_bar( color = "black", stat = "identity", position = "dodge") +  
  scale_x_discrete(breaks = lb) + 
  scale_fill_brewer(palette = "PRGn", direction = -1, 
                    labels = c("Não Populista", "Populista (0.75 >) ")) +
  #ggtitle("Distribuição da Dimensão de Maniqueísmo por Níveis de Populismo") +
  xlab("Maniqueísmo Normalizada entre 0 e 1") + 
  ylab("Contagem") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  geom_text(aes(label = counts, group = M1_n), position = position_dodge2(width = .7), vjust = -.1,
            size = 4) + 
  labs(fill = "Populismo (2 Categorias)")


plot_maniq_pop_2c



####
## 2a dimensão: People-centrism 

table(dim_pop$PC1_n)

# Criando o label:
lb <- sort(unique(dim_pop$PC1_n)) 
lb <- as.factor(lb)

# Agrupando apenas pela variável e criando uma coluna com a contagem:
df_pc <- dim_pop %>%
  group_by(PC1_n) %>%
  summarize(counts = n()) 


df_pc$percent <- (df_pc$counts / (sum(df_pc$counts))) * 100

plot_pc <- ggplot(data = df_pc, aes(x = as.factor(PC1_n), y = counts,
                                            fill = as.factor(PC1_n))) +
  geom_bar(color = "black", stat = "identity") +  
  scale_x_discrete(breaks = lb) + 
  scale_fill_brewer(palette = "YlOrRd", direction = 1) +
  #ggtitle("Distribuição da Dimensão de People-Centrism") +
  xlab("People-Centrism Normalizada entre 0 e 1") + 
  ylab("Contagem") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  theme(legend.position="none") + 
  geom_text(aes(label = counts), position = position_stack(vjust = 0.5)) 


plot_pc

## Comparando People-Centrism com Populismo:
# Selecionando apenas as duas variáveis, agrupando por elas e criando uma coluna de
  # contagem:
pc_pop <- dim_pop %>%
  select(PC1_n, pop_3n) %>%
  group_by(PC1_n, pop_3n)%>%
  summarize(counts = n())

pc_pop_2c <- dim_pop %>%
  select(PC1_n, pop_2c) %>%
  group_by(PC1_n, pop_2c)%>%
  summarize(counts = n())


# Plotando:
plot_pc_pop_2c <- ggplot(data = pc_pop_2c, aes(x = as.factor(PC1_n), y = counts, 
                                                fill = as.factor(pop_2c))) +
  geom_bar( color = "black", stat = "identity", position = "dodge") +  
  scale_x_discrete(breaks = lb) + 
  scale_fill_brewer(palette = "PRGn", direction = -1,
                    labels = c("Não Populista", "Populista (0.75 >) "))+
  #ggtitle("Distribuição da Dimensão de People-Centrism por Níveis de Populismo") +
  xlab("People-Centrism Normalizada entre 0 e 1") + 
  ylab("Contagem") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  geom_text(aes(label = counts, group = PC1_n), position = position_dodge2(width = .7), vjust = -.1,
            size = 4) + 
  labs(fill = "Populismo (2 Categorias)")


plot_pc_pop_2c



####
## 3a dimensão: Antielitismo
table(dim_pop$AE1_n)

# Criando o label:
lb <- sort(unique(dim_pop$AE1_n)) 
lb <- as.factor(lb)

# Agrupando apenas pela variável e criando uma coluna com a contagem:
df_ae <- dim_pop %>%
  group_by(AE1_n) %>%
  summarize(counts = n()) 

df_ae$percent <- ( df_ae$counts / (sum(df_ae$counts)) ) * 100

plot_ae <- ggplot(data = df_ae, aes(x = as.factor(AE1_n), y = counts,
                                    fill = as.factor(AE1_n))) +
  geom_bar(color = "black", stat = "identity") +  
  scale_x_discrete(breaks = lb) + 
  scale_fill_brewer(palette = "YlOrRd", direction = 1) +
 # ggtitle("Distribuição da Dimensão de Antielitismo") +
  xlab("Antielitismo Normalizada entre 0 e 1") + 
  ylab("Contagem") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  theme(legend.position="none") + 
  geom_text(aes(label = counts), position = position_stack(vjust = 0.5)) 


plot_ae

## Comparando Antielitismo com Populismo:
# Selecionando apenas as duas variáveis, agrupando por elas e criando uma coluna de
# contagem:
ae_pop <- dim_pop %>%
  select(AE1_n, pop_3n) %>%
  group_by(AE1_n, pop_3n)%>%
  summarize(counts = n())

ae_pop_2c <- dim_pop %>%
  select(AE1_n, pop_2c) %>%
  group_by(AE1_n, pop_2c)%>%
  summarize(counts = n())


# Plotando:
plot_ae_pop_2c <- ggplot(data = ae_pop_2c, aes(x = as.factor(AE1_n), y = counts, 
                                         fill = as.factor(pop_2c))) +
  geom_bar( color = "black", stat = "identity", position = "dodge") +  
  scale_x_discrete(breaks = lb) + 
  scale_fill_brewer(palette = "PRGn", direction = -1,
                    labels = c("Não Populista", "Populista (0.75 >) ")) +
  #ggtitle("Distribuição da Dimensão de Antielitismo por Níveis de Populismo") +
  xlab("Antielitismo Normalizada entre 0 e 1") + 
  ylab("Contagem") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size = 0.5,
                                 linetype = "solid")) + 
  geom_text(aes(label = counts, group = AE1_n), position = position_dodge2(width = .7), vjust = -.1,
            size = 4) + 
  labs(fill = "Populismo (2 Categorias)")


plot_ae_pop_2c







