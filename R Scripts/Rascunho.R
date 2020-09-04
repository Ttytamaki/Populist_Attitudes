### Teste 20/08 ###




# Selecionando apenas as variáveis que iremos utilizar #


br <- e19 %>%
  select(M1, AE1, PC1, PC2,
         sexo, ed, id, ideo2.2,
         voto_b)


br2 <- e19 %>%
  select(M1, AE1, PC1, PC2,
         sexo, ed, id, ideo2.2, renda, corrup1, corrup2,
         voto_b)


# Removendo os NA's do banco: para que não haja nenhum problema nos próximos passos
## Transformações, modelos e etc.;

### Utilizando funçao apenas para verificar o número de NA's por variável
# source(here::here("functions", "check_na.R"), encoding = "UTF-8")
# check_NA(br)
###

# Banco COM as variáveis de Corrupção e Renda
br2 <- as.data.frame(br2)
br2 <- na.exclude(br2)

## Criando outro banco com BR2 SEM NA's
br3 <- as.data.frame(br2)
br3 <- na.exclude(br3)
View(br3)



# Banco SEM as variáveis de Corupção e Renda
br <- as.data.frame(br)
br <- na.exclude(br)

## Criando um OUTRO banco com BR sem NA's
br_1 <- as.data.frame(br)
br_1 <- na.exclude(br_1)
View(br_1)




### br2 - Com as variáveis sugeridas pelo Bruno;
### br - Com as variáveis originais;

br2 <- br2 %>%
  mutate(M1_n = norm01(M1),
         AE1_n = norm01(AE1),
         PC1_n = norm01(PC1),
         PC2_n = norm01(PC2)) %>%
  mutate(pop = M1_n * AE1_n * PC1_n)

br <- br %>%
  mutate(M1_n = norm01(M1),
         AE1_n = norm01(AE1),
         PC1_n = norm01(PC1),
         PC2_n = norm01(PC2)) %>%
  mutate(pop = M1_n * AE1_n * PC1_n)

#### TESTE #####
br_1.1 <- as.data.frame(br)
br3.1 <- as.data.frame(br2)



# br_1
br_1 <- br_1 %>%
  mutate(M1_n = norm01(M1),
         AE1_n = norm01(AE1),
         PC1_n = norm01(PC1),
         PC2_n = norm01(PC2)) %>%
  mutate(pop = M1_n * AE1_n * PC1_n)
table(br_1$pop)

# br_1.1
names(br_1.1)
nrow(br_1.1)

br_1.1 <- br_1.1 %>%
  mutate(M1_n = range01(M1),
         AE1_n = range01(AE1),
         PC1_n = range01(PC1),
         PC2_n = range01(PC2)) %>%
  mutate(pop = M1_n * AE1_n * PC1_n)
table(br_1.1$pop)

# br3
names(br3)
nrow(br3)

br3 <- br3 %>%
  mutate(M1_n = norm01(M1),
         AE1_n = norm01(AE1),
         PC1_n = norm01(PC1),
         PC2_n = norm01(PC2)) %>%
  mutate(pop = M1_n * AE1_n * PC1_n)

table(br3$pop)

# br3.1
nrow(br3.1)

br3.1 <- br3.1 %>%
  mutate(M1_n = range01(M1),
         AE1_n = range01(AE1),
         PC1_n = range01(PC1),
         PC2_n = range01(PC2)) %>%
  mutate(pop = M1_n * AE1_n * PC1_n)
table(br3.1$pop)


## Regressões:

modelo1 <- glm(voto_b ~ pop * ideo2.2 + sexo + id + ed, data = br1,
               family = binomial(link = "logit"),
               maxit = 100)

summary(modelo1)
exp1 <- exp(modelo1$coefficients)
p.5 <- list(summary(modelo1)$coefficients[,4])

stargazer(title = "Regressão: Atitudes Populistas x Voto em 2018", 
          modelo1, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          deep.var.labels = c("Voto em Bolsonaro"),
          perl = TRUE,
          covariate.labels=c("Populismo",
                             "Ideologia: Esquerda",
                             "Ideologia: Direita",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Ser de Esquerda e Populista",
                             "Ser de Direita e Populista"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "Atitudes Populistas x Voto Bolso.html")
)


#### VER COMO LIDAR COM OS NA'S ####
######### UTILIZAR O BANCO DE DADOS E19 - COM A FUNÇÃO DO BRUNO #########
######### E VER COMO LIDAR COM OS NA'S DURANTE A REGRESSÃO  #########


### Teste 20/08 ###

### e19 ###
