library(tidyverse)
library(haven)
library(here)
library(stargazer)

source(here::here("00 - Tratamento das Variáveis.R"), encoding = "UTF-8")

# Verificando quais variáveis estão no banco de dados:
names(e19)

vars <- c("M1", "AE1", "PC1", "PC2", "sexo", "ed", "id", "ideo2.2", "fx_renda", 
         "corrup1", "corrup2","voto_b", "pop")
# Verificando o formato das variáveis:
map(e19[vars], ~class(.))

# $M1  <- Escala de Likert
# [1] "numeric"
# $AE1 <- Escala de Likert
# [1] "numeric"
# $PC1 <- Escala de Likert
# [1] "numeric"
# $PC2 <- Escala de Likert
# [1] "numeric"
# $sexo 
# [1] "numeric" <- Binária (Referência: Mulher)
# $ed <- Faixa
# [1] "haven_labelled" "vctrs_vctr"     "double"        
# $id
# [1] "numeric"
# $fx_renda <- Faixa
# [1] "numeric"
# $ideo2.2 
# [1] "factor" <- Referência: Centro
# $corrup1 <- 1 a 4
# [1] "numeric"
# $corrup2 <- 1 a 4
# [1] "numeric"
# $voto_b 
# [1] "factor" <- Referência: Outros
# $pop
# [1] "numeric" <- Não compensatória, multidimensional;

# Mudar a variável "ed"

table(e19$ed) # Faixa de educação;
e19$ed <- as.numeric(e19$ed)

## Conferindo o formato das variáveis:
map(e19[vars], ~class(.)) 



# Realizando a regressão logística:
modelo1 <- glm(voto_b ~ pop * ideo2.2 + sexo + id + ed, data = e19,
               na.action = na.omit,
               family = binomial(link = "logit"),
               maxit = 100)
summary(modelo1)
exp1 <- exp(modelo1$coefficients)
p.5 <- list(summary(modelo1)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          modelo1, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populismo",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Ser de Direita e Populista",
                             "Ser de Esquerda e Populista"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "Modelo1.html")
)



#### Corrup1 + fx_Renda:
modelo2 <- glm(voto_b ~ pop * ideo2.2 + sexo + id + ed + corrup1 + fx_renda, 
               data = e19,
               na.action = na.omit,
               family = binomial(link = "logit"),
               maxit = 100)
summary(modelo2)
exp1 <- exp(modelo2$coefficients)
p.5 <- list(summary(modelo2)$coefficients[,4])

stargazer(title = "Regressão Modelo 2", 
          modelo2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populismo",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "Renda",
                             "Ser de Direita e Populismo",
                             "Ser de Esquerda e Populismo"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "02.html")
)

#### Corrup2 + fx_Renda:
modelo2 <- glm(voto_b ~ pop * ideo2.2 + sexo + id + ed + corrup2 + fx_renda, 
               data = e19,
               na.action = na.omit,
               family = binomial(link = "logit"),
               maxit = 100)
summary(modelo2)
exp1 <- exp(modelo2$coefficients)
p.5 <- list(summary(modelo2)$coefficients[,4])

stargazer(title = "Regressão Modelo 2", 
          modelo2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populismo",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "Renda",
                             "Ser de Direita e Populismo",
                             "Ser de Esquerda e Populismo"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "03.html")
)


#######################################################################################

### ACRESCENTANDO A VARIÁVEL PC2 ###

modelo3 <- glm(voto_b ~ pop * ideo2.2 + sexo + id + ed + PC2, 
               data = e19,
               na.action = na.omit,
               family = binomial(link = "logit"),
               maxit = 100)
summary(modelo3)
exp1 <- exp(modelo3$coefficients)
p.5 <- list(summary(modelo3)$coefficients[,4])

stargazer(title = "Regressão Modelo 3", 
          modelo3, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populismo",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Var. PC2",
                             "Ser de Direita e Populismo",
                             "Ser de Esquerda e Populismo"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "03 - Modelo3 - PC2.html")
)


### PC2 + corrup e fx_renda ###
modelo3.2 <- glm(voto_b ~ pop * ideo2.2 + sexo + id + ed + corrup1 + fx_renda + PC2, 
                 data = e19,
               na.action = na.omit,
               family = binomial(link = "logit"),
               maxit = 100)
summary(modelo3.2)
exp1 <- exp(modelo3.2$coefficients)
p.5 <- list(summary(modelo3.2)$coefficients[,4])

stargazer(title = "Regressão Modelo 3", 
          modelo3.2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populismo",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "Renda",
                             "Var. PC2",
                             "Ser de Direita e Populismo",
                             "Ser de Esquerda e Populismo"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "03.2 - Modelo2 - PC2, Corrup1 e fx_renda.html")
)


### PC2 + Corrup2 e fx_renda ###
modelo3.3 <- glm(voto_b ~ pop * ideo2.2 + sexo + id + ed + corrup2 + fx_renda + PC2, 
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
summary(modelo3.3)
exp1 <- exp(modelo3.3$coefficients)
p.5 <- list(summary(modelo3.3)$coefficients[,4])

stargazer(title = "Regressão Modelo 3", 
          modelo3.3, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populismo",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "Renda",
                             "Var. PC2",
                             "Ser de Direita e Populismo",
                             "Ser de Esquerda e Populismo"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "03.3 - Modelo2 - PC2, Corrup2 e fx_renda.html")
)



#######################################

###
# Regressões com a variável atitudes populistas 3 escalas

# Transformando em Fator:

e19 <- e19 %>%
  mutate(pop_3n_c = as.factor(pop_3n))


##
m_3n <- glm(voto_b ~ pop_3n_c * ideo2.2 + sexo + id + ed, data = e19,
               na.action = na.omit,
               family = binomial(link = "logit"),
               maxit = 100)
summary(m_3n)
exp1 <- exp(m_3n$coefficients)
p.5 <- list(summary(m_3n)$coefficients[,4])

stargazer(title = "Regressão", 
          m_3n, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Somewhat Populista",
                             "Populista Perfeito",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Ser de Direita e Somewhat Populista",
                             "Ser de Direita e Populista Puro",
                             "Ser de Esquerda e Somewhat Populista",
                             "Ser de Esquerda e Populista Puro"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "01 - Modelo1_Pop 3n.html")
)

##
# + fx_renda e corrupção1

m_3n.2 <- glm(voto_b ~ pop_3n_c * ideo2.2 + sexo + id + ed + corrup1 + fx_renda,
            data = e19,
            na.action = na.omit,
            family = binomial(link = "logit"),
            maxit = 100)
summary(m_3n.2)
exp1 <- exp(m_3n.2$coefficients)
p.5 <- list(summary(m_3n.2)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_3n.2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Somewhat Populista",
                             "Populista Perfeito",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "Renda",
                             "Ser de Direita e Somewhat Populista",
                             "Ser de Direita e Populista Puro",
                             "Ser de Esquerda e Somewhat Populista",
                             "Ser de Esquerda e Populista Puro"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "Corrup1.html")
)


##
# + PC2
m_3n.4 <- glm(voto_b ~ pop_3n_c * ideo2.2 + sexo + id + ed + PC2, data = e19,
            na.action = na.omit,
            family = binomial(link = "logit"),
            maxit = 100)
summary(m_3n.4)
exp1 <- exp(m_3n.4$coefficients)
p.5 <- list(summary(m_3n.4)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_3n.4, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Somewhat Populista",
                             "Populista Perfeito",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "PC2",
                             "Ser de Direita e Somewhat Populista",
                             "Ser de Direita e Populista Puro",
                             "Ser de Esquerda e Somewhat Populista",
                             "Ser de Esquerda e Populista Puro"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "01 - Modelo1_Pop 3n + PC2.html")
)

##
# + PC2 + fx_renda + corrup1
m_3n.5 <- glm(voto_b ~ pop_3n_c * ideo2.2 + sexo + id + ed + PC2  + fx_renda + corrup1, data = e19,
              na.action = na.omit,
              family = binomial(link = "logit"),
              maxit = 100)
summary(m_3n.5)
exp1 <- exp(m_3n.5$coefficients)
p.5 <- list(summary(m_3n.5)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_3n.5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Somewhat Populista",
                             "Populista Perfeito",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "PC2",
                             "Renda",
                             "Corrupção",
                             "Ser de Direita e Somewhat Populista",
                             "Ser de Direita e Populista Puro",
                             "Ser de Esquerda e Somewhat Populista",
                             "Ser de Esquerda e Populista Puro"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "01 - Modelo1_Pop 3n + PC2.html")
)

#######################################

###
# Regressões com a variável atitudes populistas 4 Níveis:

m_4n <- glm(voto_b ~ pop_4n * ideo2.2 + sexo + id + ed, 
            data = e19,
              na.action = na.omit,
              family = binomial(link = "logit"),
              maxit = 100)
summary(m_4n)
exp1 <- exp(m_4n$coefficients)
p.5 <- list(summary(m_4n)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_4n, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populismo 4 Níveis",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Ser de Direita e Populismo",
                             "Ser de Esquerda e Populismo"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "01 - Modelo1_Pop 3n + PC2.html")
)


##
# + Renda e Corrupção1:

m_4n.1 <- glm(voto_b ~ pop_4n * ideo2.2 + sexo + id + ed + fx_renda + corrup1, 
            data = e19,
            na.action = na.omit,
            family = binomial(link = "logit"),
            maxit = 100)
summary(m_4n.1)
exp1 <- exp(m_4n.1$coefficients)
p.5 <- list(summary(m_4n.1)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_4n.1, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populismo 4 Níveis",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Renda",
                             "Corrupção",
                             "Ser de Direita e Populismo",
                             "Ser de Esquerda e Populismo"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "01 - Modelo1_Pop 3n + PC2.html")
)


# + PC2:

m_4n.2 <- glm(voto_b ~ pop_4n * ideo2.2 + sexo + id + ed + PC2, 
              data = e19,
              na.action = na.omit,
              family = binomial(link = "logit"),
              maxit = 100)
summary(m_4n.2)
exp1 <- exp(m_4n.2$coefficients)
p.5 <- list(summary(m_4n.2)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_4n.2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populismo 4 Níveis",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "PC2",
                             "Ser de Direita e Populismo",
                             "Ser de Esquerda e Populismo"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "01 - Modelo1_Pop 3n + PC2.html")
)

# + PC2 + Renda + Corrupção1:

m_4n.3 <- glm(voto_b ~ pop_4n * ideo2.2 + sexo + id + ed + PC2 + fx_renda + corrup1, 
              data = e19,
              na.action = na.omit,
              family = binomial(link = "logit"),
              maxit = 100)
summary(m_4n.3)
exp1 <- exp(m_4n.3$coefficients)
p.5 <- list(summary(m_4n.3)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_4n.3, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populismo 4 Níveis",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "PC2",
                             "Renda",
                             "Corrupção",
                             "Ser de Direita e Populismo",
                             "Ser de Esquerda e Populismo"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "01 - Modelo1_Pop 3n + PC2.html")
)

#######################################

# 4 Níveis (categóricas)

# Transformando em categórica:
e19$pop_4n_c

m_4n.c <- glm(voto_b ~ pop_4n_c * ideo2.2 + sexo + id + ed + PC2 + fx_renda + corrup1, 
              data = e19,
              na.action = na.omit,
              family = binomial(link = "logit"),
              maxit = 100)
summary(m_4n.c)
exp1 <- exp(m_4n.c$coefficients)
p.5 <- list(summary(m_4n.c)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_4n.c, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Somewhat Populista",
                             "Populista",
                             "Populista Puro",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "PC2",
                             "Renda",
                             "Corrupção",
                             "Ser de Direita e Somewhat Populista",
                             "Ser de Direita e Populista",
                             "Ser de Direita e Populista Puro",
                             "Ser de Esquerda e Somewhat Populista",
                             "Ser de Esquerda e Populista",
                             "Ser de Esquerda e Populita Puro"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "14.2 - Pop. 4 Níveis (Categórica) + PC2 + Faixa de Renda + Corrupção 1.html")
)

#######################################

###
# Regressões com a variável atitudes populistas 2 Categorias:

m_2c <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed, 
            data = e19,
            na.action = na.omit,
            family = binomial(link = "logit"),
            maxit = 100)
summary(m_2c)
exp1 <- exp(m_2c$coefficients)
p.5 <- list(summary(m_2c)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_2c, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Ser de Direita e Populista",
                             "Ser de Esquerda e Populista"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "15 - Pop. 2 Categorias Base.html")
)


# + Renda e Corrupção
m_2c.1 <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed + fx_renda + corrup1, 
            data = e19,
            na.action = na.omit,
            family = binomial(link = "logit"),
            maxit = 100)
summary(m_2c.1)
exp1 <- exp(m_2c.1$coefficients)
p.5 <- list(summary(m_2c.1)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_2c.1, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Renda",
                             "Corrupção",
                             "Ser de Direita e Populista",
                             "Ser de Esquerda e Populista"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "16 - Pop. 2 Categorias + Faixa de Renda + Corrupção 1.html")
)


# + PC2 + Renda e Corrupção
m_2c.2 <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed + PC2 + fx_renda + corrup1, 
              data = e19,
              na.action = na.omit,
              family = binomial(link = "logit"),
              maxit = 100)
summary(m_2c.2)
exp1 <- exp(m_2c.2$coefficients)
p.5 <- list(summary(m_2c.2)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_2c.2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista",
                             "Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "PC2",
                             "Renda",
                             "Corrupção",
                             "Ser de Direita e Populista",
                             "Ser de Esquerda e Populista"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "17 - Pop. 2 Categorias + PC2 + Faixa de Renda + Corrupção 1.html")
)




#######################################

###
# Regressões com a Variável de Atitudes Populistas como Variável Dependente:

# Atitudes Populistas: 2 Categorias
## Var. Independente: Ideologia, Sexo, Idade, Educação, PC2, Renda, Corrupção

m_pop2c <- glm(pop_2c ~ ideo2.2 + sexo + id + ed + fx_renda + corrup1, 
              data = e19,
              na.action = na.omit,
              family = binomial(link = "logit"),
              maxit = 100)
summary(m_pop2c)
exp1 <- exp(m_pop2c$coefficients)
p.5 <- list(summary(m_pop2c)$coefficients[,4])

stargazer(title = "Regressão Modelo 1", 
          m_pop2c, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Ser Populista",
          perl = TRUE,
          covariate.labels=c("Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Renda",
                             "Corrupção"
          ),
          apply.coef = exp,
          p.auto = F,
          p = p.5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "Var. Dep. Pop 2 Categorias + PC2 + Faixa de Renda + Corrupção 1.html")
)


# Atitudes Populistas Original (17 valores);

class(e19$pop)
modelo <- lm(pop_4n ~ ideo2.2 + sexo + id + ed + fx_renda + corrup1,
             data = e19, na.action = na.omit)
summary(modelo)
stargazer(title = "Regressão Modelo 1", 
          modelo, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Populismo",
          perl = TRUE,
          covariate.labels=c("Ideologia: Direita",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Renda",
                             "Corrupção"
          ), out = here::here("anexos", 
          "Var. Dep. Pop 4 Níveis + PC2 + Faixa de Renda + Corrupção 1.html")
)
