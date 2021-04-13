# ---
# 02 - Regressões
# Regressões para diferntes modelos
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
library(stargazer)
library(car)


source(here::here("00 - Tratando e Criando as Variaveis.R"), encoding = "UTF-8")


###
# Regressões:


#######################
# Atitudes Populistas #
#######################


###
# Modelo 1: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Atitudes Populistas, Sexo, Idade, Educação

m1 <- glm(voto_b ~ pop_2c + sexo + id + ed, 
            data = e19,
            na.action = na.omit,
            family = binomial(link = "logit"),
            maxit = 100)
# Exponencial:
exp1 <- exp(m1$coefficients)
# P-valor:
p1 <- list(summary(m1)$coefficients[,4])


###
# Modelo 2: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação  
m2 <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"),
          maxit = 100)
# Exponencial:
exp2 <- exp(m2$coefficients)
# P-valor:
p2 <- list(summary(m2)$coefficients[,4])


###
# Modelo 3: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, Corrupção
m3 <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed + corrup1, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"),
          maxit = 100)
# Exponencial:
exp3 <- exp(m3$coefficients)
# P-valor:
p3 <- list(summary(m3)$coefficients[,4])


###
# Modelo 4: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, Corrupção, PC2
m4 <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed + corrup1 + PC2, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"),
          maxit = 100)
# Exponencial:
exp4 <- exp(m4$coefficients)
# P-valor:
p4 <- list(summary(m4)$coefficients[,4])


###
# Modelo 5: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, Corrupção, PC2, 
    # Antipetismo
m5 <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed + corrup1 + PC2 + antipt, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"),
          maxit = 100)
# Exponencial:
exp5 <- exp(m5$coefficients)
# P-valor:
p5 <- list(summary(m5)$coefficients[,4])


###
# Modelo 6: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, Corrupção, PC2, 
    # Antipetismo, Renda
m6 <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed + corrup1 + PC2 + antipt + 
            fx_renda,
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"),
          maxit = 100)
# Exponencial:
exp6 <- exp(m6$coefficients)
# P-valor:
p6 <- list(summary(m6)$coefficients[,4])


###
# Modelo 7: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, Corrupção, PC2, 
    # Antipetismo, Renda, Religião
m7 <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed + corrup1 + PC2 + antipt + 
            fx_renda + relig,
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"),
          maxit = 100)
# Exponencial:
exp7 <- exp(m7$coefficients)
# P-valor:
p7 <- list(summary(m7)$coefficients[,4])





#############################################
# Tipologia Atitudes Populistas x Ideologia #
#############################################

###
# Modelo 1: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Tipologia Atitudes Populistas, Sexo, Idade, Educação

mt1 <- glm(voto_b ~ pop_ed_sart + sexo + id + ed, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"),
          maxit = 100)
# Exponencial:
exp_t1 <- exp(mt1$coefficients)
# P-valor:
p_t1 <- list(summary(mt1)$coefficients[,4])



###
# Modelo 2: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Tipologia Atitudes Populistas, Sexo, Idade, Educação, Corrupção

mt2 <- glm(voto_b ~ pop_ed_sart + sexo + id + ed + corrup1, 
           data = e19,
           na.action = na.omit,
           family = binomial(link = "logit"),
           maxit = 100)
# Exponencial:
exp_t2 <- exp(mt2$coefficients)
# P-valor:
p_t2 <- list(summary(mt2)$coefficients[,4])



###
# Modelo 3: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Tipologia Atitudes Populistas, Sexo, Idade, Educação, Corrupção,
    # PC2

mt3 <- glm(voto_b ~ pop_ed_sart + sexo + id + ed + corrup1 + PC2, 
           data = e19,
           na.action = na.omit,
           family = binomial(link = "logit"),
           maxit = 100)
# Exponencial:
exp_t3 <- exp(mt3$coefficients)
# P-valor:
p_t3 <- list(summary(mt3)$coefficients[,4])



###
# Modelo 4: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Tipologia Atitudes Populistas, Sexo, Idade, Educação, Corrupção,
    # PC2, Antipetismo

mt4 <- glm(voto_b ~ pop_ed_sart + sexo + id + ed + corrup1 + PC2 + antipt, 
           data = e19,
           na.action = na.omit,
           family = binomial(link = "logit"),
           maxit = 100)
# Exponencial:
exp_t4 <- exp(mt4$coefficients)
# P-valor:
p_t4 <- list(summary(mt4)$coefficients[,4])



###
# Modelo 5: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Tipologia Atitudes Populistas, Sexo, Idade, Educação, Corrupção,
    # PC2, Antipetismo

mt5 <- glm(voto_b ~ pop_ed_sart + sexo + id + ed + corrup1 + PC2 + antipt + relig, 
           data = e19,
           na.action = na.omit,
           family = binomial(link = "logit"),
           maxit = 100)
# Exponencial:
exp_t5 <- exp(mt5$coefficients)
# P-valor:
p_t5 <- list(summary(mt5)$coefficients[,4])



###
# Modelo 6: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Tipologia Atitudes Populistas, Sexo, Idade, Educação, Corrupção,
    # PC2, Antipetismo

mt6 <- glm(voto_b ~ pop_ed_sart + sexo + id + ed + corrup1 + PC2 + antipt + 
             relig + fx_renda, 
           data = e19,
           na.action = na.omit,
           family = binomial(link = "logit"),
           maxit = 100)
# Exponencial:
exp_t6 <- exp(mt6$coefficients)
# P-valor:
p_t6 <- list(summary(mt6)$coefficients[,4])



#####################
###
# VIFS das Regressões:

vif(mt1)
  # 1.18
vif(mt2)
  # 1.19
vif(mt3)
  # 1.17
vif(mt4)
  # 1.21
vif(mt5)
  # 1.21
vif(mt6)
  # 1.39


vif(m1)
  # 1.18
vif(m2)
  # 6.13
vif(m3)
  # 6.16
vif(m4)
  # 6.14
vif(m5)
  # 5.86
vif(m6)
  # 6.44 
vif(m7)
  # 6.49





