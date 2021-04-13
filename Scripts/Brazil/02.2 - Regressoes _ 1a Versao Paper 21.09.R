# ---
# 02.2 - Regressões: Paper 21/09/2020 (1a Versão do Paper)
# Regressões para diferntes modelos
# Atitudes Populistas e Voto em Bolsonaro em 2018 (2020)
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 22/09/2020
# ---


## PREAMBULO -------------------------------------------------------------------

library(tidyverse)
library(here)
library(stargazer)


source(here::here("00 - Tratando e Criando as Variaveis.R"), encoding = "UTF-8")



######################################
# Regressões para o Paper 21/09/2020 #
######################################

# Var.: pop_2c - Categórica
# pop_ed_sart - Tipologia
# pop_goertz - Goertziana (Minimo)
# pop_ad - Adição


## ## ## ## ## ## ## ##
# pop_2c - Categórica #
## ## ## ## ## ## ## ##
# Modelo 1: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
mod_cat_1 <- glm(voto_b ~ pop_2c  + sexo + id + ed + fx_renda,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_cat_exp <- exp(mod_cat_1$coefficients)
# P-valor:
mod_cat_p <- list(summary(mod_cat_1)$coefficients[,4])


##
# Modelo 2:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda +
          # Ideologia
mod_cat_2 <- glm(voto_b ~ pop_2c  + sexo + id + ed + fx_renda + ideo2.2,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_cat_exp2 <- exp(mod_cat_2$coefficients)
# P-valor:
mod_cat_p2 <- list(summary(mod_cat_2)$coefficients[,4])


##
# Modelo 3:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda +
          # Interação com Ideologia
mod_cat_3 <- glm(voto_b ~ pop_2c * ideo2.2  + sexo + id + ed + fx_renda,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_cat_exp3 <- exp(mod_cat_3$coefficients)
# P-valor:
mod_cat_p3 <- list(summary(mod_cat_3)$coefficients[,4])


##
# Modelo 4:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda +
          # Interação com Ideologia +
          # PC2
mod_cat_4 <- glm(voto_b ~ pop_2c * ideo2.2  + sexo + id + ed + fx_renda +
                   PC2,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_cat_exp4 <- exp(mod_cat_4$coefficients)
# P-valor:
mod_cat_p4 <- list(summary(mod_cat_4)$coefficients[,4])


##
# Modelo 5 (Apendice):
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes Populistas Categórica +
          # Demographic Controls: Sexo, Idade, Educação e Renda +
          # Interação com Ideologia + 
          # PC2 +
          # Antipt
mod_cat_5 <- glm(voto_b ~ pop_2c * ideo2.2  + sexo + id + ed + 
                   fx_renda +
                   PC2 +
                   antipt,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_cat_exp5 <- exp(mod_cat_5$coefficients)
# P-valor:
mod_cat_p5 <- list(summary(mod_cat_5)$coefficients[,4])


##
# Modelo 6 (Apendice):
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes Populistas Categórica +
          # Demographic Controls: Sexo, Idade, Educação e Renda +
          # Interação com Ideologia + 
          # PC2 +
          # Antipt +
          # Corrupção
mod_cat_6 <- glm(voto_b ~ pop_2c * ideo2.2  + sexo + id + ed + 
                   fx_renda +
                   PC2 +
                   antipt + 
                   corrup1,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_cat_exp6 <- exp(mod_cat_6$coefficients)
# P-valor:
mod_cat_p6 <- list(summary(mod_cat_6)$coefficients[,4])


##
# Modelo 7 (Apendice):
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes Populistas Categórica +
          # Demographic Controls: Sexo, Idade, Educação e Renda +
          # Interação com Ideologia + 
          # PC2 +
          # Antipt +
          # Corrupção +
          # Religião (Ser Evangélico)
mod_cat_7 <- glm(voto_b ~ pop_2c * ideo2.2  + sexo + id + ed + 
                   fx_renda +
                   PC2 +
                   antipt + 
                   corrup1 +
                   relig,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_cat_exp7 <- exp(mod_cat_7$coefficients)
# P-valor:
mod_cat_p7 <- list(summary(mod_cat_7)$coefficients[,4])



##############################


# ## ## ## ## ## ## ## ## #
# pop_ed_sart - Tipologia #
# ## ## ## ## ## ## ## ## #

# Modelo 1: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Tipologia + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
mod_tip_1 <- glm(voto_b ~ pop_ed_sart  + sexo + id + ed + fx_renda,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_tip_exp <- exp(mod_tip_1$coefficients)
# P-valor:
mod_tip_p <- list(summary(mod_tip_1)$coefficients[,4])


##
# Modelo 2:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda +
          # PC2
mod_tip_2 <- glm(voto_b ~ pop_ed_sart + sexo + id + ed + fx_renda + PC2,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_tip_exp2 <- exp(mod_tip_2$coefficients)
# P-valor:
mod_tip_p2 <- list(summary(mod_tip_2)$coefficients[,4])


##
# Modelo 3 (Apendice):
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes Populistas Tipologia +
          # Demographic Controls: Sexo, Idade, Educação e Renda +
          # Interação com Ideologia + 
          # PC2 +
          # Antipt +
mod_tip_3 <- glm(voto_b ~ pop_ed_sart + sexo + id + ed + fx_renda + 
                   PC2 +
                   antipt,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_tip_exp3 <- exp(mod_tip_3$coefficients)
# P-valor:
mod_tip_p3 <- list(summary(mod_tip_3)$coefficients[,4])


##
# Modelo 4 (Apendice):
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes Populistas Tipologia +
          # Demographic Controls: Sexo, Idade, Educação e Renda +
          # Interação com Ideologia + 
          # PC2 +
          # Antipt +
          # Corrupção
mod_tip_4 <- glm(voto_b ~ pop_ed_sart + sexo + id + ed + fx_renda + 
                   PC2 +
                   antipt +
                   corrup1,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_tip_exp4 <- exp(mod_tip_4$coefficients)
# P-valor:
mod_tip_p4 <- list(summary(mod_tip_4)$coefficients[,4])


##
# Modelo 5 (Apendice):
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes Populistas Tipologia +
          # Demographic Controls: Sexo, Idade, Educação e Renda +
          # Interação com Ideologia + 
          # PC2 +
          # Antipt +
          # Corrupção +
          # Religião (Ser Evangélico)
mod_tip_5 <- glm(voto_b ~ pop_ed_sart + sexo + id + ed + fx_renda + 
                   PC2 +
                   antipt +
                   corrup1 +
                   relig,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_tip_exp5 <- exp(mod_tip_5$coefficients)
# P-valor:
mod_tip_p5 <- list(summary(mod_tip_5)$coefficients[,4])



##############################


## ## ## ## ## ## ## ## ##
# pop_goertz - Goertziana #
## ## ## ## ## ## ## ## ##

# Modelo 1: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
mod_gtz_1 <- glm(voto_b ~ pop_gz  + sexo + id + ed + fx_renda,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_gtz_exp <- exp(mod_gtz_1$coefficients)
# P-valor:
mod_gtz_p <- list(summary(mod_gtz_1)$coefficients[,4])


# Modelo 2: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda +
          # Ideologia
mod_gtz_2 <- glm(voto_b ~ pop_gz  + sexo + id + ed + fx_renda + ideo2.2,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_gtz_exp2 <- exp(mod_gtz_2$coefficients)
# P-valor:
mod_gtz_p2 <- list(summary(mod_gtz_2)$coefficients[,4])


# Modelo 3: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda +
          # Interação Ideologia
mod_gtz_3 <- glm(voto_b ~ pop_gz * ideo2.2  + sexo + id + ed + fx_renda,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_gtz_exp3 <- exp(mod_gtz_3$coefficients)
# P-valor:
mod_gtz_p3 <- list(summary(mod_gtz_3)$coefficients[,4])


# Modelo 4: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda +
          # Interação Ideologia + 
          # PC2
mod_gtz_4 <- glm(voto_b ~ pop_gz * ideo2.2  + sexo + id + ed + fx_renda +
                   PC2,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_gtz_exp4 <- exp(mod_gtz_4$coefficients)
# P-valor:
mod_gtz_p4 <- list(summary(mod_gtz_4)$coefficients[,4])


# Modelo 5: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda +
          # Interação Ideologia + 
          # PC2 +
          # Antipt
mod_gtz_5 <- glm(voto_b ~ pop_gz * ideo2.2  + sexo + id + ed + fx_renda +
                   PC2 +
                   antipt,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_gtz_exp5 <- exp(mod_gtz_5$coefficients)
# P-valor:
mod_gtz_p5 <- list(summary(mod_gtz_5)$coefficients[,4])


# Modelo 6: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda +
          # Interação Ideologia + 
          # PC2 +
          # Antipt #
          # Corrupção
mod_gtz_6 <- glm(voto_b ~ pop_gz * ideo2.2  + sexo + id + ed + fx_renda +
                   PC2 +
                   antipt +
                   corrup1,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_gtz_exp6 <- exp(mod_gtz_6$coefficients)
# P-valor:
mod_gtz_p6 <- list(summary(mod_gtz_6)$coefficients[,4])


# Modelo 7: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda +
          # Interação Ideologia + 
          # PC2 +
          # Antipt #
          # Corrupção +
          # Religião (Ser Evangélico)
mod_gtz_7 <- glm(voto_b ~ pop_gz * ideo2.2  + sexo + id + ed + fx_renda +
                   PC2 +
                   antipt +
                   corrup1 +
                   relig,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_gtz_exp7 <- exp(mod_gtz_7$coefficients)
# P-valor:
mod_gtz_p7 <- list(summary(mod_gtz_7)$coefficients[,4])



##############################


## ## ## ## ## ## ## ## ## ## ##
# pop_ad_n - Additive Approach #
## ## ## ## ## ## ## ## ## ## ##

# Modelo 1: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
mod_ad_1 <- glm(voto_b ~ pop_ad_n  + sexo + id + ed + fx_renda,
                 data = e19,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_ad_exp <- exp(mod_ad_1$coefficients)
# P-valor:
mod_ad_p <- list(summary(mod_ad_1)$coefficients[,4])


# Modelo 2: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + Ideologia
mod_ad_2 <- glm(voto_b ~ pop_ad_n  + sexo + id + ed + fx_renda +
                  ideo2.2,
                data = e19,
                na.action = na.omit,
                family = binomial(link = "logit"),
                maxit = 100)
# Exponencial:
mod_ad_exp2 <- exp(mod_ad_2$coefficients)
# P-valor:
mod_ad_p2 <- list(summary(mod_ad_2)$coefficients[,4])


# Modelo 3: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + Interação Ideologia
mod_ad_3 <- glm(voto_b ~ pop_ad_n*ideo2.2 + sexo + id + ed + fx_renda,
                data = e19,
                na.action = na.omit,
                family = binomial(link = "logit"),
                maxit = 100)
# Exponencial:
mod_ad_exp3 <- exp(mod_ad_3$coefficients)
# P-valor:
mod_ad_p3 <- list(summary(mod_ad_3)$coefficients[,4])


# Modelo 4: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + Interação Ideologia
          # + PC2
mod_ad_4 <- glm(voto_b ~ pop_ad_n*ideo2.2 + sexo + id + ed + fx_renda +
                  PC2,
                data = e19,
                na.action = na.omit,
                family = binomial(link = "logit"),
                maxit = 100)
# Exponencial:
mod_ad_exp4 <- exp(mod_ad_4$coefficients)
# P-valor:
mod_ad_p4 <- list(summary(mod_ad_4)$coefficients[,4])


# Modelo 5: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + Interação Ideologia
          # + PC2
          # + Corrupção (APENDICE)
mod_ad_5 <- glm(voto_b ~ pop_ad_n*ideo2.2 + sexo + id + ed + fx_renda +
                  PC2 +
                  antipt,
                data = e19,
                na.action = na.omit,
                family = binomial(link = "logit"),
                maxit = 100)
# Exponencial:
mod_ad_exp5 <- exp(mod_ad_5$coefficients)
# P-valor:
mod_ad_p5 <- list(summary(mod_ad_5)$coefficients[,4])


# Modelo 6: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + Interação Ideologia
          # + PC2
          # + Antipt (APENDICE)
          # + Corrupção
mod_ad_6 <- glm(voto_b ~ pop_ad_n*ideo2.2 + sexo + id + ed + fx_renda +
                  PC2 +
                  antipt +
                  corrup1,
                data = e19,
                na.action = na.omit,
                family = binomial(link = "logit"),
                maxit = 100)
# Exponencial:
mod_ad_exp6 <- exp(mod_ad_6$coefficients)
# P-valor:
mod_ad_p6 <- list(summary(mod_ad_6)$coefficients[,4])


# Modelo 7: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + Interação Ideologia
          # + PC2
          # + Antipt (APENDICE)
          # + Corrupção
          # + Religião (Ser Evangélico)
mod_ad_7 <- glm(voto_b ~ pop_ad_n*ideo2.2 + sexo + id + ed + fx_renda +
                  PC2 +
                  antipt +
                  corrup1 +
                  relig,
                data = e19,
                na.action = na.omit,
                family = binomial(link = "logit"),
                maxit = 100)
# Exponencial:
mod_ad_exp7 <- exp(mod_ad_7$coefficients)
# P-valor:
mod_ad_p7 <- list(summary(mod_ad_7)$coefficients[,4])



############################################################


### ### ### ### ### ### ### ### ### ###
# Apenas Com os Eleitores de Direita #
### ### ### ### ### ### ### ### ### ###

## Banco de Dados Filtrado pelos de Direita: e19_dir
## ideo: Variável de Aultolocalização Ideológica de Direita, Numérica;
# 6 a 10, onde 10 é extrema direita;



# Var.: pop_2c - Categórica
# pop_ed_sart - Tipologia
# pop_goertz - Goertziana (Minimo)
# pop_ad - Adição (To do)


## ## ## ## ## ## ## ##
# pop_2c - Categórica #
## ## ## ## ## ## ## ##

# Modelo 1: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
mod_dir_cat_1 <- glm(voto_b ~ pop_2c  + sexo + id + ed + fx_renda,
                     data = e19_dir,
                     na.action = na.omit,
                     family = binomial(link = "logit"),
                     maxit = 100)
# Exponencial:
mod_dir_cat_exp <- exp(mod_dir_cat_1$coefficients)
# P-valor:
mod_dir_cat_p <- list(summary(mod_dir_cat_1)$coefficients[,4])


# Modelo 2: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # PC2
mod_dir_cat_2 <- glm(voto_b ~ pop_2c + sexo + id + ed + fx_renda +
                       PC2,
                     data = e19_dir,
                     na.action = na.omit,
                     family = binomial(link = "logit"),
                     maxit = 100)
# Exponencial:
mod_dir_cat_exp2 <- exp(mod_dir_cat_2$coefficients)
# P-valor:
mod_dir_cat_p2 <- list(summary(mod_dir_cat_2)$coefficients[,4])


# Modelo 3: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + PC2
          # + AntiPT
mod_dir_cat_3 <- glm(voto_b ~ pop_2c + sexo + id + ed + fx_renda +
                       PC2 +
                       antipt,
                     data = e19_dir,
                     na.action = na.omit,
                     family = binomial(link = "logit"),
                     maxit = 100)
# Exponencial:
mod_dir_cat_exp3 <- exp(mod_dir_cat_3$coefficients)
# P-valor:
mod_dir_cat_p3 <- list(summary(mod_dir_cat_3)$coefficients[,4])


# Modelo 4: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + PC2
          # + AntiPT
          # + Corrupção
mod_dir_cat_4 <- glm(voto_b ~ pop_2c + sexo + id + ed + fx_renda +
                       PC2 +
                       antipt +
                       corrup1,
                     data = e19_dir,
                     na.action = na.omit,
                     family = binomial(link = "logit"),
                     maxit = 100)
# Exponencial:
mod_dir_cat_exp4 <- exp(mod_dir_cat_4$coefficients)
# P-valor:
mod_dir_cat_p4 <- list(summary(mod_dir_cat_4)$coefficients[,4])


# Modelo 5: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
# Demographic Controls: Sexo, Idade, Educação, Renda
# + PC2
# + AntiPT
# + Corrupção
# + Religião (Ser Evangélico)
mod_dir_cat_5 <- glm(voto_b ~ pop_2c + sexo + id + ed + fx_renda +
                       PC2 +
                       antipt +
                       corrup1 +
                       relig,
                     data = e19_dir,
                     na.action = na.omit,
                     family = binomial(link = "logit"),
                     maxit = 100)
# Exponencial:
mod_dir_cat_exp5 <- exp(mod_dir_cat_5$coefficients)
# P-valor:
mod_dir_cat_p5 <- list(summary(mod_dir_cat_5)$coefficients[,4])


##############################


## ## ## ## ## ## ## ## ##
# pop_goertz - Goertziana #
## ## ## ## ## ## ## ## ##

# Modelo 1: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
mod_dir_gtz_1 <- glm(voto_b ~ pop_gz  + sexo + id + ed + fx_renda,
                 data = e19_dir,
                 na.action = na.omit,
                 family = binomial(link = "logit"),
                 maxit = 100)
# Exponencial:
mod_dir_gtz_exp <- exp(mod_dir_gtz_1$coefficients)
# P-valor:
mod_dir_gtz_p <- list(summary(mod_dir_gtz_1)$coefficients[,4])


# Modelo 2: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + PC2
mod_dir_gtz_2 <- glm(voto_b ~ pop_gz  + sexo + id + ed + fx_renda +
                       PC2,
                     data = e19_dir,
                     na.action = na.omit,
                     family = binomial(link = "logit"),
                     maxit = 100)
# Exponencial:
mod_dir_gtz_exp2 <- exp(mod_dir_gtz_2$coefficients)
# P-valor:
mod_dir_gtz_p2 <- list(summary(mod_dir_gtz_2)$coefficients[,4])


# Modelo 3: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + PC2
          # + Antipt
mod_dir_gtz_3 <- glm(voto_b ~ pop_gz  + sexo + id + ed + fx_renda +
                       PC2 +
                       antipt,
                     data = e19_dir,
                     na.action = na.omit,
                     family = binomial(link = "logit"),
                     maxit = 100)
# Exponencial:
mod_dir_gtz_exp3 <- exp(mod_dir_gtz_3$coefficients)
# P-valor:
mod_dir_gtz_p3 <- list(summary(mod_dir_gtz_3)$coefficients[,4])


# Modelo 4: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + PC2
          # + Antipt
          # + Corrupção
mod_dir_gtz_4 <- glm(voto_b ~ pop_gz  + sexo + id + ed + fx_renda +
                       PC2 +
                       antipt +
                       corrup1,
                     data = e19_dir,
                     na.action = na.omit,
                     family = binomial(link = "logit"),
                     maxit = 100)
# Exponencial:
mod_dir_gtz_exp4 <- exp(mod_dir_gtz_4$coefficients)
# P-valor:
mod_dir_gtz_p4 <- list(summary(mod_dir_gtz_4)$coefficients[,4])


# Modelo 5: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + PC2
          # + Antipt
          # + Corrupção
          # + Religião (Ser Evangélico)
mod_dir_gtz_5 <- glm(voto_b ~ pop_gz  + sexo + id + ed + fx_renda +
                       PC2 +
                       antipt +
                       corrup1 +
                       relig,
                     data = e19_dir,
                     na.action = na.omit,
                     family = binomial(link = "logit"),
                     maxit = 100)
# Exponencial:
mod_dir_gtz_exp5 <- exp(mod_dir_gtz_5$coefficients)
# P-valor:
mod_dir_gtz_p5 <- list(summary(mod_dir_gtz_5)$coefficients[,4])



##############################


## ## ## ## ## ## ## ##
# pop_goertz - Adição #
## ## ## ## ## ## ## ##

# Modelo 1: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Adição + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
mod_dir_ad_1 <- glm(voto_b ~ pop_ad_n  + sexo + id + ed + fx_renda,
                     data = e19_dir,
                     na.action = na.omit,
                     family = binomial(link = "logit"),
                     maxit = 100)
# Exponencial:
mod_dir_ad_exp <- exp(mod_dir_ad_1$coefficients)
# P-valor:
mod_dir_ad_p <- list(summary(mod_dir_ad_1)$coefficients[,4])


# Modelo 2: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Adição + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + PC2
mod_dir_ad_2 <- glm(voto_b ~ pop_ad_n  + sexo + id + ed + fx_renda +
                      PC2,
                    data = e19_dir,
                    na.action = na.omit,
                    family = binomial(link = "logit"),
                    maxit = 100)
# Exponencial:
mod_dir_ad_exp2 <- exp(mod_dir_ad_2$coefficients)
# P-valor:
mod_dir_ad_p2 <- list(summary(mod_dir_ad_2)$coefficients[,4])


# Modelo 3: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Adição + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + PC2
          # + AntiPT
mod_dir_ad_3 <- glm(voto_b ~ pop_ad_n  + sexo + id + ed + fx_renda +
                      PC2 +
                      antipt,
                    data = e19_dir,
                    na.action = na.omit,
                    family = binomial(link = "logit"),
                    maxit = 100)
# Exponencial:
mod_dir_ad_exp3 <- exp(mod_dir_ad_3$coefficients)
# P-valor:
mod_dir_ad_p3 <- list(summary(mod_dir_ad_3)$coefficients[,4])


# Modelo 4: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Adição + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + PC2
          # + AntiPT
          # + Corrupção
mod_dir_ad_4 <- glm(voto_b ~ pop_ad_n  + sexo + id + ed + fx_renda +
                      PC2 +
                      antipt +
                      corrup1,
                    data = e19_dir,
                    na.action = na.omit,
                    family = binomial(link = "logit"),
                    maxit = 100)
# Exponencial:
mod_dir_ad_exp4 <- exp(mod_dir_ad_4$coefficients)
# P-valor:
mod_dir_ad_p4 <- list(summary(mod_dir_ad_4)$coefficients[,4])


# Modelo 5: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Adição + 
          # Demographic Controls: Sexo, Idade, Educação, Renda
          # + PC2
          # + AntiPT
          # + Corrupção
          # + Religião (Ser Evangélico)
mod_dir_ad_5 <- glm(voto_b ~ pop_ad_n  + sexo + id + ed + fx_renda +
                      PC2 +
                      antipt +
                      corrup1 +
                      relig,
                    data = e19_dir,
                    na.action = na.omit,
                    family = binomial(link = "logit"),
                    maxit = 100)
# Exponencial:
mod_dir_ad_exp5 <- exp(mod_dir_ad_5$coefficients)
# P-valor:
mod_dir_ad_p5 <- list(summary(mod_dir_ad_5)$coefficients[,4])
