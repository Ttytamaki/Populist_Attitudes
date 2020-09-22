# ---
# 03.2 - Tabelas Stargazer: Regressões 21/09/2020 (1a Versão do Paper)
# Criando Tabelas das Regressões: Atitudes Populistas
# Atitudes Populistas e Voto em Bolsonaro em 2018 (2020)
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 21/09/2020
# ---


## PREAMBULO -------------------------------------------------------------------

library(tidyverse)
library(here)
library(stargazer)


source(here::here("00 - Tratando e Criando as Variaveis.R"), encoding = "UTF-8")
source(here::here("02 - Regressoes.R"), encoding = "UTF-8")



#######################
# Atitudes Populistas #
####################### 

# Modelo 1: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica (mod_cat_1) + 
          # Demographic Controls: Sexo, Idade, Educação, Renda

stargazer(title = "Modelo 1: Voto x Atitudes Populistas Cat", 
          mod_cat_1, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda"
          ),
          p.auto = F,
          p = mod_cat_p,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "01 - AP Cat. + Demographic Controls.html")
)


##
# Modelo 2:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica (mod_cat_2) + 
          # Demographic Controls: Sexo, Idade, Educação, Renda +
          # Ideologia

stargazer(title = "Modelo 2: Voto x Atitudes Populistas Cat", 
          mod_cat_2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "Ideologia: Direita (ref.: Centro)",
                             "Ideologia: Esquerda"
          ),
          p.auto = F,
          p = mod_cat_p2,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "02 - AP Cat. + Demographic Controls & Ideo.html")
)


##
# Modelo 3:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica (mod_cat_3) + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# Interação com Ideologia

stargazer(title = "Modelo 3: Voto x Atitudes Populistas Cat", 
          mod_cat_3, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ideologia: Direita (ref.: Centro)",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "Direita e Populista (ref.: Centro e Populista)",
                             "Esquerda e Populista"
          ),
          p.auto = F,
          p = mod_cat_p3,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "03 - AP Cat. + Demographic Controls & Interaction w. Ideo.html")
)


##
# Modelo 4:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica (mod_cat_4) + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# Interação com Ideologia +
# PC2

stargazer(title = "Modelo 4: Voto x Atitudes Populistas Cat", 
          mod_cat_4, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ideologia: Direita (ref.: Centro)",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Direita e Populista (ref.: Centro e Populista)",
                             "Esquerda e Populista"
          ),
          p.auto = F,
          p = mod_cat_p4,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "04 - AP Cat. + Demographic Controls & Interaction w. Ideo & PC2.html")
)


##
# Final:
# Var. Dep: Voto em Bolsonaro
# Var. Ind.: Step-Wise -> Modelo 1 > Modelo 4

md <- c("Demographic Controls", "+ Ideologia",
        "+ Interação Ideologia", "+ PC2")

stargazer(title = "Voto x Atitudes Populistas - Categórica", 
          mod_cat_1, mod_cat_2, mod_cat_3, mod_cat_4, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1), 
          column.labels = c(md),
          #dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Direita e Populista (ref.: Centro e Populista)",
                             "Esquerda e Populista",
                             "Ideologia: Direita (ref.: Centro)",
                             "Ideologia: Esquerda"
          ),
          p.auto = F,
          p = c(mod_cat_p, mod_cat_p2, mod_cat_p3, mod_cat_p4),
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "05 - Atitudes Populistas Cat., Step-Wise.html")
)


##
# Modelo 5:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica (mod_cat_4) + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# Interação com Ideologia +
# PC2 +
# Antipt

stargazer(title = "Modelo 5: Voto x Atitudes Populistas Cat", 
          mod_cat_5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ideologia: Direita (ref.: Centro)",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Direita e Populista (ref.: Centro e Populista)",
                             "Esquerda e Populista"
          ),
          p.auto = F,
          p = mod_cat_p5,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "06 - AP Cat. + Demographic Controls & Interaction w. Ideo & PC2 & Antipt.html")
)


##
# Modelo 6:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica (mod_cat_4) + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# Interação com Ideologia +
# PC2 +
# Antipt +
# Corrupção

stargazer(title = "Modelo 6: Voto x Atitudes Populistas Cat", 
          mod_cat_6, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ideologia: Direita (ref.: Centro)",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Percepção da Corrupção",
                             "Direita e Populista (ref.: Centro e Populista)",
                             "Esquerda e Populista"
          ),
          p.auto = F,
          p = mod_cat_p6,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "07 - AP Cat. + Demographic Controls & Interaction w. Ideo & PC2 & Antipt & Corrupção.html")
)


##
# Modelo 7:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica (mod_cat_4) + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# Interação com Ideologia +
# PC2 +
# Antipt +
# Corrupção +
# Religião (Ser Evangélico)

stargazer(title = "Modelo 7: Voto x Atitudes Populistas Cat", 
          mod_cat_7, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ideologia: Direita (ref.: Centro)",
                             "Ideologia: Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Percepção da Corrupção",
                             "Ser Evangélico (ref.: não ser)",
                             "Direita e Populista (ref.: Centro e Populista)",
                             "Esquerda e Populista"
          ),
          p.auto = F,
          p = mod_cat_p7,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "08 - AP Cat. + Demographic Controls & Interaction w. Ideo & PC2 & Antipt & Corrupção & Relig.html")
)


##
# Final (Apendice):
# Var. Dep: Voto em Bolsonaro
# Var. Ind.: Step-Wise -> Modelo 1 > Modelo 7

md <- c("Demographic Controls", "+ Ideologia",
        "+ Interação Ideologia", "+ PC2", 
        "(Ap.) + Antipt", "(Ap.) + Corrupção.", "(Ap.) + Religião")

stargazer(title = "Voto x Atitudes Populistas - Categórica", 
          mod_cat_1, mod_cat_2, mod_cat_3, mod_cat_4, mod_cat_5, mod_cat_6, mod_cat_7, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1, 1, 1), 
          column.labels = c(md),
          #dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Percepção da Corrupção",
                             "Ser Evangélico (ref.: não ser)",
                             "Direita e Populista (ref.: Centro e Populista)",
                             "Esquerda e Populista",
                             "Ideologia: Direita (ref.: Centro)",
                             "Ideologia: Esquerda"
          ),
          p.auto = F,
          p = c(mod_cat_p, mod_cat_p2, mod_cat_p3, mod_cat_p4, mod_cat_p5, mod_cat_p6, mod_cat_p7),
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "09 - (APENDICE) Atitudes Populistas Cat., Step-Wise.html")
)


##################################
# Atitudes Populistas Goertziana #
##################################

# Modelo 1: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana (mod_gtz_1) + 
# Demographic Controls: Sexo, Idade, Educação, Renda

stargazer(title = "Modelo 1: Voto x Atitudes Populistas Goertz", 
          mod_gtz_1, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Atitudes Populistas",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda"
          ),
          p.auto = F,
          p = mod_gtz_p,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "10 - AP  + Demographic Controls.html")
)


# Modelo 2: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana (mod_gtz_2) + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# Ideologia
stargazer(title = "Modelo 2: Voto x Atitudes Populistas Goertz", 
          mod_gtz_2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Atitudes Populistas",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda"
          ),
          p.auto = F,
          p = mod_gtz_p2,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "11 - AP  + Demographic Controls & Ideo .html")
)



# Modelo 3: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana (mod_gtz_3) + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# Interação Ideologia
stargazer(title = "Modelo 3: Voto x Atitudes Populistas Goertz", 
          mod_gtz_3, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Atitudes Populistas",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "Direita e Atitudes Populistas (ref.: Centro e Atitudes Populistas)",
                             "Esquerda e Atitudes Populistas"
          ),
          p.auto = F,
          p = mod_gtz_p3,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "12 - AP  + Demographic Controls & Interação Ideo .html")
)



# Modelo 4: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# Interação Ideologia + 
# PC2
stargazer(title = "Modelo 4: Voto x Atitudes Populistas Goertz", 
          mod_gtz_4, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Atitudes Populistas",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Direita e Atitudes Populistas (ref.: Centro e Atitudes Populistas)",
                             "Esquerda e Atitudes Populistas"
          ),
          p.auto = F,
          p = mod_gtz_p4,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "13 - AP  + Demographic Controls & Interação Ideo & PC2 .html")
)


##
# Final:
# Var. Dep: Voto em Bolsonaro
# Var. Ind.: Step-Wise -> Modelo 1 > Modelo 4

md <- c("Demographic Controls", "+ Ideologia",
        "+ Interação Ideologia", "+ PC2")

stargazer(title = "Voto x Atitudes Populistas - Goertziana", 
          mod_gtz_1, mod_gtz_2, mod_gtz_3, mod_gtz_4, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1), 
          column.labels = c(md),
          #dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Atitudes Populistas",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Direita e Atitudes Populistas (ref.: Centro e Atitudes Populistas)",
                             "Esquerda e Atitudes Populistas",
                             "Ideologia: Direita (ref.: Centro)",
                             "Ideologia: Esquerda"
          ),
          p.auto = F,
          p = c(mod_gtz_p, mod_gtz_p2, mod_gtz_p3, mod_gtz_p4),
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "14 - Atitudes Populistas Goertz, Step-Wise.html")
)


# Modelo 5: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# Interação Ideologia + 
# PC2 +
# Antipt
stargazer(title = "Modelo 5: Voto x Atitudes Populistas Goertz", 
          mod_gtz_5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Atitudes Populistas",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: Não ser)",
                             "Direita e Atitudes Populistas (ref.: Centro e Atitudes Populistas)",
                             "Esquerda e Atitudes Populistas"
          ),
          p.auto = F,
          p = mod_gtz_p5,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "15 - AP  + Demographic Controls & Interação Ideo & PC2 & Antipt .html")
)


# Modelo 5: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# Interação Ideologia + 
# PC2 +
# Antipt +
# Corrupção 
stargazer(title = "Modelo 5: Voto x Atitudes Populistas Goertz", 
          mod_gtz_6, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Atitudes Populistas",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: Não ser)",
                             "Percepção da Corrupção",
                             "Direita e Atitudes Populistas (ref.: Centro e Atitudes Populistas)",
                             "Esquerda e Atitudes Populistas"
          ),
          p.auto = F,
          p = mod_gtz_p6,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "16 - AP  + Demographic Controls & Interação Ideo & PC2 & Antipt & Corrup .html")
)


# Modelo 6: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Goertziana + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# Interação Ideologia + 
# PC2 +
# Antipt +
# Corrupção +
# Religião (Ser Evangélico)
stargazer(title = "Modelo 6: Voto x Atitudes Populistas Goertz", 
          mod_gtz_7, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Atitudes Populistas",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: Não ser)",
                             "Percepção da Corrupção",
                             "Ser Evangélico (ref.: Não ser)",
                             "Direita e Atitudes Populistas (ref.: Centro e Atitudes Populistas)",
                             "Esquerda e Atitudes Populistas"
          ),
          p.auto = F,
          p = mod_gtz_p7,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "17 - AP  + Demographic Controls & Interação Ideo & PC2 & Antipt & Corrup & Relig .html")
)


##
# Final (Apendice):
# Var. Dep: Voto em Bolsonaro
# Var. Ind.: Step-Wise -> Modelo 1 > Modelo 4

md <- c("Demographic Controls", "+ Ideologia",
        "+ Interação Ideologia", "+ PC2", 
        "(Ap.) + Antipt", "(Ap.) + Corrp.", "(Ap.) + Relig")

stargazer(title = "Voto x Atitudes Populistas - Goertziana", 
          mod_gtz_1, mod_gtz_2, mod_gtz_3, mod_gtz_4, mod_gtz_5, mod_gtz_6, mod_gtz_7, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1, 1, 1, 1), 
          column.labels = c(md),
          #dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Atitudes Populistas",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Percepção da Corrupção",
                             "Ser Evangélico (ref.: não ser)",
                             "Direita e Atitudes Populistas (ref.: Centro e Atitudes Populistas)",
                             "Esquerda e Atitudes Populistas",
                             "Ideologia: Direita (ref.: Centro)",
                             "Ideologia: Esquerda"
          ),
          p.auto = F,
          p = c(mod_gtz_p, mod_gtz_p2, mod_gtz_p3, mod_gtz_p4, mod_gtz_p5, mod_gtz_p6, mod_gtz_p7),
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "18 - (APENDICE) Atitudes Populistas Goertz, Step-Wise.html")
)



#################################
# Atitudes Populistas Tipologia #
#################################

# Modelo 1: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Tipologia (mod_tip_1) + 
# Demographic Controls: Sexo, Idade, Educação, Renda
stargazer(title = "Modelo 1: Voto x Tipologia Atitudes Populistas", 
          mod_tip_1, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populistas de Centro (ref.: Não Populistas)",
                             "Populistas de Direita",
                             "Populistas de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda"
          ),
          p.auto = F,
          p = mod_tip_p,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "20 - Tipologia AP  + Demographic Controls.html")
)


##
# Modelo 2:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# PC2
stargazer(title = "Modelo 2: Voto x Tipologia Atitudes Populistas", 
          mod_tip_2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populistas de Centro (ref.: Não Populistas)",
                             "Populistas de Direita",
                             "Populistas de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2"
          ),
          p.auto = F,
          p = mod_tip_p2,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "21 - Tipologia AP  + Demographic Controls & PC2.html")
)


##
# Modelo 4:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# PC2 +
# Antipt
stargazer(title = "Modelo 3: Voto x Tipologia Atitudes Populistas", 
          mod_tip_3, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populistas de Centro (ref.: Não Populistas)",
                             "Populistas de Direita",
                             "Populistas de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)"
          ),
          p.auto = F,
          p = mod_tip_p3,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "22 - Tipologia AP  + Demographic Controls & PC2 & AntiPT.html")
)


##
# Modelo 5:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# PC2 +
# Antipt +
# Corrupçãp
stargazer(title = "Modelo 4: Voto x Tipologia Atitudes Populistas", 
          mod_tip_4, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populistas de Centro (ref.: Não Populistas)",
                             "Populistas de Direita",
                             "Populistas de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Percepção da Corrupção"
          ),
          p.auto = F,
          p = mod_tip_p4,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "23 - Tipologia AP  + Demographic Controls & PC2 & AntiPT & Corrup.html")
)


##
# Modelo 6:
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes populistas Categórica + 
# Demographic Controls: Sexo, Idade, Educação, Renda +
# PC2 +
# Antipt +
# Corrupção +
# Religião (Ser Evangélico)
stargazer(title = "Modelo 5: Voto x Tipologia Atitudes Populistas", 
          mod_tip_5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populistas de Centro (ref.: Não Populistas)",
                             "Populistas de Direita",
                             "Populistas de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Percepção da Corrupção",
                             "Ser Evangélico (ref.: não ser)"
          ),
          p.auto = F,
          p = mod_tip_p5,
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "24 - Tipologia AP  + Demographic Controls & PC2 & AntiPT & Corrup & Relig.html")
)


##
# Final (Apendice):
# Var. Dep: Voto em Bolsonaro
# Var. Ind.: Step-Wise -> Modelo 1 > Modelo 4

md <- c("Demographic Controls", "+ PC2", 
        "(Ap.) + Antipt", "(Ap.) + Corrp.", "(Ap.) + Relig")

stargazer(title = "Voto x Atitudes Populistas - Tipologia", 
          mod_tip_1, mod_tip_2, mod_tip_3, mod_tip_4, mod_tip_5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1, 1), 
          column.labels = c(md),
          #dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populistas de Centro (ref.: Não Populistas)",
                             "Populistas de Direita",
                             "Populistas de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Percepção da Corrupção",
                             "Ser Evangélico (ref.: não ser)"
          ),
          p.auto = F,
          p = c(mod_tip_p, mod_tip_p2, mod_tip_p3, mod_tip_p4, mod_tip_p5),
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "25 - (APENDICE) Atitudes Populistas Tipologia, Step-Wise.html")
          
)



##############################
# Atitudes Populistas Adição #
##############################

##
# Final (Apendice):
# Var. Dep: Voto em Bolsonaro
# Var. Ind.: Step-Wise -> Modelo 1 > Modelo 7

md <- c("Demographic Controls", "+ Ideologia",
        "+ Interação Ideologia", "+ PC2", 
        "(Ap.) + Antipt", "(Ap.) + Corrupção.", "(Ap.) + Religião")

stargazer(title = "Voto x Atitudes Populistas - Adição", 
          mod_ad_1, mod_ad_2, mod_ad_3, mod_ad_4, mod_ad_5, mod_ad_6, mod_ad_7, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1, 1, 1, 1), 
          column.labels = c(md),
          #dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populismo (Escala Adição)",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Percepção da Corrupção",
                             "Ser Evangélico (ref.: não ser)",
                             "Direita e Populista (ref.: Centro e Populista)",
                             "Esquerda e Populista",
                             "Ideologia: Direita (ref.: Centro)",
                             "Ideologia: Esquerda"
          ),
          p.auto = F,
          p = c(mod_ad_p, mod_ad_p2, mod_ad_p3, mod_ad_p4, mod_ad_p5, mod_ad_p6, mod_ad_p7),
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "30 - (APENDICE) Atitudes Populistas Ad., Step-Wise.html")
)




########################################
# Atitudes Populistas - Filtro Direita #
########################################


##################################
# Atitudes Populistas Categórica #
##################################
##
# Final:
# Var. Dep: Voto em Bolsonaro
# Var. Ind.: Step-Wise -> Modelo 1 > Modelo 5

md <- c("Demographic Controls", "+ PC2", 
        "(Ap.) + Antipt", "(Ap.) + Corrupção.", "(Ap.) + Religião")

stargazer(title = "(Apenas Direita) Voto x Atitudes Populistas - Categórica", 
          mod_dir_cat_1, mod_dir_cat_2, mod_dir_cat_3, mod_dir_cat_4, mod_dir_cat_5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1, 1), 
          column.labels = c(md),
          #dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Ser Populista (ref.: Não ser)",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Percepção da Corrupção",
                             "Ser Evangélico (ref.: não ser)"
          ),
          p.auto = F,
          p = c(mod_dir_cat_p, mod_dir_cat_p2, mod_dir_cat_p3, mod_dir_cat_p4, mod_dir_cat_p5),
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "40 - (APENAS DIREITA) Atitudes Populistas Cat., Step-Wise.html")
)


##################################
# Atitudes Populistas Goertziana #
##################################
##
# Final:
# Var. Dep: Voto em Bolsonaro
# Var. Ind.: Step-Wise -> Modelo 1 > Modelo 5

md <- c("Demographic Controls", "+ PC2", 
        "(Ap.) + Antipt", "(Ap.) + Corrupção.", "(Ap.) + Religião")

stargazer(title = "(Apenas Direita) Voto x Atitudes Populistas - Goertziana", 
          mod_dir_gtz_1, mod_dir_gtz_2, mod_dir_gtz_3, mod_dir_gtz_4, mod_dir_gtz_5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1, 1), 
          column.labels = c(md),
          #dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Atitudes Populistas",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Percepção da Corrupção",
                             "Ser Evangélico (ref.: não ser)"
          ),
          p.auto = F,
          p = c(mod_dir_gtz_p, mod_dir_gtz_p2, mod_dir_gtz_p3, mod_dir_gtz_p4, mod_dir_gtz_p5),
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "50 [!!!] - (APENAS DIREITA) Atitudes Populistas Goertz, Step-Wise.html")
)


##############################
# Atitudes Populistas Adição #
##############################
##
# Final:
# Var. Dep: Voto em Bolsonaro
# Var. Ind.: Step-Wise -> Modelo 1 > Modelo 5

md <- c("Demographic Controls", "+ PC2", 
        "(Ap.) + Antipt", "(Ap.) + Corrupção.", "(Ap.) + Religião")

stargazer(title = "(Apenas Direita) Voto x Atitudes Populistas - Goertziana", 
          mod_dir_ad_1, mod_dir_ad_2, mod_dir_ad_3, mod_dir_ad_4, mod_dir_ad_5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1, 1), 
          column.labels = c(md),
          #dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Ser Populista (ref.: Não ser)",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Faixa de Renda",
                             "PC2",
                             "Ser Antipetista (ref.: não ser)",
                             "Percepção da Corrupção",
                             "Ser Evangélico (ref.: não ser)"
          ),
          p.auto = F,
          p = c(mod_dir_ad_p, mod_dir_ad_p2, mod_dir_ad_p3, mod_dir_ad_p4, mod_dir_ad_p5),
          report = ('vc*sp'), out = here::here("anexos/1a Versao", 
                                               "60 [!!!] - (APENAS DIREITA) Atitudes Populistas Adição, Step-Wise.html")
)
