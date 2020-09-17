<<<<<<< HEAD
# ---
# 03 - Tabelas Stargazer 
# Criando Tabelas das Regressões: Atitudes Populistas
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


source(here::here("00 - Tratando e Criando as Variaveis.R"), encoding = "UTF-8")
source(here::here("02 - Regressoes.R"), encoding = "UTF-8")


#################################
# Tipologia Atitudes Populistas #
################################# 

###
# Tabela 1: Modelo 1
stargazer(title = "Modelo 1: Voto x Atitudes Populistas", 
          m1, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação"
          ),
          p.auto = F,
          p = p1,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "01 - Voto x AP, Sexo, Idade, Ed.html")
)


###
# Tabela 2: Modelo 2
stargazer(title = "Modelo 2: Voto x Atitudes Populistas", 
          m2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista",
                             "Ser de Direita",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Ser de Direita e Populista",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p2,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "02 - Voto x AP e Ideo, Sexo, Idade, Ed.html")
)


###
# Tabela 3: Modelo 3
stargazer(title = "Modelo 3: Voto x Atitudes Populistas", 
          m3, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista",
                             "Ser de Direita",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "Ser de Direita e Populista",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p3,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "03 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup.html")
)


###
# Tabela 4: Modelo 4
stargazer(title = "Modelo 4: Voto x Atitudes Populistas", 
          m4, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista",
                             "Ser de Direita",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser de Direita e Populista",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p4,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "04 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup, PC2.html")
)


###
# Tabela 5: Modelo 5
stargazer(title = "Modelo 5: Voto x Atitudes Populistas", 
          m5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser Antipetista (ref.: Não)",
                             "Ser de Direita e Populista (ref.: Centro e Populista)",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "05 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipt.html")
)


###
# Tabela 6: Modelo 6
stargazer(title = "Modelo 6: Voto x Atitudes Populistas", 
          m6, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser Antipetista (ref.: Não)",
                             "Faixa de Renda",
                             "Ser de Direita e Populista (ref.: Centro e Populista)",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p6,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "06 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipt, Renda.html")
)


###
# Tabela 7: Modelo 7
stargazer(title = "Modelo 7: Voto x Atitudes Populistas", 
          m7, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser Antipetista (ref.: Não)",
                             "Faixa de Renda",
                             "Religião: Evangélico (ref.: Outras)",
                             "Ser de Direita e Populista (ref.: Centro e Populista)",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p7,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "07 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipt, Renda, Religiao.html")
)


###
# Tabela 8: Setpwise Modelos 1-7


md <- c("Base", "+ Interação com Ideologia",
        "+ Corrupção", "+ PC2", "+ Antipetismo",
        "+ Renda", "+ Religião")
stargazer(title = "Modelo 7: Voto x Atitudes Populistas", 
          m1, m2, m3, m4, m5, m6, m7, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1, 1, 1, 1), 
          column.labels = c(md),
          #dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser Antipetista (ref.: Não)",
                             "Faixa de Renda",
                             "Religião: Evangélico (ref.: Outras)",
                             "Ser de Direita e Populista (ref.: Centro e Populista)",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = c(p1, p2, p3, p4, p5, p6, p7),
          report = ('vc*sp'), out = here::here("anexos", 
                                               "08 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipt, Renda, Religiao.html")
)




#############################################
# Tipologia Atitudes Populistas x Ideologia #
#############################################


# Tabela 10: Modelo 1
stargazer(title = "Modelo 1: Voto x Tipologia Atitudes Populistas", 
          mt1, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação"
                             ),
          p.auto = F,
          p = p_t1,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "10 - Voto x TAP e Ideo, Sexo, Idade, Ed.html")
)


# Tabela 11: Modelo 2
stargazer(title = "Modelo 2: Voto x Tipologia Atitudes Populistas", 
          mt2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Corrupção"
          ),
          p.auto = F,
          p = p_t2,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "11 - Voto x TAP e Ideo, Sexo, Idade, Ed, Corrup.html")
)



# Tabela 12: Modelo 3
stargazer(title = "Modelo 3: Voto x Tipologia Atitudes Populistas", 
          mt3, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "PC2"
          ),
          p.auto = F,
          p = p_t3,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "12 - Voto x TAP e Ideo, Sexo, Idade, Ed, Corrup, PC2.html")
)


# Tabela 13: Modelo 4
stargazer(title = "Modelo 4: Voto x Tipologia Atitudes Populistas", 
          mt4, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "PC2",
                             "Antipetismo"
          ),
          p.auto = F,
          p = p_t4,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "13 - Voto x TAP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipetismo.html")
)


# Tabela 14: Modelo 5
stargazer(title = "Modelo 5: Voto x Tipologia Atitudes Populistas", 
          mt5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "PC2",
                             "Antipetismo",
                             "Religião: Evangélico"
          ),
          p.auto = F,
          p = p_t5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "14 - Voto x TAP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipetismo, Relig.html")
)


# Tabela 15: Modelo 6
stargazer(title = "Modelo 6: Voto x Tipologia Atitudes Populistas", 
          mt6, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "PC2",
                             "Antipetismo",
                             "Religião: Evangélico",
                             "Renda"
          ),
          p.auto = F,
          p = p_t6,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "15 - Voto x TAP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipetismo, Relig, Renda.html")
)


###
# Tabela 16: Setpwise Modelos 1-6


md <- c("Base", "+ Corrupção", "+ PC2", "+ Antipetismo",
        "+ Religião",
        "+ Renda")

stargazer(title = "Modelo 7: Voto x Atitudes Populistas", 
          mt1, mt2, mt3, mt4, mt5, mt6, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1, 1, 1),
          column.labels = c(md),
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser Antipetista (ref.: Não)",
                             "Religião: Evangélico (ref.: Outras)",
                             "Faixa de Renda"
          ),
          p.auto = F,
          p = c(p_t1, p_t2, p_t3, p_t4, p_t5, p_t6),
          report = ('vc*sp'), out = here::here("anexos", 
                                               "16 - Voto x TAP , Sexo, Idade, Ed, Corrup, PC2, Antipt, Renda, Religiao.html")
)

=======
library(tidyverse)
library(here)
library(stargazer)


source(here::here("00 - Tratando e Criando as Variaveis.R"), encoding = "UTF-8")
source(here::here("02 - Regressoes.R"), encoding = "UTF-8")


#################################
# Tipologia Atitudes Populistas #
################################# 

###
# Tabela 1: Modelo 1
stargazer(title = "Modelo 1: Voto x Atitudes Populistas", 
          m1, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação"
          ),
          p.auto = F,
          p = p1,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "01 - Voto x AP, Sexo, Idade, Ed.html")
)


###
# Tabela 2: Modelo 2
stargazer(title = "Modelo 2: Voto x Atitudes Populistas", 
          m2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista",
                             "Ser de Direita",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Ser de Direita e Populista",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p2,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "02 - Voto x AP e Ideo, Sexo, Idade, Ed.html")
)


###
# Tabela 3: Modelo 3
stargazer(title = "Modelo 3: Voto x Atitudes Populistas", 
          m3, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista",
                             "Ser de Direita",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "Ser de Direita e Populista",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p3,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "03 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup.html")
)


###
# Tabela 4: Modelo 4
stargazer(title = "Modelo 4: Voto x Atitudes Populistas", 
          m4, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista",
                             "Ser de Direita",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser de Direita e Populista",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p4,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "04 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup, PC2.html")
)


###
# Tabela 5: Modelo 5
stargazer(title = "Modelo 5: Voto x Atitudes Populistas", 
          m5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser Antipetista (ref.: Não)",
                             "Ser de Direita e Populista (ref.: Centro e Populista)",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "05 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipt.html")
)


###
# Tabela 6: Modelo 6
stargazer(title = "Modelo 6: Voto x Atitudes Populistas", 
          m6, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser Antipetista (ref.: Não)",
                             "Faixa de Renda",
                             "Ser de Direita e Populista (ref.: Centro e Populista)",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p6,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "06 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipt, Renda.html")
)


###
# Tabela 7: Modelo 7
stargazer(title = "Modelo 7: Voto x Atitudes Populistas", 
          m7, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser Antipetista (ref.: Não)",
                             "Faixa de Renda",
                             "Religião: Evangélico (ref.: Outras)",
                             "Ser de Direita e Populista (ref.: Centro e Populista)",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = p7,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "07 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipt, Renda, Religiao.html")
)


###
# Tabela 8: Setpwise Modelos 1-7


md <- c("Base", "+ Interação com Ideologia",
        "+ Corrupção", "+ PC2", "+ Antipetismo",
        "+ Renda", "+ Religião")
stargazer(title = "Modelo 7: Voto x Atitudes Populistas", 
          m1, m2, m3, m4, m5, m6, m7, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1, 1, 1, 1), 
          column.labels = c(md),
          #dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista (ref.: Não Populista)",
                             "Ser de Direita (ref.: Centro)",
                             "Ser de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser Antipetista (ref.: Não)",
                             "Faixa de Renda",
                             "Religião: Evangélico (ref.: Outras)",
                             "Ser de Direita e Populista (ref.: Centro e Populista)",
                             "Ser de Esquerda e Populista"
          ),
          p.auto = F,
          p = c(p1, p2, p3, p4, p5, p6, p7),
          report = ('vc*sp'), out = here::here("anexos", 
                                               "08 - Voto x AP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipt, Renda, Religiao.html")
)




#############################################
# Tipologia Atitudes Populistas x Ideologia #
#############################################


# Tabela 10: Modelo 1
stargazer(title = "Modelo 1: Voto x Tipologia Atitudes Populistas", 
          mt1, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação"
                             ),
          p.auto = F,
          p = p_t1,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "10 - Voto x TAP e Ideo, Sexo, Idade, Ed.html")
)


# Tabela 11: Modelo 2
stargazer(title = "Modelo 2: Voto x Tipologia Atitudes Populistas", 
          mt2, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Corrupção"
          ),
          p.auto = F,
          p = p_t2,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "11 - Voto x TAP e Ideo, Sexo, Idade, Ed, Corrup.html")
)



# Tabela 12: Modelo 3
stargazer(title = "Modelo 3: Voto x Tipologia Atitudes Populistas", 
          mt3, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "PC2"
          ),
          p.auto = F,
          p = p_t3,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "12 - Voto x TAP e Ideo, Sexo, Idade, Ed, Corrup, PC2.html")
)


# Tabela 13: Modelo 4
stargazer(title = "Modelo 4: Voto x Tipologia Atitudes Populistas", 
          mt4, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "PC2",
                             "Antipetismo"
          ),
          p.auto = F,
          p = p_t4,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "13 - Voto x TAP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipetismo.html")
)


# Tabela 14: Modelo 5
stargazer(title = "Modelo 5: Voto x Tipologia Atitudes Populistas", 
          mt5, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "PC2",
                             "Antipetismo",
                             "Religião: Evangélico"
          ),
          p.auto = F,
          p = p_t5,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "14 - Voto x TAP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipetismo, Relig.html")
)


# Tabela 15: Modelo 6
stargazer(title = "Modelo 6: Voto x Tipologia Atitudes Populistas", 
          mt6, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = "Voto em Bolsonaro",
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Corrupção",
                             "PC2",
                             "Antipetismo",
                             "Religião: Evangélico",
                             "Renda"
          ),
          p.auto = F,
          p = p_t6,
          report = ('vc*sp'), out = here::here("anexos", 
                                               "15 - Voto x TAP e Ideo, Sexo, Idade, Ed, Corrup, PC2, Antipetismo, Relig, Renda.html")
)


###
# Tabela 16: Setpwise Modelos 1-6


md <- c("Base", "+ Corrupção", "+ PC2", "+ Antipetismo",
        "+ Religião",
        "+ Renda")

stargazer(title = "Modelo 7: Voto x Atitudes Populistas", 
          mt1, mt2, mt3, mt4, mt5, mt6, 
          header = F, type="latex",
          dep.var.caption = c("Variável Dependente"),
          dep.var.labels = ("Voto em Bolsonaro x Outros"),
          column.separate = c(1, 1, 1, 1, 1, 1),
          column.labels = c(md),
          perl = TRUE,
          covariate.labels=c("Populista de Centro(ref.: Não Populista)",
                             "Populista de Direita",
                             "Populista de Esquerda",
                             "Sexo: Ser Homem (ref.: ser Mulher)",
                             "Idade",
                             "Educação",
                             "Percepção da Corrupção",
                             "PC2",
                             "Ser Antipetista (ref.: Não)",
                             "Religião: Evangélico (ref.: Outras)",
                             "Faixa de Renda"
          ),
          p.auto = F,
          p = c(p_t1, p_t2, p_t3, p_t4, p_t5, p_t6),
          report = ('vc*sp'), out = here::here("anexos", 
                                               "16 - Voto x TAP , Sexo, Idade, Ed, Corrup, PC2, Antipt, Renda, Religiao.html")
)

>>>>>>> 48d1554cb387271c0416869ba405be0c6ad8a4f1
