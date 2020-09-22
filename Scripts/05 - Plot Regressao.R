# ---
# 05 - Plot das Regressões 
# Tratamento do Banco de Dados e Criação da Variável: Atitudes Populistas
# Atitudes Populistas e Voto em Bolsonaro em 2018 (2020)
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 20/09/2020
# ---


## PREAMBULO -------------------------------------------------------------------

library(sjPlot)
library(effects)
library(here)

source(here::here("02.2 - Regressoes _ 1a Versao Paper 21.09.R"), encoding = "UTF-8")



##################################
# Atitudes Populistas Categórica #
##################################

## Plotando o gráfico do Modelo [mod_cat_4]
##  mod_cat_4: 
    # Var. Dep.: Voto em Bolsonaro
    # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, 
      # PC2
plt_mod_cat_4 <- plot_model(mod_cat_4, type = "int",
           title = "Modelo com Dem. Controls e PC2",
           axis.title = c("Atitudes Populistas Categórica", "Voto em Bolsonaro"),
           legend.title = "Ideologia")

## Plotando o gráfico do Modelo [mod_cat_7] (APENDICE)
##  mod_cat_7: 
    # Var. Dep.: Voto em Bolsonaro
    # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, 
    # PC2 + AntiPT + Corrupção + Religião
plt_mod_cat_7 <- plot_model(mod_cat_7, type = "int",
                            title = "Modelo com Demographic Controls, PC2. AntiPT, Corrupção e Religião",
                            axis.title = c("Atitudes Populistas Categórica", "Voto em Bolsonaro"),
                            legend.title = "Ideologia")


##################################
# Atitudes Populistas Goertziana #
##################################

## Plotando o gráfico do Modelo [mod_gtz_4]
##  mod_gtz_4: 
    # Var. Dep.: Voto em Bolsonaro
    # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, 
          # PC2
plt_mod_gtz_4 <- plot_model(mod_gtz_4, type = "int",
                            title = "Modelo com Dem. Controls e PC2",
                            axis.title = c("Atitudes Populistas Goertziana", "Voto em Bolsonaro"),
                            legend.title = "Ideologia")

## Plotando o gráfico do Modelo [mod_gtz_7] (APENDICE)
##  mod_gtz_7: 
    # Var. Dep.: Voto em Bolsonaro
    # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, 
          # PC2 + AntiPT + Corrupção + Religião
plt_mod_gtz_7 <- plot_model(mod_gtz_7, type = "int",
                            title = "Modelo com Demographic Controls, PC2. AntiPT, Corrupção e Religião",
                            axis.title = c("Atitudes Populistas Goertziana", "Voto em Bolsonaro"),
                            legend.title = "Ideologia")



##############################
# Atitudes Populistas Adição #
##############################

## Plotando o gráfico do Modelo [mod_ad_4]
##  mod_ad_4: 
    # Var. Dep.: Voto em Bolsonaro
    # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, 
    # PC2
plt_mod_ad_4 <- plot_model(mod_ad_4, type = "int",
                            title = "Modelo com Dem. Controls e PC2",
                            axis.title = c("Atitudes Populistas Adição", "Voto em Bolsonaro"),
                            legend.title = "Ideologia")

## Plotando o gráfico do Modelo [mod_ad_7] (APENDICE)
##  mod_ad_7: 
    # Var. Dep.: Voto em Bolsonaro
    # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, 
    # PC2 + AntiPT + Corrupção + Religião
plt_mod_ad_7 <- plot_model(mod_ad_7, type = "int",
                            title = "Modelo com Demographic Controls, PC2. AntiPT, Corrupção e Religião",
                            axis.title = c("Atitudes Populistas Adição", "Voto em Bolsonaro"),
                            legend.title = "Ideologia")


#################################
# Atitudes Populistas Tipologia #
#################################


# Plotando o gráfico do Modelo mod_tip_2
##  mod_tip_2: 
    # Var. Dep.: Voto em Bolsonaro
    # Var. Ind.: Tipologia Atitudes Populistas, Sexo, Idade, Educação, Corrupção,
              # PC2
plt_mod_tip_2 <- plot_model(mod_tip_2, type = "pred", terms = "pop_ed_sart",
           title = "Modelo com Dem. Controls e PC2",
           axis.title = c("Tipologia de Populistas", "Voto em Bolsonaro"))

# Plotando o gráfico do Modelo mod_tip_5
##  mod_tip_5: 
    # Var. Dep.: Voto em Bolsonaro
    # Var. Ind.: Tipologia Atitudes Populistas, Sexo, Idade, Educação, Corrupção,
        # PC2, Antipetismo, Corrupção e Religião
plt_mod_tip_5 <- plot_model(mod_tip_5, type = "pred", terms = "pop_ed_sart",
                            title = "Modelo com Demographic Controls, PC2. AntiPT, Corrupção e Religião",
                            axis.title = c("Tipologia de Populistas", "Voto em Bolsonaro"))



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

########################################
# FILTRADO APENAS ELEITORES DE DIREITA #
########################################

##################################
# Atitudes Populistas Categórica #
##################################


# Plotando o gráfico do Modelo mod_dir_cat_2
##  mod_dir_cat_2: 
    # Var. Dep.: Voto em Bolsonaro
    # Var. Ind.: Atitudes Populistas Categóricas, Sexo, Idade, Educação, Corrupção,
    # PC2
plt_mod_dir_cat_2 <- plot_model(mod_dir_cat_2, type = "pred", terms = "pop_2c",
                            title = "Modelo com Dem. Controls e PC2",
                            axis.title = c("Atitudes Populistas Categóricas", "Voto em Bolsonaro"))

# Plotando o gráfico do Modelo mod_dir_cat_5
##  mod_dir_cat_5: 
    # Var. Dep.: Voto em Bolsonaro
    # Var. Ind.: Atitudes Populistas Categóricas, Sexo, Idade, Educação, Corrupção,
    # PC2, Antipetismo, Corrupção e Religião
plt_mod_dir_cat_5<- plot_model(mod_dir_cat_5, type = "pred", terms = "pop_2c",
                                title = "Modelo com Demographic Controls, PC2. AntiPT, Corrupção e Religião",
                                axis.title = c("Atitudes Populistas Categóricas", "Voto em Bolsonaro"))



##################################
# Atitudes Populistas Goertziana #
##################################

# Plotando o gráfico do Modelo mod_dir_gtz_2
##  mod_dir_gtz_2: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes Populistas Categóricas, Sexo, Idade, Educação, Corrupção,
# PC2
plt_mod_dir_gtz_2 <- plot_model(mod_dir_gtz_2, type = "pred", terms = "pop_gz",
                                title = "Modelo com Dem. Controls e PC2",
                                axis.title = c("Atitudes Populistas Goertz", "Voto em Bolsonaro"))


# Plotando o gráfico do Modelo mod_dir_gtz_5
##  mod_dir_gtz_5: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes Populistas Categóricas, Sexo, Idade, Educação, Corrupção,
# PC2, Antipetismo, Corrupção e Religião
plt_mod_dir_gtz_5<- plot_model(mod_dir_gtz_5, type = "pred", terms = "pop_gz",
                               title = "Modelo com Demographic Controls, PC2. AntiPT, Corrupção e Religião",
                               axis.title = c("Atitudes Populistas Goertz", "Voto em Bolsonaro"))



##############################
# Atitudes Populistas Adição #
##############################

# Plotando o gráfico do Modelo mod_dir_ad_2
##  mod_dir_ad_2: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes Populistas Categóricas, Sexo, Idade, Educação, Corrupção,
# PC2
plt_mod_dir_ad_2 <- plot_model(mod_dir_ad_2, type = "pred", terms = "pop_ad_n",
                                title = "Modelo com Dem. Controls e PC2",
                                axis.title = c("Atitudes Populistas Adição", "Voto em Bolsonaro"))


# Plotando o gráfico do Modelo mod_dir_gtz_5
##  mod_dir_gtz_5: 
# Var. Dep.: Voto em Bolsonaro
# Var. Ind.: Atitudes Populistas Categóricas, Sexo, Idade, Educação, Corrupção,
# PC2, Antipetismo, Corrupção e Religião
plt_mod_dir_ad_5 <- plot_model(mod_dir_ad_5, type = "pred", terms = "pop_ad_n",
                               title = "Modelo com Demographic Controls, PC2. AntiPT, Corrupção e Religião",
                               axis.title = c("Atitudes Populistas Adição", "Voto em Bolsonaro"))



