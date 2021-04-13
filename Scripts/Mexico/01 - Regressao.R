# ---
# 01 - Regressões
# Modelos de Regressão
# ECPR: Brazil, Mexico, and Chile
# ---
# Eduardo Ryô Tamaki
# Mestrando em Ciência Política da UFMG
# e-mail: eduardo.rtamaki@gmail.com
# ---
# 08/04/2021
# ---


## PREAMBULO -------------------------------------------------------------------

# Limpando o environment:
rm(list = ls())


# Carregando os pacotes:
pacman::p_load(tidyverse, haven, sjlabelled, polycor, psych, tikzDevice, stargazer, ggrepel,
               texreg, sjPlot, grid, gridExtra)
options(tikzDocumentDeclaration = "\\documentclass[12pt]{article}")
# Tikz, mesmo não trabalhando com LaTeX



##########
# MEXICO #
##########


# Carregar o Banco:
load("mx.RData")




# Voto: pelepre_o3
# Ideologia: p77_o3


# Preparando a variável com nome dos candidatos:
mx <- mx %>%
  mutate(vote_name = tolower(rio::characterize(pelepre_o3))) %>%
  mutate(vote_name = str_remove_all(vote_name, "\\s[+-](.*)"))

# Arrumando o nome de um dos candidatos:
mx$vote_name[mx$vote_name == "jaime heliodoro rodrã\u008dguez calderón"] <- "jaime heliodoro rodriguez calderón"


# Agora, para melhorar a visualização, transformar as variáveis colocando a primeira letra do nome em maiusculo:
mx$vote_name <- str_to_title(mx$vote_name)


# Preparando a Variável de Ideologia:
mx <- mx %>%
  mutate(ideo = case_when(
    as.numeric(p77_o3) > 10 ~ NA_real_,
    T ~ as.numeric(p77_o3))
    )


p.vote <- mx %>%
  mutate(vote_name = case_when(
    vote_name  == "No Sabe" ~ NA_character_,
    vote_name %in% c("Anulado", "Ninguno") ~ "Invalid Ballot",
    T ~ vote_name
  )) %>%
  group_by(vote_name) %>%
  summarise(nobs = n(),
            right = mean(ideo, na.rm = T),
            pop = mean(pop_add, na.rm = T),
            popsd = sd(pop_add, na.rm = T),
            ideosd = sd(ideo, na.rm = T)
            ) %>%
  filter(nobs > 30 & vote_name != "Missing") %>%
  na.omit()

p.vote_graf <- p.vote %>%
  ggplot(aes(pop, right, label = vote_name)) + 
  geom_pointrange(aes(ymin = right - 1.96 * (ideosd/sqrt(nobs)), 
                      ymax = right + 1.96*(ideosd/sqrt(nobs)))
                  ) +
  geom_errorbarh(aes(xmin = pop - 1.96 * (popsd/sqrt(nobs)),
                     xmax = pop + 1.96 * (popsd/sqrt(nobs)), height = 0)
                 ) +
  theme_minimal() +
  geom_text_repel(show.legend = F) +
  labs(
    x = "Populist Attitudes (1--5 additive index)",
    y = "left (0) -- Right (10) Ideology"
  ) 



tikz(file = "Fig_Tables/plot_descriptive_vote.tex", width = 5, height = 5)
p.vote_graf
endoffile <- dev.off()




#####------------------


### Regressões
##
# Regressões Base
##

m1 <- glm(voto_amlo ~ pop_2c + sexo + id + ed + fx_renda,
          data = mx,
          na.action = na.omit,
          family = binomial(link = "logit"))


m2 <- glm(voto_amlo ~ ideo + sexo + id + ed + fx_renda,
          data = mx,
          na.action = na.omit, 
          family = binomial(link = "logit"))

m3 <- glm(voto_amlo ~  ideo * pop_2c + sexo + id + ed + fx_renda, 
          data = mx,
          na.action = na.omit,
          family = binomial(link = "logit"))

# Pop_add como 4o e 5o Modelo:

m4 <- glm(voto_amlo ~ pop_add + sexo + id + ed+ fx_renda, 
          data = mx,
          na.action = na.omit,
          family = binomial(link = "logit"))

m5 <- glm(voto_amlo ~ ideo * pop_add + sexo + id + ed + fx_renda,   
          data = mx,
          na.action = na.omit,
          family = binomial(link = "logit"))


models <- list(m1, m2, m3, m4, m5)

## Exportar a Tabela: 
# Copiar e Colar o resultado no Overleaf:
texreg(l = models,
       custom.coef.names = c('Intercept','Populism (cat)','Male','Age','Education',
                             'Income','Left-right ideology',
                             'Populism (cat) * Ideology',  
                             'Populism (additive)','Populism (add) * Ideology'),
       include.deviance = F, include.loglik = F, booktabs = T, leading.zero = F, 
       single.row = F, 
       caption = 'Logistic Regression Models Predicting of Voting for Lopez Obrador in 2018',
       label = 'tab:reg1', fontsize = 'small',
       custom.model.names = paste(unlist(map(1:length(models), ~paste0('(',.x,')'))), sep= ','))


## Plotando os gráficos dos Modelos 3 e 5:
p1 <- plot_model(m3, type = 'int',color= 'bw') + 
  theme_minimal() +
  ggtitle('Populism as categorical (Model 3)') + 
  ylab('Predicted Probabilities of Voting Lopez Obrador') + 
  xlab('Left (0) - Right (10) Ideology') + 
  theme(legend.position = 'bottom') + 
  geom_density(data = mx[as.numeric(names(m3$fitted.values)),],
               mapping = aes(x = ideo), fill = 'gray30', col=c('gray80'),alpha = .2,
               inherit.aes = F) +
  labs(linetype = 'Populism') 

p2 <- plot_model(m5, type = 'int',color= 'bw', mdrt.values = 'meansd') + 
  theme_minimal() +
  ggtitle('Populism as continuous (Model 5)') + 
  ylab('Predicted Probabilities of Voting Lopez Obrador') + 
  xlab('Left (0) - Right (10) Ideology') +
  theme(legend.position = 'bottom') + 
  geom_density(data = mx[as.numeric(names(m3$fitted.values)),],
               mapping = aes(x = ideo), fill = 'gray30', col=c('gray80'),alpha = .2,
               inherit.aes = F) +
  scale_linetype_manual(values = c('solid','dotted', 'dashed'),labels = c('Mean - 1SD', 'Mean (4.05)','Mean + 1SD')) +
  labs(linetype = 'Populism') 


pdf(file = "Fig_Tables/plot_interactions.pdf", width = 6, height = 9)
grid.arrange(p1, p2, ncol=1)
endoffile <- dev.off()





##
# Regressões Dimensões Separadas
##

m1 <- glm(voto_amlo ~ m1 + ideo + sexo + id + ed + fx_renda, 
          data = mx,
          na.action = na.omit,
          family = binomial(link = "logit"))
summary(m1)

m2 <- glm(voto_amlo ~ ae1 + ideo + sexo + id + fx_renda+ ed, 
          data = mx,
          na.action = na.omit,
          family = binomial(link = "logit"))
summary(m2)

m3 <- glm(voto_amlo ~  pc1 + ideo + sexo + id + ed + fx_renda, 
          data = mx,
          na.action = na.omit,
          family = binomial(link = "logit"))
summary(m3)

m4 <- glm(voto_amlo ~ m1*ideo + sexo + id + ed + fx_renda, 
          data = mx,
          na.action = na.omit,
          family = binomial(link = "logit"))
summary(m4)

m5 <- glm(voto_amlo ~ ae1*ideo + sexo + id + fx_renda+ ed, 
          data = mx,
          na.action = na.omit,
          family = binomial(link = "logit"))
summary(m5)

m6 <- glm(voto_amlo ~  pc1*ideo + sexo + id + ed + fx_renda, 
          data = mx,
          na.action = na.omit,
          family = binomial(link = "logit"))
summary(m6)


## Texreg:
models <- list(m1,m2,m3,m4,m5,m6)
texreg(l = models,
       custom.coef.names = c('Intercept','Manichaean','Left-right ideology',
                             'Male','Age','Education','Income',
                             'Antielitism','People-centrism',
                             'Manich * Ideology','Antiel * Ideology', 'People-cent. * Ideology'),
       include.deviance = F, include.loglik = F, booktabs = T, leading.zero = F, 
       single.row = F, 
       caption = 'Logistic Regression Models Predicting of Voting for Lopez Obrador in 2018 by Sub-dimensions of Populism',
       label = 'tab:reg:subdim', fontsize = 'small',
       custom.model.names = paste(unlist(map(1:length(models), ~paste0('(',.x,')'))), sep= ','))
