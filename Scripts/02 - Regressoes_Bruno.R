### Replication files. Models in the main paper

## Packages

library(tidyverse)
library(texreg)
library(polycor)
library(psych)
library(haven)
library(stargazer)
library(rio)
library(ggrepel)
library(tikzDevice)
options(tikzDocumentDeclaration = "\\documentclass[12pt]{article}")
library(gridExtra)

# Eu particularmente nao acho que abrir toda a source com o script novo seja uma boa ideia. Pra esse projeto talvez nao tenha problema, mas assim que voce comecar a trabalhar com bases de dados maiores, ter tudo isso aberto no environment ao mesmo tempo vai travar ou deixar o R bem mais lento. Alem disso, nao tem necessidade abrir todo objeto que ja foi feito no projeto, sendo que a maioria vai ser inutil pra esse script.
#source(here::here("00 - Tratando e Criando as Variaveis.R"), encoding = "UTF-8") 
rm(list = ls())
## Load the preprocessed data from 00 file
load('eseb2018_working.RData')

## Define the additive index as the simple mean of the three items without normalizing
## Use the raw self ideological placement instead of categorical cutoffs

e19 <- e19 %>%
  mutate(pop_add = rowMeans(select(., M1, AE1, PC1), na.rm=T),
         ideo = case_when(as.numeric(q18) > 10 ~ NA_real_,
                          T ~ as.numeric(q18)))


## PC2 is not part of the CSES populism battery and we should keep it that way.
## Measurement
pop_var <- e19 %>%
  select(M1, AE1, AE2, AE3, AE4, PC1)

poly_values <- polychoric(pop_var)

### Psych
scree(pop_var, main = 'Scree Plot for Populist Attitudes Items')

# Figure for appendix:
tikz(file = "Figures and tables/scree_plot.tex", width = 6, height = 5)
scree(pop_var, main = 'Scree Plot for Populist Attitudes Items')
dev.off()

parallel <- fa.parallel(pop_var, fm = 'minres', fa = 'fa')
print(parallel)

fa2 <- fa(r = poly_values$rho, nfactors = 1, rotate = "oblimin")
fa2
## There's three antielitism items there. Take AE1 as having the highest loading to construct the scale;

M1 <- c("What people call compromise in politics is really just selling out on one's principles.")
AE1 <- c(" Most politicians do not care about the people.")
AE2 <- c("Most politicians are trustworthy.")
AE3 <- c("Politicians are the main problem in Brazil.")
AE4 <- c("Most politicians care only about the interests of the rich and powerful.")
PC1 <- c("The people, and not politicians, should make our most important policy decisions.")


PC2 <- c("The will of the majority should always prevail, even over the 
         rights of minorities.")

# Populism descriptive table:
descs <- e19 %>%
  select(., M1, AE1, AE2, AE3, AE4, PC1) %>%
  na.omit() %>%
  summarise(across(everything(), 
                   list(mean = mean))) %>%
  t()

df.desc <- data.frame(Item = rbind(M1, AE1, AE2, AE3, AE4, PC1), Mean = descs,
                      Loading = as.vector(fa2$loadings))

stargazer(df.desc, summary = F, digits = 2,
          title = 'Populist Attitudes Items, Means, and Factor Loadings', label = "tab:efa",
          header = F)



## Figure with levels of populism by vote choice:
p.vote <- e19 %>%
  mutate(.,vote_name = case_when(vote_name %in% c('Não respondeu (Esp.)','Não sabe/ Não lembra (Esp.)') ~ NA_character_,
                               vote_name %in% c('Votou em branco (Esp.)','Anulou o voto (Esp.)') ~ 'Invalid ballot',
                               T ~ vote_name )) %>%
  group_by(vote_name) %>%
  summarise(nobs = n(),
            pop = mean(pop_add, na.rm=T),
            right = mean(ideo, na.rm=T),
            popsd = sd(pop_add, na.rm=T),
            ideosd = sd(ideo, na.rm=T)) %>%
  filter(.,nobs > 30 & vote_name != 'Missing') %>%
  na.omit() %>%
  ggplot(aes(pop, right, label= vote_name)) + 
  geom_pointrange(aes(ymin = right - 1.96*(ideosd/sqrt(nobs)), ymax = right + 1.96*(ideosd/sqrt(nobs)))) + 
  geom_errorbarh(aes(xmin = pop - 1.96*(popsd/sqrt(nobs)), xmax = pop + 1.96*(popsd/sqrt(nobs)), height = 0)) +
     theme_minimal() +geom_text_repel(show.legend = F) + 
  ylab('Left (0) -- Right (10) Ideology') + xlab('Populist Attitudes (1--5 additive index)')


tikz(file = "Figures and tables/plot_descriptive_vote.tex", width = 5, height = 5)
p.vote
endoffile <- dev.off()


###
# Regression


#######################
# Atitudes Populistas #
#######################


m1 <- glm(voto_b ~ pop_2c + sexo + id + ed + fx_renda, 
            data = e19,
            na.action = na.omit,
            family = binomial(link = "logit"))

m2 <- glm(voto_b ~ ideo + sexo + id + fx_renda+ ed, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"))

m3 <- glm(voto_b ~  ideo * pop_2c + sexo + id + ed + fx_renda, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"))

## Now with pop_add as IV:
m4 <- glm(voto_b ~ pop_add + sexo + id + ed+ fx_renda, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"))

m5 <- glm(voto_b ~ ideo * pop_add + sexo + id + ed + fx_renda,   
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"))
summary(m5)


models <- list(m1, m2, m3, m4, m5)

## export table: (copy + paste the R output to Overleaf)
texreg(l = models,
       custom.coef.names = c('Intercept','Populism (cat)','Male','Age','Education','Income','Left-right ideology',
                             'Populism (cat) * Ideology',  
                             'Populism (additive)','Populism (add) * Ideology'),
       include.deviance = F, include.loglik = F, booktabs = T, leading.zero = F, 
       single.row = F, caption = 'Logistic Regression Models Predicting of Voting for Jair Bolsonaro in 2018',
       label = 'tab:reg1', fontsize = 'small',
       custom.model.names = paste(unlist(map(1:length(models), ~paste0('(',.x,')'))), sep= ','))


## Plot the interactions from models 3 and 6:
p1 <- plot_model(m3, type = 'int',color= 'bw') + theme_minimal() +
  ggtitle('Populism as categorical (Model 3)') + ylab('Predicted Probabilities of Voting Bolsonaro') + 
  xlab('Left (0) - Right (10) Ideology') + theme(legend.position = 'bottom') + 
  geom_density(data = e19[as.numeric(names(m3$fitted.values)),],
           mapping = aes(x = ideo), fill = 'gray30', col=c('gray80'),alpha = .2,
           inherit.aes = F) +
  labs(linetype = 'Populism') 

p1

p2 <- plot_model(m6, type = 'int',color= 'bw', mdrt.values = 'meansd') + theme_minimal() +
  ggtitle('Populism as continuous (Model 5)') + ylab('Predicted Probabilities of Voting Bolsonaro') + 
  xlab('Left (0) - Right (10) Ideology') + theme(legend.position = 'bottom') + 
  geom_density(data = e19[as.numeric(names(m3$fitted.values)),],
               mapping = aes(x = ideo), fill = 'gray30', col=c('gray80'),alpha = .2,
               inherit.aes = F) +
  scale_linetype_manual(values = c('solid','dotted', 'dashed'),labels = c('Mean - 1SD', 'Mean (4.05)','Mean + 1SD')) +
  labs(linetype = 'Populism') 
p2


pdf(file = "Figures and tables/plot_interactions.pdf", width = 6, height = 9)
grid.arrange(p1, p2, ncol=1)
endoffile <- dev.off()


## Throw in PC2 and antiPT and religious controls:
m6 <- glm(voto_b ~ pop_2c + ideo + sexo + id + ed + fx_renda + PC2, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"))

m7 <- glm(voto_b ~ pop_add + ideo + sexo + id + ed + fx_renda + PC2, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"))

m8 <- glm(voto_b ~ pop_add + ideo + sexo + id + ed + PC2+ fx_renda + antipt + relig, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"))

m9 <- glm(voto_b ~ pop_2c + ideo + sexo + id + ed + PC2+ fx_renda + antipt + relig, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"))

models <- list(m6,m7,m8,m9)

## export table: (copy + paste the R output to Overleaf)
texreg(l = models,
       custom.coef.names = c('Intercept','Populism (cat)','Left-right ideology','Male','Age','Education','Income',
                             'Illiberalism',
                             'Populism (add.)','Anti-PT','Evangelical'),
       include.deviance = F, include.loglik = F, booktabs = T, leading.zero = F, 
       single.row = F, caption = 'Logistic Regression Models Predicting of Voting for Jair Bolsonaro in 2018 including Illiberal Attitudes',
       label = 'tab:reg2', fontsize = 'small',
       custom.model.names = paste(unlist(map(1:length(models), ~paste0('(',.x+5,')'))), sep= ','))




## Results on the subsample:
m1 <- glm(voto_b ~ pop_2c + sexo + id + ed + fx_renda, 
          data = subset(e19, ideo >= 6),
          na.action = na.omit,
          family = binomial(link = "logit"))

m2 <- glm(voto_b ~ pop_2c  + sexo + id + fx_renda+ ed, 
          data = subset(e19, ideo >= 6),
          na.action = na.omit,
          family = binomial(link = "logit"))

m3 <- glm(voto_b ~ pop_2c + sexo + id + ed + fx_renda+ corrup1, 
          data = subset(e19, ideo >= 6),
          na.action = na.omit,
          family = binomial(link = "logit"))

m4 <- glm(voto_b ~ pop_2c  + sexo + id + ed + corrup1+ fx_renda + PC2, 
          data = subset(e19, ideo >= 6),
          na.action = na.omit,
          family = binomial(link = "logit"))

## Now with pop_add as IV:
m5 <- glm(voto_b ~ pop_add + sexo + id + ed+ fx_renda, 
          data = subset(e19, ideo >= 6),
          na.action = na.omit,
          family = binomial(link = "logit"))

m6 <- glm(voto_b ~ pop_add * ideo + sexo + id + ed+ fx_renda, 
          data = subset(e19, ideo >= 6),
          na.action = na.omit,
          family = binomial(link = "logit"))

m7 <- glm(voto_b ~ pop_add * ideo + sexo + id + ed + corrup1  + fx_renda,   
          data = subset(e19, ideo >= 6),
          na.action = na.omit,
          family = binomial(link = "logit"))
summary(m7)

m8 <- glm(voto_b ~ pop_add * ideo + sexo + id + ed + corrup1 + PC2+ fx_renda, 
          data = subset(e19, ideo >= 6),
          na.action = na.omit,
          family = binomial(link = "logit"))


###
# Modelo 5: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, Corrupção, PC2, 
    # Antipetismo
m5 <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed + corrup1 + PC2 + antipt, 
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"))

###
# Modelo 6: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, Corrupção, PC2, 
    # Antipetismo, Renda
m6 <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed + corrup1 + PC2 + antipt + 
            fx_renda,
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"))

###
# Modelo 7: 
  # Var. Dep.: Voto em Bolsonaro
  # Var. Ind.: Atitudes Populistas * Ideologia, Sexo, Idade, Educação, Corrupção, PC2, 
    # Antipetismo, Renda, Religião
m7 <- glm(voto_b ~ pop_2c * ideo2.2 + sexo + id + ed + corrup1 + PC2 + antipt + 
            fx_renda + relig,
          data = e19,
          na.action = na.omit,
          family = binomial(link = "logit"))



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





