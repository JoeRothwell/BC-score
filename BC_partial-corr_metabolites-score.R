# Case-control breast cancer study
# partial correlations between score and metabolites

source("BC-prep_data_calc_score.R")
library(writexl)
library(broom)
library(corrplot)

# Mutating variables as factor for partial correlations
df.scores$FASTING <- as.factor(df.scores$FASTING) #fasting status befor blood collection
df.scores$MENOPAUSE <- as.factor(df.scores$MENOPAUSE) #menopausal status
df.scores$SMK <- as.factor(df.scores$SMK) #smoking status
df.scores$DIAGSAMPLINGCat3 <- as.factor(df.scores$DIAGSAMPLINGCat3) #time between blood collection and diagnosis
df.scores$CO <- as.factor(df.scores$CO) #oral contraceptives
df.scores$DIABETE <- as.factor(df.scores$DIABETE) #total non-alcoholic energy intake

#Partial correlation controlling for Fasting 
partialcor1 <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(score ~ FASTING, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING, data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}

#Partial correlation controlling for Fasting + smoking
partialcor2 <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(score ~ FASTING + SMK, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING + SMK, data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}

#Partial correlation controlling for Fasting + smoking + menopausal status
partialcor3 <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(score ~ FASTING + SMK + MENOPAUSE, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE, data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}

#Partial correlation controlling for Fasting + smoking + menopausal status + oral contractpion
partialcor4 <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(score ~ FASTING + SMK + MENOPAUSE + CO, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO , data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}

#Partial correlation controlling for Fasting + smoking + menopausal status + oral contractpion + time
partialcor5 <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(score ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3, data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}

#Partial correlation controlling for Fasting + smoking + menopausal status + oral contractpion + time (between thawing and analysis ?)+ stocktime
# + RTH (ration taille/hanche) + diabete + durthsbmb (duree utilisation traitement hormonal)
partialcor6 <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(score ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}

pcorlist9 <- apply(metabolo, 2, partialcor1)
pcordat9 <- map_dfr(pcorlist1, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)
pcordat9

pcorlist1 <- apply(metabolo, 2, partialcor1)
pcordat1 <- map_dfr(pcorlist1, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% 
  rename(estimate1= estimate)
pcorlist2 <- apply(metabolo, 2, partialcor2)
pcordat2 <- map_dfr(pcorlist2, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% 
  rename(estimate2= estimate) %>% select(estimate2, compound)
pcorlist3 <- apply(metabolo, 2, partialcor3)
pcordat3 <- map_dfr(pcorlist3, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% 
  rename(estimate3= estimate) %>% select(estimate3, compound)
pcorlist4 <- apply(metabolo, 2, partialcor4)
pcordat4 <- map_dfr(pcorlist4, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% 
  rename(estimate4= estimate) %>% select(estimate4, compound)
pcorlist5 <- apply(metabolo, 2, partialcor5)
pcordat5 <- map_dfr(pcorlist5, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% 
  rename(estimate5= estimate) %>% select(estimate5, compound)
pcorlist6 <- apply(metabolo, 2, partialcor6)
pcordat6 <- map_dfr(pcorlist6, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% 
  rename(estimate6= estimate) %>% select(estimate6, compound)

pcordat <- pcordat1 %>% select(compound, method, estimate1) %>%
  left_join(pcordat2, by = "compound") %>% left_join(pcordat3, by = "compound") %>% left_join(pcordat4, by = "compound") %>% left_join(pcordat5, by = "compound") %>% left_join(pcordat6, by = "compound") 
view(head(pcordat)) 

#TO DO :
# trier tous les pcordat dans le meme ordre
# faire une table avec seulement les "estimate" et le nom des composés
# peut etre join les tables par "compound" ? 
# plot la table finale en heatmap

plot_pcor <- ggplot(pcordat, aes(method, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() + labs(title = 'Partial correlations with WCRF/AICR score')
plot_pcor

heatmap(pcordat)
