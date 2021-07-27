# Case-control breast cancer study
# partial correlations between score and metabolites
source("BC-prep_data_calc_score.R")
library(writexl)
library(broom)
library(corrplot)

# Mutating variables as factor for partial correlations
df.scores$FASTING <- as.factor(df.scores$FASTING) #fasting status before blood collection
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

#Partial correlation controlling for Fasting + smoking + menopausal status + oral contraception
partialcor4 <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(score ~ FASTING + SMK + MENOPAUSE + CO, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO , data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}

#Partial correlation controlling for Fasting + smoking + menopausal status + oral contraception + time between blood draw and cancer diagnosis
partialcor5 <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(score ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3, data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}

#Partial correlation controlling for Fasting + smoking + menopausal status + oral contraception + time (between analysis and diagnosis)+ stocktime
# + RTH (rapport taille/hanche) + diabete + durthsbmb (duree utilisation traitement hormonal)
partialcor6 <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(score ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}


pcorlist1 <- apply(metabolo, 2, partialcor1)
pcorlist2 <- apply(metabolo, 2, partialcor2)
pcorlist3 <- apply(metabolo, 2, partialcor3)
pcorlist4 <- apply(metabolo, 2, partialcor4)
pcorlist5 <- apply(metabolo, 2, partialcor5)
pcorlist6 <- apply(metabolo, 2, partialcor6)

pcordat1 <- map_dfr(pcorlist1, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% select(estimate, compound) %>% mutate(model="Mod 1") #%>% rename(estimate1= estimate)
pcordat2 <- map_dfr(pcorlist2, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% select(estimate, compound) %>% mutate(model="Mod 2")#%>% rename(estimate2= estimate) %>% select(estimate2, compound)
pcordat3 <- map_dfr(pcorlist3, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% select(estimate, compound) %>% mutate(model="Mod 3")#%>% rename(estimate3= estimate) %>% select(estimate3, compound)
pcordat4 <- map_dfr(pcorlist4, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% select(estimate, compound) %>% mutate(model="Mod 4")#%>% rename(estimate4= estimate) %>% select(estimate4, compound)
pcordat5 <- map_dfr(pcorlist5, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% select(estimate, compound) %>% mutate(model="Mod 5")#%>% rename(estimate5= estimate) %>% select(estimate5, compound)
pcordat6 <- map_dfr(pcorlist6, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate) %>% select(estimate, compound) %>% mutate(model="Mod 6")#%>% rename(estimate6= estimate) %>% select(estimate6, compound)

# Bind all tables
pcordat <- pcordat1 %>% rbind(pcordat2) %>% rbind(pcordat3) %>% rbind(pcordat4) %>% rbind(pcordat5) %>% rbind(pcordat6)

plot_pcor <- ggplot(pcordat, aes(model, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() + #labs(title = 'Partial correlations with WCRF/AICR score') +
  geom_vline(xintercept = 1.5, linetype = "solid") + geom_vline(xintercept = 2.5, linetype = "solid")+ geom_vline(xintercept = 3.5, linetype = "solid")+ geom_vline(xintercept = 4.5, linetype = "solid")+ geom_vline(xintercept = 5.5, linetype = "solid")
plot_pcor

