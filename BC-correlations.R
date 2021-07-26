# Case-control breast cancer study
# simple and partial correlations between score and metabolites
# simple correlations between score components and metabolites
source("BC-prep_data_calc_score.R")
library(writexl)
library(broom)
library(corrplot)

# Plot of score components correlations (with each other)---------------------------------------------------------------------
mcor <- cor(table_scores, use = "complete.obs")
tabcor <- cor(table_componentsFR)
corrplot(tabcor, tl.col = "black", type = "upper") #,  title = "Corrélations entre les composantes du score")

# Modeles lineaire pour vérifier que toutes les composantes du score apportent bien de l'information
lin_mod <- lm (score ~ BMI + TTAILLE + TotalAPQ3 + fruitveg + TDF + percent_aUPF + Rmeat + Pmeat + sugary_drinks + ALCOHOL + allaitement_dureecum, data = df.scores)
#summary(lin_mod)

lin_mod2 <- lm (score ~ sc.BMI + sc.TT + sc.PA + sc.FV + sc.TDF + sc.UPF + sc.MEAT + sc.SD + sc.ALC + sc.BFD , data = df.scores)
#summary(lin_mod2)

# WCRF/AICR full score simple correlations with metabolites ---------------------------------------------------------------------

# Simple correlation for WCRF score - Spearman correlation
simplecorSP <- function(x) cor.test(table_scores$score, x, method = "spearman")
corlistSP <- apply(metabolo, 2, simplecorSP)

# Convert to data frame and add compound names, order by correlation
cordatSP <- map_dfr(corlistSP, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)
write_xlsx(cordatSP, "C:\\Users\\Clougher\\score\\results_data_tables\\spearman_score_and_metabolites.xlsx") 


# Simple correlation for WCRF score - Pearsons correlation
simplecorPE <- function(x) cor.test(table_scores$score, x, method = "pearson")
corlistPE <- apply(metabolo, 2, simplecorPE)

# Convert to data frame and add compound names, order by correlation
cordatPE <- map_dfr(corlistPE, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)
write_xlsx(cordatPE, "C:\\Users\\Clougher\\score\\results_data_tables\\pearson_score_and_metabolites.xlsx") 


# Plot correlations
plotSP <- ggplot(cordatSP, aes(method, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() + labs(title = 'corrélation Spearman simple score WCRF - métabolites')
plotSP

plotPE <- ggplot(cordatPE, aes(method, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() + labs(title = 'corrélation Pearson simple score WCRF - métabolites')
plotPE

# WCRF/AICR full score partial correlations with metabolites ---------------------------------------------------------

# Mutating variables as factor for partial correlations
df.scores$FASTING <- as.factor(df.scores$FASTING) #fasting status befor blood collection
df.scores$MENOPAUSE <- as.factor(df.scores$MENOPAUSE) #menopausal status
df.scores$SMK <- as.factor(df.scores$SMK) #smoking status
df.scores$DIAGSAMPLINGCat3 <- as.factor(df.scores$DIAGSAMPLINGCat3) #time between blood collection and diagnosis
df.scores$CO <- as.factor(df.scores$CO) #oral contraceptives
df.scores$DIABETE <- as.factor(df.scores$DIABETE) #total non-alcoholic energy intake

#Partial correlation controlling for Fasting and smoking status
partialcor <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(score ~ MENOPAUSE + SMK+ DIAGSAMPLINGCat3 + CO + DIABETE, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ MENOPAUSE + SMK+ DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}

pcorlist <- apply(metabolo, 2, partialcor)
pcordat <- map_dfr(pcorlist, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)
#write_xlsx(pcordat, "C:\\Users\\Clougher\\score\\results_data_tables\\partial_corr-time-smk-menop_newcovar_metab.xlsx") 
#write_xlsx(pcordat, "Users/MacSuzanne/score/results_data_tables/partial_corr-time.xlsx") 

plot_pcor <- ggplot(pcordat, aes(method, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() + labs(title = 'Time-Smk-Menop')
plot_pcor

# Individual score components simple correlations with metabolites ---------------------------------------------------------------------

# Simple correlation for ALCOHOL
simplecor.ALC <- function(x) cor.test(table_scores$ALCOHOL[table_scores$ALCOHOL > 0], x, method = "spearman")
corlist.ALC <- apply(metabolo[table_scores$ALCOHOL > 0, ], 2, simplecor.ALC)
# Convert to data frame and add compound names, order by correlation
cordat.ALC <- map_dfr(corlist.ALC, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)

# Simple correlation for BMI
simplecor.BMI <- function(x) cor.test(table_scores$BMI[table_scores$BMI > 0], x, method = "spearman")
corlist.BMI <- apply(metabolo[table_scores$BMI > 0, ], 2, simplecor.BMI)
# Convert to data frame and add compound names, order by correlation
cordat.BMI <- map_dfr(corlist.BMI, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)