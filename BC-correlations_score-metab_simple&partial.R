# Case-control breast cancer study
# simple and partial correlations between score and metabolites
# simple correlations between score components and metabolites
source("BC-prep_data_calc_score.R")
library(writexl)
library(broom)
library(corrplot)

# WCRF/AICR full score simple correlations with metabolites ---------------------------------------------------------------------

simplecor <- function(x) cor.test(table_scores$score, x, method = "spearman")
corlist <- apply(metabolo, 2, simplecor)

# Convert to data frame and add compound names, order by correlation
cordat <- map_dfr(corlist, tidy, .id = "feat") %>% 
  mutate(p.valFDR = p.adjust(p.value, method = "fdr")) %>% select(method, estimate, p.value, p.valFDR) %>%
  bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)
cordat.sign <- cordat %>% filter(p.value < 0.05) #only compounds with significative p.values
cordat.signFDR <- cordat %>% filter(p.valFDR < 0.05) #only compounds with significative FDR p.values

# Tried with Pearson method as well, similar results. 
# Keeping Spearman because it excludes outliers and some outliers remain in metabolites data

# Plot simple correlation between score and metabolites
plot_simplecorr <- ggplot(cordat, aes(method, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() #+ labs(title = 'corrélation Spearman simple score WCRF - métabolites') + theme(plot.title = element_text(hjust = 0.45, vjust=2.12))


# WCRF/AICR full score partial correlations with metabolites ---------------------------------------------------------

# Mutating variables as factor for partial correlations
df.scores$FASTING <- as.factor(df.scores$FASTING) #fasting status before blood collection
df.scores$MENOPAUSE <- as.factor(df.scores$MENOPAUSE) #menopausal status
df.scores$SMK <- as.factor(df.scores$SMK) #smoking status
df.scores$CO <- as.factor(df.scores$CO) #oral contraceptives
df.scores$DIABETE <- as.factor(df.scores$DIABETE) #total non-alcoholic energy intake

#Partial correlation controlling for multiple factors
partialcor <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(score ~ FASTING + SMK + MENOPAUSE + CO + STOCKTIME + DURTHSBMB, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + STOCKTIME + DURTHSBMB, data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}

pcorlist <- apply(metabolo, 2, partialcor)
pcordat <- map_dfr(pcorlist, tidy, .id = "feat") %>%
  mutate(p.valFDR = p.adjust(p.value, method = "fdr")) %>% 
  select(method, estimate, p.value, p.valFDR) %>% #adding adjust p.values (FDR = false discovery rate)
  bind_cols(compound = colnames(metabolo)) %>% 
  arrange(-estimate)

pcordat.sign <- pcordat %>% filter(p.value < 0.05) # 33 compounds with significative p.values
pcordat.signFDR <- pcordat %>% filter(p.valFDR < 0.05) # 19 compounds with FDR p.value <0,05

# Plot partial correlations between full score and metabolites
plot_pcor <- ggplot(pcordat, aes(method, compound)) +
  geom_tile(aes(fill = estimate)) +
  geom_text(aes(label = estimate)) #+ scale_fill_gradientn(colors, values = values)

# Plot heatmap partial VS simple correlation 
pcordat_forplot <- pcordat %>% select(estimate, compound) %>% mutate(model="Partielle") # select only estimates and correlation type
cordat_forplot <- cordat %>% select(estimate, compound) %>% mutate(model="Simple")
cor_and_pcordat <- cordat_forplot %>% rbind(pcordat_forplot) # bind simple and partial corr data
cor_and_pcordat$model <- factor(cor_and_pcordat$model, levels = c("Simple", "Partielle")) # reorder model names for plot x axis order (since defaut = alphabetical)

plot_cor_and_pcor <- ggplot(cor_and_pcordat, aes(model, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() + 
  geom_vline(xintercept = 1.5, linetype = "solid") +
  xlab("Type de correlation") + ylab("Metabolite") 


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