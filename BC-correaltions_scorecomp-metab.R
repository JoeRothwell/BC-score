# Case-control breast cancer study
# partial correlations between score components and metabolites
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

#Partial correlation for BMI
pcorBMI <- function(x) {
  # Linear model of score and confounders
  mod1 <- lm(BMI ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH + RTH, data = df.scores[df.scores$score > 0, ])
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
}


pcorTTAILLE <- function(x) { #Partial correlation for waist circumference
  mod1 <- lm(TTAILLE ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH + RTH, data = df.scores[df.scores$score > 0, ])
  cor.test(residuals(mod1), residuals(mod2), method = "spearman") }

pcorPHYS <- function(x) { #Partial correlation for physical activity
  mod1 <- lm(TotalAPQ3 ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH + RTH, data = df.scores[df.scores$score > 0, ])
  cor.test(residuals(mod1), residuals(mod2), method = "spearman") }

pcorFV <- function(x) { #Partial correlation for fruits and vegetables
  mod1 <- lm(fruitveg ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH + RTH, data = df.scores[df.scores$score > 0, ])
  cor.test(residuals(mod1), residuals(mod2), method = "spearman") }

pcorFIBRE <- function(x) { #Partial correlation for fiber
  mod1 <- lm(TDF ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH + RTH, data = df.scores[df.scores$score > 0, ])
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")}

pcorUPF <- function(x) { #Partial correlation for aUPF
  mod1 <- lm(percent_aUPF ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH + RTH, data = df.scores[df.scores$score > 0, ])
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")}

pcorRMEAT <- function(x) { #Partial correlation for red meat
  mod1 <- lm(Rmeat ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH + RTH, data = df.scores[df.scores$score > 0, ])
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")}

pcorPMEAT <- function(x) { #Partial correlation for processed meat
  mod1 <- lm(Pmeat ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH + RTH, data = df.scores[df.scores$score > 0, ])
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")}

pcorSUGD <- function(x) { #Partial correlation for sugary drinks
  mod1 <- lm(sugary_drinks ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH + RTH, data = df.scores[df.scores$score > 0, ])
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")}

pcorALC <- function(x) { #Partial correlation for walcohol
  mod1 <- lm(ALCOHOL ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH + RTH, data = df.scores[df.scores$score > 0, ])
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")}

pcorBFEED <- function(x) { #Partial correlation for breastfeeding
  mod1 <- lm(allaitement_dureecum ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH, data = df.scores[df.scores$score > 0, ])
  mod2 <- lm(x ~ FASTING + SMK + MENOPAUSE + CO + DIAGSAMPLINGCat3 + STOCKTIME + DURTHSBMB + DIABETE + RTH + RTH, data = df.scores[df.scores$score > 0, ])
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")}

# Calculate correlations
pcorBMI_list <- apply(metabolo, 2, pcorBMI)
pcorTTAILLE_list <- apply(metabolo, 2, pcorTTAILLE)
pcorPHYS_list <- apply(metabolo, 2, pcorPHYS)
pcorFV_list <- apply(metabolo, 2, pcorFV)
pcorFIBRE_list <- apply(metabolo, 2, pcorFIBRE)
pcorUPF_list <- apply(metabolo, 2, pcorUPF)
pcorRMEAT_list <- apply(metabolo, 2, pcorRMEAT)
pcorPMEAT_list <- apply(metabolo, 2, pcorPMEAT)
pcorSUGD_list <- apply(metabolo, 2, pcorSUGD)
pcorALC_list <- apply(metabolo, 2, pcorALC)
pcorBFEED_list <- apply(metabolo, 2, pcorBFEED)

# Tidy partial correlations list 
cordat <- function(partialcor_list) {
  map_dfr(partialcor_list, tidy, .id = "feat") %>%
  mutate(p.valFDR = p.adjust(p.value, method = "fdr")) %>% 
  select(method, estimate, p.value, p.valFDR) %>% #adding adjust p.values (FDR = false discovery rate)
  bind_cols(compound = colnames(metabolo)) %>% 
  arrange(-estimate)
}

pcorBMI_dat <- cordat(pcorBMI_list) %>% mutate(score_component="BMI")
pcorTTAILLE_dat <- cordat(pcorTTAILLE_list) %>% mutate(score_component="Waist circumference")
pcorPHYS_dat <- cordat(pcorPHYS_list) %>% mutate(score_component="Physical activity")
pcorFV_dat <- cordat(pcorFV_list) %>% mutate(score_component="Fruits and vegetables")
pcorFIBRE_dat <- cordat(pcorFIBRE_list) %>% mutate(score_component="Fibre")
pcorUPF_dat <- cordat(pcorUPF_list) %>% mutate(score_component="aUPF")
pcorRMEAT_dat <- cordat(pcorRMEAT_list) %>% mutate(score_component="Red meat")
pcorPMEAT_dat <- cordat(pcorPMEAT_list) %>% mutate(score_component="Processed meat")
pcorSUGD_dat <- cordat(pcorSUGD_list) %>% mutate(score_component="Sugary drinks")
pcorALC_dat <- cordat(pcorALC_list) %>% mutate(score_component="Alcohol")
pcorBFEED_dat <- cordat(pcorBFEED_list) %>% mutate(score_component="Breastfeeding")

# Bind corr data
all_dat <- pcorBMI_dat %>% rbind(pcorTTAILLE_dat) %>% rbind(pcorPHYS_dat) %>% rbind(pcorFV_dat) %>% 
  rbind(pcorFIBRE_dat) %>% rbind(pcorUPF_dat) %>% rbind(pcorRMEAT_dat) %>% rbind(pcorPMEAT_dat) %>%
  rbind(pcorSUGD_dat) %>% rbind(pcorALC_dat) %>% rbind(pcorBFEED_dat)
all_dat$model <- factor(all_dat$score_component, levels = c("BMI", "PWaist circumference", "Physical activity", "Fruits and vegetables",
                                                  "Fibre", "aUPF", "Red meat", "Processed meat",
                                                  "Sugary drinks", "Alcohol", "Breatdfeeding")) # reorder model names for plot x axis order (since default = alphabetical)

# Plot each score component's correlation with metabolites (in a single heatmap)
plot_allcor <- ggplot(all_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() + 
  xlab("Score component") + ylab("Metabolite") 
plot_allcor

#only compounds with significative p.values
pcordatBMI.sign <- pcorBMI_dat %>% filter(p.value < 0.05)  #22 with significative p.value
pcordatTTAILLE.sign <- pcorTTAILLE_dat %>% filter(p.value < 0.05) #20
pcordatPHYS.sign <- pcorPHYS_dat %>% filter(p.value < 0.05) # 1 (isoleucine)
pcordatFV.sign <- pcorFV_dat %>% filter(p.value < 0.05) #2 (acetone, aspartate)
pcordatFIBRE.sign <- pcorFIBRE_dat %>% filter(p.value < 0.05) #3 (mannose, creatinine, acetone)
pcordatUPF.sign <- pcorUPF_dat %>% filter(p.value < 0.05) #3 (dimethylamine, glycerophosphocholine, choline)
pcordatRMEAT.sign <- pcorRMEAT_dat %>% filter(p.value < 0.05) #3 (glutamate, glutamine, glycine)
pcordatPMEAT.sign <- pcorPMEAT_dat %>% filter(p.value < 0.05) #1 (methanol)
pcordatSUGD.sign <- pcorSUGD_dat %>% filter(p.value < 0.05) #3 (mannose, glycerophosphocholine, choline)
pcordatALC.sign <- pcorALC_dat %>% filter(p.value < 0.05) #10 (glycerophosphocholine, choline, ornithine, isoleucine, creatinine, glutamate, glutamine, phenylalanine, methionine, NAC)
pcordatBFEED.sign <- pcorBFEED_dat %>% filter(p.value < 0.05) #1 (methanol)

# Individual plots for each score component
plot_pcorBMI <- ggplot(pcorBMI_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() +
  xlab("Score component") + ylab("Metabolite") 

plot_pcorTTAILLE <- ggplot(pcorTTAILLE_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() +
  xlab("Score component") + ylab("Metabolite") 

plot_pcorPHYS <- ggplot(pcorPHYS_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() +
  xlab("Score component") + ylab("Metabolite") 

plot_pcorFV <- ggplot(pcorFV_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() +
  xlab("Score component") + ylab("Metabolite") 

plot_pcorFIBRE <- ggplot(pcorFIBRE_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() +
  xlab("Score component") + ylab("Metabolite") 

plot_pcorUPF <- ggplot(pcorUPF_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() +
  xlab("Score component") + ylab("Metabolite") 

plot_pcorRMEAT <- ggplot(pcorRMEAT_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() +
  xlab("Score component") + ylab("Metabolite") 

plot_pcorPMEAT <- ggplot(pcorPMEAT_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() +
  xlab("Score component") + ylab("Metabolite") 

plot_pcorSUGD <- ggplot(pcorSUGD_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() +
  xlab("Score component") + ylab("Metabolite") 

plot_pcorALC <- ggplot(pcorALC_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() +
  xlab("Score component") + ylab("Metabolite") 

plot_pcorBFEED <- ggplot(pcorBFEED_dat, aes(score_component, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() +
  xlab("Score component") + ylab("Metabolite") 