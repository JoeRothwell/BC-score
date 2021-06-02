## Version of the code where lines with missing data for the score are replaced by the variables' means

library(haven)
library(tidyverse)
library(readxl)

# Datasets-----------------------------------------------------------------------

# Food data
alim <- read_sas("frjour.sas7bdat")

# Breastfeeding data
bfeed <- read_sas("d_grossesse_20190107_corrections.sas7bdat") %>% rename(ident = IDENT)

# Physical activity data 
physact <- read_sas("physicalact.sas7bdat") %>% rename(ident = IDENT)

# Correspondence ident-COBBMB
id <- read_xls("E3N_cancer du sein_21072014.xls") %>% mutate(ident = IDENT) %>% select(c("CODBMB", "ident"))

# Fiber data
fiber <- read_sas("nut_fra2.sas7bdat") %>% select(ident, alcool, FIBR, SDF, TDF)

# Metadata
meta <- read_csv("metadata.csv", na = "9999")
meta$CODBMB <- as.character(meta$CODBMB)

# Create a single table (containing cases and controls)
scoredata <- meta %>% 
  left_join(id, by = "CODBMB") %>% left_join(alim, by = "ident") %>%
  left_join(fiber, by = "ident") %>% left_join(bfeed, by = "ident") %>%
  left_join(physact, by = "ident")


# Manipulating data for score -----------------------------------------------------------------------

#Renaming all (food) columns starting with "_" 
data_xnames <- scoredata %>%
  rename_at(vars(starts_with('_')), funs(str_replace(., '_', 'x'))) 

#Calculating g/day intake of total fruits & vegetables, red meat, processed meat, drinks, alcohol, aUPF, total food intake and percentage of aUPF in total food intake
data_xnames_sums <- data_xnames %>%
  rowwise() %>%
  mutate (fruitveg = sum(x2, x3, x35, x40, x53), #g/day
          Rmeat = sum(x46_3, x46_4, x46_6, x46_7, x46_9, x47_2)*7, #g/week
          Pmeat = sum(x19, x36, x37, x41_4, x41_7, x42, x47_6)*7, #g/week
          sugary_drinks = sum(x7, x21), #g/day
          aUPF = sum(x10_4, x10_7, x11_2, x11_3, x11_5, x11_6, x14, x15, x16_3, x16_4, 
                     x16_5, x16_6, x17_4, x17_5, x17_6, x22, x23, x24, x33, x47_1, 
                     x47_5, x47_8, x50, x54, x55, 
                     MG2, MG3, MG4, MG12, MG15, VIN1, VIN2), #g/day
          total_food = 
            sum(x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, 
                x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, 
                x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, 
                x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, 
                x55, x56, x57, x58, x59, 
                SUCRE, EDULC, LAIT, VIN, MG)) %>% #g/day
  mutate (percent_aUPF = (aUPF/total_food) *100) #percent of aUPF in total food intake (g/day)

# Replace intake = NA with median value
# remplacement par la moyenne plutot qu'enlever les participantes avec des données manquantes pour pouvoir calculer les corrélations après  
data_xnames_sums$percent_aUPF[is.na(data_xnames_sums$percent_aUPF)] <- median(data_xnames_sums$percent_aUPF, na.rm = T)
data_xnames_sums$allaitement_dureecum[is.na(data_xnames_sums$allaitement_dureecum)] <- median(data_xnames_sums$allaitement_dureecum, na.rm = T)
data_xnames_sums$TTAILLE[is.na(data_xnames_sums$TTAILLE)] <- median(data_xnames_sums$TTAILLE, na.rm = T)

#Tertiles : needed for aUPF consumption cutoff points
tertiles_UPF <- quantile(data_xnames_sums$percent_aUPF, probs = c(1/3, 2/3))
tertile_UPF1 <- as.numeric(tertiles_UPF[1]) #cut point n°1 (fully-met recommendation)
tertile_UPF2 <- as.numeric(tertiles_UPF[2]) #cut point n°2 (half-met recommendation)

# Score -----------------------------------------------------------------------

df.scores <- data_xnames_sums %>% 
  mutate(sc.BMI1 = ifelse(BMI >= 18.5 & BMI < 30, 0.25, 0), # At least 0.25 for this condition
         sc.BMI2 = ifelse(BMI >= 18.5 & BMI < 25, 0.25, 0), # Another 0.25 for this condition
         sc.TT1  = ifelse(TTAILLE <= 88, 0.25, 0), 
         sc.TT2  = ifelse(TTAILLE <= 80, 0.25, 0),
         sc.PA1  = ifelse(TotalAPQ3 >= 9.375, 0.5, 0), 
         sc.PA2  = ifelse(TotalAPQ3 >= 18.75, 0.5, 0),
         sc.FV1 = ifelse(fruitveg >= 200, 0.25, 0),
         sc.FV2 = ifelse(fruitveg >= 400, 0.25, 0),
         sc.TDF1 = ifelse(TDF >= 15, 0.25, 0),
         sc.TDF2 = ifelse(TDF >= 30, 0.25, 0),
         sc.UPF1 = ifelse(percent_aUPF < tertile_UPF2, 0.5, 0),
         sc.UPF2 = ifelse(percent_aUPF < tertile_UPF1, 0.5, 0),
         sc.MEAT1 = ifelse(Rmeat < 500 & Pmeat < 100, 0.5, 0),
         sc.MEAT2 = ifelse(Rmeat < 500 & Pmeat < 21, 0.5, 0),
         sc.SD1 = ifelse(sugary_drinks <= 250, 0.5, 0),
         sc.SD2 = ifelse(sugary_drinks == 0, 0.5, 0),
         sc.ALC1 = ifelse(ALCOHOL <= 14, 0.5, 0),
         sc.ALC2 = ifelse(ALCOHOL == 0, 0.5, 0),
         sc.BFD1 = ifelse(nullipare == 0 & allaitement_dureecum > 0 & allaitement_dureecum < 6, 0.5, 0),
         sc.BFD2 = ifelse(nullipare == 0 & allaitement_dureecum > 6, 0.5, 0),
         # Add up sc.BMI + sc.TT + sc.PA + other recommendations to get score         
         sc.BMI = sc.BMI1 + sc.BMI2, sc.TT = sc.TT1 + sc.TT2, sc.PA = sc.PA1 + sc.PA2,
         sc.FV = sc.FV1 + sc.FV2, sc.TDF = sc.TDF1 + sc.TDF2, sc.UPF= sc.UPF1 + sc.UPF2, sc.MEAT = sc.MEAT1 + sc.MEAT2,
         sc.SD = sc.SD1 + sc.SD2, sc.ALC = sc.ALC1 + sc.ALC2,
         sc.BFD = sc.BFD1 + sc.BFD2,
         # Get overall score
         score = sc.BMI + sc.TT + sc.PA + sc.FV + sc.TDF + sc.UPF + sc.MEAT + sc.SD + sc.ALC + sc.BFD)

# Score distribution histogram
hist(df.scores$score, xlab = "WCRF/AICR score", main = paste("Scores in the E3N cancer group"))

# Create a table containing only data relevant for the score
table_scores <- df.scores %>%
  select(CT, BMI,  TTAILLE, TotalAPQ3, fruitveg, TDF, percent_aUPF, Rmeat, Pmeat, sugary_drinks, ALCOHOL, allaitement_dureecum, score)

# Metabolomics dataset---------------------------------------------------------------------

# Get metabolomics data (unscaled)
ints <- read_tsv("1510_XMetaboliteE3N_cpmg_unscaled.txt") #contains the whole group
ints.ctrl <- ints[meta$CT == 0, ] #contains only the 790 controls
ints.cases <- ints[meta$CT == 1, ] #contains only the cases

# Scale to unit variance
metabolo <- scale(ints)

# WCRF Correlations---------------------------------------------------------------------

# Simple correlation for WCRF score - Spearman correlation
simplecorSP <- function(x) cor.test(table_scores$score[table_scores$score > 0], x, method = "spearman")
corlistSP <- apply(metabolo[table_scores$score > 0, ], 2, simplecorSP)
print(corlistSP)

# Convert to data frame and add compound names, order by correlation
library(broom)
cordatSP <- map_dfr(corlistSP, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)
cordatSP

# Simple correlation for WCRF score - Pearsons correlation
simplecorPE <- function(x) cor.test(table_scores$score[table_scores$score > 0], x, method = "pearson")
corlistPE <- apply(metabolo[table_scores$score > 0, ], 2, simplecorPE)
print(corlistPE)

# Convert to data frame and add compound names, order by correlation
cordatPE <- map_dfr(corlistPE, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)
cordatPE

# Plot correlations
library(ggplot2)
plotSP <- ggplot(cordatSP, aes(method, compound)) +
  geom_tile(aes(fill = estimate))
plotSP

plotPE <- ggplot(cordatPE, aes(method, compound)) +
  geom_tile(aes(fill = estimate))
plotPE

# Components Correlations---------------------------------------------------------------------

# Simple correlation for alcohol
simplecor.ALC <- function(x) cor.test(table_scores$ALCOHOL[table_scores$ALCOHOL > 0], x, method = "spearman")
corlist.ALC <- apply(metabolo[table_scores$ALCOHOL > 0, ], 2, simplecor.ALC)

# Convert to data frame and add compound names, order by correlation
cordat.ALC <- map_dfr(corlist.ALC, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)

# Simple correlation for BMI
simplecor.BMI <- function(x) cor.test(table_scores$BMI[table_scores$BMI > 0], x, method = "spearman")
corlist.BMI <- apply(metabolo[table_scores$BMI > 0, ], 2, simplecor.BMI)

# Convert to data frame and add compound names, order by correlation
cordat.BMI <- map_dfr(corlist.BMI, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)

# Plot metabolites distribution --------------------------------------
normal_distrib <- function (x) {
  qqnorm(metabolo[,x], main =colnames(metabolo)[x])
  qqline(metabolo[,x])
}

n <- ncol(metabolo)
for(i in c(1:n)){normal_distrib(i)}

