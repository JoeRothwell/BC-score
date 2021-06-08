library(ggplot2)
library(haven)
library(tidyverse)
library(readxl)
library(grid)

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

# Create a single table (containing both cases and controls)
scoredata <- meta %>% 
  left_join(id, by = "CODBMB") %>% left_join(alim, by = "ident") %>%
  left_join(fiber, by = "ident") %>% left_join(bfeed, by = "ident") %>%
  left_join(physact, by = "ident")


# Manipulating data for score -----------------------------------------------------------------------

#Renaming all columns starting with "_" (from 'alim'/frjour table)
data_xnames <- scoredata %>%
  rename_at(vars(starts_with('_')), funs(str_replace(., '_', 'x'))) 

#Calculating intakes necessary for score 
# g/day intake of total fruits & vegetables, red meat, processed meat, sugary drinks, aUPF, total food intake and percentage of aUPF in total food intake
data_xnames_sums <- data_xnames %>%
  rowwise() %>%
  mutate (fruitveg = sum(x2, x3, x35, x40, x41_2, x41_5, x41_11, x53), #g/day
          Rmeat = sum(x46_3, x46_7, x46_9, x47_2)*7, #g/week
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

#Cleaning missing data in percentage of aUPF, breastfeeding and waist circumference
data_UPF <- data_xnames_sums %>% pull(percent_aUPF)
data_allaitement <- data_xnames_sums %>% pull(allaitement_dureecum)
data_TTAILLE <- data_xnames_sums %>% pull(TTAILLE)
#Create a vector containing row numbers with missing data
rows_missing_data <- c(which(is.na(data_UPF)), which(is.na(data_allaitement)), which(is.na(data_TTAILLE))) 
clean_data <- data_xnames_sums[-rows_missing_data,] #remove rows where data is missing

#Tertiles : needed for aUPF consumption cutoff points
tertiles_UPF <- quantile(clean_data$percent_aUPF, probs = c(1/3, 2/3))
tertile_UPF1 <- as.numeric(tertiles_UPF[1]) #cut point n°1 (fully-met recommendation)
tertile_UPF2 <- as.numeric(tertiles_UPF[2]) #cut point n°2 (half-met recommendation)

# Calculate score -----------------------------------------------------------------------

df.scores <- clean_data %>% 
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
         sc.BFD1 = ifelse(allaitement_dureecum > 0, 0.5, 0),
         sc.BFD2 = ifelse(allaitement_dureecum >= 6, 0.5, 0),
         # Add up sc.BMI + sc.TT + sc.PA + other recommendations to get score         
         sc.BMI = sc.BMI1 + sc.BMI2, sc.TT = sc.TT1 + sc.TT2, sc.PA = sc.PA1 + sc.PA2,
         sc.FV = sc.FV1 + sc.FV2, sc.TDF = sc.TDF1 + sc.TDF2, sc.UPF= sc.UPF1 + sc.UPF2, sc.MEAT = sc.MEAT1 + sc.MEAT2,
         sc.SD = sc.SD1 + sc.SD2, sc.ALC = sc.ALC1 + sc.ALC2,
         sc.BFD = sc.BFD1 + sc.BFD2,
         # Get overall score
         score = sc.BMI + sc.TT + sc.PA + sc.FV + sc.TDF + sc.UPF + sc.MEAT + sc.SD + sc.ALC + sc.BFD)

# Tables with score info -----------------------------------------------------------------------

# Table containing only data relevant for the score and full score
table_scores <- df.scores %>%
  select(CT, BMI, TTAILLE, TotalAPQ3, fruitveg, TDF, percent_aUPF, Rmeat, Pmeat, sugary_drinks, ALCOHOL, allaitement_dureecum, score) %>%
  mutate(CT = factor(CT, levels = c("0", "1"), labels = c("Controls", "Cases")))

# Matrix containing score information
matrix_scores <- data.matrix(table_scores)
matrixCTR <- table_scores %>% filter (CT == "Controls") %>% select(-CT) %>% data.matrix
matrixCS <- table_scores %>% filter (CT == "Cases") %>% select(-CT) %>% data.matrix

# Tables with score decomposition (as factors)
score_decompCTR <- df.scores %>%
  filter (CT == 0) %>%
  select(sc.BMI,  sc.TT, sc.PA, sc.FV, sc.TDF, sc.UPF, sc.MEAT, sc.SD, sc.ALC, sc.BFD, score) %>%
  mutate(sc.BMI = as.factor(sc.BMI), sc.TT = as.factor(sc.TT),sc.PA = as.factor(sc.PA), sc.FV = as.factor(sc.FV), sc.TDF = as.factor(sc.TDF), sc.UPF = as.factor(sc.UPF), sc.MEAT = as.factor(sc.MEAT), sc.SD = as.factor(sc.SD), sc.ALC = as.factor(sc.ALC), sc.BFD = as.factor(sc.BFD))
#summary(score_decompCTR)

score_decompCS <- df.scores %>%
  filter (CT == 1) %>%
  select(sc.BMI,  sc.TT, sc.PA, sc.FV, sc.TDF, sc.UPF, sc.MEAT, sc.SD, sc.ALC, sc.BFD, score) %>%
  mutate(sc.BMI = as.factor(sc.BMI), sc.TT = as.factor(sc.TT),sc.PA = as.factor(sc.PA), sc.FV = as.factor(sc.FV), sc.TDF = as.factor(sc.TDF), sc.UPF = as.factor(sc.UPF), sc.MEAT = as.factor(sc.MEAT), sc.SD = as.factor(sc.SD), sc.ALC = as.factor(sc.ALC), sc.BFD = as.factor(sc.BFD))
#summary(score_decompCS)


# Score histograms -----------------------------------------------------------------------

# Score distribution simple histogram
hist(df.scores$score, xlab = "WCRF/AICR score", main = paste("Scores in the E3N cancer group"), xlim=range(2, 8))

# Score distribution histogram, colors according to status case VS control
ggplot(table_scores) +
  aes(x = score, fill = CT, xmin = 2, xmax =8) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 22) +
  labs(x = "WCRF/AICR score", title = "WCRF/AICR scores in the E3N cancer group") 

# Test Student for score components ----------------------------------------------------------------------

# Checking variables normal distribution
normal_distrib_comp <- function (x) {
  qqnorm(matrix_scores[,x], main =colnames(matrix_scores)[x])
  qqline(matrix_scores[,x])
}
for(i in c(2:13)){normal_distrib_comp(i)}

# Test F de comparaison et test de Student apparié
var.test(BMI ~ CT, data = table_scores)
t.test(BMI ~ CT, data = table_scores)
var.test(TTAILLE ~ CT, data = table_scores)
t.test(TTAILLE ~ CT, data = table_scores)
var.test(TotalAPQ3 ~ CT, data = table_scores)
t.test(TotalAPQ3 ~ CT, data = table_scores)
var.test(fruitveg ~ CT, data = table_scores)
t.test(fruitveg ~ CT, data = table_scores)
var.test(TDF ~ CT, data = table_scores)
t.test(TDF ~ CT, data = table_scores)
var.test(percent_aUPF ~ CT, data = table_scores)
t.test(percent_aUPF ~ CT, data = table_scores)
var.test(Rmeat ~ CT, data = table_scores)
t.test(Rmeat ~ CT, data = table_scores)
var.test(Pmeat ~ CT, data = table_scores)
t.test(Pmeat ~ CT, data = table_scores)
var.test(sugary_drinks ~ CT, data = table_scores)
t.test(sugary_drinks ~ CT, data = table_scores)
var.test(ALCOHOL ~ CT, data = table_scores)
t.test(ALCOHOL ~ CT, data = table_scores)
var.test(allaitement_dureecum ~ CT, data = table_scores)
t.test(allaitement_dureecum ~ CT, data = table_scores)
var.test(score ~ CT, data = table_scores)
t.test(score ~ CT, data = table_scores)

# Standard deviations
apply(matrixCTR, 2, sd)
apply(matrixCS, 2, sd)

# Quantiles for data that doesn't follow a normal distribution
quantile(table_scores$sugary_drinks, probs = c(1/4, 3/4))
quantile(table_scores$ALCOHOL, probs = c(1/4, 3/4))
quantile(table_scores$allaitement_dureecum, probs = c(1/4, 3/4))


# Graphs for data distribution ---------------------------------------------------------------------

# Text for different points in WCRF/AICR score
rec0 <- grobTree(textGrob("0", gp=gpar(col="#000099", fontsize="11", fontface="bold")))
rec025 <- grobTree(textGrob("0.25", gp=gpar(col="#000099", fontsize="11", fontface="bold")))
rec05 <- grobTree(textGrob("0.5", gp=gpar(col="#000099", fontsize="11", fontface="bold")))
rec1 <- grobTree(textGrob("1", gp=gpar(col="#000099", fontsize="11", fontface="bold")))
text <- grobTree(textGrob('Points for WCRF/AICR score', gp=gpar(col="#000099", fontsize="10")))

# BMI
ggplot(table_scores) +
  aes(x = BMI, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x = "BMI (kg/m2)", title = "BMI distribution") +
  geom_vline(xintercept = 25, linetype = "dotted") + geom_vline(xintercept = 18.5, linetype = "dashed") + geom_vline(xintercept = 30, linetype = "dashed") +
  annotation_custom(rec0, xmax = 18.5, ymin = 270) +
  annotation_custom(rec0, xmin = 30, ymin = 270) +
  annotation_custom(rec025, xmin = 25, xmax = 30, ymin = 270) +
  annotation_custom(rec05, xmin =18.5, xmax = 25, ymin = 270) +
  annotation_custom(text, xmin =30, xmax = 55, ymin = 245)

ggplot(table_scores, aes(x = BMI, fill = CT)) +
  geom_histogram(alpha=0.2, position='identity')


ggplot(table_scores) +
  aes(x = BMI, fill = CT) +
  geom_histogram(color = "black", show.legend = FALSE) +
  facet_grid(CT ~ .) +
  labs(x = "BMI")

# Waist circumference
ggplot(table_scores) +
  aes(x = TTAILLE, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Waist circumference (cm)", title = "Waist circumference distribution") +
  geom_vline(xintercept = 88, linetype = "dashed") + geom_vline(xintercept = 80, linetype = "dashed") +
  annotation_custom(rec0, xmax = 80, ymin = 208) + 
  annotation_custom(rec025, xmin =80, xmax = 88, ymin = 208) + 
  annotation_custom(rec05, xmin =88, ymin = 208) +
  annotation_custom(text, xmin =90, xmax = 132, ymin = 170)

# Physical activity
ggplot(table_scores) +
  aes(x = TotalAPQ3, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Physical activity (MET hours/week)", title = "Physical activity distribution") +
  geom_vline(xintercept = 9.375, linetype = "dashed") + geom_vline(xintercept = 18.75, linetype = "dashed") +
  annotation_custom(rec0, xmax = 9, ymin = 185) + 
  annotation_custom(rec05, xmin =9.4, xmax = 18, ymin = 185) + 
  annotation_custom(rec1, xmin =19, ymin = 185) +
  annotation_custom(text, xmin =75, xmax = 220, ymin = 150)

# Fruits and vegetables
ggplot(table_scores) +
  aes(x = fruitveg , fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Fruits & vegetables (g/day)", title = "Fruits and vegetables consumption distribution") +
  geom_vline(xintercept = 200, linetype = "dashed") + geom_vline(xintercept = 400, linetype = "dashed") +
  annotation_custom(rec0, xmax = 200, ymin = 190) + 
  annotation_custom(rec025, xmin =200, xmax = 400, ymin = 190) + 
  annotation_custom(rec05, xmin =400, ymin = 190) +
  annotation_custom(text, xmin =1000, xmax = 2200, ymin = 160)

# Total dietary fiber
ggplot(table_scores) +
  aes(x = TDF, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Fiber (g/day)", title = "Total fiber consumption distribution") +
  geom_vline(xintercept = 15, linetype = "dashed") + geom_vline(xintercept = 30, linetype = "dashed") +
  annotation_custom(rec0, xmax = 15, ymin = 180) + 
  annotation_custom(rec025, xmin =15, xmax = 30, ymin = 180) + 
  annotation_custom(rec05, xmin =30, ymin = 180) +
  annotation_custom(text, xmin =30, xmax = 65, ymin = 150)

# aUPF
ggplot(table_scores) +
  aes(x = percent_aUPF, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Percentage of aUPF in total food consumption", title = "Percentage of aUPF distribution") +
  geom_vline(xintercept = tertile_UPF1, linetype = "dashed") + geom_vline(xintercept = tertile_UPF2, linetype = "dashed") +
  annotation_custom(rec0, xmin = tertile_UPF2, ymin = 162) + 
  annotation_custom(rec05, xmin =tertile_UPF1, xmax = tertile_UPF2, ymin = 162) + 
  annotation_custom(rec1, xmax =tertile_UPF1, ymin = 162) +
  annotation_custom(text, xmin = 10, xmax = 27, ymin = 140)

# Red meat
ggplot(table_scores) +
  aes(x = Rmeat, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Red meat consumption (g/week)", title = "Red meat concumption distribution") +
  geom_vline(xintercept = 500, linetype = "dashed") +
  annotation_custom(rec0, xmin = 1000, xmax = 1250, ymin = 175) + 
  annotation_custom(rec05, xmin =0, xmax = 400, ymin = 150) + 
  annotation_custom(rec1, xmin = 0, xmax = 400, ymin = 175) +
  annotation_custom(text, xmin = 750, xmax = 1500, ymin = 150)

# Processed meat
ggplot(table_scores) +
  aes(x = Pmeat, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Processed meat consumption (g/week)", title = "Processed meat concumption distribution") +
  geom_vline(xintercept = 21, linetype = "dashed") +geom_vline(xintercept = 100, linetype = "dashed") +
  annotation_custom(rec0, xmin = 600, xmax = 900, ymin = 190) + 
  annotation_custom(rec05, xmin = 21, xmax = 100, ymin = 190) + 
  annotation_custom(rec1, xmax = 10, ymin = 190) +
  annotation_custom(text, xmin = 500, xmax = 1200, ymin = 160)

# Sugary drinks
ggplot(table_scores) +
  aes(x = sugary_drinks, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Sugary drinks (g/day)", title = "Sugary drinks consumption distribution") +
  geom_vline(xintercept = 0, linetype = "dashed") + geom_vline(xintercept = 250, linetype = "dashed") +
  annotation_custom(rec0, xmin = 250, ymin = 620) + 
  annotation_custom(rec05, xmin =0, xmax = 250, ymin = 620) + 
  annotation_custom(rec1, xmax =0, ymin = 620) +
  annotation_custom(text, xmin =300, xmax = 600, ymin = 500)

# Alcohol
ggplot(table_scores) +
  aes(x =ALCOHOL, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Total ethanol (g/day)", title = "Total ethanol consumption distribution") +
  geom_vline(xintercept = 0, linetype = "dashed") + geom_vline(xintercept = 14, linetype = "dashed") +
  annotation_custom(rec0, xmin = 14, ymin = 335) + 
  annotation_custom(rec05, xmin =0, xmax = 14, ymin = 335) + 
  annotation_custom(rec1, xmax =0, ymin = 335) +
  annotation_custom(text, xmin =50, xmax = 160, ymin = 300, ymax = 325)

# Breastfeeding
ggplot(table_scores) +
  aes(x =allaitement_dureecum, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Breastfeeding (months)", title = "Breastfeeding distribution") +
  geom_vline(xintercept = 0, linetype = "dashed") + geom_vline(xintercept = 6, linetype = "dashed") +
  annotation_custom(rec0, xmax = 0, ymin = 370) + 
  annotation_custom(rec05, xmin =0, xmax = 6, ymin = 370) + 
  annotation_custom(rec1, xmin =6, ymin = 370) +
  annotation_custom(text, xmin =20, xmax = 35, ymin = 330)

  
# Metabolomics dataset ---------------------------------------------------------------------

# Get metabolomics data (unscaled)
ints <- read_tsv("1510_XMetaboliteE3N_cpmg_unscaled.txt") #contains the whole group
ints_clean <- ints[-rows_missing_data,] #remove rows where data was missing for the score
#ints.ctrl <- ints[meta$CT == 0, ] #contains only the 790 controls
#ints.cases <- ints[meta$CT == 1, ] #contains only the cases

# Scale to unit variance
metabolo <- scale(ints_clean)

# Check metabolites' normal distribution --------------------------------------
normal_distrib_metab <- function (x) {
  qqnorm(metabolo[,x], main =colnames(metabolo)[x])
  qqline(metabolo[,x])
}

n <- ncol(metabolo)
for(i in c(1:n)){normal_distrib_metab(i)}


# WCRF/AICR full score simple correlations with metabolites ---------------------------------------------------------------------
library("writexl")

# Simple correlation for WCRF score - Spearman correlation
simplecorSP <- function(x) cor.test(table_scores$score, x, method = "spearman")
corlistSP <- apply(metabolo, 2, simplecorSP)

# Convert to data frame and add compound names, order by correlation
library(broom)
cordatSP <- map_dfr(corlistSP, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)
write_xlsx(cordatSP, "C:\\Users\\Clougher\\score\\spearman_score_and_metabolites.xlsx") 


# Simple correlation for WCRF score - Pearsons correlation
simplecorPE <- function(x) cor.test(table_scores$score, x, method = "pearson")
corlistPE <- apply(metabolo, 2, simplecorPE)

# Convert to data frame and add compound names, order by correlation
cordatPE <- map_dfr(corlistPE, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)
write_xlsx(cordatPE, "C:\\Users\\Clougher\\score\\pearson_score_and_metabolites.xlsx") 


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

#Partial correlation controlling for Fasting and smoking status
partialcor <- function(x) {
  
  # Linear model of food intake and confounders
  mod1 <- lm(score ~ FASTING + SMK, data = df.scores[df.scores$score > 0, ])
  
  # Linear model of metabolites and confounders
  mod2 <- lm(x ~ FASTING + SMK, data = df.scores[df.scores$score > 0, ])
  
  # Correlate the two sets of residuals              
  cor.test(residuals(mod1), residuals(mod2), method = "spearman")
  
}

pcorlist <- apply(metabolo, 2, partialcor)
pcordat <- map_dfr(pcorlist, tidy) %>% bind_cols(compound = colnames(metabolo)) %>% arrange(-estimate)
head(pcordat)

plot_pcor <- ggplot(pcordat, aes(method, compound)) +
  geom_tile(aes(fill = estimate)) +
  scale_fill_gradient2() + labs(title = 'Partial correlation - Fasting and Smoking status')
plot_pcor

# Individual score components simple correlations---------------------------------------------------------------------

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

