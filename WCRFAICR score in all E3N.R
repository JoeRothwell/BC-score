library(ggplot2)
library(haven)
library(tidyverse)
library(corrplot)

# Datasets-----------------------------------------------------------------------

# Food data
alim <- read_sas("frjour.sas7bdat")  

# Breastfeeding data
bfeed <- read_sas("d_grossesse_20190107_corrections.sas7bdat") %>% rename(ident = IDENT)

# Physical activity data 
physact <- read_sas("physicalact.sas7bdat") %>% rename(ident = IDENT) %>% select(ident, TotalAPQ3)  

# Fiber data
fiber <- read_sas("nut_fra2.sas7bdat") %>% select(ident, alcool, FIBR, SDF, TDF)  

# BMI and waist size data
size <- read_sas("anthropoq1q9_1.sas7bdat") %>% select(ident, imcbmb, imcq3, ttaillebmb, ttailleq4, thanchebmb, taille, poidsbmb, agebmb)

# Create a single table (containing both cases and controls)
scoredata_all <-  alim %>%
  left_join(size, by = "ident") %>% left_join(fiber, by = "ident") %>% left_join(bfeed, by = "ident") %>%
  left_join(physact, by = "ident")
#dim(scoredata_all) # 74 552 rows 

# Manipulating data for score -----------------------------------------------------------------------

#Renaming all columns starting with "_" (from 'alim'/frjour table)
data_xnames_all <- scoredata_all %>%
  rename_at(vars(starts_with('_')), funs(str_replace(., '_', 'x'))) 

#Calculating intakes necessary for score 
# g/day intake of total fruits & vegetables, red meat, processed meat, sugary drinks, aUPF, total food intake and percentage of aUPF in total food intake
data_xnames_sums_all <- data_xnames_all %>%
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

# Cleaning missing data in percentage of BMI, waist circumference and breastfeeding 
# No missing data for food components since all the tables were linked to the alim table
# Checking if/where data is missing
#length(which(is.na(data_xnames_sums_all$imcq3))) #3 719 missing
#length(which(is.na(data_xnames_sums_all$ttailleq4))) #10 659 missing
#length(which(is.na(data_xnames_sums_all$allaitement_dureecum))) #3 336 missing

clean_data_all1 <- data_xnames_sums_all %>% filter(!is.na(ttailleq4))

#length(which(is.na(clean_data_all1$imcq3))) #2 920 missing
clean_data_all2 <- clean_data_all1 %>% filter(!is.na(imcq3))
#length(which(is.na(clean_data_all2$allaitement_dureecum))) # 1 675 missing
clean_data_all <- clean_data_all2 %>% filter(!is.na(allaitement_dureecum))
#dim(clean_data_all)
# 59 268 rows remaining 

#Tertiles : needed for aUPF consumption cutoff points
tertiles_UPF_all <- quantile(clean_data_all$percent_aUPF, probs = c(1/3, 2/3))
tertile_UPF1_all <- as.numeric(tertiles_UPF_all[1]) #cut point n°1 (fully-met recommendation)
tertile_UPF2_all <- as.numeric(tertiles_UPF_all[2]) #cut point n°2 (half-met recommendation)

# Calculate score -----------------------------------------------------------------------

df.scores_all0 <- clean_data_all %>% 
  mutate(sc.BMI1 = ifelse(imcq3 >= 18.5 & imcq3 < 30, 0.25, 0), # At least 0.25 for this condition
         sc.BMI2 = ifelse(imcq3 >= 18.5 & imcq3 < 25, 0.25, 0), # Another 0.25 for this condition
         sc.TT1  = ifelse(ttailleq4 <= 88, 0.25, 0), 
         sc.TT2  = ifelse(ttailleq4 <= 80, 0.25, 0),
         sc.PA1  = ifelse(TotalAPQ3 >= 9.375, 0.5, 0), 
         sc.PA2  = ifelse(TotalAPQ3 >= 18.75, 0.5, 0),
         sc.FV1 = ifelse(fruitveg >= 200, 0.25, 0),
         sc.FV2 = ifelse(fruitveg >= 400, 0.25, 0),
         sc.TDF1 = ifelse(TDF >= 15, 0.25, 0),
         sc.TDF2 = ifelse(TDF >= 30, 0.25, 0),
         sc.UPF1 = ifelse(percent_aUPF < tertile_UPF2_all, 0.5, 0),
         sc.UPF2 = ifelse(percent_aUPF < tertile_UPF1_all, 0.5, 0),
         sc.MEAT1 = ifelse(Rmeat < 500 & Pmeat < 100, 0.5, 0),
         sc.MEAT2 = ifelse(Rmeat < 500 & Pmeat < 21, 0.5, 0),
         sc.SD1 = ifelse(sugary_drinks <= 250, 0.5, 0),
         sc.SD2 = ifelse(sugary_drinks == 0, 0.5, 0),
         sc.ALC1 = ifelse(alcool <= 14, 0.5, 0),
         sc.ALC2 = ifelse(alcool == 0, 0.5, 0),
         sc.BFD1 = ifelse(allaitement_dureecum > 0, 0.5, 0),
         sc.BFD2 = ifelse(allaitement_dureecum >= 6, 0.5, 0),
         # Add up sc.BMI + sc.TT + sc.PA + other recommendations to get score         
         sc.BMI = sc.BMI1 + sc.BMI2, sc.TT = sc.TT1 + sc.TT2, 
         sc.PA = sc.PA1 + sc.PA2,
         sc.FV = sc.FV1 + sc.FV2, sc.TDF = sc.TDF1 + sc.TDF2, sc.UPF= sc.UPF1 + sc.UPF2, sc.MEAT = sc.MEAT1 + sc.MEAT2,
         sc.SD = sc.SD1 + sc.SD2, sc.ALC = sc.ALC1 + sc.ALC2,
         sc.BFD = sc.BFD1 + sc.BFD2,
         # Get overall score
         score =  sc.BMI + sc.TT + sc.PA + sc.FV + sc.TDF + sc.UPF + sc.MEAT + sc.SD + sc.ALC + sc.BFD, 
         # Score by categories (0 pt: score<2, 1pt: 2 <= score < 4, 2pts: 4 <= score < 6, 3pts: 6 <= score) 
         score_cat1 = ifelse(score >= 2, 1, 0), score_cat2 = ifelse(score >= 4, 1, 0), score_cat3 = ifelse(score >= 6, 1, 0),
         score_cat = score_cat1 + score_cat2 + score_cat3) 

# Score by quartiles
quartiles_score_all <- quantile(df.scores_all0$score, probs = c(1/4, 2/4, 3/4))
quartiles_score1_all <- as.numeric(quartiles_score_all[1]) 
quartiles_score2_all <- as.numeric(quartiles_score_all[2])
quartiles_score3_all <- as.numeric(quartiles_score_all[3]) 

quartiles_score_all

df.scores_all <- df.scores_all0 %>%
  mutate(score_quart1 = ifelse(score >= quartiles_score3_all, 1, 0), 
         score_quart2 = ifelse(score >= quartiles_score2_all, 1, 0), 
         score_quart3 = ifelse(score >= quartiles_score1_all, 1, 0),
         score_quart = score_quart1 + score_quart2 + score_quart3)


# Tables with score info -----------------------------------------------------------------------

# Table containing only data relevant for the score and full score
table_scores_all <- df.scores_all %>%
  select(imcq3, ttailleq4, TotalAPQ3, fruitveg, TDF, percent_aUPF, Rmeat, Pmeat, sugary_drinks, alcool, allaitement_dureecum, score)

# Table containing only score components
table_components_all <- df.scores_all %>%
  select(imcq3, ttailleq4, TotalAPQ3, fruitveg, TDF, percent_aUPF, Rmeat, Pmeat, sugary_drinks, alcool, allaitement_dureecum) %>%
  rename(BMI=imcq3, Waist_circ=ttailleq4, Physical_act=TotalAPQ3, Fruits_Veg=fruitveg, Fiber=TDF, 
          aUPF=percent_aUPF, Red_meat=Rmeat, Processed_meat=Pmeat, Sugary_drinks=sugary_drinks, Alcohol=alcool, Breastfeeding=allaitement_dureecum)

# Table to look at the score components' distribution
table_components_all_factors <- df.scores_all %>%
  select(sc.BMI, sc.TT, sc.PA, sc.FV, sc.TDF, sc.UPF, sc.MEAT, sc.SD, sc.ALC, sc.BFD, score) %>%
  mutate(sc.BMI=factor(sc.BMI), sc.TT=factor(sc.TT), sc.PA=factor(sc.PA), sc.FV=factor(sc.FV), sc.TDF=factor(sc.TDF), 
         sc.UPF=factor(sc.UPF), sc.MEAT=factor(sc.MEAT), sc.SD=factor(sc.SD), sc.ALC=factor(sc.ALC), sc.BFD=factor(sc.BFD))
#summary(table_components_all_factors)

# For subsetting ---------------------------------------------------------------------

# Number of participants per score value and category
summary(as.factor(df.scores_all$score))
summary(as.factor(df.scores_all$score_cat))

# Score histograms -----------------------------------------------------------------------

# Score distribution simple histogram
hist(df.scores_all$score, xlab = "WCRF/AICR score", main = paste("Scores in the E3N cohort"), xlim=range(2, 8))

# Score distribution histogram, colors according to status case VS control
# need to find variabe indicating breast cancer
ggplot(table_scores_all) +
  aes(x = score, fill = CT, xmin = 2, xmax =8) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 22) +
  labs(x = "WCRF/AICR score", title = "WCRF/AICR scores in the E3N cancer group") 

# Score components correlations---------------------------------------------------------------------
tabcor_all <- cor(table_components_all)
corrplot(tabcor_all, tl.col = "black", type = "upper") #, title = "Score components correlations-E3N cohort")

#en français
tableFR <- df.scores_all %>%
  select(imcq3, ttailleq4, TotalAPQ3, fruitveg, TDF, percent_aUPF, Rmeat, Pmeat, sugary_drinks, alcool, allaitement_dureecum) %>%
  rename(IMC=imcq3, Tour_taille=ttailleq4, Activite_physique=TotalAPQ3, Fruits_Leg=fruitveg, Fibres=TDF, 
         aUPF=percent_aUPF, Viande_rouge=Rmeat, Viande_transformee=Pmeat, Boissons_sucrees=sugary_drinks, Alcool=alcool, Allaitement=allaitement_dureecum)
tabcor_allFR <- cor(tableFR)
corrplot(tabcor_allFR, tl.col = "black", type = "upper")
