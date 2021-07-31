# E3N cohort
# Preparing data and calculating WCRF/AICR score
library(tidyverse)
library(haven)

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
size <- read_sas("anthropoq1q9_1.sas7bdat") %>% select(ident, imcq3, ttailleq4, taille, ageq3)

# Breast cancer data
cancer_postmenop <- read_sas("baseline_breast_cancer.sas7bdat") %>% select(ident, datepoint, ddiag, dtdc, ktous, ksein, agefin, statfin, duree_suivi, duree_suivi_1)
cancer <- read_sas("baseline_2.sas7bdat") %>% select(ident, datepoint, ddiag, dtdc, ktous, ksein, agefin, duree_suivi) %>%
  mutate (duree_suivi1 = agefin - ageq3ve)
# 0 : pre-menopause, 1 to 3 : post-menopause (1 unknown, 2 naturally, 3 artificially)

# Menopause
menopause <- read_sas("d01_20201103_menopauseq1q11.sas7bdat") %>% select(ident, agemeno)

# Smoking
#smk <- read_sas("D01_20131018_debfinexpo_FR_Q1Q8.sas7bdat") #%>% mutate(ident=IDENT) %>% select(ident, tabacq3)
# table only contains info on 32730 observations, too small

# Education
educ <- read_sas("D01_20180914_niveau_etudes_Q1.sas7bdat") %>% mutate(ident = IDENT) %>% select(ident, bacfemme2)

# Work
work <- read_sas("d02_20160404_prof_q2.sas7bdat") %>% mutate(ident = IDENT) %>% select(ident, PROFQ2_F, SALAIREF)

# Create a single table (containing both cases and controls)
scoredata_all <-  alim %>%
  left_join(size, by = "ident") %>% left_join(menopause, by = "ident") %>% left_join(fiber, by = "ident") %>% left_join(bfeed, by = "ident") %>%
  left_join(physact, by = "ident") %>% left_join(cancer, by = "ident") %>% left_join(educ, by = "ident") %>% left_join(work, by = "ident")
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

# Checking and cleaning missing data -----------------------------------------------------------------------
# Concerns BMI, waist circumference and breastfeeding 
#length(which(is.na(data_xnames_sums_all$imcq3))) #3 719 missing
#length(which(is.na(data_xnames_sums_all$ttailleq4))) #10 659 missing
#length(which(is.na(data_xnames_sums_all$allaitement_dureecum))) #3 336 missing

clean_data_all1 <- data_xnames_sums_all %>% filter(!is.na(ttailleq4))
clean_data_all2 <- clean_data_all1 %>% filter(!is.na(imcq3))
clean_data_all <- clean_data_all2 %>% filter(!is.na(allaitement_dureecum))
# 59 268 rows remaining 

# Calculate score -----------------------------------------------------------------------

#Tertiles : needed for aUPF consumption cutoff points
tertiles_UPF_all <- quantile(clean_data_all$percent_aUPF, probs = c(1/3, 2/3))
tertile_UPF1_all <- as.numeric(tertiles_UPF_all[1]) #cut point n°1 (fully-met recommendation)
tertile_UPF2_all <- as.numeric(tertiles_UPF_all[2]) #cut point n°2 (half-met recommendation)

# Calculating score
df.scores_all0 <- clean_data_all %>% 
  mutate(sc.BMI1 = ifelse(imcq3 >= 18.5 & imcq3 < 30, 0.25, 0), # At least 0.25 for this condition
         sc.BMI2 = ifelse(imcq3 >= 18.5 & imcq3 < 25, 0.25, 0), # Another 0.25 for this condition
         sc.TT1 = ifelse(ttailleq4 <= 88, 0.25, 0), 
         sc.TT2 = ifelse(ttailleq4 <= 80, 0.25, 0),
         sc.PA1 = ifelse(TotalAPQ3 >= 9.375, 0.5, 0), 
         sc.PA2 = ifelse(TotalAPQ3 >= 18.75, 0.5, 0),
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
         score_cat = score_cat1 + score_cat2 + score_cat3,
         # Score by other categories (0 pt: score<4, 1pt: 2 <= score < 6, 2pts 6 <= score)
         score_catbis1 = ifelse(score >= 4, 1, 0), score_catbis2 = ifelse(score >= 6, 1, 0),
         score_catbis = score_catbis1 + score_catbis2,
         # Determine menopausal status
         menop_status = ifelse(agemeno.x <= ageq3, 1,0)) 

# Create score quartiles and categories -----------------------------------------------------------------------

# Calculate quartiles
quartiles_score_all <- quantile(df.scores_all0$score, probs = c(1/4, 2/4, 3/4))
quartiles_score1_all <- as.numeric(quartiles_score_all[1]) 
quartiles_score2_all <- as.numeric(quartiles_score_all[2])
quartiles_score3_all <- as.numeric(quartiles_score_all[3]) 

df.scores_all <- df.scores_all0 %>%
  mutate(score_quart1 = ifelse(score >= quartiles_score3_all, 1, 0), 
         score_quart2 = ifelse(score >= quartiles_score2_all, 1, 0), 
         score_quart3 = ifelse(score >= quartiles_score1_all, 1, 0),
         score_quart = score_quart1 + score_quart2 + score_quart3)

# Mutate score quartiles and categories to factors
df.scores_all$score_cat <- as.factor(df.scores_all$score_cat)
df.scores_all$score_catbis <- as.factor(df.scores_all$score_catbis)
df.scores_all$score_quart <- as.factor(df.scores_all$score_quart)


# Tables with score info -----------------------------------------------------------------------
score_varlist_all <- c("imcq3", "ttailleq4", "TotalAPQ3", "fruitveg", "TDF", "percent_aUPF", "Rmeat", "Pmeat", "sugary_drinks", "alcool", "allaitement_dureecum")

# Table containing only data relevant for the score and full score
table_scores_all <- df.scores_all %>% select(score_varlist_all, score)

# Table containing only score components
table_components_all <- df.scores_all %>% select(score_varlist_all) %>%
rename(BMI=imcq3, Waist_circ=ttailleq4, Physical_act=TotalAPQ3, Fruits_Veg=fruitveg, Fiber=TDF, 
         aUPF=percent_aUPF, Red_meat=Rmeat, Processed_meat=Pmeat, Sugary_drinks=sugary_drinks, Alcohol=alcool, Breastfeeding=allaitement_dureecum)

# Table to look at the score components' distribution
table_components_all_factors <- df.scores_all %>% transmute_at(vars(sc.BMI:score), as.factor)

# For subsetting ---------------------------------------------------------------------
# by menopausal status
pre_all <- df.scores_all$menop_status == 0
post_all <- df.scores_all$menop_status == 1

# by score categories
cat0_2_all <- df.scores_all$score_cat == 0 
cat2_4_all <- df.scores_all$score_cat == 1
cat4_6_all <- df.scores_all$score_cat == 2
cat6_8_all <- df.scores_all$score_cat == 3

#varlist_all <- c("ident", "ageq3", "alcool", "nullipare", "age1ergross", "TotalAPQ3", "bacfemme2", "PROFQ2_F", "SALAIREF", "score", "score_cat")

# table with women with different score categories - only a few variables
#soc0_2_all <- df.scores_all[cat0_2_all,] %>% transmute_at(vars(varlist_all), as.factor)
#soc2_4_all <- df.scores_all[cat2_4_all,] %>% transmute_at(vars(varlist_all), as.factor)
#soc4_6_all <- df.scores_all[cat4_6_all,] %>% transmute_at(vars(varlist_all), as.factor)
#soc6_8_all <- df.scores_all[cat6_8_all,] %>% transmute_at(vars(varlist_all), as.factor)

# Number of participants per score value and category
#summary(as.factor(df.scores_all$score))
#summary(as.factor(df.scores_all$score_cat))