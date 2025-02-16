# Case-control breast cancer study
# Preparing data and calculating WCRF/AICR score
# Originally coded by S. Clougher, JR renamed and tidied 2/5/2023
library(tidyverse)
library(readxl)
library(haven)

# Datasets-----------------------------------------------------------------------

# Food, breastfeeding, PA, fibre, education, population density, work
alim   <- read_sas("frjour.sas7bdat")
bfeed  <- read_sas("d_grossesse_20190107_corrections.sas7bdat") %>% rename(ident = IDENT)
physact <- read_sas("physicalact.sas7bdat") %>% rename(ident = IDENT) %>% select(ident, TotalAPQ3) 
fiber <- read_sas("nut_fra2.sas7bdat") %>% select(ident, KCAL, alcool, FIBR, SDF, TDF)
educ <- read_sas("D01_20180914_niveau_etudes_Q1.sas7bdat") %>% mutate(ident = IDENT) %>% select(ident, bacfemme2)
city <- read_sas("d01_20190625_ville_q4.sas7bdat")
work <- read_sas("d02_20160404_prof_q2.sas7bdat") %>% mutate(ident = IDENT) %>% select(ident, PROFQ2_F, SALAIREF)

# Correspondence ident-CODBMB
id <- read_xls("E3N_cancer du sein_21072014.xls") %>% mutate(ident = IDENT) %>% select(c("CODBMB", "ident"))

# Metadata (previously extracted for metabolomics dataset)
meta <- read_csv("metadata.csv", na = "9999")
meta$CODBMB <- as.character(meta$CODBMB)

# Create a single table (containing both cases and controls)
scoredata <- meta %>% 
  left_join(id, by = "CODBMB") %>% left_join(alim, by = "ident") %>%
  left_join(fiber, by = "ident") %>% left_join(bfeed, by = "ident") %>%
  left_join(physact, by = "ident") %>% left_join(educ, by = "ident") %>%
  left_join(city, by = "ident") %>% left_join(work, by = "ident")

# JR: Remove main datasets
rm(list = c("alim", "bfeed", "physact", "fiber", "educ", "city", "work", "id", "meta"))


# Changing column names -----------------------------------------------------------------------

# Replace leading underscores with x (from 'alim'/frjour table)
scoredata <- scoredata %>% rename_at(vars(starts_with('_')), funs(str_replace(., '_', 'x'))) 

#Calculating intakes necessary for score (adds 7 variables)
# g/day intake of total fruits & vegetables, red meat, processed meat, sugary drinks, aUPF, total food intake and 
# percentage of aUPF in total food intake
scoredata <- scoredata %>%
  rowwise() %>%
  mutate(fruitveg = 
            sum(x2, x3, x35, x40, x41_2, x41_5, x41_11, x53), #g/day
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
  mutate(percent_aUPF = (aUPF/total_food) *100) #percent of aUPF in total food intake (g/day)

# Removing missing data -----------------------------------------------------------------------
# Percentage of aUPF, breastfeeding and waist circumference
clean_data <- scoredata %>% filter(!is.na(TTAILLE) & !is.na(percent_aUPF) & !is.na(allaitement_dureecum))
vec <- scoredata$CODBMB %in% clean_data$CODBMB

# Calculate score -----------------------------------------------------------------------

#Tertiles : needed for aUPF consumption cutoff points
tertiles_UPF <- quantile(clean_data$percent_aUPF, probs = c(1/3, 2/3))
tertile_UPF1 <- as.numeric(tertiles_UPF[1]) #cut point n°1 (fully-met recommendation)
tertile_UPF2 <- as.numeric(tertiles_UPF[2]) #cut point n°2 (half-met recommendation)

# Calculating score
df.scores0 <- clean_data %>% 
  mutate(sc.BMI1 = ifelse(BMI >= 18.5 & BMI < 30, 0.25, 0), # At least 0.25 for this condition
         sc.BMI2 = ifelse(BMI >= 18.5 & BMI < 25, 0.25, 0), # Another 0.25 for this condition
         sc.TT1  = ifelse(TTAILLE <= 88, 0.25, 0), 
         sc.TT2  = ifelse(TTAILLE <= 80, 0.25, 0),
         sc.PA1  = ifelse(TotalAPQ3 >= 9.375, 0.5, 0), 
         sc.PA2  = ifelse(TotalAPQ3 >= 18.75, 0.5, 0),
         sc.FV1  = ifelse(fruitveg >= 200, 0.25, 0),
         sc.FV2  = ifelse(fruitveg >= 400, 0.25, 0),
         sc.TDF1 = ifelse(TDF >= 15, 0.25, 0),
         sc.TDF2 = ifelse(TDF >= 30, 0.25, 0),
         sc.UPF1 = ifelse(percent_aUPF < tertile_UPF2, 0.5, 0),
         sc.UPF2 = ifelse(percent_aUPF < tertile_UPF1, 0.5, 0),
         sc.MEAT1 = ifelse(Rmeat < 500 & Pmeat < 100, 0.5, 0),
         sc.MEAT2 = ifelse(Rmeat < 500 & Pmeat < 21, 0.5, 0),
         sc.SD1  = ifelse(sugary_drinks <= 250, 0.5, 0),
         sc.SD2  = ifelse(sugary_drinks == 0, 0.5, 0),
         sc.ALC1 = ifelse(ALCOHOL <= 14, 0.5, 0),
         sc.ALC2 = ifelse(ALCOHOL == 0, 0.5, 0),
         sc.BFD1 = ifelse(allaitement_dureecum > 0, 0.5, 0),
         sc.BFD2 = ifelse(allaitement_dureecum >= 6, 0.5, 0),
         # Add up sc.BMI + sc.TT + sc.PA + other recommendations to get score         
         sc.BMI = sc.BMI1 + sc.BMI2, sc.TT = sc.TT1 + sc.TT2, sc.PA = sc.PA1 + sc.PA2,
         sc.FV  = sc.FV1 + sc.FV2, sc.TDF = sc.TDF1 + sc.TDF2, sc.UPF= sc.UPF1 + sc.UPF2, sc.MEAT = sc.MEAT1 + sc.MEAT2,
         sc.SD  = sc.SD1 + sc.SD2, sc.ALC = sc.ALC1 + sc.ALC2,
         sc.BFD = sc.BFD1 + sc.BFD2,
         # Get overall score
         score = sc.BMI + sc.TT + sc.PA + sc.FV + sc.TDF + sc.UPF + sc.MEAT + sc.SD + sc.ALC + sc.BFD,
         # Score by categories (1pt: scor<4, 2pts: 4 < score < 6, 3pts: score > 6) 
         score_cat1 = ifelse(score >= 2, 1, 0), score_cat2 = ifelse(score >= 4, 1, 0), score_cat3 = ifelse(score >= 6, 1, 0),
         score_cat  = score_cat1 + score_cat2 + score_cat3)

# Create score quartiles and categories -----------------------------------------------------------------------

# Calculate quartiles
quartiles_score  <- quantile(df.scores0$score, probs = c(1/4, 2/4, 3/4))
quartiles_score1 <- as.numeric(quartiles_score[1]) 
quartiles_score2 <- as.numeric(quartiles_score[2])
quartiles_score3 <- as.numeric(quartiles_score[3]) 


df.scores <- df.scores0 %>%
  mutate(score_quart1 = ifelse(score >= quartiles_score3, 1, 0), 
         score_quart2 = ifelse(score >= quartiles_score2, 1, 0), 
         score_quart3 = ifelse(score >= quartiles_score1, 1, 0),
         score_quart = score_quart1 + score_quart2 + score_quart3)

# Mutate score quartiles and categories to factors
df.scores$score_cat <- as.factor(df.scores$score_cat)
df.scores$score_quart <- as.factor(df.scores$score_quart)


# Tables with score info -----------------------------------------------------------------------
varlist <- c("BMI", "TTAILLE", "TotalAPQ3", "fruitveg", "TDF", "percent_aUPF", "Rmeat", 
                   "Pmeat", "sugary_drinks", "ALCOHOL", "allaitement_dureecum")

# Table containing only data relevant for the score and full score
table_scores <- df.scores %>% select(CT, varlist, score) %>%
  mutate(CT = factor(CT, levels = c("0", "1"), labels = c("Controls", "Cases")))

# Table containing only score components 
table_components <- df.scores %>% select (varlist) %>%
  rename(Waist_circ.=TTAILLE, Physical_activity=TotalAPQ3, Fruits_Veg=fruitveg, Fiber=TDF, 
          aUPF=percent_aUPF, Red_meat=Rmeat, Processed_meat=Pmeat, Sugary_drinks=sugary_drinks, 
          Alcohol=ALCOHOL, Breastfeeding=allaitement_dureecum)

table_componentsFR <- df.scores %>% select (varlist) %>%
  rename(IMC = BMI, "Tour de taille"=TTAILLE, "Activite physique"=TotalAPQ3, "Fruits et legumes"=fruitveg, 
          "Fibres"=TDF, "AUTa"=percent_aUPF, "Viande rouge"=Rmeat, "Viande transformee"=Pmeat, 
          "Boissons sucrees"=sugary_drinks, "Alcool"=ALCOHOL, "Allaitement"=allaitement_dureecum)

table_componentsEN <- df.scores %>% select (varlist) %>%
  rename("Waist circumference"=TTAILLE, "Physical activity"=TotalAPQ3, "Fruits & vegetables"=fruitveg, 
          "Fibre"=TDF, "aUPF"=percent_aUPF, "Red meat"=Rmeat, "Processed meat"=Pmeat, 
          "Sugar-sweetened drinks"=sugary_drinks, "Alcohol"=ALCOHOL, "Breastfeeding"=allaitement_dureecum)


# Matrix containing score information
matrix_scores <- data.matrix(table_scores)
matrixCTR <- table_scores %>% filter (CT == "Controls") %>% select(-CT) %>% data.matrix
matrixCS <- table_scores %>% filter (CT == "Cases") %>% select(-CT) %>% data.matrix

# Tables with score decomposition (as factors)
#easy way to select only the needed variables and mutate them into factors
score_decompCTR <- df.scores %>% filter(CT == 0) %>% transmute_at(vars(sc.BMI:score), as.factor)
score_decompCS <- df.scores %>% filter(CT == 1) %>% transmute_at(vars(sc.BMI:score), as.factor)


# For subsetting ---------------------------------------------------------------------
# by menopausal status
pre <- df.scores$MENOPAUSE == 0
post <- df.scores$MENOPAUSE == 1

#dim(df.scores[pre,]) # 343
#dim(df.scores[post,]) # 1191

# Number of participants per score value and category
#summary(as.factor(df.scores$score))
#summary(as.factor(df.scores$score_cat))

# By score category
cat0_2 <- df.scores$score_cat == 0 #actually useless, no one with score < 2 in the case control study
cat2_4 <- df.scores$score_cat == 1
cat4_6 <- df.scores$score_cat == 2
cat6_8 <- df.scores$score_cat == 3

varlist1 <- c("ID", "SMK", "AGE", "ALCOHOL", "Life_Alcohol_Pattern_1", "CO", "MENOPAUSE", "DIABETE", 
             "nullipare", "age1ergross", "TotalAPQ3", "bacfemme2", "COMHAB1", "comtra1", "COMHAB2", 
             "COMTRAV2", "PROFQ2_F", "SALAIREF", "score", "score_cat")

# table with women with scores from 2 to 4 - only a few variables
soc2_4 <- df.scores[cat2_4,] %>% transmute_at(vars(varlist1), as.factor)
  
# table with women with scores from 4 to 6 - only a few variables
soc4_6 <- df.scores[cat4_6,] %>% transmute_at(vars(varlist1), as.factor)

# table with women with scores from 6 to 8 - only a few variables
soc6_8 <- df.scores[cat6_8,] %>% transmute_at(vars(varlist1), as.factor)
  
# Metabolomics dataset ---------------------------------------------------------------------

# Get metabolomics data (unscaled)
ints <- read_tsv("1510_XMetaboliteE3N_cpmg_unscaled.txt") #contains the whole group

#remove rows where data was missing for the score
#ints_clean <- ints[-rows_missing_data,]
ints_clean <- ints[vec, ]

# Scale to unit variance
metabolo <- scale(ints_clean)
