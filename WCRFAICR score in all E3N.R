library(ggplot2)
library(haven)
library(tidyverse)

# Datasets-----------------------------------------------------------------------

# Food data
alim <- read_sas("frjour.sas7bdat")  

# Breastfeeding data
bfeed <- read_sas("d_grossesse_20190107_corrections.sas7bdat") %>% rename(ident = IDENT)

# Physical activity data 
physact <- read_sas("physicalact.sas7bdat") %>% rename(ident = IDENT)  

# Fiber data
fiber <- read_sas("nut_fra2.sas7bdat") %>% select(ident, alcool, FIBR, SDF, TDF)  

# Create a single table (containing both cases and controls)
scoredata_all <-  alim %>%
  left_join(fiber, by = "ident") %>% left_join(bfeed, by = "ident") %>%
  left_join(physact, by = "ident")
dim(scoredata_all)


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

#Cleaning missing data in percentage of aUPF, breastfeeding and waist circumference
# Checking if/where data is missing

#BMI : data missing 
#TTAILLE : data missing
#TotalAPQ3
which(is.na(data_xnames_sums_all$TotalAPQ3)) #nothing missing
#fruitveg
which(is.na(data_xnames_sums_all$fruitveg)) #nothing missing
#TDF
which(is.na(data_xnames_sums_all$TDF)) #nothing missing
#percent_aUPF
which(is.na(data_xnames_sums_all$percent_aUPF)) #nothing missing
#Rmeat
which(is.na(data_xnames_sums_all$Rmeat)) #nothing missing
#Pmeat
which(is.na(data_xnames_sums_all$Pmeat)) #nothing missing
#sugary_drinks
which(is.na(data_xnames_sums_all$sugary_drinks)) #nothing missing
#ALCOHOL 
which(is.na(data_xnames_sums_all$alcool)) #nothing missing
#allaitement_dureecum
data_allaitement <- which(is.na(data_xnames_sums_all$allaitement_dureecum))
length(data_allaitement) #3336 rows with missing breastfeeding data

clean_data_all <- data_xnames_sums_all %>% filter(!is.na(allaitement_dureecum))
dim(clean_data_all)
# 71186 rows remaining
                                                  
#Tertiles : needed for aUPF consumption cutoff points
tertiles_UPF <- quantile(clean_data_all$percent_aUPF, probs = c(1/3, 2/3))
tertile_UPF1 <- as.numeric(tertiles_UPF[1]) #cut point n°1 (fully-met recommendation)
tertile_UPF2 <- as.numeric(tertiles_UPF[2]) #cut point n°2 (half-met recommendation)

# Calculate score -----------------------------------------------------------------------

df.scores_all <- clean_data_all %>% 
  mutate(#sc.BMI1 = ifelse(BMI >= 18.5 & BMI < 30, 0.25, 0), # At least 0.25 for this condition
         #sc.BMI2 = ifelse(BMI >= 18.5 & BMI < 25, 0.25, 0), # Another 0.25 for this condition
         #sc.TT1  = ifelse(TTAILLE <= 88, 0.25, 0), 
         #sc.TT2  = ifelse(TTAILLE <= 80, 0.25, 0),
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
         sc.ALC1 = ifelse(alcool <= 14, 0.5, 0),
         sc.ALC2 = ifelse(alcool == 0, 0.5, 0),
         sc.BFD1 = ifelse(allaitement_dureecum > 0, 0.5, 0),
         sc.BFD2 = ifelse(allaitement_dureecum >= 6, 0.5, 0),
         # Add up sc.BMI + sc.TT + sc.PA + other recommendations to get score         
         #sc.BMI = sc.BMI1 + sc.BMI2, sc.TT = sc.TT1 + sc.TT2, 
         sc.PA = sc.PA1 + sc.PA2,
         sc.FV = sc.FV1 + sc.FV2, sc.TDF = sc.TDF1 + sc.TDF2, sc.UPF= sc.UPF1 + sc.UPF2, sc.MEAT = sc.MEAT1 + sc.MEAT2,
         sc.SD = sc.SD1 + sc.SD2, sc.ALC = sc.ALC1 + sc.ALC2,
         sc.BFD = sc.BFD1 + sc.BFD2,
         # Get overall score
         score =  sc.PA + sc.FV + sc.TDF + sc.UPF + sc.MEAT + sc.SD + sc.ALC + sc.BFD) #sc.BMI + sc.TT removed

# Tables with score info -----------------------------------------------------------------------

# Table containing only data relevant for the score and full score
table_scores_all <- df.scores_all %>%
  select(CT, BMI, TTAILLE, TotalAPQ3, fruitveg, TDF, percent_aUPF, Rmeat, Pmeat, sugary_drinks, ALCOHOL, allaitement_dureecum, score) %>%
  mutate(CT = factor(CT, levels = c("0", "1"), labels = c("Controls", "Cases")))


# Score histograms -----------------------------------------------------------------------

# Score distribution simple histogram
hist(df.scores_all$score, xlab = "WCRF/AICR score", main = paste("Scores in the E3N cohort"), xlim=range(2, 8))

summary(df.scores_all$score)

# Score distribution histogram, colors according to status case VS control
ggplot(table_scores_all) +
  aes(x = score, fill = CT, xmin = 2, xmax =8) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 22) +
  labs(x = "WCRF/AICR score", title = "WCRF/AICR scores in the E3N cancer group") 
