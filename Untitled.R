# Case-control breast cancer study
# tables for paper/master's thesis
source("BC-prep_data_calc_score.R")
source("E3N-prep_data_calc_score.R")
library(writexl)

# Table for baseline characteristics in breast cancer group----------------------------------------------
baseline <- df.scores %>% select(CT, AGE, bacfemme2, sc.BMI, RTHCat1, sc.PA, SMK, DIABETE, DIAGSAMPLINGCat1, MENOPAUSE, CO, Estriol_vag_or, Estro_THM, Pg_seul, THM_E_Pg, Trait_Horm, allaitement_dureecum, sc.BFD, score, score_cat) %>%
  mutate(bacfemme2=factor(bacfemme2), sc.BMI=factor(sc.BMI), RTHCat1=factor(RTHCat1), sc.PA=factor(sc.PA), SMK=factor(SMK), DIABETE=factor(DIABETE), DIAGSAMPLINGCat1=factor(DIAGSAMPLINGCat1), MENOPAUSE=factor(MENOPAUSE), CO=factor(CO), Estriol_vag_or=factor(Estriol_vag_or), Estro_THM=factor(Estro_THM), Pg_seul=factor(Pg_seul), THM_E_Pg=factor(THM_E_Pg), Trait_Horm=factor(Trait_Horm), sc.BFD=factor(sc.BFD), scorefact=factor(score))
baselineCTR <- baseline$CT== 0
baselineCS <- baseline$CT == 1
summaryCTR <- summary(baseline[baselineCTR,])
summaryCS <- summary(baseline[baselineCS,])

baselineCTR_table <- baseline[baselineCTR,]
baselineCTR_post <- baselineCTR_table$MENOPAUSE==1
summaryCTR_post <- summary(baselineCTR_table[baselineCTR_post,])

baselineCS_table <- baseline[baselineCS,]
baselineCS_post <- baselineCS_table$MENOPAUSE==1
summaryCS_post <- summary(baselineCS_table[baselineCS_post,])

write_xlsx(summaryCTR, "/Users/MacSuzanne/score/results_data_tables/baselineCTR.xlsx")
write_xlsx(summaryCS, "/Users/MacSuzanne/score/results_data_tables/baselineCS.xlsx")

#stanadard deviation for age
age_CTR <- baselineCTR_table %>% pull(AGE)
age_CS <- baselineCS_table %>% pull(AGE)

#standard deviation for score
scoresCTR <- baselineCTR_table %>% pull(score)
scoresCS <- baselineCS_table %>% pull(score)
sd(scoresCTR)
sd(scoresCS)

#by score category
CTRcat2_4 <- baselineCTR_table$score_cat == 1
CTRcat4_6 <- baselineCTR_table$score_cat == 2
CTRcat6_8 <- baselineCTR_table$score_cat == 3

CScat2_4 <- baselineCS_table$score_cat == 1
CScat4_6 <- baselineCS_table$score_cat == 2
CScat6_8 <- baselineCS_table$score_cat == 3

# Table for baseline characteristics in E3N -------------------------------------------------------
baseline_all0 <- df.scores_all %>% select(ksein, ageq3, bacfemme2, menop_status, score_cat)
baseline_all <- bind_cols(baseline_all0, table_components_all_factors) %>%
  mutate(ksein=factor(ksein), bacfemme2=factor(bacfemme2), menop_status=factor(menop_status))
  
cat0_2_all <- df.scores_all$score_cat == 0 
cat2_4_all <- df.scores_all$score_cat == 1
cat4_6_all <- df.scores_all$score_cat == 2
cat6_8_all <- df.scores_all$score_cat == 3
summary02_all <- summary(baseline_all[cat0_2_all,])
summary24_all <- summary(baseline_all[cat2_4_all,])
summary46_all <- summary(baseline_all[cat4_6_all,])
summary68_all <- summary(baseline_all[cat6_8_all,])

table02 <- baseline_all[cat0_2_all,]
table24 <- baseline_all[cat2_4_all,]
table46 <- baseline_all[cat4_6_all,]
table68 <- baseline_all[cat6_8_all,]
age_all <- baseline_all %>% pull(ageq3)
age02 <- table02 %>% pull(ageq3)
age24 <- table24 %>% pull(ageq3)
age46 <- table46 %>% pull(ageq3)
age68 <- table68 %>% pull(ageq3)
sd(age_all)
sd(age02)
sd(age24)
sd(age46)
sd(age68)
