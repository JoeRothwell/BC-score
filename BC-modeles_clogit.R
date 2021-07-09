 # Case-control breast cancer study

source("BC-prep_data_calc_score.R")
library(survival)
library(writexl)


# Modeles score-breast cancer ---------------------------------------------------------------------

# All participants
mod1 <- clogit(CT ~ score + strata(MATCH), data = df.scores)

mod2 <- clogit(CT ~ score + SMK + DIABETE + RTH + strata(MATCH), data = df.scores)

mod3 <- clogit(CT ~ score + SMK + DIABETE + RTH + MENOPAUSE + CO + strata(MATCH), data = df.scores)

mod4 <- clogit(CT ~ score + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                 Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores)

# Modèles stratifiés par statut ménopausique
# Pre-menopausal women only
mod5 <- clogit(CT ~ score + SMK +DIABETE +  RTH + CO + Estro_THM + Pg_seul +  strata(MATCH), data = df.scores[pre,])

# Post-menopausal women only
mod6 <- clogit(CT ~ score + SMK +DIABETE +  RTH + CO + Estriol_vag_or +  Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores[post,])

# score by categories (1pt: score<4, 2pts: 4 < score < 6, 3pts: score > 6) 
df.scores$score_cat <- as.factor(df.scores$score_cat)

mod7 <- clogit(CT ~ score_cat + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                 Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores)

# score by categories (0pt: score < 2, 1pt: 2 <= score < 4, 2pts: 4 <= score < 6, 3pts: 6 <= score) 
df.scores$score_cat <- as.factor(df.scores$score_cat)

mod7 <- clogit(CT ~ score_cat + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                 Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores)


# Score by quartiles
df.scores$score_quart <- as.factor(df.scores$score_quart)

mod8 <- clogit(CT ~ score_quart + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                 Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores)

# model summaries
reg_data <- function(x){
  print(summary(x))
  mod_data <- inner_join(as.data.frame(summary(x)$coefficients), as.data.frame(summary(x)$conf.int))
  #write_xlsx(mod_data, "C:\\Users\\Clougher\\score\\results_data_tables\\mod_summary.xlsx")
  #write_xlsx(mod_data, "/Users/MacSuzanne/score/results_data_tables/mod7_summary.xlsx")
}

reg_data(mod1)
reg_data(mod2)
reg_data(mod3)
reg_data(mod4)
reg_data(mod5)
reg_data(mod6)
reg_data(mod7)
reg_data(mod8)
