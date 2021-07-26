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
# only 173 events, not many pre-menopausal women
mod5 <- clogit(CT ~ score + SMK +DIABETE +  RTH + CO + Estro_THM + Pg_seul +  strata(MATCH), data = df.scores[pre,])

# Post-menopausal women only
mod6 <- clogit(CT ~ score + SMK +DIABETE +  RTH + CO + Estriol_vag_or +  Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores[post,])

# score by categories (0pt: score < 2, 1pt: 2 <= score < 4, 2pts: 4 <= score < 6, 3pts: 6 <= score) 
mod7 <- clogit(CT ~ score_cat + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                 Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores)

# Score by quartiles
mod8 <- clogit(CT ~ score_quart + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                 Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores)

# Adding level of education as a variable
# normal score
mod9 <- clogit(CT ~ score + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                 Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + strata(MATCH), data = df.scores)
# score as categories
mod10 <- clogit(CT ~ score_cat + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                  Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + strata(MATCH), data = df.scores)

# Adding age and daily energy intake (kcal) as a variable
# normal score
mod11 <- clogit(CT ~ score + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                 Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + AGE + KCAL + strata(MATCH), data = df.scores)
# score as categories
mod12 <- clogit(CT ~ score_cat + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                  Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + AGE + KCAL + strata(MATCH), data = df.scores)

# only added age
mod13 <- clogit(CT ~ score_cat + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                  Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + AGE + strata(MATCH), data = df.scores)
# only added daily energy intake
mod14 <- clogit(CT ~ score_cat + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                  Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + KCAL + strata(MATCH), data = df.scores)

# Models' summaries
reg_data <- function(x){
  print(summary(x))
  mod_data <- inner_join(as.data.frame(summary(x)$coefficients), as.data.frame(summary(x)$conf.int))
  #write_xlsx(mod_data, "C:\\Users\\Clougher\\score\\results_data_tables\\mod12_summary.xlsx")
  #write_xlsx(mod_data, "/Users/MacSuzanne/score/results_data_tables/mod7_summary.xlsx")
}

modlist <- list(mod1, mod2, mod3, mod4, mod9, mod11) #models with score - all participants
modlist_scorecat <- list(mod7, mod10, mod12, mod13, mod14) #models with score as categories - all participants
modlist_menop <- list(mod5, mod6) #mod5 only pre-menopause, mod6 only post-menopause

ml_names <- c("score only", "+ smk + diabete + RTH", "+ menopause + CO", "+ Estriol_vag_or + Estro_THM + Pg_seul + THM_E_Pg",
                   "+ etudes", "+ age + kcal")
ml_scorecat_names <- c("smk : THM_E_Pg", "+ etudes", "etudes + age + kcal", "etudes + age", "etudes + kcal")
ml_menop <- c("pre-menopause", "post-menopause")


library(broom)
tablemods <- map_df(modlist, ~tidy(., exponentiate = T)) %>% filter(term == "score") %>%
  mutate_if(is.numeric, ~round(.,2)) %>% unite(conf.int, sep = " _ ") %>%
  add_column(model = ml_names, .before = T)


reg_data(mod1)
reg_data(mod2)
reg_data(mod3)
reg_data(mod4)
reg_data(mod5)
reg_data(mod6)
reg_data(mod7)
reg_data(mod8)
reg_data(mod9)
reg_data(mod10)
reg_data(mod11)
reg_data(mod12)