 # Case-control breast cancer study
source("BC-prep_data_calc_score.R")
library(survival)
library(writexl)

# Modeles score-breast cancer, stratifiés par appariement cas-témoin (MATCH)

mod1 <- clogit(CT ~ score + strata(MATCH), data = df.scores)
mod2 <- clogit(CT ~ score + SMK + DIABETE + RTH + strata(MATCH), data = df.scores)
mod3 <- clogit(CT ~ score + SMK + DIABETE + RTH + MENOPAUSE + CO + strata(MATCH), data = df.scores)
mod4 <- clogit(CT ~ score + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores)
mod5 <- clogit(CT ~ score + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + strata(MATCH), data = df.scores)
mod6 <- clogit(CT ~ score + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + KCAL + strata(MATCH), data = df.scores)
mod7 <- clogit(CT ~ score + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + KCAL + AGE + strata(MATCH), data = df.scores)

# Sepaerated pre and post menopausal participants
# only 173 events, not many pre-menopausal women
mod8 <- clogit(CT ~ score + SMK +DIABETE +  RTH + CO + Estro_THM + Pg_seul +  strata(MATCH), data = df.scores[pre,])
mod9 <- clogit(CT ~ score + SMK +DIABETE +  RTH + CO + Estriol_vag_or +  Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores[post,])


# Presenting data in a singular table
modlist <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7) #models with score - all participants
modlist_menop <- list(mod8, mod9) #mod5 only pre-menopause, mod6 only post-menopause

ml_names <- c("no adjustment variables", "+ smoking +diabete + RTH", "+ menopause + CO", "+ THM","+ etudes", "+ kcal intake", "+ age")
ml_menop <- c("pre-menopause", "post-menopause")

library(broom)
tablemods <- map_df(modlist, ~tidy(., exponentiate = T, conf.int=T)) %>% filter(term == "score") %>%
  mutate_if(is.numeric, ~round(.,2)) %>% unite(OR.CI, estimate, conf.high, conf.low, sep = "-") %>% 
  add_column(model = ml_names, .before = T)


# With score as quartiles 
mod4.3 <- clogit(CT ~ score_quart + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + 
                   Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores)

# With score as categories -----------------------------------------------------------------------------------
# score by categories (0pt: score < 2, 1pt: 2 <= score < 4, 2pts: 4 <= score < 6, 3pts: 6 <= score) 
mod4.2 <- clogit(CT ~ score_cat + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + Estro_THM + Pg_seul + THM_E_Pg + strata(MATCH), data = df.scores)
mod5.2 <- clogit(CT ~ score_cat + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + strata(MATCH), data = df.scores)
mod6.2 <- clogit(CT ~ score_cat + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + KCAL + strata(MATCH), data = df.scores)
mod7.2 <- clogit(CT ~ score_cat + SMK + DIABETE + RTH + MENOPAUSE + CO + Estriol_vag_or + Estro_THM + Pg_seul + THM_E_Pg + bacfemme2 + AGE + KCAL + strata(MATCH), data = df.scores)

# Put models in separate lists (separate columns in table)
modlist_scorecat <- list(mod4.2, mod5.2, mod6.2, mod7.2) #models with score as categories - all participants
ml_scorecat_names <- c("smk : THM_E_Pg","smk : THM_E_Pg", "+ etudes","+ etudes",  "+ kcal", "+ kcal", "+ age", "+ age")
catlist <- c("score_cat2", "score_cat3")
catvalues <- c("score 4-5.75", "score 6-8")

# One table with all the score info on all the models with score as categories
tablemods_cat0 <- map_df(modlist_scorecat, ~tidy(., exponentiate = T, conf.int=T, conf.level=0.95)) %>% filter(term %in% catlist) %>%
  mutate_if(is.numeric, ~round(.,2)) %>% unite(HR.CI, estimate, conf.low, conf.high, sep = "-") %>% add_column(model = ml_scorecat_names, .before = T)

# Creatinga table with relevant data for the last model (mod7.2)
tablemods_lastmodel <- tidy(mod7.2, exponentiate = T, conf.int=T, conf.level=0.95) %>% filter(term %in% catlist) %>% 
  mutate_if(is.numeric, ~round(.,2)) %>% unite(HR.CI, estimate, conf.low, conf.high, sep = "-") %>% add_column(model = catvalues, .before = T) %>% select(-term)


# Models' summaries --------------------------------------------------------------------------------------------------------
reg_data <- function(x){
  print(summary(x))
  mod_data <- inner_join(as.data.frame(summary(x)$coefficients), as.data.frame(summary(x)$conf.int))
  #write_xlsx(mod_data, "C:\\Users\\Clougher\\score\\results_data_tables\\mod12_summary.xlsx")
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
reg_data(mod9)
reg_data(mod10)
reg_data(mod11)
reg_data(mod12)