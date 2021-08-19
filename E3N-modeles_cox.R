# E3N cohort
# Graphs for score distribution and checking variables' distribution
source("E3N-prep_data_calc_score.R")
sulibrary("survival")

# Score as a continuous variable (0.25 points increments) -------------------------------
# Univariate cox model
# bacfemme2 = niveau d'education
# statfin_dquest=statut menopausique a la fin du questionnaire
coxmod1 <- coxph(Surv(duree_suivi1, ksein) ~ score, data=df.scores_all)
coxmod2 <- coxph(Surv(duree_suivi1, ksein) ~ score + bacfemme2, data=df.scores_all)
coxmod3 <- coxph(Surv(duree_suivi1, ksein) ~ score + bacfemme2 + statfin_dquest, data=df.scores_all)

coxmod_list <- list(coxmod1, coxmod2, coxmod3) #models with score - all participants
coxmod_names <- c("no adjustment variables", "+ education level", "+ menopause")

library(broom)
tablecox <- map_df(coxmod_list, ~tidy(., exponentiate = T, conf.int=T, conf.level=0.95)) %>% filter(term == a) %>%
  mutate_if(is.numeric, ~round(.,2)) %>% unite(HR.CI, estimate, conf.low, conf.high, sep = "-") %>%
  add_column(model = coxmod_names, .before = T)

# Score as categories -------------------------------
# 0 pt: score<2, 1pt: 2 <= score < 4, 2pts: 4 <= score < 6, 3pts: 6 <= score

#score as categories (4 total)
coxmod1.2 <- coxph(Surv(duree_suivi1, ksein) ~ score_cat, data=df.scores_all)
coxmod2.2 <- coxph(Surv(duree_suivi1, ksein) ~ score_cat + bacfemme2, data=df.scores_all)
coxmod3.2 <- coxph(Surv(duree_suivi1, ksein) ~ score_cat + bacfemme2 + statfin_dquest, data=df.scores_all)

coxmodcat_list <- list(coxmod1.2, coxmod2.2, coxmod3.2) #models with score categories - all participants
coxmodcat_3names <- c("no adjustment variables", "no adjustment variables", "no adjustment variables", "+ education level", "+ education level", "+ education level", "+ menopause", "+ menopause","+ menopause")
coxmodcat_1name <- c("no adjustment variables", "+ education level", "+ menopause")
catlist <- c("score_cat1", "score_cat2", "score_cat3")
catvalues <- c("score 2-3.75", "score 4-5.75", "score 6-8")

# One table with all the score info on all the models with score as categories
tablecox_cat0 <- map_df(coxmodcat_list, ~tidy(., exponentiate = T, conf.int=T, conf.level=0.95)) %>% filter(term %in% catlist) %>%
  mutate_if(is.numeric, ~round(.,2)) %>% unite(HR.CI, estimate, conf.low, conf.high, sep = "-") %>% add_column(model = coxmodcat_3names, .before = T)

# Creatinga table with relevant data for the last model (coxmod3.2)
tablecox_lastmodel <- tidy(coxmod3.2, exponentiate = T, conf.int=T, conf.level=0.95) %>% filter(term %in% catlist) %>% 
  mutate_if(is.numeric, ~round(.,2)) %>% unite(HR.CI, estimate, conf.low, conf.high, sep = "-") %>% add_column(model = catvalues, .before = T) %>% select(-term)

# Score as other categories -------------------------------
# 0 pt: score<4, 1pt: 2 <= score < 6, 2pts 6 <= score

#score as categories (3 total)
coxmod1.3 <- coxph(Surv(duree_suivi1, ksein) ~ score_catbis, data=df.scores_all)
coxmod2.3 <- coxph(Surv(duree_suivi1, ksein) ~ score_catbis + bacfemme2, data=df.scores_all)
coxmod3.3 <- coxph(Surv(duree_suivi1, ksein) ~ score_catbis + bacfemme2 + statfin_dquest, data=df.scores_all)

# by menopausal status
coxmod3pre <- coxph(Surv(duree_suivi1, ksein) ~ score + bacfemme2 + statfin_dquest, data = df.scores_all[pre_all,])
coxmod3post <- coxph(Surv(duree_suivi1, ksein) ~ score+ bacfemme2 + statfin_dquest, data = df.scores_all[post_all,])

coxmod3.3pre <- coxph(Surv(duree_suivi1, ksein) ~ score_catbis + bacfemme2 + statfin_dquest, data = df.scores_all[pre_all,])
coxmod3.3post <- coxph(Surv(duree_suivi1, ksein) ~ score_catbis + bacfemme2 + statfin_dquest, data = df.scores_all[post_all,])
