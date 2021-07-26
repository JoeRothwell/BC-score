# E3N cohort
# Graphs for score distribution and checking variables' distribution
source("E3N-prep_data_calc_score.R")
library("survival")

# Univariate cox model

coxmod1 <- coxph(Surv(duree_suivi1, ksein) ~ score, data=df.scores_all)
#adjusting for education level
coxmod2 <- coxph(Surv(duree_suivi1, ksein) ~ score + bacfemme2, data=df.scores_all)
#adjusting for education level and menopausal status
coxmod3 <- coxph(Surv(duree_suivi1, ksein) ~ score + bacfemme2 + statfin_dquest, data=df.scores_all)

summary(coxmod1)
summary(coxmod2)
summary(coxmod3)

# Score as categories -------------------------------

#score as categories (4 total)
coxmod1.2 <- coxph(Surv(duree_suivi1, ksein) ~ score_cat, data=df.scores_all)
#score categories, adjusting for education level
coxmod2.2 <- coxph(Surv(duree_suivi1, ksein) ~ score_cat + bacfemme2, data=df.scores_all)
#score categories, adjusting for education level and menopausal status
coxmod3.2 <- coxph(Surv(duree_suivi1, ksein) ~ score_cat + bacfemme2 + statfin_dquest, data=df.scores_all)

summary(coxmod1.2)
summary(coxmod2.2)
summary(coxmod3.2)

summary(as.factor(df.scores_all$statfin_dquest.x))
summary(as.factor(df.scores_all$statfin_dquest.y))
ggr <- df.scores_all %>% select(ident, statfin_dquest.x, statfin_dquest.y)
