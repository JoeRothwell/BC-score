# E3N cohort
# Graphs for score distribution and checking variables' distribution
source("E3N-prep_data_calc_score.R")
library("survival")

# Univariate cox model
coxmod1 <- coxph(Surv(duree_suivi_1, ksein) ~ score, data=df.scores_all)

coxmod2 <- coxph(Surv(duree_suivi_1, ksein) ~ score_cat, data=df.scores_all)

coxmod3 <- coxph(Surv(duree_suivi_1, ksein) ~ score + bacfemme2, data=df.scores_all)

summary(coxmod1)
summary(coxmod2)
summary(coxmod3)