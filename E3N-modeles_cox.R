# E3N cohort
# Graphs for score distribution and checking variables' distribution
source("E3N-prep_data_calc_score.R")
library("survival")

# -------  Trying cox regression models
data("lung")
head(lung)
# univariate cox model
res.cox <- coxph(Surv(time, status) ~ sex, data=lung)
res.cox
summary(res.cox)
# multivariate cox model
multicox <- coxph(Surv(time, status) ~ sex + ph.ecog + wt.loss, data=lung)
summary(multicox)

# ------ end of tests

# Univariate cox model
coxmod1 <- coxph(Surv(duree_suivi_1, ksein) ~ score, data=df.scores_all)
summary(coxmod1)

df.scores_all$score_cat <- as.factor(df.scores_all$score_cat)
coxmod2 <- coxph(Surv(duree_suivi_1, ksein) ~ score_cat, data=df.scores_all)
summary(coxmod2)
