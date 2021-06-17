# Case-control breast cancer study

source("BC-prep_data_calc_score.R")
library(survival)


# Modeles score-breast cancer ---------------------------------------------------------------------

# All participants
mod1 <- clogit(CT ~ score + strata(MATCH), data = df.scores)
mod1


