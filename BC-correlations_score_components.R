# Case-control breast cancer study
# score components' correlation
source("BC-prep_data_calc_score.R")
library(broom)
library(corrplot)

# Plot of score components correlations (with each other)
mcor <- cor(table_scores, use = "complete.obs")
tabcor <- cor(table_componentsFR)
corrplot(tabcor, method = "color", type = "upper", 
         diag = FALSE,  #remove diagonal
         tl.col="black", tl.srt=40, tl.cex = 0.9, #change labels color (col), orientation (srt) and size (cex)
         addCoef.col = "black", #add correlation coefficients
         number.cex = 0.5, #change correlation coefficients' font size
         mar=c(0,0,1,0),) #lower the title

# Modeles lineaire pour vérifier que toutes les composantes du score apportent bien de l'information
lin_mod <- lm (score ~ BMI + TTAILLE + TotalAPQ3 + fruitveg + TDF + percent_aUPF + Rmeat + Pmeat + sugary_drinks + ALCOHOL + allaitement_dureecum, data = df.scores)

lin_mod2 <- lm (score ~ sc.BMI + sc.TT + sc.PA + sc.FV + sc.TDF + sc.UPF + sc.MEAT + sc.SD + sc.ALC + sc.BFD , data = df.scores)


# Test Student for score components ----------------------------------------------------------------------

# Checking variables normal distribution
normal_distrib_comp <- function (x) {
  qqnorm(matrix_scores[,x], main =colnames(matrix_scores)[x])
  qqline(matrix_scores[,x])
}
for(i in c(2:13)){normal_distrib_comp(i)}

# Test F de comparaison et test de Student apparié
var.test(BMI ~ CT, data = table_scores)
t.test(BMI ~ CT, data = table_scores)
var.test(TTAILLE ~ CT, data = table_scores)
t.test(TTAILLE ~ CT, data = table_scores)
var.test(TotalAPQ3 ~ CT, data = table_scores)
t.test(TotalAPQ3 ~ CT, data = table_scores)
var.test(fruitveg ~ CT, data = table_scores)
t.test(fruitveg ~ CT, data = table_scores)
var.test(TDF ~ CT, data = table_scores)
t.test(TDF ~ CT, data = table_scores)
var.test(percent_aUPF ~ CT, data = table_scores)
t.test(percent_aUPF ~ CT, data = table_scores)
var.test(Rmeat ~ CT, data = table_scores)
t.test(Rmeat ~ CT, data = table_scores)
var.test(Pmeat ~ CT, data = table_scores)
t.test(Pmeat ~ CT, data = table_scores)
var.test(sugary_drinks ~ CT, data = table_scores)
t.test(sugary_drinks ~ CT, data = table_scores)
var.test(ALCOHOL ~ CT, data = table_scores)
t.test(ALCOHOL ~ CT, data = table_scores)
var.test(allaitement_dureecum ~ CT, data = table_scores)
t.test(allaitement_dureecum ~ CT, data = table_scores)
var.test(score ~ CT, data = table_scores)
t.test(score ~ CT, data = table_scores)

# Standard deviations
apply(matrixCTR, 2, sd)
apply(matrixCS, 2, sd)

# Quantiles for data that doesn't follow a normal distribution
quantile(table_scores$sugary_drinks, probs = c(1/4, 3/4))
quantile(table_scores$ALCOHOL, probs = c(1/4, 3/4))
quantile(table_scores$allaitement_dureecum, probs = c(1/4, 3/4))