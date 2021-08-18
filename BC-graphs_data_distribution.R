# Case-control breast cancer study
# Graphs for score distribution and checking variables' distribution
source("BC-prep_data_calc_score.R")
library(ggplot2)
library(tidyverse)
library(grid)

# Score histograms -----------------------------------------------------------------------

# Score distribution simple histogram
hist(df.scores$score, xlab = "Score WCRF/AICR",  ylab = "Nombre de participantes", xlim=range(2, 8), main = NULL)

# Score distribution histogram, colors according to status case VS control
ggplot(table_scores) +
  aes(x = score, fill = CT, xmin = 2, xmax =8) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 22) +
  labs(x = "Score WCRF/AICR", y = "Nombre de participantes") +
  scale_fill_discrete(name = "Statut")

# Plot metabolites distribution --------------------------------------
normal_distrib <- function (x) {
  qqnorm(metabolo[,x], main =colnames(metabolo)[x])
  qqline(metabolo[,x])
}

n <- ncol(metabolo)
for(i in c(1:n)){normal_distrib(i)}


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


# Graphs for data distribution ---------------------------------------------------------------------

# Text for different points in WCRF/AICR score
rec0 <- grobTree(textGrob("0", gp=gpar(col="#000099", fontsize="11", fontface="bold")))
rec025 <- grobTree(textGrob("0.25", gp=gpar(col="#000099", fontsize="11", fontface="bold")))
rec05 <- grobTree(textGrob("0.5", gp=gpar(col="#000099", fontsize="11", fontface="bold")))
rec1 <- grobTree(textGrob("1", gp=gpar(col="#000099", fontsize="11", fontface="bold")))
text <- grobTree(textGrob('Points for WCRF/AICR score', gp=gpar(col="#000099", fontsize="10")))

# BMI
ggplot(table_scores) +
  aes(x = BMI, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x = "BMI (kg/m2)", title = "BMI distribution") +
  geom_vline(xintercept = 25, linetype = "dotted") + geom_vline(xintercept = 18.5, linetype = "dashed") + geom_vline(xintercept = 30, linetype = "dashed") +
  annotation_custom(rec0, xmax = 18.5, ymin = 270) +
  annotation_custom(rec0, xmin = 30, ymin = 270) +
  annotation_custom(rec025, xmin = 25, xmax = 30, ymin = 270) +
  annotation_custom(rec05, xmin =18.5, xmax = 25, ymin = 270) +
  annotation_custom(text, xmin =30, xmax = 55, ymin = 245)

# Expand to see code for the graphs of the all the other score variables
ggplot(table_scores, aes(x = BMI, fill = CT)) +
  geom_histogram(alpha=0.2, position='identity')


ggplot(table_scores) +
  aes(x = BMI, fill = CT) +
  geom_histogram(color = "black", show.legend = FALSE) +
  facet_grid(CT ~ .) +
  labs(x = "BMI")

# Waist circumference
ggplot(table_scores) +
  aes(x = TTAILLE, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Waist circumference (cm)", title = "Waist circumference distribution") +
  geom_vline(xintercept = 88, linetype = "dashed") + geom_vline(xintercept = 80, linetype = "dashed") +
  annotation_custom(rec0, xmax = 80, ymin = 208) + 
  annotation_custom(rec025, xmin =80, xmax = 88, ymin = 208) + 
  annotation_custom(rec05, xmin =88, ymin = 208) +
  annotation_custom(text, xmin =90, xmax = 132, ymin = 170)

# Physical activity
ggplot(table_scores) +
  aes(x = TotalAPQ3, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Physical activity (MET hours/week)", title = "Physical activity distribution") +
  geom_vline(xintercept = 9.375, linetype = "dashed") + geom_vline(xintercept = 18.75, linetype = "dashed") +
  annotation_custom(rec0, xmax = 9, ymin = 185) + 
  annotation_custom(rec05, xmin =9.4, xmax = 18, ymin = 185) + 
  annotation_custom(rec1, xmin =19, ymin = 185) +
  annotation_custom(text, xmin =75, xmax = 220, ymin = 150)

# Fruits and vegetables
ggplot(table_scores) +
  aes(x = fruitveg , fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Fruits & vegetables (g/day)", title = "Fruits and vegetables consumption distribution") +
  geom_vline(xintercept = 200, linetype = "dashed") + geom_vline(xintercept = 400, linetype = "dashed") +
  annotation_custom(rec0, xmax = 200, ymin = 190) + 
  annotation_custom(rec025, xmin =200, xmax = 400, ymin = 190) + 
  annotation_custom(rec05, xmin =400, ymin = 190) +
  annotation_custom(text, xmin =1000, xmax = 2200, ymin = 160)

# Total dietary fiber
ggplot(table_scores) +
  aes(x = TDF, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Fiber (g/day)", title = "Total fiber consumption distribution") +
  geom_vline(xintercept = 15, linetype = "dashed") + geom_vline(xintercept = 30, linetype = "dashed") +
  annotation_custom(rec0, xmax = 15, ymin = 180) + 
  annotation_custom(rec025, xmin =15, xmax = 30, ymin = 180) + 
  annotation_custom(rec05, xmin =30, ymin = 180) +
  annotation_custom(text, xmin =30, xmax = 65, ymin = 150)

# aUPF
ggplot(table_scores) +
  aes(x = percent_aUPF, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Percentage of aUPF in total food consumption", title = "Percentage of aUPF distribution") +
  geom_vline(xintercept = tertile_UPF1, linetype = "dashed") + geom_vline(xintercept = tertile_UPF2, linetype = "dashed") +
  annotation_custom(rec0, xmin = tertile_UPF2, ymin = 162) + 
  annotation_custom(rec05, xmin =tertile_UPF1, xmax = tertile_UPF2, ymin = 162) + 
  annotation_custom(rec1, xmax =tertile_UPF1, ymin = 162) +
  annotation_custom(text, xmin = 10, xmax = 27, ymin = 140)

# Red meat
ggplot(table_scores) +
  aes(x = Rmeat, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Red meat consumption (g/week)", title = "Red meat concumption distribution") +
  geom_vline(xintercept = 500, linetype = "dashed") +
  annotation_custom(rec0, xmin = 1000, xmax = 1250, ymin = 175) + 
  annotation_custom(rec05, xmin =0, xmax = 400, ymin = 150) + 
  annotation_custom(rec1, xmin = 0, xmax = 400, ymin = 175) +
  annotation_custom(text, xmin = 750, xmax = 1500, ymin = 150)

# Processed meat
ggplot(table_scores) +
  aes(x = Pmeat, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Processed meat consumption (g/week)", title = "Processed meat concumption distribution") +
  geom_vline(xintercept = 21, linetype = "dashed") +geom_vline(xintercept = 100, linetype = "dashed") +
  annotation_custom(rec0, xmin = 600, xmax = 900, ymin = 190) + 
  annotation_custom(rec05, xmin = 21, xmax = 100, ymin = 190) + 
  annotation_custom(rec1, xmax = 10, ymin = 190) +
  annotation_custom(text, xmin = 500, xmax = 1200, ymin = 160)

# Sugary drinks
ggplot(table_scores) +
  aes(x = sugary_drinks, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Sugary drinks (g/day)", title = "Sugary drinks consumption distribution") +
  geom_vline(xintercept = 0, linetype = "dashed") + geom_vline(xintercept = 250, linetype = "dashed") +
  annotation_custom(rec0, xmin = 250, ymin = 620) + 
  annotation_custom(rec05, xmin =0, xmax = 250, ymin = 620) + 
  annotation_custom(rec1, xmax =0, ymin = 620) +
  annotation_custom(text, xmin =300, xmax = 600, ymin = 500)

# Alcohol
ggplot(table_scores) +
  aes(x =ALCOHOL, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Total ethanol (g/day)", title = "Total ethanol consumption distribution") +
  geom_vline(xintercept = 0, linetype = "dashed") + geom_vline(xintercept = 14, linetype = "dashed") +
  annotation_custom(rec0, xmin = 14, ymin = 335) + 
  annotation_custom(rec05, xmin =0, xmax = 14, ymin = 335) + 
  annotation_custom(rec1, xmax =0, ymin = 335) +
  annotation_custom(text, xmin =50, xmax = 160, ymin = 300, ymax = 325)

# Breastfeeding
ggplot(table_scores) +
  aes(x =allaitement_dureecum, fill = CT) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  labs(x ="Breastfeeding (months)", title = "Breastfeeding distribution") +
  geom_vline(xintercept = 0, linetype = "dashed") + geom_vline(xintercept = 6, linetype = "dashed") +
  annotation_custom(rec0, xmax = 0, ymin = 370) + 
  annotation_custom(rec05, xmin =0, xmax = 6, ymin = 370) + 
  annotation_custom(rec1, xmin =6, ymin = 370) +
  annotation_custom(text, xmin =20, xmax = 35, ymin = 330)


