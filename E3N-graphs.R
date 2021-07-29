# E3N cohort
# Graphs for score distribution and checking variables' distribution
source("E3N-prep_data_calc_score.R")
library(ggplot2)
library(tidyverse)
library(grid)
library(corrplot)

# Score distribution simple histogram
hist(df.scores_all$score, xlab = "score WCRF/AICR", ylab = "Fréquence", xlim=range(1, 8), main = NULL)

# Score distribution histogram, colors according to status case VS control
# need to find variable indicating breast cancer
ggplot(df.scores_all) +
  aes(x = score, xmin = 2, xmax =8) +  # k=0=pas de cancer du sein, k=1=cancer du sein
  geom_histogram(alpha = 0.5, position = "identity", bins = 22) +
  labs(x = "WCRF/AICR score", title = "WCRF/AICR scores in the E3N cancer group") 

# Score components correlations---------------------------------------------------------------------
tabcor_all <- cor(table_components_all)
corrplot(tabcor_all,  type = "upper", tl.col="black", tl.srt=40,  title = "Corrélations entre les composantes du score", mar=c(0,0,1,0)) #, title = "Score components correlations-E3N cohort")
 #tl.srt changes labels orientation, mar lowers the title


#en français
tableFR <- df.scores_all %>%
  select(imcq3, ttailleq4, TotalAPQ3, fruitveg, TDF, percent_aUPF, Rmeat, Pmeat, sugary_drinks, alcool, allaitement_dureecum) %>%
  rename(IMC=imcq3, Tour_taille=ttailleq4, Activite_physique=TotalAPQ3, Fruits_Leg=fruitveg, Fibres=TDF, 
         aUPF=percent_aUPF, Viande_rouge=Rmeat, Viande_transformee=Pmeat, Boissons_sucrees=sugary_drinks, Alcool=alcool, Allaitement=allaitement_dureecum)
tabcor_allFR <- cor(tableFR)
corrplot(tabcor_allFR, tl.col = "black", type = "upper")