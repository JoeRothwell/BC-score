# Score components' table as factors
table_scores2 <- df.scores %>%
  select(CT, sc.BMI,  sc.TT, sc.PA, sc.FV, sc.TDF, sc.UPF, sc.MEAT, sc.SD, sc.ALC, sc.BFD, score) %>%
  mutate(CT = factor(CT, levels = c("0", "1"), labels = c("Controls", "Cases")))

# Score components individual values as factors
scores_fct <- table_scores2 %>%
  mutate(CT = as.factor(CT), sc.BMI = as.factor(sc.BMI), sc.TT = as.factor(sc.TT),sc.PA = as.factor(sc.PA), sc.FV = as.factor(sc.FV), sc.TDF = as.factor(sc.TDF), sc.UPF = as.factor(sc.UPF), sc.MEAT = as.factor(sc.MEAT), sc.SD = as.factor(sc.SD), sc.ALC = as.factor(sc.ALC), sc.BFD = as.factor(sc.BFD))


#Waist circumference

#Physical activity

#Fruits and vegetables

#Total fiber consumption

#Red meat

#Processed meat

#Percentage of aUPF

#Sugary drinks

#Alcohol

#Breastfeeding

#wCRF/AICR score

#Waist circumference
qqnorm(table_scores$TTAILLE, main = 'Waist circumference')
qqline(table_scores$TTAILLE)
#Physical activity
qqnorm(table_scores$TotalAPQ3, main = 'Physical activity')
qqline(table_scores$TotalAPQ3)
#Fruits and vegetables
qqnorm(table_scores$fruitveg, main = 'Fruits & vegetables')
qqline(table_scores$fruitveg)
#Total fiber consumption
qqnorm(table_scores$TDF, main = 'Fiber')
qqline(table_scores$TDF)
#Red meat
qqnorm(table_scores$Rmeat, main = 'Red meat')
qqline(table_scores$Rmeat)
#Processed meat
qqnorm(table_scores$Pmeat, main = 'Processed meat')
qqline(table_scores$Pmeat)
#Percentage of aUPF
qqnorm(table_scores$percent_aUPF, main = 'aUPF')
qqline(table_scores$percent_aUPF)
#Sugary drinks
qqnorm(table_scores$sugary_drinks, main = 'Sugary drinks')
qqline(table_scores$sugary_drinks)
#Alcohol
qqnorm(table_scores$ALCOHOL, main = 'Ethanol')
qqline(table_scores$ALCOHOL)
#Breastfeeding
qqnorm(table_scores$allaitement_dureecum, main = 'Durée cumulée allaitement')
qqline(table_scores$allaitement_dureecum)
#wCRF/AICR score
qqnorm(table_scores$score, main = 'WCRF/AICR score')
qqline(table_scores$score)