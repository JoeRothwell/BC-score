# Case-control breast cancer study
# PLS models
source("BC-prep_data_calc_score.R")
library(pls)

# tests ------------------------------------------------------------------------
data(yarn)
data(oliveoil)
data(gasoline)

options(digits = 4)
view(head(gasoline))
gasTrain <- gasoline[1:50,] #first part of the data used to create the model
gasTest <- gasoline[51:60,] #second part left alone to see if the predictions with the model are ok

# model with 10 components, includes leave-one-out cross-validated predictions
gas1 <- plsr(octane ~ NIR, ncomp= 10, data = gasTrain, validation = "LOO")
summary(gas1)
plot(RMSEP(gas1), legendpos = "topright") #cross-validated RMSEP curves for the gasoline data
plot(gas1, ncomp = 2, asp = 1, line = TRUE) #cross-validated predictions for the gasoline data
plot(gas1, plottype = "score", comps = 1:3) #score plot for the gasoline data
explvar(gas1) #extraction of the explained variances
# end tests ------------------------------------------------------------------------

# Create a single table with score + metabolite information
metabolo_df <- as.data.frame(metabolo)
scores <- df.scores %>% select(score) 
metab_scores <- bind_cols(scores, metabolo_df) 

metab_names <- c(colnames(metabolo_df))

#First plsr model to see RMSEP and figure out optimum ncomp
modplsr_test <- plsr(score ~., data = metab_scores, validation = "LOO")
plot(RMSEP(modplsr_test, legendpos = "topright"))
#Second model with ncomp = 10 to have a more precise minimal number of ncomp
modplsr <- plsr(score ~., ncomp = 10, data = metab_scores, validation = "LOO")
plot(RMSEP(modplsr, legendpos = "topright"))
# ncomp = 5 seems to be the best choice

modplsr <- plsr(score ~., ncomp = 5, data = metab_scores, validation = "LOO")
plot(modplsr, ncomp = 5, asp = 1, line = TRUE) #not sure if it's a good thing to plot, pretty sure it's useless for the score
plot(modplsr, plottype= "score", comps = 1:5)
