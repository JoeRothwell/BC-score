# Case-control breast cancer study
# PLS models

source("BC-prep_data_calc_score.R")
library(pls)
data(yarn)
data(oliveoil)
data(gasoline)

options(digits = 4)
view(head(gasoline))
gasTrain <- gasoline[1:50,]
gasTest <- gasoline[51:60,]

# model with 10 components, includes leave-one-out cross-validated predictions
gas1 <- plsr(octane ~ NIR, ncomp= 10, data = gasTrain, validation = "LOO")
summary(gas1)
plot(RMSEP(gas1), legendpos = "topright") #cross-validated RMSEP curves for the gasoline data
plot(gas1, ncomp = 2, asp = 1, line = TRUE) #cross-validated predictions for the gasoline data
plot(gas1, plottype = "score", comps = 1:3) #score plot for the gasoline data
explvar(gas1) #extraction of the explained variances

# Create a single table with score + metabolite information
metabolo_df <- as.data.frame(metabolo)
scores <- df.scores %>% pull(score)
