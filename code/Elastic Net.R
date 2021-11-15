library(caret)
library(glmnet)
library(fastDummies)
library(tidyverse)
library(h2o)
data <- read.csv("dvt_imp.csv")

# Split THA and TKA
tha <- c(27130, 27132, 27134, 27137, 27138)
tka <- c(27447, 27486, 27487)

tka_ds <- data[data$cpt %in% tha,]
tha_ds <- data[data$cpt %in% tka,]

# Extract predictors
predictors <- c(1:4,7:8, 10:44,50:56)
x_tka <- tka_ds[,predictors]

# Create new predictor set with dummy variables for multilevel categorical vars
x1_tka <- dummy_cols(x_tka, select_columns = c('race','ethnicity', 'financial_class', 
                                               'provider', 'disch_disp')
                     ,remove_selected_columns = TRUE)

# Set seed prior to cv
set.seed(42)

# Reformatting data for caret
tka_dvt_fact <- as.factor(x1_tka$dvt)

tka_dvt_data <- cbind(x1_tka, tka_dvt_fact)

# Kind of cv to use
cv_5 = trainControl(method = "cv", number = 5)

# Extracting function to get best parameters
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

# Grid search & Elastic net
# Train elastic net model using caret 
# while doing a parameter tuning of alpha
tka_dvt_elnet = train(
  tka_dvt_fact ~ ., data = tka_dvt_data,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = seq(0, 1, length = 20),
                         lambda = seq(0.0001, 1, length = 100)),
  trControl = cv_5
)

# Obtain final model
get_best_result(tka_dvt_elnet)

tka_dvt_coefficients <- coef(tka_dvt_elnet$finalModel, s = tka_dvt_elnet$bestTune$lambda)

# Function to graph coefficients
graph_coefs = function(cfs) {
  
  cofs <- as.vector(cfs[,1])
  vars <- names(cfs[,1])
  
  subset_cofs <- cofs[which(cofs != 0)]
  subset_cofs <- subset_cofs[-1]
  subset_vars <- vars[which(cofs != 0)]
  subset_vars <- subset_vars[-1]
  
  df1 <- data.frame(subset_vars, subset_cofs)
  
  # Plot
  ggplot(data = df1,
         aes(x = reorder(subset_vars, subset_cofs), y = subset_cofs)) +
    geom_col(fill = "steelblue", width = .6) +
    labs(x = "Variables", y = "Coefficients") +
    coord_flip()
  
}

graph_coefs(tka_dvt_coefficients)
