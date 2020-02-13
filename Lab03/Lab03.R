library(tidyverse)

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

get_complexity = function(model) {
  length(coef(model)) - 1
}

# https://stackoverflow.com/questions/43123462/how-to-obtain-rmse-out-of-lm-result
RMSE_2 = function(model)
{
  RSS <- c(crossprod(model$residuals))
  MSE <- RSS / length(model$residuals)
  RMSE <- sqrt(MSE)
  
  return(RMSE)
}

get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

################################## EXERCISE 1 ##################################

### 1 ###
Ames <- read.csv("ames.csv", stringsAsFactors = FALSE)
Ames$OverallCond <- NULL
Ames$OverallQual <- NULL
Ames[is.na(Ames)] <- 0

### 2 ###
FullModel <- lm(SalePrice ~ ., data=Ames)
ForwardSelectStart <- lm(SalePrice ~ 1, data=Ames)

step(ForwardSelectStart, direction="forward", scope=formula(FullModel), steps=15)

comp1 <- lm(formula = SalePrice ~ Neighborhood, data = Ames)
comp2 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea, data = Ames)
comp3 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual, data = Ames)
comp4 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
              KitchenQual, data = Ames)
comp5 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
              KitchenQual + RoofMatl, data = Ames)
comp6 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
              KitchenQual + RoofMatl + BsmtFinSF1, data = Ames)
comp7 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
              KitchenQual + RoofMatl + BsmtFinSF1 + MSSubClass, data = Ames)
comp8 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
              KitchenQual + RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure, data = Ames)
comp9 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
              KitchenQual + RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + 
              Condition2, data = Ames)
comp10 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
               KitchenQual + RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + 
               Condition2 + ExterQual, data = Ames)
comp11 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
               KitchenQual + RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + 
               Condition2 + ExterQual + GarageArea, data = Ames)
comp12 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
               KitchenQual + RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + 
               Condition2 + ExterQual + GarageArea + Functional, data = Ames)
comp13 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
               KitchenQual + RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + 
               Condition2 + ExterQual + GarageArea + Functional + SaleCondition, data = Ames)
comp14 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
               KitchenQual + RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + 
               Condition2 + ExterQual + GarageArea + Functional + SaleCondition + 
               LotArea, data = Ames)
comp15 <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
               KitchenQual + RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + 
               Condition2 + ExterQual + GarageArea + Functional + SaleCondition + 
               LotArea + YearBuilt, data = Ames)

### 3 ###
Complexity <- c(1:15)

RMSEs <- c(
  RMSE_2(comp1),
  RMSE_2(comp2),
  RMSE_2(comp3),
  RMSE_2(comp4),
  RMSE_2(comp5),
  RMSE_2(comp6),
  RMSE_2(comp7),
  RMSE_2(comp8),
  RMSE_2(comp9),
  RMSE_2(comp10),
  RMSE_2(comp11),
  RMSE_2(comp12),
  RMSE_2(comp13),
  RMSE_2(comp14),
  RMSE_2(comp15)
)

plot(
  x=Complexity,
  y=RMSEs
)
################################################################################

set.seed(9)
num_obs = nrow(Ames)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = Ames[train_index, ]
test_data = Ames[-train_index, ]

fit_0 = lm(SalePrice ~ 1, data = train_data)
get_complexity(fit_0)

get_rmse(model = fit_0, data = train_data, response = "SalePrice") # train RMSE
get_rmse(model = fit_0, data = test_data, response = "SalePrice") # test RMSE

model_list <- list(comp1, comp2, comp3, comp4, comp5)

train_rmse = sapply(model_list, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(model_list, get_rmse, data = test_data, response = "SalePrice")
model_complexity = sapply(model_list, get_complexity)

plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity, test_rmse, type = "b", col = "darkorange")

################################## EXERCISE 2 ##################################

### 1 ###
model_list2 <- list(comp1, comp2, comp3, comp4, comp5, comp6, comp7, comp8, comp9, comp10,
                   comp11, comp12, comp13, comp14, comp15)

train_rmse2 = sapply(model_list2, get_rmse, data = train_data, response = "SalePrice")
test_rmse2 = sapply(model_list2, get_rmse, data = test_data, response = "SalePrice")
model_complexity2 = sapply(model_list2, get_complexity)

plot(model_complexity2, train_rmse2, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity2, test_rmse2, type = "b", col = "darkorange")

### 2 ###
step(ForwardSelectStart, direction="forward", scope=formula(FullModel))
forward <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
               KitchenQual + RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + 
               Condition2 + ExterQual + GarageArea + Functional + SaleCondition + 
               LotArea + YearBuilt + PoolQC + YearRemodAdd + Fireplaces + 
               TotalBsmtSF + BldgType + PoolArea + ScreenPorch + MSZoning + 
               BedroomAbvGr + LandSlope + LowQualFinSF + KitchenAbvGr + 
               LandContour + Condition1 + Exterior1st + MasVnrArea + Street + 
               GarageQual + GarageCond + LotConfig + TotRmsAbvGrd + WoodDeckSF + 
               GarageCars + HeatingQC + X1stFlrSF, data = Ames)

step(FullModel, direction="backward")
backward <- lm(formula = SalePrice ~ Neighborhood + GrLivArea + BsmtQual + 
                     KitchenQual + RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + 
                     Condition2 + ExterQual + GarageArea + Functional + SaleCondition + 
                     LotArea + YearBuilt + PoolQC + YearRemodAdd + Fireplaces + 
                     TotalBsmtSF + BldgType + PoolArea + ScreenPorch + MSZoning + 
                     BedroomAbvGr + LandSlope + LowQualFinSF + KitchenAbvGr + 
                     LandContour + Condition1 + Exterior1st + MasVnrArea + Street + 
                     GarageQual + GarageCond + LotConfig + TotRmsAbvGrd + WoodDeckSF + 
                     GarageCars + HeatingQC + X1stFlrSF, data = Ames)

# compBackward according to the step model is a slightly better fit, so I will test the RMSEs of
# the backward selection model

train_rmse_backward_selection = get_rmse(backward, train_data, "SalePrice")
test_rmse_backward_selection = get_rmse(backward, test_data, "SalePrice")
model_complexity_backward_selection = get_complexity(backward)

















