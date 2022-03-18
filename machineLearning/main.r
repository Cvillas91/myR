# Machine Learning
# Algorithm:
# A set of guidelines that describe how to perform a task

# regression
# Create Training and Test data
library(ggplot2)
set.seed(1033)
tr <- sample(1:nrow(cars),0.8* nrow(cars))
train <- cars[tr,]
test <- cars[-tr,]

# Build model on training data
lmMod <- lm(dist ~ speed, data = train)

# Predict
distPred <- predict(lmMod,test)

# Results
summary(lmMod)
AIC(lmMod)
actual_pred <- data.frame(actuals = test$dist, predicted = distPred)
mape <- mean(abs(actual_pred$predicted - actual_pred$actuals)/actual_pred$actuals)

# Plot
ggplot (cars, aes(x= dist, y = speed)) + geom_point() + 
  stat_smooth(method = 'lm',col = 'red')
