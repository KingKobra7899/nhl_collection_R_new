data("fenwick_data")
data$angle <- data$angle * (pi / 180)
data <- na.omit(data)

cos_interaction <- is_goal ~ xCoord + yCoord + (cos(angle) * distance) + is_rebound + shotType
train_control <- trainControl(
  verboseIter = TRUE # Number of folds
)

tune_grid <- expand.grid(alpha = c(0, 1), 
                         lambda = 10^-5:0) 
set.seed(123)
split <- createDataPartition(data$is_goal, p = 0.8, list = F)
train <- data[split,]
test <- data[-split,]
interaction_model_sin  <-train(cos_interaction,
                           data = train,
                           method = "gbm",
                           preProcess = c("scale", "center"),
                           #family = "binomial",
                           metric = "RMSE",
                           verbose = T
)
assess_xg <- function(model, data){
  preds <- predict(model, data)
  print(summary(preds))
  auc_score <- auc(data$is_goal, preds)
  brier_score<- brier(data$is_goal, preds)
  f1_score <- f1Score(data$is_goal, preds)
  log_loss <- logLoss(data$is_goal, preds)
  error <- rmse(data$is_goal, preds)
  return(data.frame(auc = auc_score, brier = brier_score, f1 = f1_score, loss = log_loss, error = error))
}

assess_xg(interaction_model_sin, test)
xg_model <- interaction_model_sin

View(xg_model$results)
