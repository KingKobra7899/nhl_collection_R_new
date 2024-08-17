data("fenwick_data")
data$angle <- data$angle * (pi / 180)
data <- na.omit(data)

cos_interaction <- is_goal ~ xCoord + yCoord + (cos(angle) * distance) + is_rebound + shotType
train_control <- trainControl(
  verboseIter = TRUE,
  classProbs = T
)

tune_grid <- expand.grid(alpha = c(0, 1), 
                         lambda = 10^-5:0) 
data$is_goal <- as.factor(data$is_goal)
levels(data$is_goal)[levels(data$is_goal)=="1"] = "yes"
levels(data$is_goal)[levels(data$is_goal)=="0"] = "no"
set.seed(123)
split <- createDataPartition(data$is_goal, p = 0.8, list = F)
train <- data[split,]
test <- data[-split,]
cos_interaction <- is_goal ~ xCoord + yCoord + (cos(angle) * distance) + is_rebound + shotType
interaction_model_sin <- train(
  cos_interaction,
  data = train,
  method = "gbm",
  preProcess = c("scale", "center"),
  metric = "ROC",  # Use "ROC" for binary classification
  trControl = trainControl(
    method = "cv",       # Cross-validation
    number = 5,          # Number of folds
    classProbs = TRUE,   # Ensure probabilities are returned
    summaryFunction = twoClassSummary,
    verboseIter = T# For ROC metric
  )
)
assess_xg <- function(model, data){
  preds <- predict(model, data, type = "prob")$yes
  data$is_goal <- as.integer(data$shot_outcome=="goal")
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

preds <- predict(interaction_model_sin, data, type = "prob")
hist(preds$yes)
View(xg_model$results)
