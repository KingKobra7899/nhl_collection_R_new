data("fenwick_data")
data$angle <- data$angle * (pi / 180)

base_formula <- is_goal ~ xCoord + yCoord + distance + angle + is_rebound + shotType
cos_sin <- is_goal ~ xCoord + yCoord + distance + sin(angle) + cos(angle) + is_rebound + shotType
cos_sin_abs <- is_goal ~ xCoord + yCoord + distance + sin(abs(angle)) + cos(abs(angle)) + is_rebound + shotType
spline_model  <- is_goal ~ xCoord + yCoord + distance + ns(angle, df = 3) + is_rebound + shotType
spline_model_abs  <- is_goal ~ xCoord + yCoord + distance + ns(abs(angle), df = 3) + is_rebound + shotType
interaction <- is_goal ~ xCoord + yCoord + distance * angle + is_rebound + shotType
train_control <- trainControl(
  method = "cv",        # Cross-validation
  number = 20,
  verboseIter = TRUE # Number of folds
)
cos_sin.model <-train(cos_sin,
                  data = data,
                  method = "glm",
                  family = "binomial",
                  metric = "RMSE"
                  )
assess_xg <- function(model){
  preds <- predict(model, data)
  auc_score <- auc(data$is_goal, preds)
  brier_score<- brier(data$is_goal, preds)
  f1_score <- f1Score(data$is_goal, preds)
  log_loss <- logLoss(data$is_goal, preds)
}