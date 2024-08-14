data("fenwick_data")
data <- data[data$shot_outcome!="missed-shot", ]
formula <- is_goal ~ xCoord + yCoord + distance + ns(angle, df = 3) + is_rebound + shotType
train_control <- trainControl(
  method = "cv",        # Cross-validation
  number = 20,
  verboseIter = TRUE # Number of folds
)
glm.model <-train(formula,
                  data = data,
                  method = "glm",
                  family = "binomial",
                  metric = "RMSE"
                  )
summary(predict(glm.model, data))
summary(glm.model$finalModel)

game_id <- 2023021168
team_id = 3
model <- glm.model

game <- get_rapm_scores(game_id, team_id, model)
