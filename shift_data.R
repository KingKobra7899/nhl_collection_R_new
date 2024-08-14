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

game_id <- 2022021241
team_id = 3
model <- glm.model

get_rapm_scores <- function(game_id, team_id, model){
game <- get_game_data(game_id, model)
presence <- (data.frame((game$on_ice)))
game_data <- game$data
url <- glue::glue("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId={game_id}")
response <- getURL(url)
shift_data <- fromJSON(response)$data
teams <- unique(shift_data$teamId)

team_players <- unique(shift_data$playerId[shift_data$teamId == team_id])



diff = ifelse(game_data$eventOwnerTeamId == team_id, game_data$xG, game_data$xG * -1)


player_presence <- presence %>% select(any_of(glue("X{team_players}")))

x <- player_presence
y = diff

ridge_model <- glmnet(x, y, alpha = 1)
best_lambda <- min(ridge_model$lambda)
rapms <- coef(ridge_model, s = best_lambda)
rapms <- data.frame(t(as.matrix(rapms)))
rapms$X.Intercept. <-NULL
rapms <- t(rapms)
rapms <- data.frame(rapms)
rapms$player <- gsub("X", "", rownames(rapms))
rownames(rapms) <- NULL
return(rapms)
}