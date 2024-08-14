library(nhlCollection)
library(ModelMetrics)
library(caret)
response <- getURL("https://api.nhle.com/stats/rest/en/game")
json <- fromJSON(response)$data

library(RCurl)
library(jsonlite)
json <- json[json$season > 20072008,]
json <- json[json$season < 20242025,]

get_pbp_data(2023020018)

ids <- json$id
data <- data.frame()

for(i in 1:length(ids)){
  current <- tryCatch({
    get_pbp_data(ids[i])$data
  }, error = function(e) {
    NULL
  })
  data <- rbind(data, current)
  print(i / length(ids))
}
save(data = data, file = "data/fenwick_data.rda")

data <- na.omit(data)
data$is_rebound <- data$is_rebound - 1
data$is_goal <- as.factor(data$is_goal)
levels(data$is_goal) <- list("1" = "1", "No"="0")
data$is_goal <- as.integer(data$shot_outcome == "goal")
summary(data$is_goal)
formula <- is_goal ~ xCoord + yCoord + angle + distance + is_rebound + shotType
fitControl = trainControl(method = "repeatedcv", repeats = 5, number = 5, verboseIter = T)
fitted_glm <- train(formula, data = data, method = "glm",
                    preProcess = c('center', 'scale'),
                    family = "binomial",
                    trControl = fitControl,
                    metric = 'mae')
summary(predict(fitted_glm, data))


auc(data$is_goal, predict(fitted_glm, data))
logLoss(data$is_goal, predict(fitted_glm, data))

player <- nhlCollection::get_player_summary(fitted_glm, 20232024, 8481554)
game <- get_pbp_data(2023021159)$data
game[game$shooter==8476459,]

player <- nhlCollection::get_player_summary(fitted_glm, 20232024, 8482109)
ggplot(data = player, aes(x = game_num)) +  geom_line(aes(y = cumsum(G) / seq_along(game_num), se = T),color = "red")+  geom_line(aes(y = cumsum(xG) / seq_along(game_num), se = T),color = "blue")
sum(player$G) / max(player$game_num)
get_player_name(8476459)
ggplot(test_data, aes(y = predict(fitted_glm, test_data))) + geom_point(aes(x = abs(angle)))+ geom_smooth(aes(x = abs(angle)))
  