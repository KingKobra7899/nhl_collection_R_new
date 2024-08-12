library(nhlCollection)
library(ModelMetrics)
library(caret)
data <- getURL("https://api.nhle.com/stats/rest/en/game")
json <- fromJSON(data)$data

json <- json[json$season > 200920,]
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

assess_xG_model <- function(model_name, actual, predicted){
  auc <- auc(actual, predicted)
  loss <- logLoss(actual, predicted)
  return(data.frame(name = model_name, loss = loss, auc = auc))
}

data <- na.omit(data)
data$is_rebound <- as.integer(as.factor(data$is_rebound))
data$is_goal <- as.factor(data$is_goal)
data$is_goal <- as.integer(data$shot_outcome == "goal")

split <- createDataPartition(data$is_goal, p = 0.8, list = F)
train_data <- data[split, ]
test_data <- data[-split, ]
formula <- is_goal ~ xCoord + yCoord + angle + distance + is_rebound + shotType
fitControl = trainControl(method = "repeatedcv", number = 5, repeats = 1, verboseIter = T)
fitted_glm <- train(formula, data = train_data, method = "glm",
                    preProcess = c('center', 'scale'),
                    trControl = fitControl,
                    family = "binomial",
                    metric = 'Rsquared')
summary(predict(fitted_glm, test_data))


auc(test_data$is_goal, predict(fitted_glm, test_data))
logLoss(test_data$is_goal, predict(fitted_glm, test_data))

player <- nhlCollection::get_player_summary(fitted_glm, 20232024, 8481554)
game <- get_pbp_data(2023021159)$data
game[game$shooter==8476459,]

player <- nhlCollection::get_player_summary(fitted_glm, 20232024, 8482109)
ggplot(data = player, aes(x = game_num)) +  geom_line(aes(y = cumsum(G) / seq_along(game_num), se = T),color = "red")+  geom_line(aes(y = cumsum(xG) / seq_along(game_num), se = T),color = "blue")
sum(player$G) / max(player$game_num)
get_player_name(8476459)
ggplot(test_data, aes(y = predict(fitted_glm, test_data))) + geom_point(aes(x = abs(angle)))+ geom_smooth(aes(x = abs(angle)))
  