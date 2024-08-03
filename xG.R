library(nhlCollection)
library(dplyr)
library(nnet)
library(ModelMetrics)
library(caret)
library(sportyR)
library(neuralnet)
data("nhl_shots_2009_2024")


columns = c("is_goal", "xCoord", "yCoord", "distance", "angle", "shotType")

clean_pbp <- function(pbp_data){
    shooter <- coalesce(pbp_data$shootingPlayerId, pbp_data$scoringPlayerId)
    pbp_data$shooter <- as.factor(shooter)
    pbp_data$shootingPlayerId <- NULL
    pbp_data$scoringPlayerId <- NULL
    pbp_data <- na.omit(pbp_data)
    pbp_data_clean <- prep_data(pbp_data, columns, 1)
    shotType <- pbp_data_clean$shotType
    pbp_data_clean$shotType <- NULL
    is_goal <- pbp_data_clean$is_goal
    pbp_data_clean <- data.frame(scale(pbp_data_clean))
    pbp_data_clean$shotType <- shotType
    pbp_data_clean$is_goal <- as.factor(is_goal)
    
    list <- list()
    list[["data"]] <- pbp_data_clean
    list[["info"]] <- pbp_data
   
    return(list)
}
data <- clean_pbp(pbp_data)$data

indices <- createDataPartition(data$is_goal, p = 0.8, list = F)
train_data <- data[indices, ]
test_data <- data[-indices, ]

nnet.model <- nnet(is_goal ~ ., train_data, size = 4)
auc(test_data$is_goal, predict(nnet.model, test_data))

game6 <- get_pbp_data(2023030221)

game6$shot_team <- case_match(game6$shot_team, "away" ~ "CAR", "home" ~ "NYR")
game6$opponent_team <- ifelse(game6$shot_team == "CAR", "NYR", "CAR")
game6 <- clean_pbp(game6)


columns <- c("xCoord", "yCoord", "shot_team", "shooter", "time", "expected_goals", "is_goal", "opponent_team")
data <- game6$data
game6_xGoals <- predict(nnet.model, data)                                                                                                                                                                                                          
game6 <- game6$info
game6$expected_goals <- game6_xGoals
game6 <- prep_data(game6, columns, 1)
game6 <- game6[order(game6$time),]



game6 <- game6 %>%
    arrange(shot_team, time) %>%              # Ensure data is ordered by shot_team and time
    group_by(shot_team) %>%                   # Group by shot team identifier
    mutate(cumulative_xG = cumsum(expected_goals))  # Calculate cumulative xG for each shot team




ggplot(game6, aes(x = time)) + geom_line(aes(y = cumulative_xG, color = shot_team)) + scale_color_manual(values = c("NYR"="blue", "CAR" = "red"))
nhl + geom_point(data = game6, aes(x = xCoord, y = yCoord, size = expected_goals, color = shot_team, alpha = is_goal)) + scale_color_manual(values = c("NYR"="blue", "CAR" = "red"))
get_player_name(8470613)
                 