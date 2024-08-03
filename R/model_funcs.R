#' @export
testError <- function(model, testData, target) {
  # Make predictions using the model
  prediction <- predict(model, testData)
  
  # Calculate error (residuals)
  error <- testData[[target]] - prediction
  
  # Calculate squared error
  error_squared <- error^2
  
  # Calculate mean squared error (MSE)
  mse <- mean(error_squared)
  
  # Calculate root mean squared error (RMSE)
  rmse <- sqrt(mse)
  
  # Calculate mean absolute error (MAE)
  mae <- mean(abs(error))
  
  # Calculate total sum of squares (TSS)
  tss <- sum((testData[[target]] - mean(testData[[target]]))^2)
  
  # Calculate residual sum of squares (RSS)
  rss <- sum(error_squared)
  
  # Calculate R-squared (coefficient of determination)
  R2 <- 1 - (rss / tss)
  
  # Return a dataframe with metrics
  return(data.frame(MSE = mse, RMSE = rmse, MAE = mae, R2 = R2))
}

#' @export
create_formula <- function(df, output_var) {
  numeric_columns <- sapply(df, is.numeric)
  numeric_columns <- setdiff(names(df)[numeric_columns], output_var)
  formula <- stats::as.formula(paste(output_var, "~", paste(numeric_columns, collapse = " + ")))
  return(formula)
}

#' @export
scaleandcenter <- function(data, target){
numeric_cols <- sapply(data, is.numeric)

# Separate numeric and non-numeric columns
data_numeric <- data[, numeric_cols]
data_numeric_scaled <- as.data.frame(scale(data_numeric))

data_numeric_scaled$target <- data[[target]]
return(data_numeric_scaled)
}

#' @export
prep_data <- function(data, features, target){
  data <- subset(data, select = features)
  return(data)
}
dist <- function(x1, y1, x2, y2){
  dx = x2 - x1
  dy = y2 - y1
  return(sqrt((dx^2) + (dy^2)))
}
radians_to_degrees <- function(radians) {
  degrees <- radians * (180 / pi)
  return(degrees)
}
get_shot_angle <- function(pbp_data){
  pbp_data <- pbp_data |> 
    dplyr::mutate(
      angle = radians_to_degrees(atan(yCoord / (xCoord - 89)))
    )
  return(pbp_data)
}

flip_sign <- function(x) {
  ifelse(x < 0, -x, x)
}

#' @export 
get_pbp_data <- function(game_id) {
  url <- glue::glue("https://api-web.nhle.com/v1/gamecenter/{game_id}/play-by-play")
  
  # Fetch and parse the data
  response <- RCurl::getURL(url)
  pbp_data <- tryCatch({
    jsonlite::fromJSON(response)
  }, error = function(e) {
    warning(paste("Failed to parse JSON for game_id", game_id, ": ", e$message))
    return(NULL)
  })
  
  if (is.null(pbp_data)) {
    return(NULL)
  }
  columns <- c("xCoord", "yCoord", "shotType", "eventOwnerTeamId")
  home = pbp_data$homeTeam$id
  away <- pbp_data$awayTeam$id
  shots_goals <- c("goal", "shot-on-goal", "missed-shot")
  plays <- pbp_data$plays
  plays_cleaned <- plays %>%
    dplyr::filter(typeDescKey %in% shots_goals)
  
  
  plays_cleaned <- plays_cleaned[as.numeric(plays_cleaned$situationCode) == 1551, ]
  play_details <- plays_cleaned$details
  shooter <- dplyr::coalesce(play_details$scoringPlayerId, play_details$shootingPlayerId)
  is_goal <- as.numeric(!is.na(play_details$scoringPlayerId))
  play_data <- prep_data(play_details, columns, 1)
  play_data$is_goal <- is_goal
  play_data$shooter <- shooter
  play_data$angle <- radians_to_degrees(get_shot_angle(play_data))
  play_data$xCoord <- flip_sign(play_data$xCoord)
  play_data$distance_from_net<- mapply(dist, play_data$xCoord, play_data$yCoord, MoreArgs = list(x2 = 89, y2 = 0))
  list <- list()
  list[["data"]] <- play_data
  list[["homeId"]] <- home
  list[["awayId"]] <- away
  return(list)
}

#' @export 
get_player_name <- function(player_id){
  url <- glue::glue("https://api-web.nhle.com/v1/player/{player_id}/landing")
  response <- RCurl::getURL(url)
  data <- jsonlite::fromJSON(response)

  first_name <- data$firstName$default
  last_name <- data$lastName$default
  name <- c(first_name, last_name)
  name <- paste(name, collapse = " ")
  return(name)
}