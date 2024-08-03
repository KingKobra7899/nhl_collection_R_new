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
  
  home <- as.numeric(pbp_data$homeTeam$id)
  away <- as.numeric(pbp_data$awayTeam$id)
  pbp <- pbp_data$plays
  
  # Filter and process play-by-play data
  pbp <- tryCatch({
    pbp[!is.na(pbp$details$shotType),]
  }, error = function(e) {
    warning(paste("Failed to filter play-by-play data for game_id", game_id, ": ", e$message))
    return(NULL)
  })
  
  if (is.null(pbp)) {
    return(NULL)
  }
  
  pbp_shots <- pbp$details
  pbp_shots$is_goal <- ifelse(is.na(pbp$details$scoringPlayerId), 0, 1)
  pbp_shots$shot_team <- ifelse(pbp$details$eventOwnerTeamId == home, "home", "away")
  pbp_shots$time <- pbp$timeInPeriod
  pbp_shots$time <- sapply(pbp_shots$time, mmss_to_decimal)
  
  columns <- c("eventOwnerTeamId", "xCoord", "yCoord", "shotType", 
               "shootingPlayerId", "goalieInNetId", 
               "scoringPlayerId", "is_goal", "shot_team", "time")
  
  pbp_shots <- pbp_shots[, columns, drop = FALSE]
  
  # Add period column
  pbp_shots$period <- pbp$periodDescriptor$number
  pbp_shots$eventOwnerTeamId <- NULL
  
  period <- pbp_shots$period - 1
  pbp_shots$time <- (pbp_shots$time + (period * 20)) * 60
  
  pbp_shots$xCoord <- pbp_shots$xCoord * ifelse(pbp_shots$xCoord < 0, -1, 1)
  pbp_shots$distance <- dist(pbp_shots$xCoord, pbp_shots$yCoord, 89, 0)
  pbp_shots <- get_shot_angle(pbp_shots)
  
  pbp_shots$shotType <- as.factor(pbp_shots$shotType)
  pbp_shots$goalieInNetId <- as.factor(pbp_shots$goalieInNetId)
  pbp_shots$shot_team <- as.factor(pbp_shots$shot_team)
  
  delta_t <- c(NA, diff(pbp_shots$time))
  delta_t[1] <- pbp_shots$time[1]
  is_rebound <- as.factor(ifelse(delta_t < 3, "yes", "no"))
  pbp_shots$is_rebound <- is_rebound

  return(pbp_shots)
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