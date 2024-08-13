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
get_shot_angle <- function(xCoord, yCoord){
  
    
      angle = radians_to_degrees(atan(yCoord / (xCoord - 89)))
    
  return(angle)
}

flip_sign <- function(x) {
  ifelse(x < 0, -x, x)
}

#' @export 
get_pbp_data <- function(game_id) {
  # Efficient URL construction
  url <- paste0("https://api-web.nhle.com/v1/gamecenter/", game_id, "/play-by-play")
  
  # Use httr for faster HTTP requests
  response <- httr::GET(url)
  
  # Parse JSON response
  pbp_data <- tryCatch({
    httr::content(response, "parsed", simplifyVector = TRUE)
  }, error = function(e) {
    warning(paste("Failed to parse JSON for game_id", game_id, ": ", e$message))
    return(NULL)
  })
  
  # Return early if there's no data
  if (is.null(pbp_data) || !("plays" %in% names(pbp_data))) {
    return(NULL)
  }
  
  # Filter shots and goals in a single step
  shots_goals <- c("goal", "shot-on-goal", "missed-shot")
  plays_cleaned <- dplyr::filter(pbp_data$plays, typeDescKey %in% shots_goals)
   plays_cleaned <- dplyr::filter(plays_cleaned, details$zoneCode %in% "O" )
  # Return early if no relevant plays
  if (nrow(plays_cleaned) == 0) {
    return(NULL)
  }
  
  # Efficient data preparation
  play_details <- plays_cleaned$details
  play_details$shot_outcome <- plays_cleaned$typeDescKey
  times <- sapply(plays_cleaned$timeInPeriod, mmss_to_decimal)
  times <- times + (20 * (plays_cleaned$periodDescriptor$number - 1))
  times <- times * 60
  threshold <- 2

# Calculate the difference between consecutive elements
  time_diff <- diff(times)

# Determine if each difference is within the threshold
  is_rebound <- ifelse(time_diff <= threshold, "Yes", "No")
  #print(str(play_details))
  play_data <- dplyr::select(play_details, xCoord, yCoord, shotType, eventOwnerTeamId) %>%
    dplyr::mutate(
      is_goal = as.numeric(!is.na(play_details$scoringPlayerId)),
      goalie = play_details$goalieInNetId,
      time = times,
      is_rebound = c("No",is_rebound),
      shot_outcome = plays_cleaned$typeDescKey,
      shooter = dplyr::coalesce(play_details$scoringPlayerId, play_details$shootingPlayerId),
      xCoord = abs(as.numeric(xCoord)),
      yCoord = as.numeric(yCoord),
      distance = sqrt((xCoord - 89)^2 + yCoord^2),  # Inline distance calculation
      angle = radians_to_degrees(atan(yCoord / (xCoord - 89)))
    )
  play_data$is_rebound <- as.integer(as.factor(play_data$is_rebound))
  return(list(
    data = play_data,
    homeId = pbp_data$homeTeam$id,
    awayId = pbp_data$awayTeam$id
  ))
}

#' @export 
get_game_data <- function(game_id, xG_model){
game_data <- get_pbp_data(game_id)$data

game_data$xG <- predict(xG_model, game_data)

return(game_data)
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

#'@export 
get_player_summary <- function(model, season, id) {
  player_log <- jsonlite::fromJSON(RCurl::getURL(glue::glue("https://api-web.nhle.com/v1/player/{id}/game-log/{season}/2")))
  gameIds <- player_log$gameLog$gameId

  all_season <- data.frame()
  i <- 1
  
  for (gameid in gameIds) {
    start_time <- Sys.time()
    
    tryCatch({
      game <- get_game_data(gameid, model)
      
      if (id %in% game$shooter) {
        game <- game[game$shooter == id, ]
        
        if (nrow(game) > 0) {
          xG <- data.frame(x = game$xCoord, y = game$yCoord, G = game$is_goal, xG = game$xG, game_num = i)
          all_season <- rbind(all_season, xG)
        } else {
          warning(glue::glue("No valid data for player {id} in game {gameid}"))
        }
      }
      
      i <- i + 1
    }, error = function(e) {
      message(glue::glue("Error processing game {gameid}: {e$message}"))
      # Optionally, print the full error message:
      # print(e)
    })
    
    end_time <- Sys.time()
    iteration_time <- end_time - start_time
    print(iteration_time)
  }
  
  return(all_season)
}


