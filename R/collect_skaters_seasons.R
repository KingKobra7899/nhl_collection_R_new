seasons <- c("20082009","20092010", "20102011", "20112012", "20122013", 
           "20132014", "20142015", "20152016", "20162017", "20172018",
"20182019", "20192020", "20202021", "20212022", "20222023", "20232024")


#' @export
getSeasonData <- function(year, minPoints){
  url <- glue::glue("https://api.nhle.com/stats/rest/en/skater/summary?limit=-1&start=17&sort=points&cayenneExp=seasonId={year}")
  response <- RCurl::getURL(url)
  apiData <- jsonlite::fromJSON(response)
  data <- apiData$data

  data$efficiency <- data$pointsPerGame / (data$timeOnIcePerGame / 60) * 60
  data$positionCode <- as.factor(data$positionCode)

  data <- data[data$points > minPoints, ]
  return(data)
}




#' @export
generate_train_data <- function(minPoints, next_points){
  seasons_data <- list()
  for(season in seasons){
    seasons_data[[season]] = getSeasonData(season, minPoints)
    print(glue::glue("{season} data loaded"))
  }

  data <- data.frame()
  for(i in 1:15){
    current <- seasons_data[[seasons[i]]]
    if(next_points){
      
      nextData <- seasons_data[[seasons[i + 1]]]
      nextData <- data.frame(playerId = nextData$playerId, next_points = nextData$points)
      current <- merge(current, nextData, by = "playerId")
      #current <- merge(current, lastData, by = "playerId")
    }
    data <- rbind(data, current)
  }
  return(data)
}
#'@export
get_money_puck_skater <- function(year){
  require("dplyr")
  year_string <- as.character(year)
  url <- glue::glue("https://moneypuck.com/moneypuck/playerData/seasonSummary/{year_string}/regular/skaters.csv")
  data <- readr::read_csv(url)
  data <- data %>% group_by(playerId) %>% summarise(across(where(is.numeric), mean, na.rm = TRUE))
  return(data)
}
unscale_column <- function(col, mean_val, sd_val) {
  return(col * sd_val + mean_val)
}

#'@export
generate_projection_data <- function(minPoints){
  season <- "20232024"
  mp <- get_money_puck_skater(as.numeric(substr(season, 1, 4)))
  return(merge(getSeasonData(season, minPoints), mp, by='playerId'))
}
#' @export
mmss_to_decimal <- function(mmss) {
  parts <- try(strsplit(mmss, ":")[[1]], silent = TRUE)
  
  if (inherits(parts, "try-error") || length(parts) != 2) {
    return(NA)  # Handle errors or unexpected format
  }
  
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  
  if (is.na(minutes) || is.na(seconds)) {
    return(15)  # Handle non-numeric values
  } else {
    return(minutes + seconds / 60)
  }
}
#'@export
getPlayerStats <- function(playerId) {
   player_string <- as.character(playerId)
  url_string <- glue::glue("https://api-web.nhle.com/v1/player/{player_string}/landing")
  response <- RCurl::getURL(url_string)
  response <- gsub("\u001A","",response)
  data <- jsonlite::fromJSON(response) 
  avgtoi <- mmss_to_decimal(data$careerTotals$regularSeason$avgToi)
  pp60 <- (data$careerTotals$regularSeason$points / data$careerTotals$regularSeason$gamesPlayed) / avgtoi
  pp60 <- pp60 * 60
  birth_year <- as.numeric(substr(data$birthDate,1,4))
  return(data.frame(playerId = playerId, points_per_60 = pp60, weight = data$weightInKilograms, birth_year = birth_year))
}

#'@export
getAllPlayerStats <- function(playerIds){
  i <- 1
  players <- data.frame()
  for(playerId in playerIds){
    print(round(i / length(playerIds)*100, 2))
    player <- getPlayerStats(playerId)
    players <- rbind(players, player)
    i <- i + 1
  }
  return(players)
}

#'@export
produce_data <- function(min_gp, features){
  
  
  proj <- generate_projection_data(min_gp)
  train <- generate_train_data(min_gp, T)
  list <- list()
  

  playerIds <- unique(proj$playerId)
  playerIds <- c(playerIds, unique(train$playerId))
  playerIds <- unique(playerIds)
  player_data <- getAllPlayerStats(playerIds)

  proj <- merge(proj, player_data, by = "playerId")
  train <- merge(train, player_data, by = "playerId")
  nextd <- train$next_points
  
  
  list[["proj_name"]] <- proj$skaterFullName
  list[["train_name"]] <- train$skaterFullName
  train$next_points <- nextd
  train$age <- as.numeric(substr(train$seasonId, 1, 4)) - train$birth_year
  proj$age <- as.numeric(substr(proj$seasonId, 1, 4)) - proj$birth_year
  list[["train"]] <- train
  list[["projection"]] <- proj
  return(list)
}

#' @export 
getClusters <- function(data, num_clusters) {
  stats <- data %>% summarise(across(everything(), list(mean = mean, sd = sd)))
  means <- stats %>% select(ends_with("_mean")) %>% rename_with(~ gsub("_mean", "", .)) %>% unlist()
  sds <- stats %>% select(ends_with("_sd")) %>% rename_with(~ gsub("_sd", "", .)) %>% unlist()
  
  data <- data.frame(scale(data))
  clusters <- kmeans(data, num_clusters)
  
  # Generate the cluster plot
  plot <- ggplot2::autoplot(clusters, data, label = TRUE, shape = FALSE, loadings = TRUE, loadings.color = "black", loadings.label = TRUE)
  
  # Calculate the mean and standard deviation for each column
  
  
  # Separate means and standard deviations into different lists
  
  # Get the cluster centers and unscale them
  centers <- data.frame(clusters$centers)
  
  
  
  # Apply the unscale function to all columns in centers
  transformed_centers <- centers %>% mutate(across(everything(), ~ unscale_column(.x, means[cur_column()], sds[cur_column()])))
  
  # Create the result list
  result_list <- list(
    plot = plot,
    centers = transformed_centers,
    labels = clusters$cluster
  )
  
  return(result_list)
}

#'@export 
prep_data_for_keras <- function(df, target){
  features <- as.matrix(df %>% select(-all_of(target)))
  labels <- df[[target]]
  
  data <- list();
  data[["x"]] <- features
  data[["y"]] <- labels
  return(data)
}
#' @export 
clean_pbp <- function(pbp_data, columns){
    shooter <- coalesce(pbp_data$shootingPlayerId, pbp_data$scoringPlayerId)
    pbp_data$shooter <- as.factor(shooter)
    pbp_data$shootingPlayerId <- NULL
    pbp_data$scoringPlayerId <- NULL
    pbp_data <- na.omit(pbp_data)
    pbp_data_clean <- prep_data(pbp_data, columns, 1)
    shotType <- pbp_data_clean$shotType
    pbp_data_clean$shotType <- NULL
    is_goal <- pbp_data_clean$is_goal
    is_rebound <- pbp_data_clean$is_rebound
    pbp_data_clean <- data.frame(scale(pbp_data_clean))
    pbp_data_clean$is_rebound <- is_rebound
    pbp_data_clean$shotType <- shotType
    pbp_data_clean$is_goal <- is_goal
    
    list <- list()
    list[["data"]] <- pbp_data_clean
    list[["info"]] <- pbp_data
   
    return(list)
}

#'@export 
normalize_vector <- function(vec) {
  min_val <- min(vec)
  max_val <- max(vec)
  # Avoid division by zero if max_val equals min_val
  if (max_val == min_val) {
    return(rep(0, length(vec)))
  }
  vector <- (vec - min_val) / (max_val - min_val)
  return(vector)
}