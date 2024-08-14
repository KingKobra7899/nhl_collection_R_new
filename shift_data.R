game_id <- 2015020468
is_on_ice <- function(shot_time, shift_start, shift_end) {
  return(shot_time >= shift_start && shot_time <= shift_end)
}
url <- glue::glue("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId={game_id}")
response <- getURL(url)
shift_data <- fromJSON(response)$data
shift_data$endTime <- (sapply(shift_data$endTime, mmss_to_decimal) * 60) + ((shift_data$period - 1) * 60)
shift_data$startTime <- sapply(shift_data$startTime, mmss_to_decimal) * 60 + ((shift_data$period - 1) * 60)
play_data <- get_pbp_data(game_id)$data
players <- unique(shift_data$playerId)

presence <- play_data

# Loop over each player and create columns indicating their presence on ice
for (player_id in players) {
  presence <- presence %>%
    rowwise() %>%
    mutate(!!paste0("player_", player_id) := {
      shifts <- shift_data %>% filter(playerId == !!player_id)
      any(sapply(1:nrow(shifts), function(i) {
        is_on_ice(time, shifts$startTime[i], shifts$endTime[i])
      }))
    }) %>%
    ungroup()
}
presence <- presence[,13:50]
