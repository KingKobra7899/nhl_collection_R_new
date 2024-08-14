game_id <- 2015020468
url <- glue::glue("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId={game_id}")
response <- getURL(url)
shift_data <- fromJSON(response)$data
shift_data$endTime <- sapply(shift_data$endTime, mmss_to_decimal) * 60
shift_data$startTime <- sapply(shift_data$startTime, mmss_to_decimal) * 60
pbp_data <- get_pbp_data(game_id)$data
pbp_data$eventOwnerTeamId <- get_team_name("NYR")
shift_data
