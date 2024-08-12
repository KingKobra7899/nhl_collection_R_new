get_team_schedule <- function(team, season){
  url <- glue::glue("https://api-web.nhle.com/v1/club-schedule-season/{team}/{season}")
  data <- jsonlite::fromJSON(RCurl::getURL(url))$games
  return(data$id)
}

schedule <- get_team_schedule("NYR", 20232024)
