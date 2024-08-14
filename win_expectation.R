data("fitted_xG_model")
readRDS("fitted_xG_model.rds")

game <- get_game_data(2023020009, fitted_glm)

sum(game$xG[game$eventOwnerTeamId==23])^2 / (sum(game$xG[game$eventOwnerTeamId==22])^2 + sum(game$xG[game$eventOwnerTeamId==23])^2)
