data <- getSeasonData("20232024", 0)

forwards <- data$positionCode != "D"
defenseman <- data$positionCode == "D"

defenseman_names <- data.frame(name = data$skaterFullName[defenseman], team = data$teamAbbrevs[defenseman])
forwards_names <- data.frame(name = data$skaterFullName[forwards], team = data$teamAbbrevs[forwards])
features <- c("goals","efficiency","plusMinus", "assists","penaltyMinutes")

trimmed_data <- prep_data(data, features, 1)
defenseman_data <- trimmed_data[defenseman,]
forward_data <- trimmed_data[forwards,]

defense <- getClusters(defenseman_data, 5)
defense$plot
