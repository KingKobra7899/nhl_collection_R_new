data <- produce_data(10, 1)

features <- c("next_points", "assists", "points", "shootingPct", "efficiency", "points_per_60", "weight", "age", "pointsPerGame")
proj_features <- c("assists", "points", "shootingPct", "efficiency", "points_per_60", "weight", "age", "pointsPerGame")
formula <- as.formula("next_points ~ points + shootingPct + dif + weight + age + assists + pointsPerGame + points_per_60")

data$train <- na.omit(data$train)
clean_data <- prep_data(data$train, features, 1)
points <- clean_data$next_points
clean_data$next_points <- NULL
clean_data <- data.frame(scale(clean_data))
clean_data$next_points <- points
clean_data$dif <- clean_data$efficiency - clean_data$points_per_60

proj_data <- data.frame(scale(prep_data(data$projection, proj_features, 1)))
proj_data$dif <- proj_data$efficiency - proj_data$points_per_60

indices <- createDataPartition(clean_data$next_points, p = 0.75, list = F)
train_data <- clean_data[indices, ]
test_data <- clean_data[-indices, ]

train_x <- prep_data_for_keras(train_data, "next_points")$x
train_y <- prep_data_for_keras(train_data, "next_points")$y

test_x <- prep_data_for_keras(test_data, "next_points")$x
test_y <- prep_data_for_keras(test_data, "next_points")$y

proj_y <- as.matrix(proj_data)

keras.model <- keras_model_sequential() %>%
  layer_dense(units = 10, input_shape = ncol(train_x), activation = "elu") %>%
  layer_dense(units = 5, activation = "elu") %>%
  layer_dense(units = 1)

keras.model %>%
  compile(
    loss = "mean_absolute_error",
    optimizer = "adam"
  )

history <- keras.model %>%
  fit(
    train_x, train_y,
    epochs = 32,
    batch_size = 16,
    validation_split = 0.2
  )

predictions <- data.frame(name = data$proj_name, points = predict(keras.model, proj_y))
