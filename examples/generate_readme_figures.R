library(targetree)

figure_dir <- file.path("examples", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

fit_and_save <- function(x, y, predictors, dataset, method, cut, lbd = NULL) {
  model <- CART$new(
    depth = 3,
    minimum_portion = 0.02,
    method = method,
    lbd = lbd,
    cut = cut,
    feature_name = predictors
  )
  model$fit(x, y)
  model$plot(
    title = sprintf("%s: %s", dataset, toupper(method)),
    split_rule_lines = 2,
    title_font_size = 18,
    save_path = file.path(
      figure_dir,
      sprintf("%s-%s.png", tolower(dataset), method)
    )
  )
  invisible(model)
}

data("diabetes", package = "targetree")
diabetes_predictors <- setdiff(names(diabetes), "Outcome")
diabetes_x <- diabetes[diabetes_predictors]
diabetes_y <- diabetes$Outcome

fit_and_save(
  diabetes_x, diabetes_y, diabetes_predictors,
  "Diabetes", "cart", 0.60
)
fit_and_save(
  diabetes_x, diabetes_y, diabetes_predictors,
  "Diabetes", "mdfs", 0.60
)
fit_and_save(
  diabetes_x, diabetes_y, diabetes_predictors,
  "Diabetes", "pfs", 0.60, 0.5
)

data("forestfires", package = "targetree")
forestfires_y <- as.integer(forestfires$area > 5)
forestfires_predictors <- c(
  "X", "Y", "FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain"
)
forestfires_x <- forestfires[forestfires_predictors]

fit_and_save(
  forestfires_x, forestfires_y, forestfires_predictors,
  "Forestfires", "cart", 1 / 3
)
fit_and_save(
  forestfires_x, forestfires_y, forestfires_predictors,
  "Forestfires", "mdfs", 1 / 3
)
fit_and_save(
  forestfires_x, forestfires_y, forestfires_predictors,
  "Forestfires", "pfs", 1 / 3, 0.5
)
