library(targetree)

set.seed(42)
n <- 500L
X <- matrix(rnorm(n * 2L), ncol = 2L)
p <- plogis(rowSums(X))
y <- rbinom(n, 1, p)

for (method in c("cart", "pfs", "mdfs")) {
  lbd <- if (method == "pfs") 0.5 else NULL
  model <- CART$new(
    depth = 4,
    minimum_portion = 0.05,
    method = method,
    lbd = lbd,
    cut = 0.3
  )
  if (method == "pfs") model$fit(X, y, prob = p) else model$fit(X, y)
  predictions <- model$predict(X)
  stopifnot(
    length(predictions) == n,
    min(predictions) >= 0,
    max(predictions) <= 1,
    sum(model$get_risk(X, y)) == n
  )
}

honest <- CART$new(depth = 3, minimum_portion = 0.05, method = "cart")
honest$fit(X[1:300, ], y[1:300])
honest$honest_approach(X[301:500, ], y[301:500])
stopifnot(length(honest$predict(X, honest = TRUE)) == n)

set.seed(7)
mixed <- data.frame(
  x = rnorm(300),
  category = sample(c("A", "B", "C"), 300, replace = TRUE)
)
mixed_p <- plogis(mixed$x + ifelse(mixed$category == "A", 1, -0.5))
mixed_y <- rbinom(300, 1, mixed_p)
categorical <- CART$new(
  depth = 4,
  minimum_portion = 0.05,
  method = "mdfs",
  categorical_features = "category"
)
categorical$fit(mixed, mixed_y)
stopifnot(length(categorical$predict(mixed)) == 300L)

bad_method <- try(CART$new(3, 0.05, method = "bogus"), silent = TRUE)
bad_lambda <- try(CART$new(3, 0.05, method = "cart", lbd = 0.5), silent = TRUE)
stopifnot(inherits(bad_method, "try-error"), inherits(bad_lambda, "try-error"))

figure <- tempfile(fileext = ".png")
categorical$plot(save_path = figure)
stopifnot(file.exists(figure), file.info(figure)$size > 0)

auto_figure <- tempfile(fileext = ".png")
auto_layout <- plot_cart_tree(
  categorical$tree,
  feature_name = categorical$feature_name,
  cut = categorical$cut,
  title = "Automatic title",
  save_path = auto_figure,
  split_rule_lines = 2
)
manual_figure <- tempfile(fileext = ".png")
manual_layout <- plot_cart_tree(
  categorical$tree,
  feature_name = categorical$feature_name,
  cut = categorical$cut,
  save_path = manual_figure,
  font_size = 13,
  split_rule_lines = 1,
  title_font_size = 17
)
bad_font <- try(
  plot_cart_tree(categorical$tree, save_path = tempfile(fileext = ".png"),
                 font_size = 0),
  silent = TRUE
)
bad_lines <- try(
  plot_cart_tree(categorical$tree, save_path = tempfile(fileext = ".png"),
                 split_rule_lines = 3),
  silent = TRUE
)
bad_title_font <- try(
  plot_cart_tree(categorical$tree, save_path = tempfile(fileext = ".png"),
                 title_font_size = 0),
  silent = TRUE
)
pdf_figure <- tempfile(fileext = ".pdf")
plot_warnings <- character()
withCallingHandlers(
  plot_cart_tree(
    categorical$tree,
    feature_name = categorical$feature_name,
    cut = categorical$cut,
    save_path = pdf_figure,
    split_rule_lines = 2
  ),
  warning = function(warning) {
    plot_warnings <<- c(plot_warnings, conditionMessage(warning))
    invokeRestart("muffleWarning")
  }
)
stopifnot(
  file.exists(auto_figure), file.info(auto_figure)$size > 0,
  file.exists(manual_figure), file.info(manual_figure)$size > 0,
  file.exists(pdf_figure), file.info(pdf_figure)$size > 0,
  length(plot_warnings) == 0L,
  auto_layout$font_size >= 4, auto_layout$font_size <= 24,
  auto_layout$title_font_size > auto_layout$font_size,
  auto_layout$split_rule_lines == 2L,
  manual_layout$font_size == 13,
  manual_layout$title_font_size == 17,
  manual_layout$split_rule_lines == 1L,
  inherits(bad_font, "try-error"),
  inherits(bad_lines, "try-error"),
  inherits(bad_title_font, "try-error")
)

# Deterministic parity fixture generated from the Python reference package.
n_parity <- 80L
i <- 0:(n_parity - 1L)
X_parity <- cbind((i %% 17) / 16, ((i * 7) %% 23) / 22)
p_parity <- plogis(-2.1 + 3.0 * X_parity[, 1] + 1.4 * X_parity[, 2])
y_parity <- as.numeric((((i * 13) %% 101) / 100) < p_parity)

cart_parity <- CART$new(
  3, 0.09, method = "cart", cut = 0.35,
  feature_name = c("x1", "x2")
)
cart_parity$fit(X_parity, y_parity)
stopifnot(
  cart_parity$tree$feature == 1L,
  isTRUE(all.equal(cart_parity$tree$threshold, 0.40625)),
  identical(unname(cart_parity$get_risk(X_parity, y_parity)),
            c(41L, 1L, 30L, 8L))
)

mdfs_parity <- CART$new(3, 0.09, method = "mdfs", cut = 0.35)
mdfs_parity$fit(X_parity, y_parity)
stopifnot(
  isTRUE(all.equal(mdfs_parity$tree$threshold, 0.40625)),
  isTRUE(all.equal(mdfs_parity$tree$right$left$threshold, 0.65625)),
  isTRUE(all.equal(mdfs_parity$tree$right$right$threshold, 0.65625)),
  identical(unname(mdfs_parity$get_risk(X_parity, y_parity)),
            c(41L, 1L, 30L, 8L))
)

prob_parity <- CART$new(3, 0.09, method = "pfs", lbd = 0.4, cut = 0.35)
prob_parity$fit(X_parity, y_parity, p_parity)
stopifnot(
  isTRUE(all.equal(prob_parity$tree$threshold, 0.46875)),
  isTRUE(all.equal(
    sort(unique(prob_parity$predict(X_parity))),
    c(0.174033899718177, 0.302655514540545, 0.336355726578437,
      0.484639676546076, 0.676343310157126)
  ))
)

parity_categories <- c("A", "B", "C", "D")[(i %% 4) + 1L]
X_category_parity <- data.frame(
  x = X_parity[, 1],
  category = parity_categories
)
y_category_parity <- as.numeric(
  (X_parity[, 1] + as.numeric(parity_categories %in% c("A", "C")) * 0.45) > 0.72
)
category_parity <- CART$new(
  3, 0.09, method = "mdfs", cut = 0.35,
  categorical_features = "category"
)
category_parity$fit(X_category_parity, y_category_parity)
stopifnot(
  isTRUE(all.equal(category_parity$tree$threshold, 0.28125)),
  identical(sort(category_parity$tree$right$threshold), c("B", "D")),
  identical(category_parity$predict(X_category_parity), y_category_parity)
)

# Public example datasets and cross-language regression results.
data("diabetes", package = "targetree")
stopifnot(nrow(diabetes) == 768L, ncol(diabetes) == 9L)
diabetes_x <- diabetes[setdiff(names(diabetes), "Outcome")]
diabetes_y <- diabetes$Outcome

example_risk <- function(x, y, method, cut, lbd = NULL) {
  model <- CART$new(
    depth = 3, minimum_portion = 0.02,
    method = method, lbd = lbd, cut = cut
  )
  model$fit(x, y)
  unname(model$get_risk(x, y))
}

stopifnot(
  identical(example_risk(diabetes_x, diabetes_y, "cart", 0.60),
            c(150L, 118L, 57L, 443L)),
  identical(example_risk(diabetes_x, diabetes_y, "mdfs", 0.60),
            c(160L, 108L, 63L, 437L)),
  identical(example_risk(diabetes_x, diabetes_y, "pfs", 0.60, 0.5),
            c(160L, 108L, 63L, 437L))
)

data("forestfires", package = "targetree")
forestfires_y <- as.integer(forestfires$area > 5)
forestfires_x <- forestfires[c(
  "X", "Y", "FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain"
)]
stopifnot(
  identical(example_risk(forestfires_x, forestfires_y, "cart", 1 / 3),
            c(28L, 123L, 14L, 352L)),
  identical(example_risk(forestfires_x, forestfires_y, "mdfs", 1 / 3),
            c(53L, 98L, 55L, 311L)),
  identical(example_risk(forestfires_x, forestfires_y, "pfs", 1 / 3, 0.5),
            c(49L, 102L, 47L, 319L))
)
