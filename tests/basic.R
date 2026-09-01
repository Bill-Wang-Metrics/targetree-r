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
            c(41L, 1L, 26L, 12L))
)

mdfs_parity <- CART$new(3, 0.09, method = "mdfs", cut = 0.35)
mdfs_parity$fit(X_parity, y_parity)
stopifnot(
  isTRUE(all.equal(mdfs_parity$tree$threshold, 0.40625)),
  isTRUE(all.equal(mdfs_parity$tree$right$left$threshold, 0.84375)),
  isTRUE(all.equal(mdfs_parity$tree$right$right$threshold, 0.46875)),
  identical(unname(mdfs_parity$get_risk(X_parity, y_parity)),
            c(41L, 1L, 26L, 12L))
)

prob_parity <- CART$new(3, 0.09, method = "pfs", lbd = 0.4, cut = 0.35)
prob_parity$fit(X_parity, y_parity, p_parity)
stopifnot(
  isTRUE(all.equal(prob_parity$tree$threshold, 0.46875)),
  isTRUE(all.equal(
    sort(unique(prob_parity$predict(X_parity))),
    c(0.11889853588978094, 0.26590779446957746, 0.2949354477132042,
      0.46002438178327915, 0.6763433101571257)
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
