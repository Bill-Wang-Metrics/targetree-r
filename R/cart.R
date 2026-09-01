#' Threshold-focused classification and regression tree
#'
#' An R6 implementation of CART with Penalized Final Split (PFS) and Maximum
#' Distance Final Split (MDFS) criteria. The implementation follows the Python
#' `targetree` reference algorithm.
#'
#' @section Construction:
#' `model <- CART$new(depth, minimum_portion, lbd = NULL, cut = 0.5,
#' method = "pfs", feature_name = NULL, calibrated = FALSE,
#' categorical_features = NULL)`
#'
#' `categorical_features` uses R's one-based column positions, or column names.
#'
#' @section Methods:
#' \describe{
#'   \item{`fit(features, target, prob = NULL)`}{Fit the tree and return the
#'   model invisibly. `features` may be a matrix or data frame.}
#'   \item{`predict(features, honest = FALSE, probs = NULL)`}{Return leaf
#'   probability estimates. `probs` is retained for API compatibility.}
#'   \item{`get_risk(features, target, honest = FALSE, probs = NULL)`}{Return
#'   named integer counts `TP`, `FN`, `FP`, and `TN`.}
#'   \item{`honest_approach(features, target)`}{Replace no splits, but attach
#'   held-out outcome means to all existing leaves.}
#'   \item{`print_tree()`}{Print a text representation of the fitted tree.}
#'   \item{`plot(...)`}{Plot the fitted tree using `plot_cart_tree()`.}
#' }
#'
#' @examples
#' set.seed(42)
#' x <- matrix(rnorm(400), ncol = 2)
#' p <- plogis(rowSums(x))
#' y <- rbinom(nrow(x), 1, p)
#' model <- CART$new(depth = 3, minimum_portion = 0.05,
#'                   method = "mdfs", cut = 0.3)
#' model$fit(x, y)
#' head(model$predict(x))
#'
#' @export
CART <- R6::R6Class(
  "CART",
  public = list(
    depth = NULL,
    minimum_portion = NULL,
    lbd = NULL,
    cut = NULL,
    tree = NULL,
    method = NULL,
    feature_name = NULL,
    calibrated = NULL,
    categorical_features = NULL,

    initialize = function(depth, minimum_portion, lbd = NULL, cut = 0.5,
                          method = "pfs", feature_name = NULL,
                          calibrated = FALSE, categorical_features = NULL) {
      if (length(depth) != 1L || !is.finite(depth) || depth < 0 || depth != floor(depth)) {
        stop("`depth` must be a non-negative integer.", call. = FALSE)
      }
      if (length(minimum_portion) != 1L || !is.finite(minimum_portion) ||
          minimum_portion < 0 || minimum_portion > 1) {
        stop("`minimum_portion` must be a number in [0, 1].", call. = FALSE)
      }
      if (length(cut) != 1L || !is.finite(cut) || cut < 0 || cut > 1) {
        stop("`cut` must be a number in [0, 1].", call. = FALSE)
      }
      if (!method %in% c("cart", "pfs", "mdfs")) {
        stop(sprintf("Invalid method '%s'. Choose from 'cart', 'pfs', or 'mdfs'.",
                     method), call. = FALSE)
      }

      defaults <- c(cart = 0, pfs = 0, mdfs = 1)
      if (is.null(lbd)) lbd <- unname(defaults[[method]])
      if (length(lbd) != 1L || !is.finite(lbd)) {
        stop("`lbd` must be a finite number.", call. = FALSE)
      }
      if (method == "cart" && lbd != 0) {
        stop("`lbd` must be 0 when method = 'cart'.", call. = FALSE)
      }
      if (method == "mdfs" && lbd != 1) {
        stop("`lbd` must be 1 when method = 'mdfs'.", call. = FALSE)
      }

      self$depth <- as.integer(depth)
      self$minimum_portion <- minimum_portion
      self$lbd <- lbd
      self$cut <- cut
      self$method <- method
      self$feature_name <- feature_name
      self$calibrated <- isTRUE(calibrated)
      self$categorical_features <- categorical_features
    },

    fit = function(features, target, prob = NULL) {
      x <- private$prepare_features(features, fitting = TRUE)
      n <- nrow(x)
      y <- .validate_vector(target, n, "target", probability = TRUE)
      p <- if (is.null(prob)) NULL else
        .validate_vector(prob, n, "prob", probability = TRUE)

      private$total <- n
      private$min_samples <- as.integer(self$minimum_portion * n)
      private$mmin_samples <- as.integer(self$minimum_portion * n / 3)

      self$tree <- if (is.null(p)) {
        private$grow(x, y, 0L)
      } else {
        private$grow_with_prob(x, y, p, 0L)
      }
      invisible(self)
    },

    predict = function(features, honest = FALSE, probs = NULL) {
      if (is.null(self$tree)) stop("Fit the model before predicting.", call. = FALSE)
      x <- private$prepare_features(features, fitting = FALSE)
      vapply(seq_len(nrow(x)), function(i) {
        private$predict_one(x[i, , drop = FALSE], self$tree, honest)
      }, numeric(1))
    },

    get_risk = function(features, target, honest = FALSE, probs = NULL) {
      estimate <- self$predict(features, honest = honest, probs = probs)
      y <- .validate_vector(target, length(estimate), "target", probability = TRUE)
      above <- y > self$cut
      predicted_above <- estimate > self$cut
      c(
        TP = as.integer(sum(predicted_above & above)),
        FN = as.integer(sum(!predicted_above & above)),
        FP = as.integer(sum(predicted_above & !above)),
        TN = as.integer(sum(!predicted_above & !above))
      )
    },

    honest_approach = function(features, target) {
      if (is.null(self$tree)) {
        stop("Fit the model before applying honest estimation.", call. = FALSE)
      }
      x <- private$prepare_features(features, fitting = FALSE)
      y <- .validate_vector(target, nrow(x), "target", probability = TRUE)
      self$tree <- private$attach_honest(self$tree, x, y)
      invisible(self)
    },

    print_tree = function(node = NULL, depth = 0L) {
      if (is.null(node)) node <- self$tree
      if (is.null(node)) stop("Fit the model before printing it.", call. = FALSE)
      indent <- paste(rep("  ", depth), collapse = "")
      if (.is_leaf(node)) {
        cat(sprintf("%sLeaf: mean=%.3f, n=%d\n", indent, node$mean, node$n))
        return(invisible(self))
      }

      name <- if (!is.null(self$feature_name)) {
        self$feature_name[[node$feature]]
      } else {
        sprintf("feature_%d", node$feature)
      }
      if (isTRUE(node$categorical)) {
        values <- paste(sort(as.character(node$threshold)), collapse = ", ")
        cat(sprintf("%s[%s in {%s}]\n", indent, name, values))
      } else {
        cat(sprintf("%s[%s <= %.4f]\n", indent, name, node$threshold))
      }
      self$print_tree(node$left, depth + 1L)
      self$print_tree(node$right, depth + 1L)
      invisible(self)
    },

    print = function(...) {
      cat(sprintf("<targetree CART: method=%s, depth=%d, cut=%g>\n",
                  self$method, self$depth, self$cut))
      if (!is.null(self$tree)) self$print_tree()
      invisible(self)
    },

    plot = function(figsize = NULL, title = NULL, save_path = NULL, ...) {
      if (is.null(self$tree)) stop("Fit the model before plotting it.", call. = FALSE)
      plot_cart_tree(
        self$tree,
        feature_name = self$feature_name,
        cut = self$cut,
        figsize = figsize,
        title = title,
        save_path = save_path,
        ...
      )
      invisible(self)
    }
  ),

  private = list(
    is_categorical = NULL,
    n_features = NULL,
    feature_columns = NULL,
    total = NULL,
    min_samples = NULL,
    mmin_samples = NULL,

    prepare_features = function(features, fitting) {
      if (is.null(dim(features)) || length(dim(features)) != 2L) {
        stop("`features` must be a two-dimensional matrix or data frame.",
             call. = FALSE)
      }
      x <- as.data.frame(features, stringsAsFactors = FALSE, check.names = FALSE)
      if (!nrow(x) || !ncol(x)) stop("`features` cannot be empty.", call. = FALSE)

      if (fitting) {
        private$n_features <- ncol(x)
        private$feature_columns <- names(x)
        cats <- self$categorical_features
        if (is.null(cats)) cats <- integer()
        if (is.character(cats)) {
          missing_names <- setdiff(cats, names(x))
          if (length(missing_names)) {
            stop(sprintf("Unknown categorical feature(s): %s.",
                         paste(missing_names, collapse = ", ")), call. = FALSE)
          }
          cats <- match(cats, names(x))
        }
        cats <- as.integer(cats)
        if (anyNA(cats) || any(cats < 1L | cats > ncol(x))) {
          stop("`categorical_features` must contain valid names or one-based positions.",
               call. = FALSE)
        }
        private$is_categorical <- seq_len(ncol(x)) %in% cats
        if (is.null(self$feature_name)) self$feature_name <- names(x)
        if (length(self$feature_name) != ncol(x)) {
          stop("`feature_name` must have one entry per feature.", call. = FALSE)
        }
      } else if (ncol(x) != private$n_features) {
        stop(sprintf("Expected %d feature columns but received %d.",
                     private$n_features, ncol(x)), call. = FALSE)
      }

      for (j in seq_len(ncol(x))) {
        if (private$is_categorical[[j]]) {
          if (anyNA(x[[j]])) stop("Categorical features cannot contain missing values.",
                                  call. = FALSE)
          if (is.factor(x[[j]])) x[[j]] <- as.character(x[[j]])
        } else {
          original_na <- is.na(x[[j]])
          converted <- suppressWarnings(as.numeric(x[[j]]))
          if (anyNA(converted) || any(original_na) || any(!is.finite(converted))) {
            stop(sprintf("Numeric feature %d contains nonnumeric or missing values.", j),
                 call. = FALSE)
          }
          x[[j]] <- converted
        }
      }
      x
    },

    split_mask = function(x, feature, threshold, categorical) {
      if (categorical) {
        left <- x[[feature]] %in% threshold
      } else {
        left <- x[[feature]] <= threshold
      }
      list(left = left, right = !left)
    },

    candidate_index = function(objective, left_count, n) {
      if (length(objective) <= 10L) return(which.min(objective))

      lower <- as.integer(0.1 * n)
      upper <- as.integer(0.9 * n)
      largest0 <- 0L
      smallest0 <- NA_integer_
      for (i0 in seq_along(left_count) - 1L) {
        value <- left_count[[i0 + 1L]]
        if (value <= lower) {
          largest0 <- i0
        } else if (value >= upper && is.na(smallest0)) {
          smallest0 <- i0
        }
      }

      start <- largest0 + 1L
      finish <- if (is.na(smallest0)) length(objective) else smallest0
      candidates <- if (start <= finish) seq.int(start, finish) else integer()
      if (!length(candidates)) return(which.min(objective))
      candidates[[which.min(objective[candidates])]]
    },

    numerical_candidates = function(x, y) {
      ord <- order(x)
      xs <- x[ord]
      ys <- y[ord]
      positions <- which(xs[-1L] != xs[-length(xs)])
      if (!length(positions)) return(NULL)
      cumulative_sum <- cumsum(ys)
      cumulative_sq <- cumsum(ys^2)
      list(
        xs = xs,
        left_count = positions,
        left_sum = cumulative_sum[positions],
        left_sq = cumulative_sq[positions]
      )
    },

    best_split_numerical = function(x, y, sum_y, sum_y2, n) {
      z <- private$numerical_candidates(x, y)
      if (is.null(z)) return(NULL)
      lc <- z$left_count
      rc <- n - lc
      left_var <- z$left_sq / lc - (z$left_sum / lc)^2
      right_var <- (sum_y2 - z$left_sq) / rc - ((sum_y - z$left_sum) / rc)^2
      objective <- (left_var * lc + right_var * rc) * 2
      index <- private$candidate_index(objective, lc, n)
      position <- lc[[index]]
      list(
        threshold = (z$xs[[position]] + z$xs[[position + 1L]]) / 2,
        impurity = objective[[index]],
        categorical = FALSE
      )
    },

    best_split_categorical = function(x, y, sum_y, sum_y2, n) {
      categories <- sort(unique(x))
      if (length(categories) == 1L) return(NULL)
      group <- match(x, categories)
      x_count <- tabulate(group, nbins = length(categories))
      y_count <- as.numeric(rowsum(y, group, reorder = TRUE))
      y2_count <- as.numeric(rowsum(y^2, group, reorder = TRUE))
      category_means <- y_count / x_count
      category_order <- order(category_means)

      x_count <- x_count[category_order]
      y_count <- y_count[category_order]
      y2_count <- y2_count[category_order]
      lc <- head(cumsum(x_count), -1L)
      ls <- head(cumsum(y_count), -1L)
      lsq <- head(cumsum(y2_count), -1L)
      rc <- n - lc
      left_var <- lsq / lc - (ls / lc)^2
      right_var <- (sum_y2 - lsq) / rc - ((sum_y - ls) / rc)^2
      objective <- (left_var * lc + right_var * rc) * 2
      index <- private$candidate_index(objective, lc, n)
      list(
        threshold = categories[category_order[seq_len(index)]],
        impurity = objective[[index]],
        categorical = TRUE
      )
    },

    best_split = function(x, y) {
      best <- NULL
      best_impurity <- Inf
      sum_y <- sum(y)
      sum_y2 <- sum(y^2)
      n <- length(y)

      for (feature in seq_len(ncol(x))) {
        if (length(unique(x[[feature]])) == 1L) next
        candidate <- if (private$is_categorical[[feature]]) {
          private$best_split_categorical(x[[feature]], y, sum_y, sum_y2, n)
        } else {
          private$best_split_numerical(x[[feature]], y, sum_y, sum_y2, n)
        }
        if (is.null(candidate)) next
        if (candidate$impurity < best_impurity) {
          best_impurity <- candidate$impurity
          best <- c(list(feature = feature), candidate)
        }
      }
      best
    },

    best_final_split = function(x, y, categorical) {
      if (categorical) {
        categories <- sort(unique(x))
        group <- match(x, categories)
        counts <- tabulate(group, nbins = length(categories))
        totals <- as.numeric(rowsum(y, group, reorder = TRUE))
        return(categories[(totals / counts) > self$cut])
      }

      z <- private$numerical_candidates(x, y)
      if (is.null(z)) return(NULL)
      lc <- z$left_count
      rc <- length(y) - lc
      left_probability <- z$left_sq / lc
      right_probability <- (sum(y^2) - z$left_sq) / rc
      objective <- (
        (left_probability - (z$left_sum / lc)^2) * lc +
          (right_probability - ((sum(y) - z$left_sum) / rc)^2) * rc
      ) * (1 - self$lbd) + (
        -abs(self$cut - left_probability) * lc -
          abs(self$cut - right_probability) * rc
      ) * self$lbd
      index <- private$candidate_index(objective, lc, length(y))
      position <- lc[[index]]
      (z$xs[[position]] + z$xs[[position + 1L]]) / 2
    },

    grow = function(x, y, depth) {
      if (depth == self$depth || length(unique(y)) == 1L ||
          length(y) < private$min_samples) {
        return(.make_leaf(y))
      }
      split <- private$best_split(x, y)
      if (is.null(split)) return(.make_leaf(y))
      mask <- private$split_mask(x, split$feature, split$threshold,
                                 split$categorical)
      if (min(sum(mask$left), sum(mask$right)) < private$mmin_samples) {
        return(.make_leaf(y))
      }

      at_leaf <- depth == self$depth - 1L ||
        min(sum(mask$left), sum(mask$right)) < private$min_samples
      if (at_leaf && self$method != "cart") {
        threshold <- private$best_final_split(
          x[[split$feature]], y, private$is_categorical[[split$feature]]
        )
        if (is.null(threshold)) return(.make_leaf(y))
        mask <- private$split_mask(
          x, split$feature, threshold, private$is_categorical[[split$feature]]
        )
        if (min(sum(mask$left), sum(mask$right)) < private$mmin_samples) {
          return(.make_leaf(y))
        }
        return(list(
          type = "node", feature = split$feature, threshold = threshold,
          categorical = private$is_categorical[[split$feature]],
          left = .make_leaf(y[mask$left]),
          right = .make_leaf(y[mask$right])
        ))
      }

      list(
        type = "node", feature = split$feature, threshold = split$threshold,
        categorical = split$categorical,
        left = private$grow(x[mask$left, , drop = FALSE], y[mask$left], depth + 1L),
        right = private$grow(x[mask$right, , drop = FALSE], y[mask$right], depth + 1L)
      )
    },

    grow_with_prob = function(x, y, p, depth) {
      if (depth == self$depth || min(p) > self$cut || max(p) < self$cut ||
          length(y) < private$min_samples) {
        return(.make_leaf(p))
      }
      split <- private$best_split(x, p)
      if (is.null(split)) return(.make_leaf(p))
      mask <- private$split_mask(x, split$feature, split$threshold,
                                 split$categorical)
      if (min(sum(mask$left), sum(mask$right)) < private$mmin_samples) {
        return(.make_leaf(p))
      }

      at_leaf <- depth == self$depth - 1L ||
        min(sum(mask$left), sum(mask$right)) < private$min_samples
      if (at_leaf && self$method != "cart") {
        threshold <- private$best_final_split(
          x[[split$feature]], y, private$is_categorical[[split$feature]]
        )
        if (is.null(threshold)) return(.make_leaf(p))
        mask <- private$split_mask(
          x, split$feature, threshold, private$is_categorical[[split$feature]]
        )
        if (min(sum(mask$left), sum(mask$right)) < private$mmin_samples) {
          return(.make_leaf(p))
        }
        return(list(
          type = "node", feature = split$feature, threshold = threshold,
          categorical = private$is_categorical[[split$feature]],
          left = .make_leaf(p[mask$left]),
          right = .make_leaf(p[mask$right])
        ))
      }

      list(
        type = "node", feature = split$feature, threshold = split$threshold,
        categorical = split$categorical,
        left = private$grow_with_prob(
          x[mask$left, , drop = FALSE], y[mask$left], p[mask$left], depth + 1L
        ),
        right = private$grow_with_prob(
          x[mask$right, , drop = FALSE], y[mask$right], p[mask$right], depth + 1L
        )
      )
    },

    predict_one = function(row, node, honest) {
      if (.is_leaf(node)) {
        if (honest) {
          if (is.null(node$honest)) {
            stop("Run `honest_approach()` before requesting honest predictions.",
                 call. = FALSE)
          }
          return(node$honest)
        }
        return(node$mean)
      }
      go_left <- if (isTRUE(node$categorical)) {
        row[[node$feature]] %in% node$threshold
      } else {
        row[[node$feature]] <= node$threshold
      }
      private$predict_one(row, if (go_left) node$left else node$right, honest)
    },

    attach_honest = function(node, x, y) {
      if (.is_leaf(node)) {
        node$honest <- if (length(y)) mean(y) else 0
        return(node)
      }
      mask <- private$split_mask(x, node$feature, node$threshold, node$categorical)
      node$left <- private$attach_honest(
        node$left, x[mask$left, , drop = FALSE], y[mask$left]
      )
      node$right <- private$attach_honest(
        node$right, x[mask$right, , drop = FALSE], y[mask$right]
      )
      node
    }
  )
)
