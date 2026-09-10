#' Create a targetree classification model
#'
#' This is the recommended public constructor for CART, PFS, and MDFS trees.
#' It returns a model with `$fit()`, `$predict()`, `$get_risk()`,
#' `$print_tree()`, and `$plot()` methods.
#'
#' @param depth Maximum tree depth as a non-negative integer.
#' @param minimum_portion Minimum terminal-node size as a proportion of the
#'   estimation sample.
#' @param lbd Final-split weight. The defaults are `0` for CART and PFS and `1`
#'   for MDFS. Supply a value between 0 and 1 when using PFS.
#' @param cut Probability threshold used to classify terminal nodes for
#'   targeting.
#' @param method Tree method: `"cart"`, `"mdfs"`, or `"pfs"`.
#' @param feature_name Optional vector of predictor names.
#' @param calibrated Whether supplied probabilities are calibrated.
#' @param categorical_features Optional categorical predictor names or R's
#'   one-based column positions.
#'
#' @return A targetree model. Call `$fit()` before prediction or plotting.
#' @export
#'
#' @examples
#' set.seed(42)
#' x <- matrix(rnorm(400), ncol = 2)
#' y <- rbinom(nrow(x), 1, plogis(rowSums(x)))
#' model <- targetree(
#'   depth = 3,
#'   minimum_portion = 0.05,
#'   method = "mdfs",
#'   cut = 0.3
#' )
#' model$fit(x, y)
#' head(model$predict(x))
targetree <- function(depth, minimum_portion, lbd = NULL, cut = 0.5,
                      method = "pfs", feature_name = NULL,
                      calibrated = FALSE, categorical_features = NULL) {
  CART$new(
    depth = depth,
    minimum_portion = minimum_portion,
    lbd = lbd,
    cut = cut,
    method = method,
    feature_name = feature_name,
    calibrated = calibrated,
    categorical_features = categorical_features
  )
}
