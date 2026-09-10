\name{targetree}
\alias{targetree}
\title{Create a Targetree Classification Model}
\usage{
targetree(
  depth,
  minimum_portion,
  lbd = NULL,
  cut = 0.5,
  method = "pfs",
  feature_name = NULL,
  calibrated = FALSE,
  categorical_features = NULL
)
}
\arguments{
  \item{depth}{Maximum tree depth as a non-negative integer.}
  \item{minimum_portion}{Minimum terminal-node size as a proportion of the
  estimation sample.}
  \item{lbd}{Final-split weight. The defaults are 0 for CART and PFS and 1 for
  MDFS. Supply a value between 0 and 1 when using PFS.}
  \item{cut}{Probability threshold used to classify terminal nodes for
  targeting.}
  \item{method}{Tree method: \code{"cart"}, \code{"mdfs"}, or
  \code{"pfs"}.}
  \item{feature_name}{Optional vector of predictor names.}
  \item{calibrated}{Whether supplied probabilities are calibrated.}
  \item{categorical_features}{Optional categorical predictor names or R's
  one-based column positions.}
}
\value{A targetree model. Call \code{$fit()} before prediction or plotting.}
\description{
The recommended public constructor for CART, PFS, and MDFS trees. It returns a
model with \code{$fit()}, \code{$predict()}, \code{$get_risk()},
\code{$print_tree()}, and \code{$plot()} methods.
}
\examples{
set.seed(42)
x <- matrix(rnorm(400), ncol = 2)
y <- rbinom(nrow(x), 1, plogis(rowSums(x)))
model <- targetree(
  depth = 3,
  minimum_portion = 0.05,
  method = "mdfs",
  cut = 0.3
)
model$fit(x, y)
head(model$predict(x))
}
