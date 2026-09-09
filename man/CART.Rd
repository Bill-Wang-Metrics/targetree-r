\name{CART}
\alias{CART}
\title{Threshold-Focused Classification and Regression Tree}
\description{
An R6 implementation of CART with Penalized Final Split (PFS) and Maximum
Distance Final Split (MDFS) criteria. The implementation follows the Python
\code{targetree} reference algorithm.
}
\section{Construction}{
\preformatted{
model <- CART$new(
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

\code{categorical_features} accepts column names or R's one-based column
positions.
}
\section{Methods}{
\describe{
  \item{\code{fit(features, target, prob = NULL)}}{
    Fit a tree to a matrix or data frame. When supplied, \code{prob} contains
    continuous probability estimates used to grow the tree.
  }
  \item{\code{predict(features, honest = FALSE, probs = NULL)}}{
    Return the fitted probability for each observation. \code{probs} is
    retained for compatibility with the Python API.
  }
  \item{\code{get_risk(features, target, honest = FALSE, probs = NULL)}}{
    Return named integer counts \code{TP}, \code{FN}, \code{FP}, and \code{TN}.
  }
  \item{\code{honest_approach(features, target)}}{
    Attach held-out outcome means to the fitted leaves.
  }
  \item{\code{print_tree()}}{Print the fitted tree.}
  \item{\code{plot(figsize = NULL, title = NULL, save_path = NULL,
  font_size = NULL, split_rule_lines = 1, ...)}}{
    Display or save the fitted tree. When \code{font_size = NULL}, the largest
    uniform font that fits every node is selected automatically.
  }
}
}
\examples{
set.seed(42)
x <- matrix(rnorm(400), ncol = 2)
p <- plogis(rowSums(x))
y <- rbinom(nrow(x), 1, p)
model <- CART$new(depth = 3, minimum_portion = 0.05,
                  method = "mdfs", cut = 0.3)
model$fit(x, y)
head(model$predict(x))
}
