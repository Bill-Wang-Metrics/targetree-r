\name{plot_cart_tree}
\alias{plot_cart_tree}
\title{Plot a Fitted Targetree Tree}
\usage{
plot_cart_tree(
  tree,
  feature_name = NULL,
  cut = 0.5,
  figsize = NULL,
  title = NULL,
  save_path = NULL,
  ...
)
}
\arguments{
  \item{tree}{A tree stored in a fitted \code{CART} object's \code{$tree} field.}
  \item{feature_name}{Optional feature names.}
  \item{cut}{Classification threshold.}
  \item{figsize}{Optional figure size in inches as \code{c(width, height)}.}
  \item{title}{Optional figure title.}
  \item{save_path}{Optional \code{.png}, \code{.pdf}, or \code{.svg} path.}
  \item{...}{Additional arguments passed to the graphics device when saving.}
}
\value{The computed tree layout, invisibly.}
\description{
Draws internal split nodes, terminal-node probabilities and sample sizes, and
a legend indicating whether each leaf exceeds the selected threshold.
}
\examples{
set.seed(1)
x <- matrix(rnorm(200), ncol = 2)
y <- rbinom(100, 1, plogis(rowSums(x)))
model <- CART$new(2, 0.05, method = "cart")
model$fit(x, y)
plot_cart_tree(model$tree, model$feature_name, model$cut)
}

