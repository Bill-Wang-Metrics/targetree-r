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
  font_size = NULL,
  split_rule_lines = 1,
  title_font_size = NULL,
  ...
)
}
\arguments{
  \item{tree}{A tree stored in a fitted \code{targetree()} model's
  \code{$tree} field.}
  \item{feature_name}{Optional feature names.}
  \item{cut}{Classification threshold.}
  \item{figsize}{Optional figure size in inches as \code{c(width, height)}.}
  \item{title}{Optional figure title.}
  \item{save_path}{Optional \code{.png}, \code{.pdf}, or \code{.svg} path.}
  \item{font_size}{Optional positive font size in points for every node label
  and the legend. The default, \code{NULL}, selects the largest uniform size
  that fits every node box.}
  \item{split_rule_lines}{Use \code{1} to display each internal-node rule on
  one line or \code{2} to put the feature name and condition on separate
  lines.}
  \item{title_font_size}{Optional positive title font size in points. The
  default, \code{NULL}, makes the title slightly larger than the resolved node
  font.}
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
model <- targetree(2, 0.05, method = "cart")
model$fit(x, y)
plot_cart_tree(model$tree, model$feature_name, model$cut,
               split_rule_lines = 2)
}
