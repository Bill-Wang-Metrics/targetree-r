# targetree for R

Native R implementation of classification and regression trees with **PFS**
(Penalized Final Split) and **MDFS** (Maximum Distance Final Split), designed
for threshold-focused binary classification.

This repository ports the functionality of the Python
[`targetree`](https://github.com/lhy-0594/targetree) package to R. It does not
require Python.

## Installation

```r
# install.packages("remotes")
remotes::install_github("Bill-Wang-Metrics/targetree-r")
```

## Quick start

```r
library(targetree)

set.seed(42)
X <- matrix(rnorm(1000), ncol = 2)
p <- plogis(rowSums(X))
y <- rbinom(nrow(X), 1, p)

model <- CART$new(
  depth = 4,
  minimum_portion = 0.05,
  method = "mdfs",
  cut = 0.30
)

model$fit(X, y)
predictions <- model$predict(X)
model$get_risk(X, y)
model$print_tree()
model$plot(title = "MDFS tree")
```

## Methods

| `method` | Default `lbd` | Description |
|---|---:|---|
| `"cart"` | 0 | Standard CART impurity splitting |
| `"pfs"` | 0 | Penalized Final Split with user-selected `lbd` |
| `"mdfs"` | 1 | Maximum Distance Final Split |

## Categorical predictors

Supply a data frame and identify categorical columns by name or by R's
one-based positions:

```r
dat <- data.frame(
  size = rnorm(300),
  region = sample(c("A", "B", "C"), 300, replace = TRUE)
)

model <- CART$new(
  depth = 3,
  minimum_portion = 0.05,
  method = "mdfs",
  categorical_features = "region"
)
model$fit(dat, rbinom(300, 1, 0.2))
```

## Probability-assisted fitting

```r
model$fit(X, y, prob = p)
```

When `prob` is provided, ordinary splits and terminal estimates use the
continuous probabilities while final PFS/MDFS split selection follows the
observed binary outcome, matching the Python reference implementation.

## Honest estimation

```r
model$fit(X[1:300, ], y[1:300])
model$honest_approach(X[301:500, ], y[301:500])
honest_predictions <- model$predict(X, honest = TRUE)
```

## Save a tree figure

```r
model$plot(save_path = "tree.png")
```

PNG, PDF, and SVG output are supported.

## Development

```r
R CMD build .
R CMD check targetree_0.1.0.tar.gz
```

