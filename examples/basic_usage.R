library(targetree)

set.seed(42)
n <- 500
X <- matrix(rnorm(n * 2), ncol = 2)
p <- plogis(rowSums(X))
y <- rbinom(n, 1, p)

cart <- targetree(depth = 4, minimum_portion = 0.05, method = "cart")
cart$fit(X, y)

mdfs <- targetree(
  depth = 4,
  minimum_portion = 0.05,
  method = "mdfs",
  cut = 0.3
)
mdfs$fit(X, y)

print(cart$get_risk(X, y))
print(mdfs$get_risk(X, y))

mdfs$plot(title = "MDFS tree", save_path = "mdfs_tree.png")
