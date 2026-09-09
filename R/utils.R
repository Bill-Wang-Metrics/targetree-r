#' Find boundary positions in a sorted vector
#'
#' Finds the largest position whose value is less than or equal to `a` and
#' the smallest position whose value is greater than or equal to `b`.
#' Positions follow R's one-based indexing convention.
#'
#' @param v A nondecreasing numeric vector.
#' @param a Lower boundary.
#' @param b Upper boundary.
#'
#' @return A named integer vector. A missing upper boundary is returned as
#'   `NA_integer_`.
#' @export
find_elements <- function(v, a, b) {
  if (!length(v)) {
    return(c(largest_le_a = 1L, smallest_ge_b = NA_integer_))
  }

  largest <- 1L
  smallest <- NA_integer_
  for (i in seq_along(v)) {
    if (v[[i]] <= a) {
      largest <- i
    } else if (v[[i]] >= b && is.na(smallest)) {
      smallest <- i
    }
  }
  c(largest_le_a = largest, smallest_ge_b = smallest)
}

.is_leaf <- function(node) {
  is.list(node) && identical(node$type, "leaf")
}

.make_leaf <- function(values) {
  # Use the same sum-over-count calculation as the Python reference. R's
  # two-pass mean() can differ by one floating-point unit at an exact policy
  # threshold (for example, 4 / 12 versus 1 / 3), changing the target label.
  list(type = "leaf", mean = sum(values) / length(values), n = length(values))
}

.validate_vector <- function(x, n, name, probability = FALSE) {
  if (!is.atomic(x) || is.matrix(x) || length(x) != n) {
    stop(sprintf("`%s` must be a vector of length %d.", name, n), call. = FALSE)
  }
  x <- as.numeric(x)
  if (anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("`%s` must contain only finite, non-missing values.", name),
         call. = FALSE)
  }
  if (probability && any(x < 0 | x > 1)) {
    stop(sprintf("`%s` must contain values in [0, 1].", name), call. = FALSE)
  }
  x
}
