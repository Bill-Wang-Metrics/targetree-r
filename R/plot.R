.tree_layout <- function(tree) {
  state <- new.env(parent = emptyenv())
  state$column <- 0
  state$max_depth <- 0
  state$nodes <- list()

  visit <- function(node, depth, id) {
    state$max_depth <- max(state$max_depth, depth)
    if (.is_leaf(node)) {
      state$column <- state$column + 1
      x <- state$column
    } else {
      left_x <- visit(node$left, depth + 1, paste0(id, "L"))
      right_x <- visit(node$right, depth + 1, paste0(id, "R"))
      x <- (left_x + right_x) / 2
    }
    state$nodes[[id]] <- list(id = id, node = node, x = x, y = -depth)
    x
  }

  visit(tree, 0, "root")
  list(nodes = state$nodes, leaves = state$column, depth = state$max_depth)
}

#' Plot a fitted targetree tree
#'
#' Draws internal split nodes, terminal-node probabilities and sample sizes,
#' and a legend indicating whether each leaf exceeds the selected threshold.
#'
#' @param tree A tree stored in a fitted `CART` object's `$tree` field.
#' @param feature_name Optional feature names.
#' @param cut Classification threshold.
#' @param figsize Optional figure size in inches, `c(width, height)`.
#' @param title Optional figure title.
#' @param save_path Optional `.png`, `.pdf`, or `.svg` path. When omitted, the
#'   current graphics device is used.
#' @param ... Additional arguments passed to the graphics device when saving.
#'
#' @return The layout data, invisibly.
#' @export
plot_cart_tree <- function(tree, feature_name = NULL, cut = 0.5,
                           figsize = NULL, title = NULL, save_path = NULL, ...) {
  if (is.null(tree)) stop("`tree` cannot be NULL.", call. = FALSE)
  layout <- .tree_layout(tree)
  if (is.null(figsize)) {
    figsize <- c(max(layout$leaves * 1.5 + 1.8, 7),
                 max((layout$depth + 1) * 1.8 + 0.7, 3.5))
  }
  if (length(figsize) != 2L || any(!is.finite(figsize)) || any(figsize <= 0)) {
    stop("`figsize` must contain a positive width and height.", call. = FALSE)
  }

  opened_device <- FALSE
  if (!is.null(save_path)) {
    extension <- tolower(tools::file_ext(save_path))
    if (extension == "png") {
      grDevices::png(save_path, width = figsize[[1]], height = figsize[[2]],
                     units = "in", res = 150, ...)
    } else if (extension == "pdf") {
      grDevices::pdf(save_path, width = figsize[[1]], height = figsize[[2]], ...)
    } else if (extension == "svg") {
      grDevices::svg(save_path, width = figsize[[1]], height = figsize[[2]], ...)
    } else {
      stop("`save_path` must end in .png, .pdf, or .svg.", call. = FALSE)
    }
    opened_device <- TRUE
  }
  if (opened_device) on.exit(grDevices::dev.off(), add = TRUE)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(0.5, 0.5, if (is.null(title)) 0.5 else 2.3, 0.5), xpd = NA)
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(0.25, layout$leaves + 0.75),
    ylim = c(-layout$depth - 0.8, 0.8),
    asp = NA
  )
  if (!is.null(title)) graphics::title(main = title, cex.main = 1.05)

  node_width <- min(0.82, 0.72 * max(1, layout$leaves / 4))
  node_height <- 0.46
  blue <- "#5B9BD5"
  white <- "#FFFFFF"

  for (entry in layout$nodes) {
    node <- entry$node
    if (.is_leaf(node)) next
    left <- layout$nodes[[paste0(entry$id, "L")]]
    right <- layout$nodes[[paste0(entry$id, "R")]]
    graphics::segments(entry$x, entry$y - node_height / 2,
                       left$x, left$y + node_height / 2,
                       col = "#777777", lwd = 1)
    graphics::segments(entry$x, entry$y - node_height / 2,
                       right$x, right$y + node_height / 2,
                       col = "#777777", lwd = 1)
  }

  for (entry in layout$nodes) {
    node <- entry$node
    if (.is_leaf(node)) {
      positive <- node$mean > cut
      graphics::rect(entry$x - node_width / 2, entry$y - node_height / 2,
                     entry$x + node_width / 2, entry$y + node_height / 2,
                     col = if (positive) blue else white, border = "#444444",
                     lwd = 1.2)
      graphics::text(entry$x, entry$y + 0.07,
                     sprintf("P(Y=1|X) = %.4f", node$mean),
                     cex = 0.67, font = 2, col = if (positive) "white" else "#111111")
      graphics::text(entry$x, entry$y - 0.09,
                     sprintf("samples = %d", node$n), cex = 0.67,
                     col = if (positive) "white" else "#111111")
    } else {
      graphics::rect(entry$x - node_width / 2, entry$y - node_height / 2,
                     entry$x + node_width / 2, entry$y + node_height / 2,
                     col = "#E6E6E6", border = "#999999")
      name <- if (is.null(feature_name)) {
        sprintf("X%d", node$feature)
      } else {
        feature_name[[node$feature]]
      }
      label <- if (isTRUE(node$categorical)) {
        sprintf("%s in {%s}", name,
                paste(sort(as.character(node$threshold)), collapse = ", "))
      } else {
        sprintf("%s <= %.4f", name, node$threshold)
      }
      graphics::text(entry$x, entry$y, label, cex = 0.67)
    }
  }

  graphics::legend(
    "topright",
    legend = c(sprintf("P(Y=1|X) > %g (positive)", cut),
               sprintf("P(Y=1|X) <= %g (negative)", cut)),
    fill = c(blue, white), border = "#444444", cex = 0.7, bg = "white"
  )
  invisible(layout)
}

