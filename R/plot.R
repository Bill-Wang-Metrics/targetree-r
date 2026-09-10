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
#' @param tree A tree stored in a fitted `targetree()` model's `$tree` field.
#' @param feature_name Optional feature names.
#' @param cut Classification threshold.
#' @param figsize Optional figure size in inches, `c(width, height)`.
#' @param title Optional figure title.
#' @param save_path Optional `.png`, `.pdf`, or `.svg` path. When omitted, the
#'   current graphics device is used.
#' @param font_size Optional positive font size in points for every node label
#'   and the legend. When `NULL`, the largest uniform size that fits every node
#'   box is selected automatically.
#' @param split_rule_lines Number of lines for internal-node rules. Use `1` for
#'   `X <= a` or `2` for the feature name and condition on separate lines.
#' @param title_font_size Optional positive title font size in points. When
#'   `NULL`, the title is made slightly larger than the resolved node font.
#' @param ... Additional arguments passed to the graphics device when saving.
#'
#' @return The layout data, invisibly.
#' @export
plot_cart_tree <- function(tree, feature_name = NULL, cut = 0.5,
                           figsize = NULL, title = NULL, save_path = NULL,
                           font_size = NULL, split_rule_lines = 1L,
                           title_font_size = NULL, ...) {
  if (is.null(tree)) stop("`tree` cannot be NULL.", call. = FALSE)
  if (!is.null(font_size) &&
      (length(font_size) != 1L || !is.numeric(font_size) ||
       !is.finite(font_size) || font_size <= 0)) {
    stop("`font_size` must be a positive number or NULL.", call. = FALSE)
  }
  if (length(split_rule_lines) != 1L ||
      !is.numeric(split_rule_lines) || !is.finite(split_rule_lines) ||
      !split_rule_lines %in% c(1, 2)) {
    stop("`split_rule_lines` must be either 1 or 2.", call. = FALSE)
  }
  if (!is.null(title_font_size) &&
      (length(title_font_size) != 1L || !is.numeric(title_font_size) ||
       !is.finite(title_font_size) || title_font_size <= 0)) {
    stop("`title_font_size` must be a positive number or NULL.", call. = FALSE)
  }
  split_rule_lines <- as.integer(split_rule_lines)

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
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    if (opened_device) grDevices::dev.off()
  }, add = TRUE)
  graphics::par(mar = c(0.5, 0.5, if (is.null(title)) 0.5 else 2.3, 0.5), xpd = NA)
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(0.25, layout$leaves + 0.75),
    ylim = c(-layout$depth - 0.8, 0.8),
    asp = NA
  )
  node_width <- min(0.82, 0.72 * max(1, layout$leaves / 4))
  node_height <- 0.55
  blue <- "#5B9BD5"
  white <- "#FFFFFF"

  mean_label <- function(value) {
    as.expression(bquote(hat(mu) == .(sprintf("%.4f", value))))
  }

  legend_labels <- as.expression(list(
    bquote(hat(mu) > .(format(cut, trim = TRUE)) ~ "(targeted)"),
    bquote(hat(mu) <= .(format(cut, trim = TRUE)) ~ "(not targeted)")
  ))

  split_label <- function(node) {
    name <- if (is.null(feature_name)) {
      sprintf("X%d", node$feature)
    } else {
      feature_name[[node$feature]]
    }
    if (isTRUE(node$categorical)) {
      values <- sprintf(
        "{%s}",
        paste(sort(as.character(node$threshold)), collapse = ", ")
      )
      if (split_rule_lines == 1L) {
        as.expression(bquote(.(name) %in% .(values)))
      } else {
        as.expression(bquote(atop(.(name), "" %in% .(values))))
      }
    } else {
      threshold <- sprintf("%.4f", node$threshold)
      if (split_rule_lines == 1L) {
        as.expression(bquote(.(name) <= .(threshold)))
      } else {
        as.expression(bquote(atop(.(name), "" <= .(threshold))))
      }
    }
  }

  split_labels <- list()
  leaf_mean_labels <- list()
  leaf_count_labels <- character()
  for (entry in layout$nodes) {
    if (.is_leaf(entry$node)) {
      leaf_mean_labels[[length(leaf_mean_labels) + 1L]] <-
        mean_label(entry$node$mean)
      leaf_count_labels <- c(
        leaf_count_labels,
        sprintf("N = %d", entry$node$n)
      )
    } else {
      split_labels[[length(split_labels) + 1L]] <- split_label(entry$node)
    }
  }

  text_dimensions <- function(label, cex, font = 1) {
    if (!is.character(label)) {
      return(c(
        width = max(graphics::strwidth(label, cex = cex, font = font)),
        height = max(graphics::strheight(label, cex = cex, font = font)) * 1.15
      ))
    }
    lines <- strsplit(label, "\n", fixed = TRUE)[[1L]]
    c(
      width = max(graphics::strwidth(lines, cex = cex, font = font)),
      height = sum(graphics::strheight(lines, cex = cex, font = font)) * 1.15
    )
  }

  font_fits <- function(points) {
    cex <- points / graphics::par("ps")
    split_ok <- all(vapply(split_labels, function(label) {
      dimensions <- text_dimensions(label, cex)
      dimensions[["width"]] <= node_width * 0.88 &&
        dimensions[["height"]] <= node_height * 0.80
    }, logical(1)))
    leaf_ok <- all(vapply(seq_along(leaf_mean_labels), function(i) {
      mean_dimensions <- text_dimensions(leaf_mean_labels[[i]], cex, font = 2)
      count_dimensions <- text_dimensions(leaf_count_labels[[i]], cex)
      max(mean_dimensions[["width"]], count_dimensions[["width"]]) <=
        node_width * 0.88 &&
        mean_dimensions[["height"]] + count_dimensions[["height"]] <=
        node_height * 0.72
    }, logical(1)))
    split_ok && leaf_ok
  }

  if (is.null(font_size)) {
    candidates <- seq(24, 4, by = -0.25)
    fitting_sizes <- candidates[vapply(candidates, font_fits, logical(1))]
    resolved_font_size <- if (length(fitting_sizes)) fitting_sizes[[1L]] else 4
  } else {
    resolved_font_size <- as.numeric(font_size)
  }
  node_cex <- resolved_font_size / graphics::par("ps")
  resolved_title_font_size <- if (is.null(title_font_size)) {
    max(14, resolved_font_size + 2)
  } else {
    as.numeric(title_font_size)
  }
  if (!is.null(title)) {
    graphics::title(
      main = title,
      cex.main = resolved_title_font_size / graphics::par("ps")
    )
  }

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
      graphics::text(entry$x, entry$y + node_height * 0.18,
                     mean_label(node$mean),
                     cex = node_cex, font = 2,
                     col = if (positive) "white" else "#111111")
      graphics::text(entry$x, entry$y - node_height * 0.18,
                     sprintf("N = %d", node$n), cex = node_cex,
                     col = if (positive) "white" else "#111111")
    } else {
      graphics::rect(entry$x - node_width / 2, entry$y - node_height / 2,
                     entry$x + node_width / 2, entry$y + node_height / 2,
                     col = "#E6E6E6", border = "#999999")
      graphics::text(entry$x, entry$y, split_label(node), cex = node_cex)
    }
  }

  graphics::legend(
    "topright",
    legend = legend_labels,
    fill = c(blue, white), border = "#444444", cex = node_cex, bg = "white"
  )
  layout$font_size <- resolved_font_size
  layout$split_rule_lines <- split_rule_lines
  layout$title_font_size <- resolved_title_font_size
  invisible(layout)
}
