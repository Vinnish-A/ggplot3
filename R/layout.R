margin3d <- function(top = 8, right = 8, bottom = 8, left = 8, unit = "px") {
  values <- c(top = top, right = right, bottom = bottom, left = left)
  if (!is.numeric(values) || any(!is.finite(values)) || any(values < 0)) {
    stop("margin3d values must be finite non-negative numbers.", call. = FALSE)
  }
  list(top = unname(top), right = unname(right), bottom = unname(bottom), left = unname(left), unit = unit)
}

layout_3d <- function(width = NULL, height = NULL, units = "px",
                      plot_margin = margin3d(8, 8, 8, 8),
                      legend_position = NULL,
                      legend_spacing = 12,
                      scene_fit = "remaining") {
  if (!is.null(width) && (!is.numeric(width) || length(width) != 1L || !is.finite(width) || width <= 0)) {
    stop("width must be NULL or a positive numeric scalar.", call. = FALSE)
  }
  if (!is.null(height) && (!is.numeric(height) || length(height) != 1L || !is.finite(height) || height <= 0)) {
    stop("height must be NULL or a positive numeric scalar.", call. = FALSE)
  }
  if (!units %in% c("px", "in", "cm", "mm")) {
    stop("units must be one of px, in, cm, or mm.", call. = FALSE)
  }
  if (!is.null(legend_position)) {
    legend_position <- match.arg(legend_position, c("none", "left", "right", "top", "bottom", "inside"))
  }
  if (!is.numeric(legend_spacing) || length(legend_spacing) != 1L || !is.finite(legend_spacing) || legend_spacing < 0) {
    stop("legend_spacing must be a non-negative numeric scalar.", call. = FALSE)
  }
  out <- list(
    width = width,
    height = height,
    units = units,
    plotMargin = plot_margin,
    titleArea = list(enabled = TRUE),
    legendArea = list(position = legend_position, spacing = legend_spacing),
    sceneViewport = list(fit = scene_fit)
  )
  class(out) <- c("ggplot3scene_layout", "list")
  out
}

render_spec <- function(width = 6.72, height = 4.8, units = c("in", "cm", "mm", "px"),
                        dpi = 100, background = "white",
                        device_pixel_ratio = NULL,
                        export = FALSE) {
  units <- match.arg(units)
  if (!is.numeric(width) || length(width) != 1L || !is.finite(width) || width <= 0) {
    stop("width must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(height) || length(height) != 1L || !is.finite(height) || height <= 0) {
    stop("height must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(dpi) || length(dpi) != 1L || !is.finite(dpi) || dpi <= 0) {
    stop("dpi must be a positive numeric scalar.", call. = FALSE)
  }
  inches <- render_size_inches(width, height, units, dpi)
  css_width <- round(inches$width * 96)
  css_height <- round(inches$height * 96)
  pixel_width <- round(inches$width * dpi)
  pixel_height <- round(inches$height * dpi)
  dpr <- device_pixel_ratio %||% (dpi / 96)
  out <- list(
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    background = background,
    cssWidth = css_width,
    cssHeight = css_height,
    pixelWidth = pixel_width,
    pixelHeight = pixel_height,
    devicePixelRatio = dpr,
    export = isTRUE(export)
  )
  class(out) <- c("ggplot3scene_render_spec", "list")
  out
}

render_size_inches <- function(width, height, units, dpi = 96) {
  switch(
    units,
    "in" = list(width = width, height = height),
    "cm" = list(width = width / 2.54, height = height / 2.54),
    "mm" = list(width = width / 25.4, height = height / 25.4),
    "px" = list(width = width / dpi, height = height / dpi)
  )
}

labs3d <- function(title = NULL, subtitle = NULL, caption = NULL,
                   x = NULL, y = NULL, z = NULL, colour = NULL, color = NULL,
                   fill = NULL, size = NULL, alpha = NULL, ...) {
  dots <- list(...)
  if (length(dots) > 0L) {
    named <- names(dots)
    if (is.null(named) || any(!nzchar(named))) {
      stop("Additional labels must be named.", call. = FALSE)
    }
  }
  labels <- c(
    list(title = title, subtitle = subtitle, caption = caption, x = x, y = y, z = z,
         colour = colour %||% color, fill = fill, size = size, alpha = alpha),
    dots
  )
  labels <- labels[!vapply(labels, is.null, logical(1))]
  class(labels) <- c("ggplot3scene_labels", "list")
  labels
}

labs <- labs3d

plot_annotation3d <- function(title = NULL, subtitle = NULL, caption = NULL) {
  labs3d(title = title, subtitle = subtitle, caption = caption)
}

performance_policy3d <- function(max_json_points = 50000,
                                 max_vector_points = 5000,
                                 default_large_data = c("warn", "sample", "binary-buffer")) {
  default_large_data <- match.arg(default_large_data)
  if (!is.numeric(max_json_points) || length(max_json_points) != 1L || !is.finite(max_json_points) || max_json_points <= 0) {
    stop("max_json_points must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(max_vector_points) || length(max_vector_points) != 1L || !is.finite(max_vector_points) || max_vector_points <= 0) {
    stop("max_vector_points must be a positive numeric scalar.", call. = FALSE)
  }
  out <- list(
    maxJsonPoints = as.integer(max_json_points),
    maxVectorPoints = as.integer(max_vector_points),
    defaultLargeData = default_large_data
  )
  class(out) <- c("ggplot3scene_performance_policy", "list")
  out
}
