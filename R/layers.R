new_layer3d <- function(type, mapping = NULL, data = NULL, params = list()) {
  layer <- list(
    type = type,
    mapping = mapping,
    data = data,
    params = params
  )
  class(layer) <- c("ggplot3scene_layer", "list")
  layer
}

geom_point3d <- function(mapping = NULL, data = NULL, size = 4, alpha = 1,
                         colour = NULL, name = "points", ...) {
  size_explicit <- !missing(size)
  alpha_explicit <- !missing(alpha)
  colour_explicit <- !missing(colour) && !is.null(colour)
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_point3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  if (!is.null(mapping) && !inherits(mapping, "ggplot3scene_aes")) {
    stop("mapping must be created with aes3().", call. = FALSE)
  }
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0) {
    stop("size must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric scalar between 0 and 1.", call. = FALSE)
  }

  new_layer3d(
    type = "point_cloud",
    mapping = mapping,
    data = data,
    params = list(
      size = size,
      alpha = alpha,
      colour = colour,
      name = name,
      size_explicit = size_explicit,
      alpha_explicit = alpha_explicit,
      colour_explicit = colour_explicit
    )
  )
}

geom_surface_grid3d <- function(x, y, z, fill = "#4477AA", alpha = 0.65,
                                name = "surface", ...) {
  fill_explicit <- !missing(fill)
  alpha_explicit <- !missing(alpha)
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_surface_grid3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("x and y must be numeric vectors.", call. = FALSE)
  }
  if (!is.matrix(z) || !is.numeric(z)) {
    stop("z must be a numeric matrix.", call. = FALSE)
  }
  expected <- c(length(x), length(y))
  if (!identical(dim(z), expected)) {
    stop(
      "dim(z) must equal c(length(x), length(y)); expected ",
      paste(expected, collapse = " x "),
      " but got ",
      paste(dim(z), collapse = " x "),
      ".",
      call. = FALSE
    )
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric scalar between 0 and 1.", call. = FALSE)
  }

  new_layer3d(
    type = "surface_grid",
    params = list(
      x = x,
      y = y,
      z = z,
      fill = fill,
      alpha = alpha,
      name = name,
      fill_explicit = fill_explicit,
      alpha_explicit = alpha_explicit
    )
  )
}
