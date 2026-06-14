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
  mapping <- normalize_aes3_mapping(mapping)
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

geom_surface_grid3d <- function(x, y, z, grid = NULL, fill = "#4477AA",
                                alpha = 0.65, name = "surface", ...) {
  fill_explicit <- !missing(fill)
  alpha_explicit <- !missing(alpha)
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_surface_grid3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }

  if (!is.null(grid)) {
    if (!missing(x) || !missing(y) || !missing(z)) {
      stop("Use either grid = grid2d(...) or x/y/z, not both.", call. = FALSE)
    }
    if (!inherits(grid, "ggplot3scene_grid2d")) {
      stop("grid must be created with grid2d().", call. = FALSE)
    }
  } else {
    if (missing(x) || missing(y) || missing(z)) {
      stop("geom_surface_grid3d() requires either grid or x, y, and z.", call. = FALSE)
    }
    grid <- grid2d(x = x, y = y, z = z, name = name)
  }

  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric scalar between 0 and 1.", call. = FALSE)
  }

  new_layer3d(
    type = "surface_grid",
    params = list(
      grid = grid,
      fill = fill,
      alpha = alpha,
      name = name,
      fill_explicit = fill_explicit,
      alpha_explicit = alpha_explicit
    )
  )
}
