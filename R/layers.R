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

projection_face3d <- function(faces = c("xy_min", "xz_min", "yz_max"),
                              alpha = 0.45,
                              size_scale = 0.8,
                              offset = 0.0001,
                              visible = TRUE) {
  allowed <- c("xy_min", "xy_max", "xz_min", "xz_max", "yz_min", "yz_max")
  if (!is.character(faces) || length(faces) == 0L || any(!faces %in% allowed)) {
    stop("faces must contain supported face ids: ", paste(allowed, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric scalar between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(size_scale) || length(size_scale) != 1L || !is.finite(size_scale) || size_scale <= 0) {
    stop("size_scale must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(offset) || length(offset) != 1L || !is.finite(offset)) {
    stop("offset must be a finite numeric scalar.", call. = FALSE)
  }
  out <- list(
    type = "source_point_faces",
    faces = unname(faces),
    alphaMultiplier = alpha,
    sizeMultiplier = size_scale,
    offset = offset,
    visible = isTRUE(visible)
  )
  class(out) <- c("ggplot3scene_projection_face", "list")
  out
}

geom_point3d <- function(mapping = NULL, data = NULL, size = 4, alpha = 1,
                         colour = NULL, name = "points", projection = NULL,
                         max_points = NULL,
                         sampling = c("none", "random", "stratified"),
                         rasterize = "auto", rasterize_threshold = 50000,
                         show_legend = TRUE,
                         ...) {
  size_explicit <- !missing(size)
  alpha_explicit <- !missing(alpha)
  colour_explicit <- !missing(colour) && !is.null(colour)
  sampling <- match.arg(sampling)
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_point3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  if (identical(projection, "faces")) {
    projection <- projection_face3d()
  }
  if (!is.null(projection) && !inherits(projection, "ggplot3scene_projection_face")) {
    stop("projection must be NULL, 'faces', or projection_face3d().", call. = FALSE)
  }
  mapping <- normalize_aes3_mapping(mapping)
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0) {
    stop("size must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric scalar between 0 and 1.", call. = FALSE)
  }
  if (!is.null(max_points) && (!is.numeric(max_points) || length(max_points) != 1L || !is.finite(max_points) || max_points <= 0)) {
    stop("max_points must be NULL or a positive numeric scalar.", call. = FALSE)
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
      colour_explicit = colour_explicit,
      projection = projection,
      max_points = if (is.null(max_points)) NULL else as.integer(max_points),
      sampling = sampling,
      rasterize = rasterize,
      rasterize_threshold = rasterize_threshold,
      show_legend = isTRUE(show_legend)
    )
  )
}

geom_path3d <- function(mapping = NULL, data = NULL, colour = "#374151",
                        alpha = 1, line_width = 1, name = "path", ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_path3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  mapping <- normalize_aes3_mapping(mapping)
  check_alpha_param(alpha)
  check_line_width_param(line_width)

  new_layer3d(
    type = "polyline3d",
    mapping = mapping,
    data = data,
    params = list(
      geom = "path",
      colour = colour,
      alpha = alpha,
      line_width = line_width,
      name = name
    )
  )
}

geom_segment3d <- function(mapping = NULL, data = NULL, colour = "#374151",
                           alpha = 1, line_width = 1, name = "segments", ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_segment3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  mapping <- normalize_aes3_mapping(mapping)
  check_alpha_param(alpha)
  check_line_width_param(line_width)

  new_layer3d(
    type = "polyline3d",
    mapping = mapping,
    data = data,
    params = list(
      geom = "segment",
      colour = colour,
      alpha = alpha,
      line_width = line_width,
      name = name
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
