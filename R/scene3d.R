ggplot3 <- function(data = NULL, mapping = aes3()) {
  if (!is.null(mapping) && !inherits(mapping, "ggplot3scene_aes")) {
    stop("mapping must be created with aes3().", call. = FALSE)
  }
  plot <- list(
    data = data,
    mapping = mapping,
    layers = list(),
    coord = coord_3d(),
    theme = theme_3d_scientific()
  )
  class(plot) <- c("ggplot3scene_plot", "list")
  plot
}

`+.ggplot3scene_plot` <- function(e1, e2) {
  if (inherits(e2, "ggplot3scene_layer")) {
    e1$layers[[length(e1$layers) + 1L]] <- e2
    return(e1)
  }
  if (inherits(e2, "ggplot3scene_coord")) {
    e1$coord <- e2
    return(e1)
  }
  if (inherits(e2, "ggplot3scene_theme")) {
    e1$theme <- merge_theme3d(e1$theme, e2)
    return(e1)
  }
  stop("Cannot add object of class ", paste(class(e2), collapse = "/"), " to a ggplot3scene plot.", call. = FALSE)
}

coord_3d <- function(projection = c("orthographic", "perspective"),
                     position = c(1.8, -2.4, 1.6),
                     target = c(0, 0, 0),
                     up = c(0, 0, 1),
                     zoom = 1,
                     aspect = c(1, 1, 1),
                     origin = c(0, 0, 0),
                     origin_mode = c("fixed", "data_min", "data_center"),
                     axis_limits = list(x = NULL, y = NULL, z = NULL),
                     grid = grid_3d(),
                     clip = c("none", "grid", "axes", "data", "all"),
                     expand = 0) {
  projection <- match.arg(projection)
  origin_mode <- match.arg(origin_mode)
  clip <- match.arg(clip)
  check_vec3 <- function(x, name) {
    if (!is.numeric(x) || length(x) != 3L || any(!is.finite(x))) {
      stop(name, " must be a finite numeric vector of length 3.", call. = FALSE)
    }
    x
  }
  if (!is.numeric(zoom) || length(zoom) != 1L || !is.finite(zoom) || zoom <= 0) {
    stop("zoom must be a positive numeric scalar.", call. = FALSE)
  }
  if (!inherits(grid, "ggplot3scene_grid3d")) {
    stop("grid must be created with grid_3d().", call. = FALSE)
  }
  axis_limits <- validate_axis_limits(axis_limits)
  if (!is.numeric(expand) || length(expand) != 1L || !is.finite(expand) || expand < 0) {
    stop("expand must be a non-negative numeric scalar.", call. = FALSE)
  }

  coord <- list(
    projection = projection,
    position = check_vec3(position, "position"),
    target = check_vec3(target, "target"),
    up = check_vec3(up, "up"),
    zoom = zoom,
    aspect = check_vec3(aspect, "aspect"),
    origin = check_vec3(origin, "origin"),
    origin_mode = origin_mode,
    axis_limits = axis_limits,
    grid = grid,
    clip = clip,
    expand = expand
  )
  class(coord) <- c("ggplot3scene_coord", "list")
  coord
}

coord_umap3d <- function(origin_mode = "data_min", positive_grid = TRUE,
                         grid_planes = "xy", z_mode = c("zero", "provided"),
                         expand = 0.05,
                         projection = "orthographic", ...) {
  z_mode <- match.arg(z_mode)
  domain <- if (isTRUE(positive_grid)) "positive" else "full"
  coord_3d(
    projection = projection,
    origin_mode = origin_mode,
    grid = grid_3d(visible = TRUE, planes = grid_planes, domain = domain),
    expand = expand,
    ...
  )
}

validate_axis_limits <- function(axis_limits) {
  if (!is.list(axis_limits)) {
    stop("axis_limits must be a list with x, y, and z entries.", call. = FALSE)
  }
  out <- list(x = axis_limits$x, y = axis_limits$y, z = axis_limits$z)
  for (axis in names(out)) {
    value <- out[[axis]]
    if (is.null(value)) {
      next
    }
    if (!is.numeric(value) || length(value) != 2L || any(!is.finite(value)) || value[[1]] > value[[2]]) {
      stop("axis_limits$", axis, " must be NULL or a finite increasing numeric vector of length 2.", call. = FALSE)
    }
  }
  out
}
