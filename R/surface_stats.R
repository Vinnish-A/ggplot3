stat_surface3d <- function(surface, fill = "#4477AA", alpha = 0.65,
                           name = NULL, ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in stat_surface3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  if (!inherits(surface, "ggplot3scene_grid2d")) {
    stop("stat_surface3d() currently requires a grid2d surface object.", call. = FALSE)
  }
  new_surface_stat_layer(
    stat = "identity_surface",
    params = list(surface = surface, fill = fill, alpha = alpha, name = name %||% surface$name %||% "surface stat")
  )
}

stat_function_surface3d <- function(fun, xlim, ylim, grid_size = 64,
                                    fill = "#4477AA", alpha = 0.65,
                                    surface_alpha = c("none", "edge_fade", "density_fade", "combined_fade"),
                                    tessellation = c("rect", "right1", "right2", "equilateral"),
                                    name = "function surface", ...) {
  surface_alpha <- match.arg(surface_alpha)
  tessellation <- match.arg(tessellation)
  if (!is.function(fun)) {
    stop("fun must be a function accepting x and y.", call. = FALSE)
  }
  xlim <- check_range2(xlim, "xlim")
  ylim <- check_range2(ylim, "ylim")
  grid_size <- check_grid_size(grid_size)
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric scalar between 0 and 1.", call. = FALSE)
  }

  dots <- list(...)
  new_surface_stat_layer(
    stat = "function_surface",
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      grid_size = grid_size,
      fill = fill,
      alpha = alpha,
      surface_alpha = surface_alpha,
      tessellation = tessellation,
      name = name,
      args = dots
    )
  )
}

stat_density_surface3d <- function(mapping = NULL, data = NULL, grid_size = 96,
                                   bandwidth = NULL, bounds = NULL,
                                   alpha = c("density_fade", "combined_fade", "edge_fade", "none"),
                                   fill = "#4477AA", opacity = 0.65,
                                   height = 1,
                                   tessellation = c("rect", "right1", "right2", "equilateral"),
                                   name = "density surface", ...) {
  alpha <- match.arg(alpha)
  tessellation <- match.arg(tessellation)
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in stat_density_surface3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  mapping <- normalize_aes3_mapping(mapping)
  grid_size <- check_grid_size(grid_size)
  bandwidth <- check_bandwidth_or_null(bandwidth)
  bounds <- check_bounds_or_null(bounds)
  if (!is.numeric(opacity) || length(opacity) != 1L || !is.finite(opacity) || opacity < 0 || opacity > 1) {
    stop("opacity must be a numeric scalar between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(height) || length(height) != 1L || !is.finite(height) || height <= 0) {
    stop("height must be a positive numeric scalar.", call. = FALSE)
  }

  new_surface_stat_layer(
    stat = "density_surface",
    mapping = mapping,
    data = data,
    params = list(
      grid_size = grid_size,
      bandwidth = bandwidth,
      bounds = bounds,
      alpha_mode = alpha,
      fill = fill,
      alpha = opacity,
      height = height,
      tessellation = tessellation,
      name = name
    )
  )
}

stat_smooth_surface3d <- function(mapping = NULL, data = NULL, grid_size = 64,
                                  bounds = NULL, fill = "#4477AA", alpha = 0.65,
                                  tessellation = c("rect", "right1", "right2", "equilateral"),
                                  name = "smooth surface", ...) {
  tessellation <- match.arg(tessellation)
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in stat_smooth_surface3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  mapping <- normalize_aes3_mapping(mapping)
  grid_size <- check_grid_size(grid_size)
  bounds <- check_bounds_or_null(bounds)
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric scalar between 0 and 1.", call. = FALSE)
  }

  new_surface_stat_layer(
    stat = "smooth_surface",
    mapping = mapping,
    data = data,
    params = list(
      grid_size = grid_size,
      bounds = bounds,
      fill = fill,
      alpha = alpha,
      tessellation = tessellation,
      name = name
    )
  )
}

new_surface_stat_layer <- function(stat, mapping = NULL, data = NULL, params = list()) {
  layer <- new_layer3d(
    type = "surface_stat",
    mapping = mapping,
    data = data,
    params = c(list(stat = stat), params)
  )
  class(layer) <- c("ggplot3scene_surface_stat_layer", class(layer))
  layer
}

compile_surface_stat_layer <- function(plot, layer, layer_index, theme) {
  p <- layer$params
  grid <- switch(
    p$stat,
    identity_surface = p$surface,
    function_surface = compute_function_surface_grid(p),
    density_surface = compute_density_surface_grid(plot, layer),
    smooth_surface = compute_smooth_surface_grid(plot, layer),
    stop("Unsupported surface stat: ", p$stat, call. = FALSE)
  )

  surface_layer <- new_layer3d(
    type = "surface_grid",
    params = list(
      grid = grid,
      fill = p$fill %||% "#4477AA",
      alpha = p$alpha %||% 0.65,
      name = p$name %||% grid$name %||% "surface stat",
      fill_explicit = TRUE,
      alpha_explicit = TRUE
    )
  )
  compiled <- compile_surface_grid_layer(surface_layer, layer_index, theme)
  compiled$metadata <- list(stat = grid$metadata$stat %||% list(type = p$stat, computedBy = "R"))
  compiled
}

compute_function_surface_grid <- function(p) {
  xgrid <- seq(p$xlim[[1]], p$xlim[[2]], length.out = p$grid_size[[1]])
  ygrid <- seq(p$ylim[[1]], p$ylim[[2]], length.out = p$grid_size[[2]])
  zmat <- outer(xgrid, ygrid, Vectorize(function(x, y) {
    value <- do.call(p$fun, c(list(x = x, y = y), p$args %||% list()))
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      stop("fun must return one finite numeric value for each x/y pair.", call. = FALSE)
    }
    value
  }))

  alpha <- surface_alpha_from_z(zmat, p$surface_alpha)
  grid2d(
    xgrid,
    ygrid,
    zmat,
    alpha = alpha,
    name = p$name,
    tessellation = p$tessellation,
    metadata = list(
      stat = list(
        type = "function_surface",
        method = "R function evaluation",
        gridSize = unname(as.integer(p$grid_size)),
        bounds = list(x = unname(p$xlim), y = unname(p$ylim)),
        computedBy = "R"
      )
    )
  )
}

compute_density_surface_grid <- function(plot, layer) {
  data <- if (is.null(layer$data)) plot$data else layer$data
  if (is.null(data)) {
    stop("stat_density_surface3d() requires data.", call. = FALSE)
  }
  data <- as.data.frame(data)
  mapping <- merge_mapping(as_mapping(plot$mapping), as_mapping(layer$mapping))
  for (required in c("x", "y")) {
    if (is.null(mapping[[required]])) {
      stop("stat_density_surface3d() requires mapping for x and y; missing ", required, ".", call. = FALSE)
    }
  }

  x <- get_mapped_column(data, mapping$x, "x")
  y <- get_mapped_column(data, mapping$y, "y")
  if (length(x) != length(y)) {
    stop("Mapped x and y columns must have the same length.", call. = FALSE)
  }
  p <- layer$params
  bounds <- p$bounds %||% list(x = expanded_range(x, 0.05), y = expanded_range(y, 0.05))
  bandwidth <- p$bandwidth %||% c(safe_bandwidth(x), safe_bandwidth(y))

  xgrid <- seq(bounds$x[[1]], bounds$x[[2]], length.out = p$grid_size[[1]])
  ygrid <- seq(bounds$y[[1]], bounds$y[[2]], length.out = p$grid_size[[2]])
  density <- gaussian_density_grid(x, y, xgrid, ygrid, bandwidth)
  zmat <- if (max(density) == 0) density else density / max(density) * p$height
  alpha <- surface_alpha_from_z(zmat, p$alpha_mode)

  grid2d(
    xgrid,
    ygrid,
    zmat,
    alpha = alpha,
    name = p$name,
    tessellation = p$tessellation,
    metadata = list(
      stat = list(
        type = "density_surface",
        method = "gaussian_kde_product_kernel",
        gridSize = unname(as.integer(p$grid_size)),
        bandwidth = unname(as.numeric(bandwidth)),
        bounds = list(x = unname(bounds$x), y = unname(bounds$y)),
        alpha = p$alpha_mode,
        computedBy = "R"
      )
    )
  )
}

compute_smooth_surface_grid <- function(plot, layer) {
  data <- if (is.null(layer$data)) plot$data else layer$data
  if (is.null(data)) {
    stop("stat_smooth_surface3d() requires data.", call. = FALSE)
  }
  data <- as.data.frame(data)
  mapping <- merge_mapping(as_mapping(plot$mapping), as_mapping(layer$mapping))
  for (required in c("x", "y", "z")) {
    if (is.null(mapping[[required]])) {
      stop("stat_smooth_surface3d() requires mapping for x, y, and z; missing ", required, ".", call. = FALSE)
    }
  }

  x <- get_mapped_column(data, mapping$x, "x")
  y <- get_mapped_column(data, mapping$y, "y")
  z <- get_mapped_column(data, mapping$z, "z")
  p <- layer$params
  bounds <- p$bounds %||% list(x = expanded_range(x, 0.05), y = expanded_range(y, 0.05))
  xgrid <- seq(bounds$x[[1]], bounds$x[[2]], length.out = p$grid_size[[1]])
  ygrid <- seq(bounds$y[[1]], bounds$y[[2]], length.out = p$grid_size[[2]])

  fit <- stats::lm(z ~ x + y + I(x^2) + I(y^2) + I(x * y))
  zmat <- outer(xgrid, ygrid, Vectorize(function(px, py) {
    stats::predict(fit, newdata = data.frame(x = px, y = py))
  }))

  grid2d(
    xgrid,
    ygrid,
    zmat,
    name = p$name,
    tessellation = p$tessellation,
    metadata = list(
      stat = list(
        type = "smooth_surface",
        method = "quadratic_lm",
        gridSize = unname(as.integer(p$grid_size)),
        bounds = list(x = unname(bounds$x), y = unname(bounds$y)),
        computedBy = "R"
      )
    )
  )
}

gaussian_density_grid <- function(x, y, xgrid, ygrid, bandwidth) {
  hx <- bandwidth[[1]]
  hy <- bandwidth[[2]]
  out <- matrix(0, nrow = length(xgrid), ncol = length(ygrid))
  norm <- length(x) * hx * hy * 2 * pi
  for (i in seq_along(x)) {
    kx <- exp(-0.5 * ((xgrid - x[[i]]) / hx)^2)
    ky <- exp(-0.5 * ((ygrid - y[[i]]) / hy)^2)
    out <- out + outer(kx, ky)
  }
  out / norm
}

surface_alpha_from_z <- function(z, mode) {
  if (identical(mode, "none")) {
    return(NULL)
  }
  if (identical(mode, "edge_fade")) {
    return(alpha_edge_fade(z))
  }
  if (identical(mode, "density_fade")) {
    return(alpha_density_fade(z))
  }
  if (identical(mode, "combined_fade")) {
    return(alpha_combined_fade(z))
  }
  stop("Unsupported surface alpha mode: ", mode, call. = FALSE)
}

check_grid_size <- function(grid_size) {
  if (!is.numeric(grid_size) || length(grid_size) < 1L || length(grid_size) > 2L || any(!is.finite(grid_size))) {
    stop("grid_size must be a finite numeric scalar or length-2 vector.", call. = FALSE)
  }
  grid_size <- as.integer(grid_size)
  if (length(grid_size) == 1L) {
    grid_size <- rep(grid_size, 2L)
  }
  if (any(grid_size < 2L)) {
    stop("grid_size values must be at least 2.", call. = FALSE)
  }
  unname(grid_size)
}

check_range2 <- function(x, name) {
  if (!is.numeric(x) || length(x) != 2L || any(!is.finite(x)) || x[[1]] >= x[[2]]) {
    stop(name, " must be a finite increasing numeric vector of length 2.", call. = FALSE)
  }
  unname(as.numeric(x))
}

check_bounds_or_null <- function(bounds) {
  if (is.null(bounds)) {
    return(NULL)
  }
  if (!is.list(bounds) || is.null(bounds$x) || is.null(bounds$y)) {
    stop("bounds must be NULL or a list with x and y ranges.", call. = FALSE)
  }
  list(x = check_range2(bounds$x, "bounds$x"), y = check_range2(bounds$y, "bounds$y"))
}

check_bandwidth_or_null <- function(bandwidth) {
  if (is.null(bandwidth)) {
    return(NULL)
  }
  if (!is.numeric(bandwidth) || length(bandwidth) < 1L || length(bandwidth) > 2L || any(!is.finite(bandwidth)) || any(bandwidth <= 0)) {
    stop("bandwidth must be NULL or a positive numeric scalar or length-2 vector.", call. = FALSE)
  }
  bandwidth <- as.numeric(bandwidth)
  if (length(bandwidth) == 1L) {
    bandwidth <- rep(bandwidth, 2L)
  }
  unname(bandwidth)
}

safe_bandwidth <- function(x) {
  bw <- stats::bw.nrd0(x)
  if (!is.finite(bw) || bw <= 0) {
    bw <- stats::sd(x) / max(length(x)^0.2, 1)
  }
  if (!is.finite(bw) || bw <= 0) {
    bw <- max(diff(range(x)), 1) / 20
  }
  bw
}

expanded_range <- function(x, expand = 0.05) {
  xr <- range(x, finite = TRUE)
  width <- diff(xr)
  pad <- if (width == 0) max(abs(xr), 1) * expand else width * expand
  unname(xr + c(-pad, pad))
}
