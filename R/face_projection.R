position_on_plane3d <- function(plane = c("xy", "xz", "yz", "zmin", "zmax", "xmin", "xmax", "ymin", "ymax"),
                                axes = c("x", "y"),
                                offset = 0,
                                clip = TRUE) {
  plane <- match.arg(plane)
  if (!is.character(axes) || length(axes) != 2L || any(!axes %in% c("x", "y", "z")) || axes[[1]] == axes[[2]]) {
    stop("axes must contain two distinct coordinate axes: x, y, or z.", call. = FALSE)
  }
  if (!is.numeric(offset) || length(offset) != 1L || !is.finite(offset)) {
    stop("offset must be a finite numeric scalar.", call. = FALSE)
  }
  if (!is.logical(clip) || length(clip) != 1L || is.na(clip)) {
    stop("clip must be TRUE or FALSE.", call. = FALSE)
  }

  out <- list(
    plane = plane,
    axes = unname(axes),
    offset = unname(as.numeric(offset)),
    clip = clip
  )
  class(out) <- c("ggplot3scene_plane_position", "list")
  out
}

geom_face_density3d <- function(mapping = NULL, data = NULL,
                                plane = c("zmin", "zmax", "xy", "xz", "yz", "xmin", "xmax", "ymin", "ymax"),
                                axes = c("x", "y"),
                                offset = 0,
                                clip = TRUE,
                                grid_size = 72,
                                bandwidth = NULL,
                                bounds = NULL,
                                alpha = c("density_fade", "combined_fade", "edge_fade", "none"),
                                fill = "#4477AA",
                                opacity = 0.5,
                                name = "face density",
                                ...) {
  plane <- match.arg(plane)
  alpha <- match.arg(alpha)
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_face_density3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  mapping <- normalize_aes3_mapping(mapping)
  if (!is.numeric(opacity) || length(opacity) != 1L || !is.finite(opacity) || opacity < 0 || opacity > 1) {
    stop("opacity must be a numeric scalar between 0 and 1.", call. = FALSE)
  }

  new_layer3d(
    type = "face_projection",
    mapping = mapping,
    data = data,
    params = list(
      projection = position_on_plane3d(plane = plane, axes = axes, offset = offset, clip = clip),
      grid_size = check_grid_size(grid_size),
      bandwidth = check_bandwidth_or_null(bandwidth),
      bounds = check_bounds_or_null(bounds),
      alpha_mode = alpha,
      fill = fill,
      opacity = opacity,
      name = name,
      geom = "density_grid"
    )
  )
}

compile_face_projection_layer <- function(plot, layer, layer_index, theme) {
  p <- layer$params
  grid <- compute_face_density_grid(plot, layer)

  list(
    id = paste0("layer-", layer_index),
    type = "face_projection",
    name = p$name,
    visible = TRUE,
    space = list(type = "face_plane"),
    plane = p$projection$plane,
    axes = unname(p$projection$axes),
    offset = p$projection$offset,
    clip = p$projection$clip,
    data = compile_grid2d_data(grid),
    style = list(
      type = p$geom,
      material = "unlit",
      fill = p$fill,
      opacity = p$opacity,
      side = "double"
    ),
    metadata = list(stat = grid$metadata$stat)
  )
}

compute_face_density_grid <- function(plot, layer) {
  data <- if (is.null(layer$data)) plot$data else layer$data
  if (is.null(data)) {
    stop("geom_face_density3d() requires data.", call. = FALSE)
  }
  data <- as.data.frame(data)
  mapping <- merge_mapping(as_mapping(plot$mapping), as_mapping(layer$mapping))
  for (required in c("x", "y")) {
    if (is.null(mapping[[required]])) {
      stop("geom_face_density3d() requires mapping for x and y; missing ", required, ".", call. = FALSE)
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
  zmat <- if (max(density) == 0) density else density / max(density)
  alpha <- surface_alpha_from_z(zmat, p$alpha_mode)

  grid2d(
    xgrid,
    ygrid,
    zmat,
    alpha = alpha,
    name = p$name,
    metadata = list(
      stat = list(
        type = "face_density",
        method = "gaussian_kde_product_kernel",
        gridSize = unname(as.integer(p$grid_size)),
        bandwidth = unname(as.numeric(bandwidth)),
        bounds = list(x = unname(bounds$x), y = unname(bounds$y)),
        plane = p$projection$plane,
        axes = unname(p$projection$axes),
        computedBy = "R"
      )
    )
  )
}
