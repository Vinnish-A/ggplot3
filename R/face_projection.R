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

geom_face_points3d <- function(mapping = NULL, data = NULL,
                               plane = c("zmin", "zmax", "xy", "xz", "yz", "xmin", "xmax", "ymin", "ymax"),
                               axes = c("x", "y"),
                               offset = 0,
                               clip = TRUE,
                               size = 3,
                               alpha = 0.75,
                               colour = NULL,
                               name = "face points",
                               ...) {
  plane <- match.arg(plane)
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_face_points3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  mapping <- normalize_aes3_mapping(mapping)
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0) {
    stop("size must be a positive numeric scalar.", call. = FALSE)
  }
  check_alpha_param(alpha)

  new_layer3d(
    type = "face_projection",
    mapping = mapping,
    data = data,
    params = list(
      projection = position_on_plane3d(plane = plane, axes = axes, offset = offset, clip = clip),
      size = size,
      alpha = alpha,
      colour = colour,
      name = name,
      geom = "points"
    )
  )
}

geom_face_path3d <- function(mapping = NULL, data = NULL,
                             plane = c("zmin", "zmax", "xy", "xz", "yz", "xmin", "xmax", "ymin", "ymax"),
                             axes = c("x", "y"),
                             offset = 0,
                             clip = TRUE,
                             colour = "#374151",
                             alpha = 1,
                             line_width = 1,
                             name = "face path",
                             ...) {
  plane <- match.arg(plane)
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_face_path3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  mapping <- normalize_aes3_mapping(mapping)
  check_alpha_param(alpha)
  check_line_width_param(line_width)

  new_layer3d(
    type = "face_projection",
    mapping = mapping,
    data = data,
    params = list(
      projection = position_on_plane3d(plane = plane, axes = axes, offset = offset, clip = clip),
      colour = colour,
      alpha = alpha,
      line_width = line_width,
      name = name,
      geom = "path"
    )
  )
}

geom_face_contour3d <- function(contours,
                                plane = c("zmin", "zmax", "xy", "xz", "yz", "xmin", "xmax", "ymin", "ymax"),
                                axes = c("x", "y"),
                                offset = 0,
                                clip = TRUE,
                                colour = "#374151",
                                alpha = 1,
                                line_width = 1,
                                name = NULL,
                                ...) {
  plane <- match.arg(plane)
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_face_contour3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  if (!inherits(contours, "ggplot3scene_contour_stack")) {
    stop("contours must be created with contour_stack().", call. = FALSE)
  }
  check_alpha_param(alpha)
  check_line_width_param(line_width)

  new_layer3d(
    type = "face_projection",
    params = list(
      projection = position_on_plane3d(plane = plane, axes = axes, offset = offset, clip = clip),
      contours = contours,
      colour = colour,
      alpha = alpha,
      line_width = line_width,
      name = name %||% contours$name %||% "face contour",
      geom = "contour_lines"
    )
  )
}

compile_face_projection_layer <- function(plot, layer, layer_index, theme) {
  p <- layer$params
  if (identical(p$geom, "density_grid")) {
    data <- compile_grid2d_data(compute_face_density_grid(plot, layer))
    metadata <- list(stat = data$metadata$stat)
    style <- list(
      type = p$geom,
      material = "unlit",
      fill = p$fill,
      opacity = p$opacity,
      side = "double"
    )
  } else if (identical(p$geom, "points")) {
    data <- compile_face_points_data(plot, layer)
    metadata <- list()
    style <- list(
      type = "points",
      material = "unlit"
    )
  } else if (identical(p$geom, "path")) {
    data <- compile_face_path_data(plot, layer)
    metadata <- list()
    style <- list(
      type = "path",
      material = "unlit",
      color = p$colour,
      opacity = p$alpha,
      width = p$line_width,
      widthUnit = "px"
    )
  } else if (identical(p$geom, "contour_lines")) {
    data <- compile_face_contour_data(p$contours)
    metadata <- list()
    style <- list(
      type = "contour_lines",
      material = "unlit",
      color = p$colour,
      opacity = p$alpha,
      width = p$line_width,
      widthUnit = "px"
    )
  } else {
    stop("Unsupported face projection geom: ", p$geom, call. = FALSE)
  }

  out <- list(
    id = paste0("layer-", layer_index),
    type = "face_projection",
    name = p$name,
    visible = TRUE,
    space = list(type = "face_plane"),
    plane = p$projection$plane,
    axes = unname(p$projection$axes),
    offset = p$projection$offset,
    clip = p$projection$clip,
    data = data,
    style = style
  )
  if (length(metadata) > 0L) {
    out$metadata <- metadata
  }
  out
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

compile_face_points_data <- function(plot, layer) {
  data <- get_face_layer_data(plot, layer, "geom_face_points3d()")
  mapping <- merge_mapping(as_mapping(plot$mapping), as_mapping(layer$mapping))
  coords <- get_face_xy(data, mapping, "geom_face_points3d()")
  n <- length(coords$x)
  color <- resolve_point_colors(data, mapping, layer$params$colour, n, "#3366CC")
  list(
    kind = "face_points",
    encoding = "json-columns",
    columns = list(
      x = unname(as.numeric(coords$x)),
      y = unname(as.numeric(coords$y)),
      color = unname(as.character(color)),
      size = rep(layer$params$size, n),
      alpha = rep(layer$params$alpha, n)
    )
  )
}

compile_face_path_data <- function(plot, layer) {
  data <- get_face_layer_data(plot, layer, "geom_face_path3d()")
  mapping <- merge_mapping(as_mapping(plot$mapping), as_mapping(layer$mapping))
  coords <- get_face_xy(data, mapping, "geom_face_path3d()")
  groups <- get_face_groups(data, mapping, length(coords$x))
  polylines <- unname(lapply(split(seq_along(coords$x), groups), function(idx) {
    list(
      x = unname(as.numeric(coords$x[idx])),
      y = unname(as.numeric(coords$y[idx]))
    )
  }))
  list(
    kind = "face_path",
    encoding = "json-polylines",
    polylines = polylines
  )
}

compile_face_contour_data <- function(contours) {
  compiled <- compile_contour_stack_data(contours)
  list(
    kind = "face_contour",
    encoding = "json-polylines",
    polylines = lapply(compiled$polylines, function(polyline) {
      list(
        x = polyline$x,
        y = polyline$y,
        level = polyline$level
      )
    }),
    levels = compiled$levels,
    metadata = compiled$metadata
  )
}

get_face_layer_data <- function(plot, layer, caller) {
  data <- if (is.null(layer$data)) plot$data else layer$data
  if (is.null(data)) {
    stop(caller, " requires data.", call. = FALSE)
  }
  as.data.frame(data)
}

get_face_xy <- function(data, mapping, caller) {
  for (required in c("x", "y")) {
    if (is.null(mapping[[required]])) {
      stop(caller, " requires mapping for x and y; missing ", required, ".", call. = FALSE)
    }
  }
  x <- get_mapped_column(data, mapping$x, "x")
  y <- get_mapped_column(data, mapping$y, "y")
  if (length(x) != length(y)) {
    stop("Mapped x and y columns must have the same length.", call. = FALSE)
  }
  list(x = x, y = y)
}

get_face_groups <- function(data, mapping, n) {
  group_mapping <- mapping$group
  if (is.null(group_mapping)) {
    return(rep("1", n))
  }
  if (!group_mapping %in% names(data)) {
    stop("Column mapped to group not found in data: ", group_mapping, call. = FALSE)
  }
  group <- data[[group_mapping]]
  if (length(group) != n) {
    stop("Column mapped to group must have the same length as x and y.", call. = FALSE)
  }
  as.character(group)
}
