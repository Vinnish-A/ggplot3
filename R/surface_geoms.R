geom_surface_mesh3d <- function(mesh, fill = "#4477AA", alpha = 0.65,
                                name = NULL, ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_surface_mesh3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  if (!inherits(mesh, "ggplot3scene_surface_mesh")) {
    stop("mesh must be created with surface_mesh().", call. = FALSE)
  }
  check_alpha_param(alpha)

  new_layer3d(
    type = "surface_mesh",
    params = list(
      mesh = mesh,
      fill = fill,
      alpha = alpha,
      name = name %||% mesh$name %||% "surface mesh"
    )
  )
}

geom_contour_stack3d <- function(contours, colour = "#374151", alpha = 1,
                                 line_width = 1, name = NULL, ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_contour_stack3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  if (!inherits(contours, "ggplot3scene_contour_stack")) {
    stop("contours must be created with contour_stack().", call. = FALSE)
  }
  check_alpha_param(alpha)
  check_line_width_param(line_width)

  new_layer3d(
    type = "contour_stack",
    params = list(
      contours = contours,
      colour = colour,
      alpha = alpha,
      line_width = line_width,
      name = name %||% contours$name %||% "contour stack"
    )
  )
}

geom_ridgeline3d <- function(ridges, colour = "#374151", alpha = 1,
                             line_width = 1, name = NULL, ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_ridgeline3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  if (!inherits(ridges, "ggplot3scene_ridgeline_stack")) {
    stop("ridges must be created with ridgeline_stack().", call. = FALSE)
  }
  check_alpha_param(alpha)
  check_line_width_param(line_width)

  new_layer3d(
    type = "ridgeline_stack",
    params = list(
      ridges = ridges,
      colour = colour,
      alpha = alpha,
      line_width = line_width,
      name = name %||% ridges$name %||% "ridgeline stack"
    )
  )
}

compile_surface_mesh_layer <- function(layer, layer_index, theme) {
  p <- layer$params
  surface_theme <- theme$material$surface %||% list()
  list(
    id = paste0("layer-", layer_index),
    type = "surface_mesh",
    name = p$name,
    visible = TRUE,
    data = compile_surface_mesh_data(p$mesh),
    material = list(
      type = "surface_mesh",
      model = surface_theme$model %||% "unlit",
      fill = p$fill,
      opacity = p$alpha,
      side = surface_theme$side %||% "double"
    )
  )
}

compile_contour_stack_layer <- function(layer, layer_index, theme) {
  p <- layer$params
  list(
    id = paste0("layer-", layer_index),
    type = "contour_stack",
    name = p$name,
    visible = TRUE,
    space = list(type = "grid_surface"),
    data = compile_contour_stack_data(p$contours),
    style = list(
      type = "polyline",
      color = p$colour,
      opacity = p$alpha,
      width = p$line_width,
      widthUnit = "px"
    )
  )
}

compile_ridgeline_stack_layer <- function(layer, layer_index, theme) {
  p <- layer$params
  list(
    id = paste0("layer-", layer_index),
    type = "ridgeline_stack",
    name = p$name,
    visible = TRUE,
    space = list(type = "grid_surface"),
    data = compile_ridgeline_stack_data(p$ridges),
    style = list(
      type = "polyline",
      color = p$colour,
      opacity = p$alpha,
      width = p$line_width,
      widthUnit = "px"
    )
  )
}

compile_surface_mesh_data <- function(mesh) {
  if (!inherits(mesh, "ggplot3scene_surface_mesh")) {
    stop("mesh must be created with surface_mesh().", call. = FALSE)
  }
  vertices <- unname(as.numeric(as.vector(t(mesh$vertices))))
  faces <- mesh$faces
  list(
    kind = "mesh3d",
    encoding = "json-mesh",
    vertices = vertices,
    faces = unname(as.integer(as.vector(t(faces - 1L)))),
    vertexCount = nrow(mesh$vertices),
    faceCount = nrow(faces),
    normals = if (is.null(mesh$normals)) NULL else unname(as.numeric(as.vector(t(mesh$normals)))),
    colors = if (is.null(mesh$colors)) NULL else unname(as.character(mesh$colors)),
    metadata = strip_classes(mesh$metadata %||% list())
  )
}

compile_contour_stack_data <- function(contours) {
  if (!inherits(contours, "ggplot3scene_contour_stack")) {
    stop("contours must be created with contour_stack().", call. = FALSE)
  }
  polylines <- normalize_polyline_stack(contours$contours, contours$levels, "contours")
  list(
    kind = "contour_stack",
    encoding = "json-polylines",
    polylines = polylines,
    levels = if (is.null(contours$levels)) NULL else unname(as.numeric(contours$levels)),
    metadata = strip_classes(contours$metadata %||% list())
  )
}

compile_ridgeline_stack_data <- function(ridges) {
  if (!inherits(ridges, "ggplot3scene_ridgeline_stack")) {
    stop("ridges must be created with ridgeline_stack().", call. = FALSE)
  }
  profiles <- normalize_polyline_stack(ridges$profiles, NULL, "profiles")
  list(
    kind = "ridgeline_stack",
    encoding = "json-polylines",
    profiles = profiles,
    metadata = strip_classes(ridges$metadata %||% list())
  )
}

normalize_polyline_stack <- function(items, levels = NULL, name) {
  if (!is.list(items)) {
    stop(name, " must be a list.", call. = FALSE)
  }
  if (length(items) == 0L) {
    return(list())
  }
  if (!is.null(levels) && length(levels) != length(items)) {
    stop("levels must be NULL or have one value per polyline.", call. = FALSE)
  }
  unname(lapply(seq_along(items), function(i) {
    normalize_polyline3d(items[[i]], level = if (is.null(levels)) NULL else levels[[i]])
  }))
}

normalize_polyline3d <- function(item, level = NULL) {
  if (is.data.frame(item)) {
    item <- as.list(item)
  }
  if (is.matrix(item)) {
    if (!is.numeric(item) || ncol(item) != 3L || nrow(item) < 2L) {
      stop("Each polyline matrix must be numeric with at least 2 rows and 3 columns.", call. = FALSE)
    }
    x <- item[, 1]
    y <- item[, 2]
    z <- item[, 3]
  } else if (is.list(item)) {
    if (is.null(item$x) || is.null(item$y) || is.null(item$z)) {
      stop("Each polyline must provide x, y, and z.", call. = FALSE)
    }
    x <- item$x
    y <- item$y
    z <- item$z
    if (is.null(level) && !is.null(item$level)) {
      level <- item$level
    }
  } else {
    stop("Each polyline must be a matrix, data.frame, or list.", call. = FALSE)
  }
  if (!is.numeric(x) || !is.numeric(y) || !is.numeric(z) ||
      length(x) < 2L || length(y) != length(x) || length(z) != length(x) ||
      any(!is.finite(x)) || any(!is.finite(y)) || any(!is.finite(z))) {
    stop("Polyline x, y, and z must be finite numeric vectors of equal length at least 2.", call. = FALSE)
  }
  list(
    x = unname(as.numeric(x)),
    y = unname(as.numeric(y)),
    z = unname(as.numeric(z)),
    level = if (is.null(level)) NULL else unname(as.numeric(level))
  )
}

check_alpha_param <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric scalar between 0 and 1.", call. = FALSE)
  }
}

check_line_width_param <- function(line_width) {
  if (!is.numeric(line_width) || length(line_width) != 1L || !is.finite(line_width) || line_width <= 0) {
    stop("line_width must be a positive numeric scalar.", call. = FALSE)
  }
}
