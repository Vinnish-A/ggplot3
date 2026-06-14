as_scene3d <- function(plot) {
  if (is_scene3d_list(plot)) {
    return(unclass(plot))
  }
  if (!inherits(plot, "ggplot3scene_plot")) {
    stop("as_scene3d() expects a ggplot3scene plot.", call. = FALSE)
  }
  if (length(plot$layers) == 0L) {
    stop("Plot has no layers.", call. = FALSE)
  }

  theme <- resolve_theme3d(plot$theme)
  compiled_layers <- vector("list", length(plot$layers))
  for (i in seq_along(plot$layers)) {
    layer <- plot$layers[[i]]
    compiled_layers[[i]] <- switch(
      layer$type,
      point_cloud = compile_point_cloud_layer(plot, layer, i, theme),
      surface_grid = compile_surface_grid_layer(layer, i, theme),
      stop("Unsupported layer type: ", layer$type, call. = FALSE)
    )
  }

  scene <- list(
    schemaVersion = "0.1.0",
    sceneId = paste0("scene-", format(Sys.time(), "%Y%m%d%H%M%S")),
    coordinateSystem = list(
      type = "cartesian3d",
      handedness = "right",
      aspect = list(mode = "manual", ratio = unname(plot$coord$aspect))
    ),
    layers = compiled_layers,
    camera = list(
      projection = plot$coord$projection,
      position = unname(plot$coord$position),
      target = unname(plot$coord$target),
      up = unname(plot$coord$up),
      zoom = plot$coord$zoom
    ),
    axes = list(
      x = list(visible = TRUE, title = "x"),
      y = list(visible = TRUE, title = "y"),
      z = list(visible = TRUE, title = "z")
    ),
    lights = list(
      ambient = theme$light$ambient,
      key = theme$light$key
    ),
    theme = theme,
    metadata = list(
      generator = "ggplot3scene-r",
      backendVersion = "0.0.1"
    )
  )
  scene
}

is_scene3d_list <- function(x) {
  is.list(x) &&
    identical(unclass(x)$schemaVersion, "0.1.0") &&
    !is.null(unclass(x)$layers) &&
    !is.null(unclass(x)$camera)
}

compile_point_cloud_layer <- function(plot, layer, layer_index, theme) {
  data <- if (is.null(layer$data)) plot$data else layer$data
  if (is.null(data)) {
    stop("point_cloud layer requires data.", call. = FALSE)
  }
  data <- as.data.frame(data)

  mapping <- merge_mapping(as_mapping(plot$mapping), as_mapping(layer$mapping))
  for (required in c("x", "y", "z")) {
    if (is.null(mapping[[required]])) {
      stop("point_cloud layer requires mapping for x, y, and z; missing ", required, ".", call. = FALSE)
    }
  }

  x <- get_mapped_column(data, mapping$x, "x")
  y <- get_mapped_column(data, mapping$y, "y")
  z <- get_mapped_column(data, mapping$z, "z")
  n <- length(x)
  if (length(y) != n || length(z) != n) {
    stop("Mapped x, y, and z columns must have the same length.", call. = FALSE)
  }

  point_theme <- theme$material$point %||% list()
  default_color <- point_theme$color %||% "#3366CC"
  size_value <- if (isTRUE(layer$params$size_explicit)) layer$params$size else point_theme$size %||% layer$params$size
  alpha_value <- if (isTRUE(layer$params$alpha_explicit)) layer$params$alpha else point_theme$opacity %||% layer$params$alpha

  color <- resolve_point_colors(data, mapping, layer$params$colour, n, default_color)
  size <- rep(size_value, n)
  alpha <- rep(alpha_value, n)

  list(
    id = paste0("layer-", layer_index),
    type = "point_cloud",
    name = layer$params$name,
    visible = TRUE,
    data = list(
      encoding = "json-columns",
      columns = list(
        x = unname(as.numeric(x)),
        y = unname(as.numeric(y)),
        z = unname(as.numeric(z)),
        color = unname(as.character(color)),
        size = unname(as.numeric(size)),
        alpha = unname(as.numeric(alpha))
      )
    ),
    material = list(
      type = point_theme$type %||% "points",
      sizeUnit = point_theme$sizeUnit %||% "screen",
      depthTest = point_theme$depthTest %||% TRUE
    )
  )
}

compile_surface_grid_layer <- function(layer, layer_index, theme) {
  p <- layer$params
  surface_theme <- theme$material$surface %||% list()
  fill <- if (isTRUE(p$fill_explicit)) p$fill else surface_theme$fill %||% p$fill
  opacity <- if (isTRUE(p$alpha_explicit)) p$alpha else surface_theme$opacity %||% p$alpha

  list(
    id = paste0("layer-", layer_index),
    type = "surface_grid",
    name = p$name,
    visible = TRUE,
    data = list(
      encoding = "json-grid",
      x = unname(as.numeric(p$x)),
      y = unname(as.numeric(p$y)),
      z = unname(as.numeric(as.vector(t(p$z)))),
      shape = c(length(p$x), length(p$y))
    ),
    material = list(
      type = surface_theme$type %||% "surface",
      model = surface_theme$model %||% "unlit",
      fill = fill,
      opacity = opacity,
      side = surface_theme$side %||% "double"
    )
  )
}

merge_mapping <- function(base, override) {
  out <- base
  for (nm in names(override)) {
    out[[nm]] <- override[[nm]]
  }
  out
}

get_mapped_column <- function(data, column, aesthetic) {
  if (!is.character(column) || length(column) != 1L) {
    stop("Mapping for ", aesthetic, " must resolve to one column name.", call. = FALSE)
  }
  if (!column %in% names(data)) {
    stop("Column mapped to ", aesthetic, " not found in data: ", column, call. = FALSE)
  }
  value <- data[[column]]
  if (!is.numeric(value)) {
    stop("Column mapped to ", aesthetic, " must be numeric: ", column, call. = FALSE)
  }
  if (any(!is.finite(value))) {
    stop("Column mapped to ", aesthetic, " contains non-finite values: ", column, call. = FALSE)
  }
  value
}

resolve_point_colors <- function(data, mapping, explicit_colour, n, default_color = "#3366CC") {
  if (!is.null(explicit_colour)) {
    return(rep(explicit_colour, n))
  }

  colour_mapping <- mapping$colour
  if (is.null(colour_mapping)) {
    colour_mapping <- mapping$color
  }

  if (!is.null(colour_mapping)) {
    if (!colour_mapping %in% names(data)) {
      stop("Column mapped to colour not found in data: ", colour_mapping, call. = FALSE)
    }
    values <- data[[colour_mapping]]
    if (is.factor(values) || is.character(values)) {
      levels <- sort(unique(as.character(values)))
      palette <- grDevices::hcl.colors(max(3L, length(levels)), "Dark 3")
      color_map <- stats::setNames(palette[seq_along(levels)], levels)
      return(unname(color_map[as.character(values)]))
    }
  }

  rep(default_color, n)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
