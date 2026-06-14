ggplot3_from_ggplot <- function(plot) {
  if (!inherits(plot, "ggplot")) {
    stop("ggplot3_from_ggplot() expects a ggplot object.", call. = FALSE)
  }

  out <- ggplot3()
  out <- apply_ggplot_theme_basics(out, plot$theme)
  for (layer in plot$layers %||% list()) {
    geom_classes <- class(layer$geom)
    layer_data <- adapter_layer_data(plot, layer)
    layer_mapping <- adapter_layer_mapping(plot, layer)

    if (any(grepl("GeomPoint", geom_classes, fixed = TRUE))) {
      converted <- adapter_add_z_defaults(layer_data, layer_mapping)
      params <- adapter_geom_params(layer, list(size = 4, alpha = 1, colour = NULL))
      out <- out + geom_point3d(
        mapping = adapter_mapping(converted$mapping),
        data = converted$data,
        size = params$size,
        alpha = params$alpha,
        colour = params$colour
      )
    } else if (any(grepl("GeomPath", geom_classes, fixed = TRUE)) ||
        any(grepl("GeomLine", geom_classes, fixed = TRUE))) {
      converted <- adapter_add_z_defaults(layer_data, layer_mapping)
      params <- adapter_geom_params(layer, list(alpha = 1, colour = "#374151", linewidth = 1))
      out <- out + geom_path3d(
        mapping = adapter_mapping(converted$mapping),
        data = converted$data,
        colour = params$colour %||% "#374151",
        alpha = params$alpha,
        line_width = params$linewidth %||% params$size %||% 1
      )
    } else if (any(grepl("GeomSegment", geom_classes, fixed = TRUE))) {
      converted <- adapter_add_segment_z_defaults(layer_data, layer_mapping)
      params <- adapter_geom_params(layer, list(alpha = 1, colour = "#374151", linewidth = 1))
      out <- out + geom_segment3d(
        mapping = adapter_mapping(converted$mapping),
        data = converted$data,
        colour = params$colour %||% "#374151",
        alpha = params$alpha,
        line_width = params$linewidth %||% params$size %||% 1
      )
    } else {
      stop(
        "ggplot3_from_ggplot() currently supports geom_point, geom_path, geom_line, and geom_segment layers.",
        call. = FALSE
      )
    }
  }
  out
}

as_scene3d_ggplot <- function(plot) {
  as_scene3d(ggplot3_from_ggplot(plot))
}

adapter_layer_data <- function(plot, layer) {
  data <- layer$data
  if (is.null(data) || inherits(data, "waiver")) {
    data <- plot$data
  }
  if (is.null(data)) {
    stop("ggplot3_from_ggplot() layer requires data.", call. = FALSE)
  }
  if (is.function(data)) {
    stop("ggplot3_from_ggplot() does not support function-valued layer data.", call. = FALSE)
  }
  as.data.frame(data)
}

adapter_layer_mapping <- function(plot, layer) {
  base <- adapter_as_mapping(plot$mapping)
  override <- if (is.null(layer$mapping) || inherits(layer$mapping, "waiver")) list() else adapter_as_mapping(layer$mapping)
  merge_mapping(base, override)
}

adapter_as_mapping <- function(mapping) {
  if (is.null(mapping) || length(mapping) == 0L) {
    return(list())
  }
  as_mapping(mapping)
}

adapter_add_z_defaults <- function(data, mapping) {
  if (is.null(mapping$z)) {
    data$.ggplot3scene_z <- 0
    mapping$z <- ".ggplot3scene_z"
  }
  list(data = data, mapping = mapping)
}

adapter_add_segment_z_defaults <- function(data, mapping) {
  converted <- adapter_add_z_defaults(data, mapping)
  data <- converted$data
  mapping <- converted$mapping
  if (is.null(mapping$zend)) {
    data$.ggplot3scene_zend <- data[[mapping$z]]
    mapping$zend <- ".ggplot3scene_zend"
  }
  list(data = data, mapping = mapping)
}

adapter_mapping <- function(mapping) {
  class(mapping) <- c("ggplot3scene_aes", "list")
  mapping
}

adapter_geom_params <- function(layer, defaults) {
  params <- defaults
  for (source in list(layer$geom_params %||% list(), layer$aes_params %||% list())) {
    for (nm in names(source)) {
      value <- source[[nm]]
      if (!is.null(value) && length(value) == 1L) {
        params[[nm]] <- value
      }
    }
  }
  if (!is.null(params$color) && is.null(params$colour)) {
    params$colour <- params$color
  }
  params
}

apply_ggplot_theme_basics <- function(plot, theme) {
  background <- ggplot_theme_fill(theme, "plot.background") %||%
    ggplot_theme_fill(theme, "panel.background")
  if (!is.null(background)) {
    plot <- plot + theme_3d(scene.background = background)
  }
  plot
}

ggplot_theme_fill <- function(theme, name) {
  element <- theme[[name]]
  fill <- element$fill %||% NULL
  if (is.character(fill) && length(fill) == 1L && nzchar(fill) && !identical(fill, NA_character_)) {
    fill
  } else {
    NULL
  }
}
