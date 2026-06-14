ggplot3_from_ggplot <- function(plot) {
  if (!inherits(plot, "ggplot")) {
    stop("ggplot3_from_ggplot() expects a ggplot object.", call. = FALSE)
  }

  out <- ggplot3(data = plot$data, mapping = plot$mapping)
  for (layer in plot$layers %||% list()) {
    geom_classes <- class(layer$geom)
    if (!any(grepl("GeomPoint", geom_classes, fixed = TRUE))) {
      stop("ggplot3_from_ggplot() currently supports only geom_point layers.", call. = FALSE)
    }
    layer_data <- if (inherits(layer$data, "waiver")) NULL else layer$data
    layer_mapping <- if (inherits(layer$mapping, "waiver")) NULL else layer$mapping
    out <- out + geom_point3d(mapping = layer_mapping, data = layer_data)
  }
  out
}

as_scene3d_ggplot <- function(plot) {
  as_scene3d(ggplot3_from_ggplot(plot))
}
