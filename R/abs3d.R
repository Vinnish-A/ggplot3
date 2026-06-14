abs_route <- function(up = 64, right = 120, units = "px") {
  if (!is.numeric(up) || length(up) != 1L || !is.finite(up)) {
    stop("up must be a finite numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(right) || length(right) != 1L || !is.finite(right)) {
    stop("right must be a finite numeric scalar.", call. = FALSE)
  }
  if (!identical(units, "px")) {
    stop("Only units = 'px' is supported for ABS routes.", call. = FALSE)
  }

  route <- list(
    units = units,
    commands = list(
      list(op = "move_anchor"),
      list(op = "screen_up", dy = unname(up)),
      list(op = "screen_right", dx = unname(right))
    )
  )
  class(route) <- c("ggplot3scene_abs_route", "list")
  route
}

geom_abs_label3d <- function(mapping = NULL, data = NULL, route = abs_route(),
                             label_offset = c(12, 0), point_size = 5,
                             line_width = 2,
                             occlusion = c("depth-test", "none"),
                             name = "ABS labels", ...) {
  occlusion <- match.arg(occlusion)
  dots <- list(...)
  if (length(dots) > 0) {
    stop("Unused arguments in geom_abs_label3d(): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  if (!is.null(mapping) && !inherits(mapping, "ggplot3scene_aes")) {
    stop("mapping must be created with aes3().", call. = FALSE)
  }
  if (!inherits(route, "ggplot3scene_abs_route")) {
    stop("route must be created with abs_route().", call. = FALSE)
  }
  if (!is.numeric(label_offset) || length(label_offset) != 2L || any(!is.finite(label_offset))) {
    stop("label_offset must be a finite numeric vector of length 2.", call. = FALSE)
  }
  if (!is.numeric(point_size) || length(point_size) != 1L || !is.finite(point_size) || point_size < 0) {
    stop("point_size must be a non-negative numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(line_width) || length(line_width) != 1L || !is.finite(line_width) || line_width <= 0) {
    stop("line_width must be a positive numeric scalar.", call. = FALSE)
  }

  new_abs_annotation_layer(
    mapping = mapping,
    data = data,
    params = list(
      route = route,
      label_offset = unname(as.numeric(label_offset)),
      point_size = point_size,
      line_width = line_width,
      occlusion = occlusion,
      name = name
    )
  )
}

new_abs_annotation_layer <- function(mapping = NULL, data = NULL, params = list()) {
  new_layer3d(
    type = "abs_annotation",
    mapping = mapping,
    data = data,
    params = params
  )
}

compile_abs_annotation_layer <- function(plot, layer, layer_index, theme) {
  data <- if (is.null(layer$data)) plot$data else layer$data
  if (is.null(data)) {
    stop("abs_annotation layer requires data.", call. = FALSE)
  }
  data <- as.data.frame(data)

  mapping <- merge_mapping(as_mapping(plot$mapping), as_mapping(layer$mapping))
  for (required in c("x", "y", "z", "label")) {
    if (is.null(mapping[[required]])) {
      stop("abs_annotation layer requires mapping for x, y, z, and label; missing ", required, ".", call. = FALSE)
    }
  }

  x <- get_mapped_column(data, mapping$x, "x")
  y <- get_mapped_column(data, mapping$y, "y")
  z <- get_mapped_column(data, mapping$z, "z")
  label <- get_mapped_label(data, mapping$label)
  n <- length(x)
  if (length(y) != n || length(z) != n || length(label) != n) {
    stop("Mapped x, y, z, and label columns must have the same length.", call. = FALSE)
  }

  route_commands <- strip_classes(layer$params$route$commands)
  label_offset <- unname(as.numeric(layer$params$label_offset))
  anchors <- vector("list", n)
  for (i in seq_len(n)) {
    anchors[[i]] <- list(
      id = paste0("abs-", layer_index, "-", i),
      position = unname(c(as.numeric(x[[i]]), as.numeric(y[[i]]), as.numeric(z[[i]]))),
      route = route_commands,
      label = list(
        text = as.character(label[[i]]),
        offset = label_offset,
        align = "left",
        valign = "middle"
      )
    )
  }

  line_depth_test <- identical(layer$params$occlusion, "depth-test")
  list(
    id = paste0("layer-", layer_index),
    type = "abs_annotation",
    name = layer$params$name,
    visible = TRUE,
    space = list(
      type = "anchored_billboard",
      units = layer$params$route$units,
      depthMode = "anchor-depth",
      occlusion = layer$params$occlusion
    ),
    data = list(
      encoding = "json-abs",
      anchors = anchors
    ),
    route = route_commands,
    label = list(
      offset = label_offset,
      align = "left",
      valign = "middle"
    ),
    style = list(
      point = list(
        visible = TRUE,
        size = layer$params$point_size,
        color = "#111827"
      ),
      line = list(
        width = layer$params$line_width,
        opacity = 1,
        color = "#111827",
        depthTest = line_depth_test
      ),
      text = list(
        size = 12,
        color = "#111827",
        background = TRUE,
        backgroundColor = "#FFFFFF",
        borderColor = "#D1D5DB",
        billboard = TRUE
      )
    )
  )
}

get_mapped_label <- function(data, column) {
  if (!is.character(column) || length(column) != 1L) {
    stop("Mapping for label must resolve to one column name.", call. = FALSE)
  }
  if (!column %in% names(data)) {
    stop("Column mapped to label not found in data: ", column, call. = FALSE)
  }
  value <- data[[column]]
  if (any(is.na(value))) {
    stop("Column mapped to label contains missing values: ", column, call. = FALSE)
  }
  as.character(value)
}
