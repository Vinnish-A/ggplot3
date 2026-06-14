abs_anchor <- function() {
  new_abs_route_command("move_anchor")
}

abs_up <- function(px) {
  new_abs_route_command("screen_offset", dx = 0, dy = -check_abs_px(px, "px"))
}

abs_down <- function(px) {
  new_abs_route_command("screen_offset", dx = 0, dy = check_abs_px(px, "px"))
}

abs_left <- function(px) {
  new_abs_route_command("screen_offset", dx = -check_abs_px(px, "px"), dy = 0)
}

abs_right <- function(px) {
  new_abs_route_command("screen_offset", dx = check_abs_px(px, "px"), dy = 0)
}

abs_offset <- function(dx, dy) {
  new_abs_route_command(
    "screen_offset",
    dx = check_abs_px(dx, "dx"),
    dy = check_abs_px(dy, "dy")
  )
}

abs_elbow <- function(direction = c("up-right", "up-left", "down-right", "down-left"),
                      first = 72, second = 150) {
  direction <- match.arg(direction)
  switch(
    direction,
    "up-right" = list(abs_up(first), abs_right(second)),
    "up-left" = list(abs_up(first), abs_left(second)),
    "down-right" = list(abs_down(first), abs_right(second)),
    "down-left" = list(abs_down(first), abs_left(second))
  )
}

abs_route <- function(..., up = 64, right = 120, units = "px") {
  if (!identical(units, "px")) {
    stop("Only units = 'px' is supported for ABS routes.", call. = FALSE)
  }

  commands <- list(...)
  if (length(commands) == 0L) {
    commands <- list(abs_anchor(), abs_up(up), abs_right(right))
  } else {
    commands <- flatten_abs_commands(commands)
    if (!any(vapply(commands, function(command) identical(command$op, "move_anchor"), logical(1)))) {
      commands <- c(list(abs_anchor()), commands)
    }
  }

  route <- list(
    units = units,
    commands = normalize_abs_route_commands(commands)
  )
  class(route) <- c("ggplot3scene_abs_route", "list")
  route
}

geom_abs_label3d <- function(mapping = NULL, data = NULL, route = abs_route(),
                             label_offset = c(12, 0), point_size = 5,
                             line_width = 2,
                             occlusion = c("depth-test", "none"),
                             leader_occlusion = NULL,
                             label_occlusion = "none",
                             anchor_occlusion = NULL,
                             hide_when_anchor_outside = TRUE,
                             name = "ABS labels", ...) {
  point_size_explicit <- !missing(point_size)
  line_width_explicit <- !missing(line_width)
  occlusion <- match.arg(occlusion)
  leader_occlusion <- leader_occlusion %||% occlusion
  anchor_occlusion <- anchor_occlusion %||% occlusion
  leader_occlusion <- match_abs_occlusion(leader_occlusion, "leader_occlusion")
  label_occlusion <- match_abs_occlusion(label_occlusion, "label_occlusion")
  anchor_occlusion <- match_abs_occlusion(anchor_occlusion, "anchor_occlusion")

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
  if (!is.logical(hide_when_anchor_outside) || length(hide_when_anchor_outside) != 1L || is.na(hide_when_anchor_outside)) {
    stop("hide_when_anchor_outside must be TRUE or FALSE.", call. = FALSE)
  }

  new_abs_annotation_layer(
    mapping = mapping,
    data = data,
    params = list(
      route = route,
      label_offset = unname(as.numeric(label_offset)),
      point_size = point_size,
      line_width = line_width,
      point_size_explicit = point_size_explicit,
      line_width_explicit = line_width_explicit,
      occlusion = list(
        anchor = anchor_occlusion,
        leader = leader_occlusion,
        label = label_occlusion
      ),
      visibility = list(
        hideWhenAnchorOutsideFrustum = hide_when_anchor_outside,
        hideLeaderWhenAnchorOutsideFrustum = hide_when_anchor_outside,
        hideLabelWhenAnchorOutsideFrustum = hide_when_anchor_outside
      ),
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

  abs_theme <- theme$abs %||% list()
  point_theme <- abs_theme$point %||% list()
  line_theme <- abs_theme$line %||% list()
  text_theme <- abs_theme$text %||% list()
  background_theme <- abs_theme$label.background %||% list()
  occlusion <- layer$params$occlusion
  point_size <- if (isTRUE(layer$params$point_size_explicit)) layer$params$point_size else point_theme$size %||% layer$params$point_size
  line_width <- if (isTRUE(layer$params$line_width_explicit)) layer$params$line_width else line_theme$width %||% layer$params$line_width

  list(
    id = paste0("layer-", layer_index),
    type = "abs_annotation",
    name = layer$params$name,
    visible = TRUE,
    space = list(
      type = "anchored_billboard",
      units = layer$params$route$units,
      depthMode = "anchor-depth"
    ),
    occlusion = occlusion,
    visibility = layer$params$visibility,
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
        visible = point_theme$visible %||% TRUE,
        size = point_size,
        color = point_theme$color %||% "#111827",
        depthTest = identical(occlusion$anchor, "depth-test")
      ),
      line = list(
        width = line_width,
        widthUnit = "px",
        geometry = "screen-ribbon",
        opacity = line_theme$opacity %||% 1,
        color = line_theme$color %||% "#111827",
        depthTest = identical(occlusion$leader, "depth-test")
      ),
      text = list(
        size = text_theme$size %||% 12,
        color = text_theme$color %||% "#111827",
        opacity = text_theme$opacity %||% 1,
        background = background_theme$visible %||% TRUE,
        backgroundColor = background_theme$fill %||% "#FFFFFF",
        borderColor = background_theme$borderColor %||% "#D1D5DB",
        padding = background_theme$padding %||% c(7, 5),
        billboard = TRUE,
        depthTest = identical(occlusion$label, "depth-test")
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

new_abs_route_command <- function(op, ...) {
  command <- c(list(op = op), list(...))
  class(command) <- c("ggplot3scene_abs_route_command", "list")
  command
}

normalize_abs_route_commands <- function(commands) {
  lapply(commands, function(command) {
    command <- unclass(command)
    if (identical(command$op, "screen_up")) {
      list(op = "screen_offset", dx = 0, dy = -unname(command$dy))
    } else if (identical(command$op, "screen_down")) {
      list(op = "screen_offset", dx = 0, dy = unname(command$dy))
    } else if (identical(command$op, "screen_right")) {
      list(op = "screen_offset", dx = unname(command$dx), dy = 0)
    } else if (identical(command$op, "screen_left")) {
      list(op = "screen_offset", dx = -unname(command$dx), dy = 0)
    } else if (identical(command$op, "screen_offset")) {
      list(op = "screen_offset", dx = unname(command$dx), dy = unname(command$dy))
    } else if (identical(command$op, "move_anchor")) {
      list(op = "move_anchor")
    } else {
      stop("Unsupported ABS route command: ", command$op, call. = FALSE)
    }
  })
}

flatten_abs_commands <- function(commands) {
  out <- list()
  append_command <- function(x) {
    if (inherits(x, "ggplot3scene_abs_route_command")) {
      out[[length(out) + 1L]] <<- x
    } else if (is.list(x) && length(x) > 0L && !is.null(x[[1]]) &&
               inherits(x[[1]], "ggplot3scene_abs_route_command")) {
      for (item in x) append_command(item)
    } else {
      stop("ABS route arguments must be commands such as abs_anchor(), abs_up(), or abs_offset().", call. = FALSE)
    }
  }
  for (command in commands) append_command(command)
  out
}

check_abs_px <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop(name, " must be a finite numeric scalar.", call. = FALSE)
  }
  unname(as.numeric(x))
}

match_abs_occlusion <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || !x %in% c("depth-test", "none")) {
    stop(name, " must be 'depth-test' or 'none'.", call. = FALSE)
  }
  x
}
