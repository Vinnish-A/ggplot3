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
  compiled_layers <- list()
  for (i in seq_along(plot$layers)) {
    layer <- plot$layers[[i]]
    compiled_layer <- switch(
      layer$type,
      point_cloud = compile_point_cloud_layer(plot, layer, length(compiled_layers) + 1L, theme),
      polyline3d = compile_polyline3d_layer(plot, layer, length(compiled_layers) + 1L, theme),
      surface_grid = compile_surface_grid_layer(layer, length(compiled_layers) + 1L, theme),
      surface_mesh = compile_surface_mesh_layer(layer, length(compiled_layers) + 1L, theme),
      surface_stat = compile_surface_stat_layer(plot, layer, length(compiled_layers) + 1L, theme),
      contour_stack = compile_contour_stack_layer(layer, length(compiled_layers) + 1L, theme),
      ridgeline_stack = compile_ridgeline_stack_layer(layer, length(compiled_layers) + 1L, theme),
      face_projection = compile_face_projection_layer(plot, layer, length(compiled_layers) + 1L, theme),
      abs_annotation = compile_abs_annotation_layer(plot, layer, length(compiled_layers) + 1L, theme),
      stop("Unsupported layer type: ", layer$type, call. = FALSE)
    )
    compiled_layers[[length(compiled_layers) + 1L]] <- compiled_layer
    if (identical(layer$type, "point_cloud") && !is.null(layer$params$projection)) {
      compiled_layers[[length(compiled_layers) + 1L]] <- compile_source_point_face_projection_layer(
        compiled_layer,
        layer$params$projection,
        length(compiled_layers) + 1L
      )
    }
  }
  bounds <- compute_scene_bounds(compiled_layers)
  coord_protocol <- compile_coord_protocol(plot$coord, bounds)
  camera_protocol <- compile_camera_view(plot$camera, plot$coord, bounds)
  guides <- compile_scene3d_guides(plot$guides, plot, compiled_layers)

  scene <- list(
    schemaVersion = "0.1.0",
    sceneId = paste0("scene-", format(Sys.time(), "%Y%m%d%H%M%S")),
    coordinateSystem = list(
      type = "cartesian3d",
      handedness = "right",
      origin = coord_protocol$origin,
      originMode = plot$coord$origin_mode,
      domain = coord_protocol$domain,
      aspect = list(mode = "manual", ratio = unname(plot$coord$aspect)),
      clip = plot$coord$clip
    ),
    layers = compiled_layers,
    camera = camera_protocol$camera,
    view = camera_protocol$view,
    axes = list(
      x = list(visible = TRUE, title = plot$labels$x %||% "x"),
      y = list(visible = TRUE, title = plot$labels$y %||% "y"),
      z = list(visible = TRUE, title = plot$labels$z %||% "z"),
      grid = coord_protocol$grid,
      style = coord_protocol$axis$style,
      labelPlacement = coord_protocol$axis$labelPlacement,
      tickPlacement = coord_protocol$axis$tickPlacement
    ),
    panels = compile_panel_protocol(plot$camera, theme),
    guides = guides,
    labels = compile_labels3d(plot$labels),
    layout = compile_layout3d(plot$layout, theme),
    render = if (is.null(plot$render)) NULL else strip_classes(plot$render),
    performance = strip_classes(plot$performance),
    lights = list(
      ambient = theme$light$ambient,
      key = theme$light$key
    ),
    theme = theme,
    metadata = list(
      generator = "ggplot3scene-r",
      backendVersion = "0.0.1",
      performancePolicy = strip_classes(plot$performance)
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
  original_n <- nrow(data)

  mapping <- merge_mapping(as_mapping(plot$mapping), as_mapping(layer$mapping))
  data <- apply_point_performance_policy(data, mapping, layer, plot$performance)
  output_n <- nrow(data)
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
    ),
    guide = compile_point_colour_guide(data, mapping, layer$params$colour, default_color, plot$labels, layer$params$show_legend),
    metadata = list(
      performance = list(
        originalPointCount = original_n,
        emittedPointCount = output_n,
        sampled = output_n < original_n,
        sampling = layer$params$sampling,
        hoverMetadata = list(mode = "limited", included = FALSE)
      ),
      rasterize = list(
        mode = layer$params$rasterize,
        threshold = layer$params$rasterize_threshold
      )
    )
  )
}

apply_point_performance_policy <- function(data, mapping, layer, policy) {
  n <- nrow(data)
  threshold <- getOption("ggplot3scene.max_json_points", policy$maxJsonPoints %||% 50000L)
  if (n > threshold) {
    warning("point_cloud layer has ", n, " points; JSON export may be large.", call. = FALSE)
  }
  max_points <- layer$params$max_points
  if (is.null(max_points) || n <= max_points || identical(layer$params$sampling, "none")) {
    return(data)
  }
  if (identical(layer$params$sampling, "stratified")) {
    colour_mapping <- mapping$colour %||% mapping$color
    if (!is.null(colour_mapping) && colour_mapping %in% names(data)) {
      groups <- split(seq_len(n), as.character(data[[colour_mapping]]))
      per_group <- max(1L, floor(max_points / length(groups)))
      idx <- unlist(lapply(groups, function(values) {
        if (length(values) <= per_group) values else sample(values, per_group)
      }), use.names = FALSE)
      if (length(idx) < max_points) {
        rest <- setdiff(seq_len(n), idx)
        idx <- c(idx, sample(rest, min(length(rest), max_points - length(idx))))
      }
      return(data[sort(idx[seq_len(min(length(idx), max_points))]), , drop = FALSE])
    }
  }
  idx <- sort(sample(seq_len(n), max_points))
  data[idx, , drop = FALSE]
}

compile_source_point_face_projection_layer <- function(source_layer, projection, layer_index) {
  list(
    id = paste0("layer-", layer_index),
    type = "face_projection",
    name = paste0(source_layer$name, " face projection"),
    visible = isTRUE(projection$visible),
    space = list(type = "face_plane"),
    sourceLayerId = source_layer$id,
    faces = unname(projection$faces),
    offset = projection$offset,
    clip = TRUE,
    data = list(kind = "source_point_cloud", encoding = "source-reference"),
    style = list(
      type = "source_points",
      material = "unlit",
      alphaMultiplier = projection$alphaMultiplier,
      sizeMultiplier = projection$sizeMultiplier,
      depthWrite = FALSE
    ),
    guide = list(show = FALSE)
  )
}

compile_polyline3d_layer <- function(plot, layer, layer_index, theme) {
  data <- if (is.null(layer$data)) plot$data else layer$data
  if (is.null(data)) {
    stop("polyline3d layer requires data.", call. = FALSE)
  }
  data <- as.data.frame(data)
  mapping <- merge_mapping(as_mapping(plot$mapping), as_mapping(layer$mapping))

  polylines <- if (identical(layer$params$geom, "segment")) {
    compile_segment_polylines(data, mapping)
  } else {
    compile_path_polylines(data, mapping)
  }

  list(
    id = paste0("layer-", layer_index),
    type = "polyline3d",
    name = layer$params$name,
    visible = TRUE,
    space = list(type = "world"),
    data = list(
      kind = "polyline3d",
      encoding = "json-polylines",
      polylines = polylines
    ),
    style = list(
      type = layer$params$geom,
      color = layer$params$colour,
      opacity = layer$params$alpha,
      width = layer$params$line_width,
      widthUnit = "px"
    )
  )
}

compile_path_polylines <- function(data, mapping) {
  for (required in c("x", "y", "z")) {
    if (is.null(mapping[[required]])) {
      stop("geom_path3d() requires mapping for x, y, and z; missing ", required, ".", call. = FALSE)
    }
  }
  x <- get_mapped_column(data, mapping$x, "x")
  y <- get_mapped_column(data, mapping$y, "y")
  z <- get_mapped_column(data, mapping$z, "z")
  groups <- get_polyline_groups(data, mapping, length(x))
  polylines <- unname(lapply(split(seq_along(x), groups), function(idx) {
    list(
      x = unname(as.numeric(x[idx])),
      y = unname(as.numeric(y[idx])),
      z = unname(as.numeric(z[idx])),
      level = NULL
    )
  }))
  polylines[vapply(polylines, function(polyline) length(polyline$x) >= 2L, logical(1))]
}

compile_segment_polylines <- function(data, mapping) {
  for (required in c("x", "y", "z", "xend", "yend", "zend")) {
    if (is.null(mapping[[required]])) {
      stop("geom_segment3d() requires mapping for x, y, z, xend, yend, and zend; missing ", required, ".", call. = FALSE)
    }
  }
  x <- get_mapped_column(data, mapping$x, "x")
  y <- get_mapped_column(data, mapping$y, "y")
  z <- get_mapped_column(data, mapping$z, "z")
  xend <- get_mapped_column(data, mapping$xend, "xend")
  yend <- get_mapped_column(data, mapping$yend, "yend")
  zend <- get_mapped_column(data, mapping$zend, "zend")
  n <- length(x)
  if (any(c(length(y), length(z), length(xend), length(yend), length(zend)) != n)) {
    stop("Segment coordinates must have the same length.", call. = FALSE)
  }
  unname(lapply(seq_len(n), function(i) {
    list(
      x = unname(as.numeric(c(x[[i]], xend[[i]]))),
      y = unname(as.numeric(c(y[[i]], yend[[i]]))),
      z = unname(as.numeric(c(z[[i]], zend[[i]]))),
      level = NULL
    )
  }))
}

get_polyline_groups <- function(data, mapping, n) {
  group_mapping <- mapping$group
  if (is.null(group_mapping)) {
    return(rep("1", n))
  }
  if (!group_mapping %in% names(data)) {
    stop("Column mapped to group not found in data: ", group_mapping, call. = FALSE)
  }
  groups <- data[[group_mapping]]
  if (length(groups) != n) {
    stop("Column mapped to group must have the same length as x/y/z.", call. = FALSE)
  }
  as.character(groups)
}

compile_surface_grid_layer <- function(layer, layer_index, theme) {
  p <- layer$params
  grid <- p$grid
  surface_theme <- theme$material$surface %||% list()
  fill <- if (isTRUE(p$fill_explicit)) p$fill else surface_theme$fill %||% p$fill
  opacity <- if (isTRUE(p$alpha_explicit)) p$alpha else surface_theme$opacity %||% p$alpha

  list(
    id = paste0("layer-", layer_index),
    type = "surface_grid",
    name = p$name,
    visible = TRUE,
    data = compile_grid2d_data(grid),
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
      palette <- ggplot3_hue_palette(length(levels))
      color_map <- stats::setNames(palette[seq_along(levels)], levels)
      return(unname(color_map[as.character(values)]))
    }
    if (is.numeric(values)) {
      if (length(values) != n || any(!is.finite(values))) {
        stop("Column mapped to colour must be finite and match the point count: ", colour_mapping, call. = FALSE)
      }
      range <- range(values)
      if (range[[1]] == range[[2]]) {
        return(rep(grDevices::hcl.colors(1L, "Blue-Red 3"), n))
      }
      palette <- grDevices::hcl.colors(256L, "Blue-Red 3")
      index <- pmax(1L, pmin(256L, as.integer(round((values - range[[1]]) / diff(range) * 255L)) + 1L))
      return(unname(palette[index]))
    }
  }

  rep(default_color, n)
}

compile_point_colour_guide <- function(data, mapping, explicit_colour, default_color = "#3366CC",
                                       labels = list(), show_legend = TRUE) {
  if (!isTRUE(show_legend) || !is.null(explicit_colour)) {
    return(NULL)
  }
  colour_mapping <- mapping$colour %||% mapping$color
  if (is.null(colour_mapping) || !colour_mapping %in% names(data)) {
    return(NULL)
  }
  values <- data[[colour_mapping]]
  title <- labels$colour %||% labels$color %||% colour_mapping
  if (is.factor(values) || is.character(values)) {
    levels <- sort(unique(as.character(values)))
    palette <- ggplot3_hue_palette(length(levels))
    return(list(
      id = paste0("guide-colour-", title),
      type = "legend",
      aesthetic = "colour",
      title = title,
      order = 1,
      entries = unname(lapply(seq_along(levels), function(i) {
        list(
          label = levels[[i]],
          value = palette[[i]],
          glyph = list(type = "point", colour = palette[[i]], size = 4, alpha = 1)
        )
      })),
      materialMode = "unlit"
    ))
  }
  if (is.numeric(values) && all(is.finite(values))) {
    palette <- grDevices::hcl.colors(7L, "Blue-Red 3")
    domain <- unname(as.numeric(range(values)))
    return(list(
      id = paste0("guide-colour-", title),
      type = "colorbar",
      aesthetic = "colour",
      title = title,
      order = 1,
      domain = domain,
      palette = unname(palette),
      bar = list(stops = unname(lapply(seq_along(palette), function(i) {
        list(t = (i - 1) / (length(palette) - 1), colour = palette[[i]])
      }))),
      breaks = unname(lapply(pretty(domain, n = 3), function(value) {
        list(value = unname(as.numeric(value)), label = format(value))
      })),
      materialMode = "unlit"
    ))
  }
  NULL
}

ggplot3_hue_palette <- function(n) {
  if (n <= 0L) {
    return(character())
  }
  hues <- seq(15, 375, length.out = n + 1L)[seq_len(n)]
  grDevices::hcl(h = hues, c = 100, l = 65)
}

compute_scene_bounds <- function(layers) {
  mins <- c(x = Inf, y = Inf, z = Inf)
  maxs <- c(x = -Inf, y = -Inf, z = -Inf)

  for (layer in layers) {
    layer_bounds <- compute_layer_bounds(layer)
    if (is.null(layer_bounds)) {
      next
    }
    mins <- pmin(mins, layer_bounds$min)
    maxs <- pmax(maxs, layer_bounds$max)
  }

  if (any(!is.finite(mins)) || any(!is.finite(maxs))) {
    mins <- c(x = -1, y = -1, z = -1)
    maxs <- c(x = 1, y = 1, z = 1)
  }

  list(min = mins, max = maxs)
}

compute_layer_bounds <- function(layer) {
  if (identical(layer$type, "point_cloud")) {
    columns <- layer$data$columns
    return(list(
      min = c(x = min(columns$x), y = min(columns$y), z = min(columns$z)),
      max = c(x = max(columns$x), y = max(columns$y), z = max(columns$z))
    ))
  }

  if (identical(layer$type, "surface_grid")) {
    data <- layer$data
    return(list(
      min = c(x = min(data$x), y = min(data$y), z = min(data$z)),
      max = c(x = max(data$x), y = max(data$y), z = max(data$z))
    ))
  }

  if (identical(layer$type, "polyline3d")) {
    return(compute_polyline_bounds(layer$data$polylines))
  }

  if (identical(layer$type, "surface_mesh")) {
    vertices <- matrix(layer$data$vertices, ncol = 3L, byrow = TRUE)
    return(list(
      min = c(x = min(vertices[, 1]), y = min(vertices[, 2]), z = min(vertices[, 3])),
      max = c(x = max(vertices[, 1]), y = max(vertices[, 2]), z = max(vertices[, 3]))
    ))
  }

  if (identical(layer$type, "contour_stack")) {
    return(compute_polyline_bounds(layer$data$polylines))
  }

  if (identical(layer$type, "ridgeline_stack")) {
    return(compute_polyline_bounds(layer$data$profiles))
  }

  if (identical(layer$type, "abs_annotation")) {
    anchors <- layer$data$anchors
    positions <- do.call(rbind, lapply(anchors, function(anchor) unlist(anchor$position)))
    colnames(positions) <- c("x", "y", "z")
    return(list(
      min = c(x = min(positions[, "x"]), y = min(positions[, "y"]), z = min(positions[, "z"])),
      max = c(x = max(positions[, "x"]), y = max(positions[, "y"]), z = max(positions[, "z"]))
    ))
  }

  if (identical(layer$type, "face_projection")) {
    data <- layer$data
    if (identical(data$kind, "source_point_cloud")) {
      return(NULL)
    }
    axes <- layer$axes
    mins <- c(x = 0, y = 0, z = 0)
    maxs <- c(x = 0, y = 0, z = 0)
    xy_bounds <- face_projection_xy_bounds(data)
    mins[[axes[[1]]]] <- xy_bounds$min[[1]]
    maxs[[axes[[1]]]] <- xy_bounds$max[[1]]
    mins[[axes[[2]]]] <- xy_bounds$min[[2]]
    maxs[[axes[[2]]]] <- xy_bounds$max[[2]]
    return(list(min = mins, max = maxs))
  }

  NULL
}

face_projection_xy_bounds <- function(data) {
  if (identical(data$kind, "grid2d")) {
    return(list(min = c(min(data$x), min(data$y)), max = c(max(data$x), max(data$y))))
  }
  if (identical(data$kind, "face_points")) {
    columns <- data$columns
    return(list(min = c(min(columns$x), min(columns$y)), max = c(max(columns$x), max(columns$y))))
  }
  if (identical(data$kind, "face_path") || identical(data$kind, "face_contour")) {
    polylines <- data$polylines
    xs <- unlist(lapply(polylines, function(polyline) polyline$x), use.names = FALSE)
    ys <- unlist(lapply(polylines, function(polyline) polyline$y), use.names = FALSE)
    if (length(xs) == 0L || length(ys) == 0L) {
      return(list(min = c(0, 0), max = c(0, 0)))
    }
    return(list(min = c(min(xs), min(ys)), max = c(max(xs), max(ys))))
  }
  list(min = c(0, 0), max = c(0, 0))
}

compute_polyline_bounds <- function(polylines) {
  if (length(polylines) == 0L) {
    return(NULL)
  }
  xs <- unlist(lapply(polylines, function(polyline) polyline$x), use.names = FALSE)
  ys <- unlist(lapply(polylines, function(polyline) polyline$y), use.names = FALSE)
  zs <- unlist(lapply(polylines, function(polyline) polyline$z), use.names = FALSE)
  list(
    min = c(x = min(xs), y = min(ys), z = min(zs)),
    max = c(x = max(xs), y = max(ys), z = max(zs))
  )
}

compile_labels3d <- function(labels) {
  defaults <- list(title = NULL, subtitle = NULL, caption = NULL)
  strip_classes(merge_list_simple(defaults, labels %||% list()))
}

compile_layout3d <- function(layout, theme) {
  out <- strip_classes(layout %||% layout_3d())
  legend_position <- theme$legend$position %||% out$legendArea$position %||% "right"
  out$legendArea$position <- legend_position
  out$legendArea$inside <- list(
    position = unname(as.numeric(theme$legend$position.inside %||% c(0.98, 0.98))),
    justification = unname(as.numeric(theme$legend$justification %||% c(1, 1)))
  )
  out$legendArea$direction <- theme$legend$direction %||% "vertical"
  out$legendArea$box <- theme$legend$box %||% "vertical"
  out$legendArea$boxSpacing <- theme$legend$box.spacing %||% 6
  out
}

compile_panel_protocol <- function(camera, theme) {
  panel_mode <- camera$panels %||% "visible"
  faces <- switch(
    panel_mode,
    visible = c("xy_min", "xy_max", "xz_min", "xz_max", "yz_min", "yz_max"),
    all = c("xy_min", "xy_max", "xz_min", "xz_max", "yz_min", "yz_max"),
    none = character(),
    dynamic = c("xy_min", "xy_max", "xz_min", "xz_max", "yz_min", "yz_max"),
    c("xy_min", "xy_max", "xz_min", "xz_max", "yz_min", "yz_max")
  )
  list(
    mode = "cube_faces",
    faces = unname(faces),
    dynamicBackFaces = panel_mode %in% c("visible", "dynamic"),
    style = list(
      fill = theme_value_fill(theme$scene$face, "#E5E5E5"),
      colour = theme_value_colour(theme$scene$face, NA),
      alpha = theme$scene$face$alpha %||% 1
    )
  )
}

theme_value_fill <- function(value, fallback) {
  if (is.character(value) && length(value) == 1L) {
    return(value)
  }
  value$fill %||% fallback
}

theme_value_colour <- function(value, fallback) {
  if (is.character(value) && length(value) == 1L) {
    return(value)
  }
  value$colour %||% value$color %||% fallback
}

compile_coord_protocol <- function(coord, bounds) {
  data_domain <- list(
    x = c(bounds$min[["x"]], bounds$max[["x"]]),
    y = c(bounds$min[["y"]], bounds$max[["y"]]),
    z = c(bounds$min[["z"]], bounds$max[["z"]])
  )
  data_domain <- expand_domain(data_domain, coord$expand)
  domain <- apply_axis_limits(data_domain, coord$axis_limits)

  origin <- switch(
    coord$origin_mode,
    fixed = unname(coord$origin),
    data_min = unname(c(bounds$min[["x"]], bounds$min[["y"]], bounds$min[["z"]])),
    data_center = unname(c(
      mean(c(bounds$min[["x"]], bounds$max[["x"]])),
      mean(c(bounds$min[["y"]], bounds$max[["y"]])),
      mean(c(bounds$min[["z"]], bounds$max[["z"]]))
    ))
  )

  grid_domain <- compile_grid_domain(domain, origin, coord$grid$domain)
  major_breaks <- compile_major_breaks(grid_domain, coord$grid$breaks)

  list(
    origin = origin,
    domain = unname_domain(domain),
    grid = list(
      visible = coord$grid$visible,
      planes = unname(coord$grid$planes),
      domainMode = coord$grid$domain,
      origin = origin,
      domain = unname_domain(grid_domain),
      majorBreaks = major_breaks,
      axisLengthFraction = coord$grid$axis_length_fraction %||% 1,
      axisArrows = coord$grid$axis_arrows %||% FALSE
    ),
    axis = list(
      style = list(
        lengthFraction = coord$axis$length_fraction,
        arrows = coord$axis$arrows,
        labels = coord$axis$labels,
        titles = coord$axis$titles,
        titlePosition = coord$axis$title_position,
        tickOffset = coord$axis$tick_offset,
        titleOffset = coord$axis$title_offset
      ),
      labelPlacement = coord$axis$label_placement,
      tickPlacement = coord$axis$tick_placement
    )
  )
}

expand_domain <- function(domain, expand) {
  if (is.null(expand) || expand <= 0) {
    return(domain)
  }
  for (axis in names(domain)) {
    range_width <- diff(domain[[axis]])
    pad <- if (range_width == 0) max(abs(domain[[axis]]), 1) * expand else range_width * expand
    domain[[axis]] <- domain[[axis]] + c(-pad, pad)
  }
  domain
}

apply_axis_limits <- function(domain, axis_limits) {
  for (axis in names(domain)) {
    if (!is.null(axis_limits[[axis]])) {
      domain[[axis]] <- axis_limits[[axis]]
    }
  }
  domain
}

compile_grid_domain <- function(domain, origin, domain_mode) {
  out <- domain
  axes <- c("x", "y", "z")
  for (i in seq_along(axes)) {
    axis <- axes[[i]]
    if (identical(domain_mode, "positive")) {
      out[[axis]] <- c(origin[[i]], max(domain[[axis]][[2]], origin[[i]]))
    } else if (identical(domain_mode, "negative")) {
      out[[axis]] <- c(min(domain[[axis]][[1]], origin[[i]]), origin[[i]])
    } else {
      out[[axis]] <- domain[[axis]]
    }
  }
  out
}

compile_major_breaks <- function(domain, breaks) {
  out <- list()
  for (axis in names(domain)) {
    if (!is.null(breaks) && !is.null(breaks[[axis]])) {
      out[[axis]] <- unname(as.numeric(breaks[[axis]]))
    } else {
      out[[axis]] <- unname(pretty(domain[[axis]], n = 5))
      out[[axis]] <- out[[axis]][out[[axis]] >= min(domain[[axis]]) & out[[axis]] <= max(domain[[axis]])]
      if (length(out[[axis]]) == 0L) {
        out[[axis]] <- unname(domain[[axis]])
      }
    }
  }
  out
}

unname_domain <- function(domain) {
  list(
    x = unname(as.numeric(domain$x)),
    y = unname(as.numeric(domain$y)),
    z = unname(as.numeric(domain$z))
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
