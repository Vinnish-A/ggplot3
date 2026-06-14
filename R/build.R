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
  bounds <- compute_scene_bounds(compiled_layers)
  coord_protocol <- compile_coord_protocol(plot$coord, bounds)

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
      z = list(visible = TRUE, title = "z"),
      grid = coord_protocol$grid
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
      palette <- grDevices::hcl.colors(max(3L, length(levels)), "Dark 3")
      color_map <- stats::setNames(palette[seq_along(levels)], levels)
      return(unname(color_map[as.character(values)]))
    }
  }

  rep(default_color, n)
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

  NULL
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
      majorBreaks = major_breaks
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
