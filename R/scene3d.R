ggplot3 <- function(data = NULL, mapping = aes3(), theme = theme_3d_gray()) {
  mapping <- normalize_aes3_mapping(mapping)
  plot <- list(
    data = data,
    mapping = mapping,
    layers = list(),
    coord = coord_3d(origin_mode = "data_min"),
    camera = NULL,
    theme = theme,
    guides = list(),
    labels = list(),
    layout = layout_3d(),
    render = NULL,
    performance = performance_policy3d()
  )
  class(plot) <- c("ggplot3scene_plot", "list")
  plot
}

`+.ggplot3scene_plot` <- function(e1, e2) {
  if (inherits(e2, "ggplot3scene_layer")) {
    e1$layers[[length(e1$layers) + 1L]] <- e2
    return(e1)
  }
  if (inherits(e2, "ggplot3scene_coord")) {
    e1$coord <- e2
    return(e1)
  }
  if (inherits(e2, "ggplot3scene_camera")) {
    e1$camera <- e2
    return(e1)
  }
  if (inherits(e2, "ggplot3scene_theme")) {
    e1$theme <- merge_theme3d(e1$theme, e2)
    return(e1)
  }
  if (inherits(e2, "ggplot3scene_guide")) {
    e1$guides[[length(e1$guides) + 1L]] <- e2
    return(e1)
  }
  if (inherits(e2, "ggplot3scene_guides")) {
    e1$guides <- c(e1$guides, unclass(e2))
    return(e1)
  }
  if (inherits(e2, "ggplot3scene_labels")) {
    e1$labels <- merge_list_simple(e1$labels, unclass(e2))
    return(e1)
  }
  if (inherits(e2, "ggplot3scene_layout")) {
    e1$layout <- e2
    return(e1)
  }
  if (inherits(e2, "ggplot3scene_render_spec")) {
    e1$render <- e2
    return(e1)
  }
  if (inherits(e2, "ggplot3scene_performance_policy")) {
    e1$performance <- e2
    return(e1)
  }
  stop("Cannot add object of class ", paste(class(e2), collapse = "/"), " to a ggplot3scene plot.", call. = FALSE)
}

merge_list_simple <- function(x, y) {
  x <- x %||% list()
  for (nm in names(y)) x[[nm]] <- y[[nm]]
  x
}

coord_3d <- function(projection = c("orthographic", "perspective"),
                     position = c(1.8, -2.4, 1.6),
                     target = c(0, 0, 0),
                     up = c(0, 0, 1),
                     zoom = 1,
                     aspect = c(1, 1, 1),
                     origin = c(0, 0, 0),
                     origin_mode = c("fixed", "data_min", "data_center"),
                     axis_limits = list(x = NULL, y = NULL, z = NULL),
                     grid = grid_3d(),
                     axis = axis_3d(
                       length_fraction = grid$axis_length_fraction %||% 1,
                       arrows = grid$axis_arrows %||% FALSE
                     ),
                     clip = c("none", "grid", "axes", "data", "all"),
                     expand = 0) {
  projection <- match.arg(projection)
  origin_mode <- match.arg(origin_mode)
  clip <- match.arg(clip)
  check_vec3 <- function(x, name) {
    if (!is.numeric(x) || length(x) != 3L || any(!is.finite(x))) {
      stop(name, " must be a finite numeric vector of length 3.", call. = FALSE)
    }
    x
  }
  if (!is.numeric(zoom) || length(zoom) != 1L || !is.finite(zoom) || zoom <= 0) {
    stop("zoom must be a positive numeric scalar.", call. = FALSE)
  }
  if (!inherits(grid, "ggplot3scene_grid3d")) {
    stop("grid must be created with grid_3d().", call. = FALSE)
  }
  if (!inherits(axis, "ggplot3scene_axis3d")) {
    stop("axis must be created with axis_3d().", call. = FALSE)
  }
  axis_limits <- validate_axis_limits(axis_limits)
  if (!is.numeric(expand) || length(expand) != 1L || !is.finite(expand) || expand < 0) {
    stop("expand must be a non-negative numeric scalar.", call. = FALSE)
  }

  coord <- list(
    projection = projection,
    position = check_vec3(position, "position"),
    target = check_vec3(target, "target"),
    up = check_vec3(up, "up"),
    zoom = zoom,
    aspect = check_vec3(aspect, "aspect"),
    origin = check_vec3(origin, "origin"),
    origin_mode = origin_mode,
    axis_limits = axis_limits,
    grid = grid,
    axis = axis,
    clip = clip,
    expand = expand
  )
  class(coord) <- c("ggplot3scene_coord", "list")
  coord
}

guide_colorbar_scene3d <- function(aesthetic = "colour", title = NULL,
                                   domain = c(0, 1),
                                   palette = c("#2166AC", "#F7F7F7", "#B2182B"),
                                   materialMode = c("unlit", "lit"),
                                   order = 1) {
  materialMode <- match.arg(materialMode)
  if (!is.character(aesthetic) || length(aesthetic) != 1L || !nzchar(aesthetic)) {
    stop("aesthetic must be a non-empty string.", call. = FALSE)
  }
  domain <- check_guide_domain(domain)
  if (!is.character(palette) || length(palette) == 0L) {
    stop("palette must be a non-empty character vector.", call. = FALSE)
  }
  new_scene3d_guide(
    type = "colorbar",
    id = paste0("guide-", aesthetic),
    aesthetic = aesthetic,
    title = title %||% aesthetic,
    order = order,
    domain = domain,
    palette = unname(palette),
    bar = list(
      stops = unname(lapply(seq_along(palette), function(i) {
        list(t = if (length(palette) == 1L) 0 else (i - 1) / (length(palette) - 1), colour = palette[[i]])
      }))
    ),
    breaks = unname(lapply(domain, function(value) {
      list(value = unname(as.numeric(value)), label = format(value))
    })),
    materialMode = materialMode
  )
}

guide_colourbar3d <- guide_colorbar_scene3d

guide_legend_scene3d <- function(aesthetic = "colour", title = NULL,
                                 labels, values,
                                 materialMode = c("unlit", "lit"),
                                 order = 1) {
  materialMode <- match.arg(materialMode)
  if (!is.character(aesthetic) || length(aesthetic) != 1L || !nzchar(aesthetic)) {
    stop("aesthetic must be a non-empty string.", call. = FALSE)
  }
  if (missing(labels) || missing(values)) {
    stop("guide_legend_scene3d() requires labels and values.", call. = FALSE)
  }
  if (!is.character(labels) || !is.character(values) || length(labels) != length(values) || length(labels) == 0L) {
    stop("labels and values must be non-empty character vectors with the same length.", call. = FALSE)
  }
  new_scene3d_guide(
    type = "legend",
    id = paste0("guide-", aesthetic),
    aesthetic = aesthetic,
    title = title %||% aesthetic,
    order = order,
    entries = unname(lapply(seq_along(labels), function(i) {
      list(
        label = labels[[i]],
        value = values[[i]],
        glyph = list(type = "point", colour = values[[i]], size = 4, alpha = 1)
      )
    })),
    materialMode = materialMode
  )
}

guide_legend3d <- guide_legend_scene3d

guides3d <- function(...) {
  values <- list(...)
  out <- list()
  for (nm in names(values)) {
    guide <- values[[nm]]
    if (is.null(guide)) {
      next
    }
    if (!inherits(guide, "ggplot3scene_guide")) {
      stop("guides3d() values must be guide_legend3d() or guide_colourbar3d() objects.", call. = FALSE)
    }
    if (nzchar(nm)) {
      guide$aesthetic <- nm
      guide$id <- paste0("guide-", nm)
    }
    out[[length(out) + 1L]] <- guide
  }
  class(out) <- c("ggplot3scene_guides", "list")
  out
}

new_scene3d_guide <- function(...) {
  out <- list(...)
  class(out) <- c("ggplot3scene_guide", "list")
  out
}

compile_scene3d_guides <- function(guides, plot = NULL, layers = list()) {
  explicit <- unname(lapply(guides %||% list(), function(guide) {
    if (!inherits(guide, "ggplot3scene_guide")) {
      stop("guides must be created with guide_*_scene3d().", call. = FALSE)
    }
    strip_classes(guide)
  }))
  if (length(explicit) > 0L) {
    return(order_scene3d_guides(explicit))
  }
  auto <- list()
  seen <- character()
  for (layer in layers) {
    guide <- layer$guide
    if (is.null(guide) || identical(guide$show, FALSE)) {
      next
    }
    key <- paste(guide$aesthetic %||% "", guide$title %||% "", sep = "::")
    if (key %in% seen) {
      next
    }
    seen <- c(seen, key)
    auto[[length(auto) + 1L]] <- strip_classes(guide)
  }
  order_scene3d_guides(auto)
}

check_guide_domain <- function(domain) {
  if (!is.numeric(domain) || length(domain) != 2L || any(!is.finite(domain)) || domain[[1]] >= domain[[2]]) {
    stop("domain must be a finite increasing numeric vector of length 2.", call. = FALSE)
  }
  unname(as.numeric(domain))
}

order_scene3d_guides <- function(guides) {
  if (length(guides) <= 1L) {
    return(guides)
  }
  guide_order <- vapply(guides, function(guide) guide$order %||% Inf, numeric(1))
  guides[order(guide_order, seq_along(guides))]
}

coord_umap3d <- function(origin_mode = "data_min", positive_grid = TRUE,
                         grid_planes = "xy", z_mode = c("zero", "provided"),
                         expand = 0.05,
                         projection = "orthographic", ...) {
  z_mode <- match.arg(z_mode)
  domain <- if (isTRUE(positive_grid)) "positive" else "full"
  coord_3d(
    projection = projection,
    origin_mode = origin_mode,
    grid = grid_3d(visible = TRUE, planes = grid_planes, domain = domain),
    expand = expand,
    ...
  )
}

validate_axis_limits <- function(axis_limits) {
  if (!is.list(axis_limits)) {
    stop("axis_limits must be a list with x, y, and z entries.", call. = FALSE)
  }
  out <- list(x = axis_limits$x, y = axis_limits$y, z = axis_limits$z)
  for (axis in names(out)) {
    value <- out[[axis]]
    if (is.null(value)) {
      next
    }
    if (!is.numeric(value) || length(value) != 2L || any(!is.finite(value)) || value[[1]] > value[[2]]) {
      stop("axis_limits$", axis, " must be NULL or a finite increasing numeric vector of length 2.", call. = FALSE)
    }
  }
  out
}
