element_material_3d <- function(...) {
  material <- list(...)
  class(material) <- c("ggplot3scene_element_material", "list")
  material
}

element_light_3d <- function(color = "#FFFFFF", intensity = 1, position = NULL, ...) {
  if (!is.numeric(intensity) || length(intensity) != 1L || !is.finite(intensity) || intensity < 0) {
    stop("intensity must be a non-negative numeric scalar.", call. = FALSE)
  }
  if (!is.null(position) && (!is.numeric(position) || length(position) != 3L || any(!is.finite(position)))) {
    stop("position must be NULL or a finite numeric vector of length 3.", call. = FALSE)
  }
  light <- c(list(color = color, intensity = intensity), list(...))
  if (!is.null(position)) {
    light$position <- position
  }
  class(light) <- c("ggplot3scene_element_light", "list")
  light
}

element_rect_3d <- function(fill = NA, colour = NA, linewidth = 0, alpha = 1) {
  list(fill = fill, colour = colour, linewidth = linewidth, alpha = alpha)
}

element_line_3d <- function(colour = "#FFFFFF", linewidth = 0.6, alpha = 1) {
  list(colour = colour, linewidth = linewidth, alpha = alpha)
}

element_text_3d <- function(size = 11, colour = "#222222", face = "plain",
                            hjust = 0, family = "", alpha = 1) {
  list(size = size, colour = colour, face = face, hjust = hjust, family = family, alpha = alpha)
}

element_plane_3d <- function(fill = "#E5E5E5", colour = NA, alpha = 1) {
  list(fill = fill, colour = colour, alpha = alpha)
}

element_abs_line <- function(color = "#111827", width = 2, opacity = 1) {
  list(color = color, width = width, opacity = opacity)
}

element_abs_point <- function(color = "#111827", size = 5, visible = TRUE) {
  list(color = color, size = size, visible = visible)
}

element_abs_text <- function(color = "#111827", size = 12, opacity = 1) {
  list(color = color, size = size, opacity = opacity)
}

element_abs_label_background <- function(fill = "#FFFFFF", borderColor = "#D1D5DB",
                                         padding = c(7, 5), visible = TRUE) {
  if (!is.numeric(padding) || length(padding) != 2L || any(!is.finite(padding))) {
    stop("padding must be a finite numeric vector of length 2.", call. = FALSE)
  }
  list(fill = fill, borderColor = borderColor, padding = unname(as.numeric(padding)), visible = visible)
}

theme_3d <- function(...) {
  values <- list(...)
  theme <- list()
  if (length(values) == 0L) {
    class(theme) <- c("ggplot3scene_theme", "list")
    return(theme)
  }
  if (is.null(names(values)) || any(!nzchar(names(values)))) {
    stop("All theme_3d() arguments must be named.", call. = FALSE)
  }

  for (nm in names(values)) {
    validate_theme_key(nm)
    parts <- strsplit(nm, ".", fixed = TRUE)[[1]]
    if (length(parts) == 1L) {
      theme[[parts]] <- values[[nm]]
    } else {
      group <- parts[[1]]
      key <- paste(parts[-1], collapse = ".")
      if (is.null(theme[[group]])) {
        theme[[group]] <- list()
      }
      theme[[group]][[key]] <- values[[nm]]
    }
  }

  class(theme) <- c("ggplot3scene_theme", "list")
  theme
}

theme_3d_scientific <- function() {
  theme <- list(
    scene = list(background = "#FFFFFF"),
    axis = list(
      grid.major = "#D9DDE3",
      line = "#4B5563",
      text = "#374151"
    ),
    material = list(
      point = list(
        type = "points",
        sizeUnit = "screen",
        depthTest = TRUE,
        color = "#3366CC",
        size = 4,
        opacity = 1
      ),
      surface = list(
        type = "surface",
        model = "unlit",
        fill = "#4477AA",
        opacity = 0.65,
        side = "double"
      )
    ),
    abs = list(
      line = element_abs_line(),
      point = element_abs_point(),
      text = element_abs_text(),
      label.background = element_abs_label_background()
    ),
    light = list(
      ambient = list(color = "#FFFFFF", intensity = 0.65),
      key = list(color = "#FFFFFF", intensity = 0.85, position = c(3, -4, 5))
    )
  )
  class(theme) <- c("ggplot3scene_theme", "list")
  theme
}

theme_3d_gray <- function(base_size = 11, base_family = "") {
  theme <- list(
    plot = list(
      background = element_rect_3d(fill = "#FFFFFF", colour = NA),
      title = element_text_3d(size = base_size * 1.25, colour = "#222222", face = "bold", family = base_family),
      subtitle = element_text_3d(size = base_size, colour = "#444444", family = base_family),
      caption = element_text_3d(size = base_size * 0.82, colour = "#555555", hjust = 1, family = base_family),
      title.position = "plot"
    ),
    scene = list(
      background = element_rect_3d(fill = "#FFFFFF", colour = NA),
      face = element_plane_3d(fill = "#E5E5E5", colour = NA, alpha = 1),
      box = element_line_3d(colour = NA, linewidth = 0)
    ),
    axis = list(
      grid.major = element_line_3d(colour = "#FFFFFF", linewidth = 0.65),
      grid.minor = NULL,
      line = element_line_3d(colour = "#FFFFFF", linewidth = 0.45),
      text = element_text_3d(size = base_size * 0.78, colour = "#333333", family = base_family),
      title = element_text_3d(size = base_size, colour = "#222222", family = base_family)
    ),
    legend = list(
      position = "right",
      position.inside = c(0.98, 0.98),
      justification = c(1, 1),
      direction = "vertical",
      box = "vertical",
      box.spacing = 6,
      background = element_rect_3d(fill = NA, colour = NA),
      key = element_rect_3d(fill = "#E5E5E5", colour = NA),
      text = element_text_3d(size = base_size * 0.8, colour = "#222222", family = base_family),
      title = element_text_3d(size = base_size, colour = "#222222", family = base_family),
      margin = margin3d(6, 6, 6, 6)
    ),
    material = list(
      point = list(
        type = "points",
        sizeUnit = "screen",
        depthTest = TRUE,
        depthWrite = TRUE,
        color = "#3366CC",
        size = 2.2,
        opacity = 1
      ),
      surface = list(
        type = "surface",
        model = "unlit",
        fill = "#4477AA",
        opacity = 0.65,
        side = "double"
      )
    ),
    abs = list(
      line = element_abs_line(),
      point = element_abs_point(),
      text = element_abs_text(),
      label.background = element_abs_label_background()
    ),
    light = list(
      ambient = list(color = "#FFFFFF", intensity = 0.8),
      key = list(color = "#FFFFFF", intensity = 0.35, position = c(1, -1, 2))
    )
  )
  class(theme) <- c("ggplot3scene_theme", "list")
  theme
}

theme_3d_minimal <- function(base_size = 11, base_family = "") {
  theme <- theme_3d_gray(base_size = base_size, base_family = base_family)
  theme$scene$face <- element_plane_3d(fill = "#FFFFFF", colour = NA, alpha = 1)
  theme$axis$grid.major <- element_line_3d(colour = "#E5E7EB", linewidth = 0.5)
  theme
}

theme_3d_void <- function(base_size = 11, base_family = "") {
  theme <- theme_3d_gray(base_size = base_size, base_family = base_family)
  theme$scene$face <- element_plane_3d(fill = "#FFFFFF", colour = NA, alpha = 0)
  theme$axis$grid.major <- NULL
  theme$axis$line <- element_line_3d(colour = NA, linewidth = 0)
  theme$axis$text <- element_text_3d(size = base_size * 0.8, colour = NA, family = base_family)
  theme
}

theme_3d_umap <- function() {
  theme <- list(
    scene = list(background = "#FFFFFF"),
    axis = list(
      grid.major = "#E3E7ED",
      line = "#6B7280",
      text = "#374151"
    ),
    material = list(
      point = list(
        type = "points",
        sizeUnit = "screen",
        depthTest = TRUE,
        color = "#3366CC",
        size = 3,
        opacity = 0.85
      ),
      surface = list(
        type = "surface",
        model = "unlit",
        fill = "#4477AA",
        opacity = 0.72,
        side = "double"
      )
    ),
    abs = list(
      line = element_abs_line(color = "#374151", opacity = 1),
      point = element_abs_point(color = "#374151", size = 5),
      text = element_abs_text(color = "#111827", size = 12),
      label.background = element_abs_label_background(fill = "#FFFFFF", borderColor = "#D1D5DB")
    ),
    light = list(
      ambient = list(color = "#FFFFFF", intensity = 0.8),
      key = list(color = "#FFFFFF", intensity = 0.55, position = c(2, -3, 4))
    )
  )
  class(theme) <- c("ggplot3scene_theme", "list")
  theme
}

merge_theme3d <- function(base, override) {
  base <- unclass(base)
  override <- unclass(override)

  merge_list <- function(x, y) {
    for (nm in names(y)) {
      if (is.list(x[[nm]]) && is.list(y[[nm]]) && !is.null(names(y[[nm]]))) {
        x[[nm]] <- merge_list(x[[nm]], y[[nm]])
      } else {
        x[[nm]] <- y[[nm]]
      }
    }
    x
  }

  out <- merge_list(base, override)
  class(out) <- c("ggplot3scene_theme", "list")
  out
}

as_json_theme <- function(theme) {
  strip_classes(theme)
}

resolve_theme3d <- function(theme) {
  if (is.null(theme)) {
    theme <- theme_3d()
  }
  if (!inherits(theme, "ggplot3scene_theme")) {
    stop("theme must be created with theme_3d() or theme_3d_scientific().", call. = FALSE)
  }
  as_json_theme(merge_theme3d(theme_3d_gray(), theme))
}

validate_theme_key <- function(key) {
  allowed <- c(
    "scene.background",
    "scene.face",
    "scene.box",
    "plot.background",
    "plot.title",
    "plot.subtitle",
    "plot.caption",
    "plot.title.position",
    "axis.grid.major",
    "axis.grid.minor",
    "axis.line",
    "axis.text",
    "axis.title",
    "legend.position",
    "legend.position.inside",
    "legend.justification",
    "legend.direction",
    "legend.box",
    "legend.box.spacing",
    "legend.background",
    "legend.key",
    "legend.text",
    "legend.title",
    "legend.margin",
    "material.point",
    "material.surface",
    "abs.line",
    "abs.point",
    "abs.text",
    "abs.label.background",
    "light.ambient",
    "light.key"
  )
  if (!key %in% allowed) {
    stop(
      "Unsupported theme_3d() element: ", key,
      ". Theme3D controls scene, axis, material, and light defaults only.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

strip_classes <- function(x) {
  if (is.list(x)) {
    x <- lapply(unclass(x), strip_classes)
  }
  x
}
