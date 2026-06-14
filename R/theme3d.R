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
  as_json_theme(merge_theme3d(theme_3d_scientific(), theme))
}

validate_theme_key <- function(key) {
  allowed <- c(
    "scene.background",
    "axis.grid.major",
    "axis.line",
    "axis.text",
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
