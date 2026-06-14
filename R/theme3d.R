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
    light = list(
      ambient = list(color = "#FFFFFF", intensity = 0.65),
      key = list(color = "#FFFFFF", intensity = 0.85, position = c(3, -4, 5))
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
