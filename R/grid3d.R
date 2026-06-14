grid2d <- function(x, y, z, alpha = NULL, mask = NULL, name = NULL, metadata = list()) {
  if (!is.numeric(x) || length(x) == 0L || any(!is.finite(x))) {
    stop("x must be a non-empty finite numeric vector.", call. = FALSE)
  }
  if (!is.numeric(y) || length(y) == 0L || any(!is.finite(y))) {
    stop("y must be a non-empty finite numeric vector.", call. = FALSE)
  }
  if (!is.matrix(z) || !is.numeric(z)) {
    stop("z must be a numeric matrix.", call. = FALSE)
  }
  expected <- c(length(x), length(y))
  if (!identical(dim(z), expected)) {
    stop(
      "dim(z) must equal c(length(x), length(y)); expected ",
      paste(expected, collapse = " x "),
      " but got ",
      paste(dim(z), collapse = " x "),
      ".",
      call. = FALSE
    )
  }
  if (any(!is.finite(z))) {
    stop("z must contain only finite values.", call. = FALSE)
  }
  if (!is.list(metadata)) {
    stop("metadata must be a list.", call. = FALSE)
  }

  alpha <- normalize_grid_alpha(alpha, expected)
  mask <- normalize_grid_mask(mask, expected)

  out <- list(
    x = unname(as.numeric(x)),
    y = unname(as.numeric(y)),
    z = z,
    alpha = alpha,
    mask = mask,
    shape = expected,
    name = name,
    metadata = metadata
  )
  class(out) <- c("ggplot3scene_grid2d", "list")
  out
}

compile_grid2d_data <- function(grid) {
  if (!inherits(grid, "ggplot3scene_grid2d")) {
    stop("grid must be created with grid2d().", call. = FALSE)
  }

  list(
    kind = "grid2d",
    encoding = "json-grid",
    x = unname(as.numeric(grid$x)),
    y = unname(as.numeric(grid$y)),
    z = unname(as.numeric(as.vector(t(grid$z)))),
    alpha = if (is.null(grid$alpha)) NULL else unname(as.numeric(as.vector(t(grid$alpha)))),
    mask = if (is.null(grid$mask)) NULL else unname(as.logical(as.vector(t(grid$mask)))),
    shape = unname(as.integer(grid$shape)),
    order = "row-major",
    metadata = strip_classes(grid$metadata %||% list())
  )
}

scene3d_table <- function(data, schema = NULL, encoding = c("json-columns", "arrow-ipc")) {
  encoding <- match.arg(encoding)
  if (encoding == "arrow-ipc") {
    stop("encoding = 'arrow-ipc' is reserved for a future release and is not implemented yet.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("scene3d_table() currently requires a data.frame.", call. = FALSE)
  }

  if (is.null(schema)) {
    schema <- infer_table_schema(data)
  }

  out <- list(
    kind = "table",
    encoding = encoding,
    schema = schema,
    columns = lapply(data, unname)
  )
  class(out) <- c("ggplot3scene_table", "list")
  out
}

compile_scene3d_table <- function(table) {
  if (!inherits(table, "ggplot3scene_table")) {
    stop("table must be created with scene3d_table().", call. = FALSE)
  }
  strip_classes(table)
}

grid_3d <- function(visible = TRUE, planes = c("xy"),
                    domain = c("full", "positive", "negative", "limits"),
                    breaks = NULL, minor_breaks = NULL,
                    axis_length_fraction = 1,
                    axis_arrows = FALSE) {
  domain <- match.arg(domain)
  allowed_planes <- c("xy", "xz", "yz")
  if (!is.logical(visible) || length(visible) != 1L || is.na(visible)) {
    stop("visible must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(planes) || length(planes) == 0L || any(!planes %in% allowed_planes)) {
    stop("planes must contain one or more of: xy, xz, yz.", call. = FALSE)
  }
  if (!is.null(breaks) && !is.list(breaks)) {
    stop("breaks must be NULL or a list.", call. = FALSE)
  }
  if (!is.null(minor_breaks) && !is.list(minor_breaks)) {
    stop("minor_breaks must be NULL or a list.", call. = FALSE)
  }
  if (!is.numeric(axis_length_fraction) || length(axis_length_fraction) != 1L ||
      !is.finite(axis_length_fraction) || axis_length_fraction <= 0 || axis_length_fraction > 1) {
    stop("axis_length_fraction must be a numeric scalar in (0, 1].", call. = FALSE)
  }
  if (!is.logical(axis_arrows) || length(axis_arrows) != 1L || is.na(axis_arrows)) {
    stop("axis_arrows must be TRUE or FALSE.", call. = FALSE)
  }

  out <- list(
    visible = visible,
    planes = unique(planes),
    domain = domain,
    breaks = breaks,
    minor_breaks = minor_breaks,
    axis_length_fraction = axis_length_fraction,
    axis_arrows = axis_arrows
  )
  class(out) <- c("ggplot3scene_grid3d", "list")
  out
}

alpha_edge_fade <- function(z, width = 0.12, max_alpha = 1, power = 2) {
  z <- check_alpha_z(z)
  if (!is.numeric(width) || length(width) != 1L || !is.finite(width) || width <= 0) {
    stop("width must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(power) || length(power) != 1L || !is.finite(power) || power <= 0) {
    stop("power must be a positive numeric scalar.", call. = FALSE)
  }
  max_alpha <- check_alpha_scalar(max_alpha, "max_alpha")

  nr <- nrow(z)
  nc <- ncol(z)
  rx <- if (nr == 1L) 1 else pmin(seq(0, 1, length.out = nr), rev(seq(0, 1, length.out = nr)))
  ry <- if (nc == 1L) 1 else pmin(seq(0, 1, length.out = nc), rev(seq(0, 1, length.out = nc)))
  distance <- outer(rx, ry, pmin)
  fade <- smoothstep01(distance / width)^power
  clamp01(fade * max_alpha)
}

alpha_density_fade <- function(z, cutoff = 0.05, softness = 0.15, max_alpha = 0.75) {
  z <- check_alpha_z(z)
  cutoff <- check_numeric_scalar(cutoff, "cutoff")
  softness <- check_numeric_scalar(softness, "softness")
  if (softness <= 0) {
    stop("softness must be positive.", call. = FALSE)
  }
  max_alpha <- check_alpha_scalar(max_alpha, "max_alpha")

  zr <- range(z, finite = TRUE)
  scaled <- if (diff(zr) == 0) matrix(1, nrow(z), ncol(z)) else (z - zr[[1]]) / diff(zr)
  fade <- smoothstep01((scaled - cutoff) / softness)
  clamp01(fade * max_alpha)
}

alpha_combined_fade <- function(z, edge_width = 0.12, cutoff = 0.05,
                                softness = 0.15, max_alpha = 0.75) {
  density <- alpha_density_fade(z, cutoff = cutoff, softness = softness, max_alpha = max_alpha)
  edge <- alpha_edge_fade(z, width = edge_width, max_alpha = 1)
  clamp01(density * edge)
}

normalize_grid_alpha <- function(alpha, expected) {
  if (is.null(alpha)) {
    return(NULL)
  }
  if (!is.numeric(alpha)) {
    stop("alpha must be numeric when supplied.", call. = FALSE)
  }
  if (length(alpha) == 1L) {
    alpha <- matrix(alpha, nrow = expected[[1]], ncol = expected[[2]])
  } else if (is.matrix(alpha)) {
    if (!identical(dim(alpha), expected)) {
      stop("alpha matrix must match dim(z).", call. = FALSE)
    }
  } else if (length(alpha) == prod(expected)) {
    alpha <- matrix(alpha, nrow = expected[[1]], ncol = expected[[2]], byrow = TRUE)
  } else {
    stop("alpha must be a scalar, a matrix matching dim(z), or a vector with length length(z).", call. = FALSE)
  }
  if (any(!is.finite(alpha))) {
    stop("alpha must contain only finite values.", call. = FALSE)
  }
  if (any(alpha < 0 | alpha > 1)) {
    stop("alpha values must be in [0, 1].", call. = FALSE)
  }
  alpha
}

normalize_grid_mask <- function(mask, expected) {
  if (is.null(mask)) {
    return(NULL)
  }
  if (!is.logical(mask) || !is.matrix(mask) || !identical(dim(mask), expected)) {
    stop("mask must be a logical matrix matching dim(z).", call. = FALSE)
  }
  mask
}

infer_table_schema <- function(data) {
  list(
    fields = unname(lapply(names(data), function(nm) {
      list(name = nm, type = scene3d_typeof(data[[nm]]))
    }))
  )
}

scene3d_typeof <- function(x) {
  if (is.numeric(x)) {
    return("float64")
  }
  if (is.integer(x)) {
    return("int32")
  }
  if (is.logical(x)) {
    return("bool")
  }
  "string"
}

check_alpha_z <- function(z) {
  if (!is.matrix(z) || !is.numeric(z)) {
    stop("z must be a numeric matrix.", call. = FALSE)
  }
  if (any(!is.finite(z))) {
    stop("z must contain only finite values.", call. = FALSE)
  }
  z
}

check_alpha_scalar <- function(x, name) {
  x <- check_numeric_scalar(x, name)
  if (x < 0 || x > 1) {
    stop(name, " must be in [0, 1].", call. = FALSE)
  }
  x
}

check_numeric_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop(name, " must be a finite numeric scalar.", call. = FALSE)
  }
  x
}

smoothstep01 <- function(x) {
  x <- clamp01(x)
  x * x * (3 - 2 * x)
}

clamp01 <- function(x) {
  dims <- dim(x)
  out <- pmax(0, pmin(1, x))
  if (!is.null(dims)) {
    dim(out) <- dims
  }
  out
}
