ggplot3 <- function(data = NULL, mapping = aes3()) {
  if (!is.null(mapping) && !inherits(mapping, "ggplot3scene_aes")) {
    stop("mapping must be created with aes3().", call. = FALSE)
  }
  plot <- list(
    data = data,
    mapping = mapping,
    layers = list(),
    coord = coord_3d(),
    theme = theme_3d_scientific()
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
  if (inherits(e2, "ggplot3scene_theme")) {
    e1$theme <- merge_theme3d(e1$theme, e2)
    return(e1)
  }
  stop("Cannot add object of class ", paste(class(e2), collapse = "/"), " to a ggplot3scene plot.", call. = FALSE)
}

coord_3d <- function(projection = c("orthographic", "perspective"),
                     position = c(1.8, -2.4, 1.6),
                     target = c(0, 0, 0),
                     up = c(0, 0, 1),
                     zoom = 1,
                     aspect = c(1, 1, 1)) {
  projection <- match.arg(projection)
  check_vec3 <- function(x, name) {
    if (!is.numeric(x) || length(x) != 3L || any(!is.finite(x))) {
      stop(name, " must be a finite numeric vector of length 3.", call. = FALSE)
    }
    x
  }
  if (!is.numeric(zoom) || length(zoom) != 1L || !is.finite(zoom) || zoom <= 0) {
    stop("zoom must be a positive numeric scalar.", call. = FALSE)
  }

  coord <- list(
    projection = projection,
    position = check_vec3(position, "position"),
    target = check_vec3(target, "target"),
    up = check_vec3(up, "up"),
    zoom = zoom,
    aspect = check_vec3(aspect, "aspect")
  )
  class(coord) <- c("ggplot3scene_coord", "list")
  coord
}
